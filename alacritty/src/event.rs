//! Process window events
use std::borrow::Cow;
use std::cmp::max;
use std::env;
#[cfg(unix)]
use std::fs;
use std::fs::File;
use std::io::Write;
use std::sync::{mpsc, Arc};
use std::time::Instant;

use glutin::dpi::PhysicalSize;
use glutin::event::{ElementState, Event as GlutinEvent, MouseButton};
use glutin::event_loop::{ControlFlow, EventLoop};
use glutin::platform::desktop::EventLoopExtDesktop;
use log::{debug, info, warn};
use serde_json as json;

use font::Size;

use alacritty_terminal::clipboard::ClipboardType;
use alacritty_terminal::config::Config;
use alacritty_terminal::event::OnResize;
use alacritty_terminal::event::{Event, EventListener, Notify};
use alacritty_terminal::grid::Scroll;
use alacritty_terminal::index::{Column, Line, Point, Side};
use alacritty_terminal::message_bar::{Message, MessageBuffer};
use alacritty_terminal::renderer::GlyphCache;
use alacritty_terminal::selection::Selection;
use alacritty_terminal::sync::FairMutex;
use alacritty_terminal::term::cell::Cell;
use alacritty_terminal::term::{SizeInfo, Term};
use alacritty_terminal::tty;
use alacritty_terminal::util::{limit, start_daemon};

use crate::config;
use crate::display::{self, FrameData, RenderEvent};
use crate::input::{self, ActionContext as _, Modifiers};
use crate::window::Window;

/// Font size change interval
pub const FONT_SIZE_STEP: f32 = 0.5;

#[derive(Default, Copy, Clone, Debug, PartialEq)]
pub struct Resize {
    dimensions: Option<PhysicalSize>,
    message_buffer: Option<()>,
    font_size: Option<Size>,
}

impl Resize {
    fn is_empty(&self) -> bool {
        self.dimensions.is_none() && self.font_size.is_none() && self.message_buffer.is_none()
    }
}

pub struct ActionContext<'a, N, T> {
    pub notifier: &'a mut N,
    pub terminal: &'a mut Term<T>,
    pub size_info: &'a mut SizeInfo,
    pub mouse: &'a mut Mouse,
    pub received_count: &'a mut usize,
    pub suppress_chars: &'a mut bool,
    pub modifiers: &'a mut Modifiers,
    pub window: &'a mut Window,
    pub message_buffer: &'a mut MessageBuffer,
    pub font_size: &'a mut Size,
    pub resize_pending: &'a mut Resize,
    original_font_size: Size,
}

impl<'a, N: Notify + 'a, T: EventListener> input::ActionContext<T> for ActionContext<'a, N, T> {
    fn write_to_pty<B: Into<Cow<'static, [u8]>>>(&mut self, val: B) {
        self.notifier.notify(val);
    }

    fn size_info(&self) -> SizeInfo {
        *self.size_info
    }

    fn scroll(&mut self, scroll: Scroll) {
        self.terminal.scroll_display(scroll);

        if let ElementState::Pressed = self.mouse().left_button_state {
            let (x, y) = (self.mouse().x, self.mouse().y);
            let size_info = self.size_info();
            let point = size_info.pixels_to_coords(x, y);
            let cell_side = self.mouse().cell_side;
            self.update_selection(Point { line: point.line, col: point.col }, cell_side);
        }
    }

    fn copy_selection(&mut self, ty: ClipboardType) {
        if let Some(selected) = self.terminal.selection_to_string() {
            if !selected.is_empty() {
                self.terminal.clipboard().store(ty, selected);
            }
        }
    }

    fn selection_is_empty(&self) -> bool {
        self.terminal.selection().as_ref().map(Selection::is_empty).unwrap_or(true)
    }

    fn clear_selection(&mut self) {
        *self.terminal.selection_mut() = None;
        self.terminal.dirty = true;
    }

    fn update_selection(&mut self, point: Point, side: Side) {
        let point = self.terminal.visible_to_buffer(point);

        // Update selection if one exists
        if let Some(ref mut selection) = self.terminal.selection_mut() {
            selection.update(point, side);
        }

        self.terminal.dirty = true;
    }

    fn simple_selection(&mut self, point: Point, side: Side) {
        let point = self.terminal.visible_to_buffer(point);
        *self.terminal.selection_mut() = Some(Selection::simple(point, side));
        self.terminal.dirty = true;
    }

    fn block_selection(&mut self, point: Point, side: Side) {
        let point = self.terminal.visible_to_buffer(point);
        *self.terminal.selection_mut() = Some(Selection::block(point, side));
        self.terminal.dirty = true;
    }

    fn semantic_selection(&mut self, point: Point) {
        let point = self.terminal.visible_to_buffer(point);
        *self.terminal.selection_mut() = Some(Selection::semantic(point));
        self.terminal.dirty = true;
    }

    fn line_selection(&mut self, point: Point) {
        let point = self.terminal.visible_to_buffer(point);
        *self.terminal.selection_mut() = Some(Selection::lines(point));
        self.terminal.dirty = true;
    }

    fn mouse_coords(&self) -> Option<Point> {
        let x = self.mouse.x as usize;
        let y = self.mouse.y as usize;

        if self.size_info.contains_point(x, y, true) {
            Some(self.size_info.pixels_to_coords(x, y))
        } else {
            None
        }
    }

    #[inline]
    fn mouse_mut(&mut self) -> &mut Mouse {
        self.mouse
    }

    #[inline]
    fn mouse(&self) -> &Mouse {
        self.mouse
    }

    #[inline]
    fn received_count(&mut self) -> &mut usize {
        &mut self.received_count
    }

    #[inline]
    fn suppress_chars(&mut self) -> &mut bool {
        &mut self.suppress_chars
    }

    #[inline]
    fn modifiers(&mut self) -> &mut Modifiers {
        &mut self.modifiers
    }

    #[inline]
    fn window(&self) -> &Window {
        self.window
    }

    #[inline]
    fn window_mut(&mut self) -> &mut Window {
        self.window
    }

    #[inline]
    fn terminal(&self) -> &Term<T> {
        self.terminal
    }

    #[inline]
    fn terminal_mut(&mut self) -> &mut Term<T> {
        self.terminal
    }

    fn spawn_new_instance(&mut self) {
        let alacritty = env::args().next().unwrap();

        #[cfg(unix)]
        let args = {
            #[cfg(not(target_os = "freebsd"))]
            let proc_prefix = "";
            #[cfg(target_os = "freebsd")]
            let proc_prefix = "/compat/linux";
            let link_path = format!("{}/proc/{}/cwd", proc_prefix, tty::child_pid());
            if let Ok(path) = fs::read_link(link_path) {
                vec!["--working-directory".into(), path]
            } else {
                Vec::new()
            }
        };
        #[cfg(not(unix))]
        let args: Vec<String> = Vec::new();

        match start_daemon(&alacritty, &args) {
            Ok(_) => debug!("Started new Alacritty process: {} {:?}", alacritty, args),
            Err(_) => warn!("Unable to start new Alacritty process: {} {:?}", alacritty, args),
        }
    }

    fn change_font_size(&mut self, delta: f32) {
        *self.font_size = max(*self.font_size + Size::new(delta), Size::new(FONT_SIZE_STEP));
        self.resize_pending.font_size = Some(*self.font_size);
        self.terminal.dirty = true;
    }

    fn reset_font_size(&mut self) {
        *self.font_size = self.original_font_size;
        self.resize_pending.font_size = Some(*self.font_size);
        self.terminal.dirty = true;
    }

    fn pop_message(&mut self) {
        self.resize_pending.message_buffer = Some(());
        self.message_buffer.pop();
    }

    fn message(&self) -> Option<&Message> {
        self.message_buffer.message()
    }
}

pub enum ClickState {
    None,
    Click,
    DoubleClick,
    TripleClick,
}

/// State of the mouse
pub struct Mouse {
    pub x: usize,
    pub y: usize,
    pub left_button_state: ElementState,
    pub middle_button_state: ElementState,
    pub right_button_state: ElementState,
    pub last_click_timestamp: Instant,
    pub click_state: ClickState,
    pub scroll_px: i32,
    pub line: Line,
    pub column: Column,
    pub cell_side: Side,
    pub lines_scrolled: f32,
    pub block_url_launcher: bool,
    pub last_button: MouseButton,
}

impl Default for Mouse {
    fn default() -> Mouse {
        Mouse {
            x: 0,
            y: 0,
            last_click_timestamp: Instant::now(),
            left_button_state: ElementState::Released,
            middle_button_state: ElementState::Released,
            right_button_state: ElementState::Released,
            click_state: ClickState::None,
            scroll_px: 0,
            line: Line(0),
            column: Column(0),
            cell_side: Side::Left,
            lines_scrolled: 0.0,
            block_url_launcher: false,
            last_button: MouseButton::Other(0),
        }
    }
}

/// The event processor
///
/// Stores some state from received events and dispatches actions when they are
/// triggered.
pub struct Processor<N> {
    notifier: N,
    mouse: Mouse,
    size_info: SizeInfo,
    received_count: usize,
    suppress_chars: bool,
    modifiers: Modifiers,
    font_size: Size,
    config: Config,
    pty_resize_handle: Box<dyn OnResize>,
    message_buffer: MessageBuffer,
    render_tx: mpsc::Sender<RenderEvent>,
    window: Window,
}

impl<N: Notify> Processor<N> {
    /// Create a new event processor
    ///
    /// Takes a writer which is expected to be hooked up to the write end of a
    /// pty.
    pub fn new(
        notifier: N,
        size_info: SizeInfo,
        pty_resize_handle: Box<dyn OnResize>,
        message_buffer: MessageBuffer,
        render_tx: mpsc::Sender<RenderEvent>,
        window: Window,
        config: Config,
    ) -> Processor<N> {
        Processor {
            notifier,
            mouse: Default::default(),
            size_info,
            received_count: 0,
            suppress_chars: false,
            modifiers: Default::default(),
            font_size: config.font.size,
            config,
            pty_resize_handle,
            message_buffer,
            render_tx,
            window,
        }
    }

    /// Check if an event is irrelevant and can be skipped
    fn skip_event(event: &GlutinEvent<Event>) -> bool {
        match event {
            GlutinEvent::UserEvent(Event::Exit) => true,
            GlutinEvent::WindowEvent { event, .. } => {
                use glutin::event::WindowEvent::*;
                match event {
                    TouchpadPressure { .. }
                    | CursorEntered { .. }
                    | CursorLeft { .. }
                    | AxisMotion { .. }
                    | HoveredFileCancelled
                    | Destroyed
                    | HoveredFile(_)
                    | Touch(_)
                    | Moved(_) => true,
                    _ => false,
                }
            },
            GlutinEvent::DeviceEvent { .. }
            | GlutinEvent::Suspended { .. }
            | GlutinEvent::NewEvents { .. }
            | GlutinEvent::EventsCleared
            | GlutinEvent::LoopDestroyed => true,
            _ => false,
        }
    }

    /// Handle events from glutin
    ///
    /// Doesn't take self mutably due to borrow checking. Kinda uggo but w/e.
    fn handle_event<T>(
        event: GlutinEvent<Event>,
        processor: &mut input::Processor<T, ActionContext<N, T>>,
        redraw_requested: &mut bool,
    ) where
        T: EventListener,
    {
        match event {
            GlutinEvent::UserEvent(event) => match event {
                Event::CursorIcon(cursor) => processor.ctx.window.set_mouse_cursor(cursor),
                Event::Title(title) => processor.ctx.window.set_title(&title),
                Event::Wakeup => processor.ctx.terminal.dirty = true,
                Event::Urgent => {
                    processor.ctx.window.set_urgent(!processor.ctx.terminal.is_focused)
                },
                Event::RedrawRequest => *redraw_requested = true,
                Event::ConfigReload(path) => {
                    processor.ctx.message_buffer.remove_topic(config::SOURCE_FILE_PATH);
                    processor.ctx.resize_pending.message_buffer = Some(());

                    if let Ok(config) = config::reload_from(&path) {
                        processor.ctx.terminal.update_config(&config);
                        *processor.config = config;
                        processor.ctx.terminal.dirty = true;
                    }
                },
                Event::Message(message) => {
                    processor.ctx.message_buffer.push(message);
                    processor.ctx.resize_pending.message_buffer = Some(());
                    processor.ctx.terminal.dirty = true;
                },
                Event::Exit => (),
            },
            GlutinEvent::WindowEvent { event, .. } => {
                use glutin::event::WindowEvent::*;
                match event {
                    CloseRequested => processor.ctx.terminal.exit(),
                    Resized(lsize) => {
                        let psize = lsize.to_physical(processor.ctx.size_info.dpr);
                        processor.ctx.resize_pending.dimensions = Some(psize);
                        processor.ctx.terminal.dirty = true;
                    },
                    KeyboardInput { input, .. } => {
                        processor.process_key(input);
                        if input.state == ElementState::Pressed {
                            // Hide cursor while typing
                            if processor.config.mouse.hide_when_typing {
                                processor.ctx.window.set_mouse_visible(false);
                            }
                        }
                    },
                    ReceivedCharacter(c) => processor.received_char(c),
                    MouseInput { state, button, modifiers, .. } => {
                        if !cfg!(target_os = "macos") || processor.ctx.terminal.is_focused {
                            processor.ctx.window.set_mouse_visible(true);
                            processor.mouse_input(state, button, modifiers);
                            processor.ctx.terminal.dirty = true;
                        }
                    },
                    CursorMoved { position: lpos, modifiers, .. } => {
                        let (x, y) = lpos.to_physical(processor.ctx.size_info.dpr).into();
                        let x: i32 = limit(x, 0, processor.ctx.size_info.width as i32);
                        let y: i32 = limit(y, 0, processor.ctx.size_info.height as i32);

                        processor.ctx.window.set_mouse_visible(true);
                        processor.mouse_moved(x as usize, y as usize, modifiers);
                    },
                    MouseWheel { delta, phase, modifiers, .. } => {
                        processor.ctx.window.set_mouse_visible(true);
                        processor.on_mouse_wheel(delta, phase, modifiers);
                    },
                    Focused(is_focused) => {
                        processor.ctx.terminal.is_focused = is_focused;
                        processor.ctx.terminal.dirty = true;

                        if is_focused {
                            processor.ctx.window.set_urgent(false);
                        } else {
                            processor.ctx.window.set_mouse_visible(true);
                        }

                        processor.on_focus_change(is_focused);
                    },
                    DroppedFile(path) => {
                        let path: String = path.to_string_lossy().into();
                        processor.ctx.write_to_pty(path.into_bytes());
                    },
                    HiDpiFactorChanged(dpr) => {
                        let dpr_change = (dpr / processor.ctx.size_info.dpr) as f32;
                        let resize_pending = &mut processor.ctx.resize_pending;

                        // Push current font to update its DPR
                        resize_pending.font_size = Some(*processor.ctx.font_size);

                        // Scale window dimensions with new DPR
                        let old_width = processor.ctx.size_info.width;
                        let old_height = processor.ctx.size_info.height;
                        let dimensions = resize_pending.dimensions.get_or_insert_with(|| {
                            PhysicalSize::new(f64::from(old_width), f64::from(old_height))
                        });
                        dimensions.width *= f64::from(dpr_change);
                        dimensions.height *= f64::from(dpr_change);

                        processor.ctx.terminal.dirty = true;
                        processor.ctx.size_info.dpr = dpr;
                    },
                    RedrawRequested => processor.ctx.terminal.dirty = true,
                    TouchpadPressure { .. }
                    | CursorEntered { .. }
                    | CursorLeft { .. }
                    | AxisMotion { .. }
                    | HoveredFileCancelled
                    | Destroyed
                    | HoveredFile(_)
                    | Touch(_)
                    | Moved(_) => (),
                }
            },
            GlutinEvent::DeviceEvent { .. }
            | GlutinEvent::Suspended { .. }
            | GlutinEvent::NewEvents { .. }
            | GlutinEvent::EventsCleared
            | GlutinEvent::Resumed
            | GlutinEvent::LoopDestroyed => (),
        }
    }

    /// Run the event loop.
    pub fn process_events<T>(
        &mut self,
        terminal: Arc<FairMutex<Term<T>>>,
        mut event_loop: EventLoop<Event>,
    ) where
        T: EventListener,
    {
        let mut event_queue = Vec::new();
        let mut redraw_requested = false;

        event_loop.run_return(|event, _event_loop, control_flow| {
            if self.config.debug.print_events {
                info!("glutin event: {:?}", event);
            }

            match (&event, tty::process_should_exit()) {
                // Check for shutdown
                (GlutinEvent::UserEvent(Event::Exit), _) | (_, true) => {
                    *control_flow = ControlFlow::Exit;
                    return;
                },
                // Process events
                (GlutinEvent::EventsCleared, _) => *control_flow = ControlFlow::Wait,
                // Buffer events
                _ => {
                    *control_flow = ControlFlow::Poll;
                    if !Self::skip_event(&event) {
                        event_queue.push(event);
                    }
                    return;
                },
            }

            let mut terminal = terminal.lock();

            let mut resize_pending = Resize::default();

            let context = ActionContext {
                terminal: &mut terminal,
                notifier: &mut self.notifier,
                mouse: &mut self.mouse,
                size_info: &mut self.size_info,
                received_count: &mut self.received_count,
                suppress_chars: &mut self.suppress_chars,
                modifiers: &mut self.modifiers,
                font_size: &mut self.font_size,
                original_font_size: self.config.font.size,
                message_buffer: &mut self.message_buffer,
                resize_pending: &mut resize_pending,
                window: &mut self.window,
            };
            let mut processor = input::Processor::new(context, &mut self.config);

            for event in event_queue.drain(..) {
                Processor::handle_event(event, &mut processor, &mut redraw_requested);
            }

            // Process resize events
            if !resize_pending.is_empty() {
                self.handle_resize(&mut terminal, resize_pending);
            }

            // Send updates to render thread
            if terminal.dirty && redraw_requested {
                // Clear dirty flag
                terminal.dirty = !terminal.visual_bell.completed();
                redraw_requested = false;

                // Update IME position
                #[cfg(not(windows))]
                self.window.update_ime_position(&terminal, &self.size_info);

                // Request redraw
                self.render_tx
                    .send(RenderEvent::Draw(Box::new(FrameData {
                        visual_bell_intensity: terminal.visual_bell.intensity(),
                        background_color: terminal.background_color(),
                        message_buffer: self.message_buffer.message().map(|m| m.to_owned()),
                        grid_cells: terminal.renderable_cells(&self.config).collect(),
                        size_info: self.size_info,
                        background_opacity: self.config.background_opacity(),
                        font_offset: (self.config.font.offset.x, self.config.font.offset.y),
                        visual_bell_color: self.config.visual_bell.color,
                        render_timer: self.config.render_timer(),
                    })))
                    .expect("send render update");
            }
        });

        self.render_tx.send(RenderEvent::Exit).expect("send render exit");

        // Write ref tests to disk
        self.write_ref_test_results(&terminal.lock());
    }

    /// Process resize events
    fn handle_resize<T>(&mut self, terminal: &mut Term<T>, resize_pending: Resize)
    where
        T: EventListener,
    {
        // Update the cell metrics with the new glyph dimensions
        if let Some(size) = resize_pending.font_size {
            if let Ok(metrics) = GlyphCache::static_metrics(&self.config, size, self.size_info.dpr)
            {
                // Update cell size
                let (cell_width, cell_height) =
                    GlyphCache::compute_cell_size(&self.config, &metrics);
                self.size_info.cell_width = cell_width;
                self.size_info.cell_height = cell_height;
            }
        }

        // Update the window dimensions
        if let Some(size) = resize_pending.dimensions {
            self.size_info.width = size.width as f32;
            self.size_info.height = size.height as f32;
        }

        let dpr = self.size_info.dpr;
        let width = self.size_info.width;
        let height = self.size_info.height;
        let cell_width = self.size_info.cell_width;
        let cell_height = self.size_info.cell_height;

        // Recalculate padding
        let mut padding_x = f32::from(self.config.window.padding.x) * dpr as f32;
        let mut padding_y = f32::from(self.config.window.padding.y) * dpr as f32;

        if self.config.window.dynamic_padding {
            padding_x = display::dynamic_padding(padding_x, width, cell_width);
            padding_y = display::dynamic_padding(padding_y, height, cell_height);
        }

        self.size_info.padding_x = padding_x.floor() as f32;
        self.size_info.padding_y = padding_y.floor() as f32;

        let mut pty_size = self.size_info;

        // Subtract message bar lines from pty size
        if resize_pending.message_buffer.is_some() {
            let lines =
                self.message_buffer.message().map(|m| m.text(&self.size_info).len()).unwrap_or(0);
            pty_size.height -= pty_size.cell_height * lines as f32;
        }

        // Resize PTY
        self.pty_resize_handle.on_resize(&pty_size);

        // Resize terminal
        terminal.resize(&pty_size);

        // Resize renderer
        let font = resize_pending.font_size.map(|fs| self.config.font.clone().with_size(fs));
        let _ = self.render_tx.send(RenderEvent::Resize((self.size_info, font)));
    }

    // Write the ref test results to the disk
    pub fn write_ref_test_results<T>(&self, terminal: &Term<T>) {
        if !self.config.debug.ref_test {
            return;
        }

        // dump grid state
        let mut grid = terminal.grid().clone();
        grid.initialize_all(&Cell::default());
        grid.truncate();

        let serialized_grid = json::to_string(&grid).expect("serialize grid");

        let serialized_size = json::to_string(&self.size_info).expect("serialize size");

        let serialized_config = format!("{{\"history_size\":{}}}", grid.history_size());

        File::create("./grid.json")
            .and_then(|mut f| f.write_all(serialized_grid.as_bytes()))
            .expect("write grid.json");

        File::create("./size.json")
            .and_then(|mut f| f.write_all(serialized_size.as_bytes()))
            .expect("write size.json");

        File::create("./config.json")
            .and_then(|mut f| f.write_all(serialized_config.as_bytes()))
            .expect("write config.json");
    }
}