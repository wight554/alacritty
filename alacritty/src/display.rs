// Copyright 2016 Joe Wilm, The Alacritty Project Contributors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

//! The display subsystem including window management, font rasterization, and
//! GPU drawing.
use std::f64;

use glutin::dpi::{PhysicalPosition, PhysicalSize};
use glutin::event_loop::EventLoopProxy;
use glutin::{ContextCurrentState, NotCurrent, PossiblyCurrent, RawContext};
use log::info;

use font::{self, Rasterize};

use alacritty_terminal::config::{Config, Font, StartupMode};
use alacritty_terminal::event::Event;
use alacritty_terminal::index::Line;
use alacritty_terminal::message_bar::Message;
use alacritty_terminal::meter::Meter;
use alacritty_terminal::renderer::rects::{RenderLines, RenderRect};
use alacritty_terminal::renderer::{self, GlyphCache, QuadRenderer};
use alacritty_terminal::term::{cell::Flags, color::Rgb, RenderableCell, SizeInfo};
use alacritty_terminal::term::text_run::TextRunIter;

use crate::window::{self, Window};

#[derive(Debug)]
pub enum Error {
    /// Error with window management
    Window(window::Error),

    /// Error dealing with fonts
    Font(font::Error),

    /// Error in renderer
    Render(renderer::Error),

    /// Error during buffer swap
    ContextError(glutin::ContextError),
}

impl ::std::error::Error for Error {
    fn cause(&self) -> Option<&dyn (::std::error::Error)> {
        match *self {
            Error::Window(ref err) => Some(err),
            Error::Font(ref err) => Some(err),
            Error::Render(ref err) => Some(err),
            Error::ContextError(ref err) => Some(err),
        }
    }

    fn description(&self) -> &str {
        match *self {
            Error::Window(ref err) => err.description(),
            Error::Font(ref err) => err.description(),
            Error::Render(ref err) => err.description(),
            Error::ContextError(ref err) => err.description(),
        }
    }
}

impl ::std::fmt::Display for Error {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        match *self {
            Error::Window(ref err) => err.fmt(f),
            Error::Font(ref err) => err.fmt(f),
            Error::Render(ref err) => err.fmt(f),
            Error::ContextError(ref err) => err.fmt(f),
        }
    }
}

impl From<window::Error> for Error {
    fn from(val: window::Error) -> Error {
        Error::Window(val)
    }
}

impl From<font::Error> for Error {
    fn from(val: font::Error) -> Error {
        Error::Font(val)
    }
}

impl From<renderer::Error> for Error {
    fn from(val: renderer::Error) -> Error {
        Error::Render(val)
    }
}

impl From<glutin::ContextError> for Error {
    fn from(val: glutin::ContextError) -> Error {
        Error::ContextError(val)
    }
}

pub enum RenderEvent {
    Resize((SizeInfo, Option<Font>)),
    Draw(Box<FrameData>),
    Exit,
}

pub struct FrameData {
    pub grid_cells: Vec<RenderableCell>,
    pub message_buffer: Option<Message>,
    pub visual_bell_intensity: f64,
    pub background_color: Rgb,
    pub size_info: SizeInfo,
    pub background_opacity: f32,
    pub font_offset: (i8, i8),
    pub visual_bell_color: Rgb,
    pub render_timer: bool,
}

/// The display wraps a window, font rasterizer, and GPU renderer
pub struct Display<T: ContextCurrentState> {
    context: RawContext<T>,
    renderer: QuadRenderer,
    glyph_cache: GlyphCache,
    meter: Meter,
    size_info: SizeInfo,
    event_proxy: EventLoopProxy<Event>,
}

impl<T: ContextCurrentState> Display<T> {
    /// Get size info about the display
    pub fn size(&self) -> &SizeInfo {
        &self.size_info
    }

    fn new_glyph_cache(
        dpr: f64,
        renderer: &mut QuadRenderer,
        config: &Config,
    ) -> Result<(GlyphCache, f32, f32), Error> {
        let rasterizer = font::Rasterizer::new(dpr as f32, (&config.font).into())?;

        // Initialize glyph cache
        let glyph_cache = {
            info!("Initializing glyph cache...");
            let init_start = ::std::time::Instant::now();

            let cache = renderer
                .with_loader(|mut api| GlyphCache::new(rasterizer, &config.font, &mut api))?;

            let stop = init_start.elapsed();
            let stop_f = stop.as_secs() as f64 + f64::from(stop.subsec_nanos()) / 1_000_000_000f64;
            info!("... finished initializing glyph cache in {}s", stop_f);

            cache
        };

        // Need font metrics to resize the window properly. This suggests to me the
        // font metrics should be computed before creating the window in the first
        // place so that a resize is not needed.
        let (cw, ch) = GlyphCache::compute_cell_size(config, &glyph_cache.font_metrics());

        Ok((glyph_cache, cw, ch))
    }
}

impl Display<PossiblyCurrent> {
    pub fn new(
        config: &Config,
        window: &mut Window,
        context: RawContext<PossiblyCurrent>,
        event_proxy: EventLoopProxy<Event>,
    ) -> Result<Display<PossiblyCurrent>, Error> {
        let dpr = window.hidpi_factor();
        info!("Device pixel ratio: {}", dpr);

        // get window properties for initializing the other subsystems
        let mut viewport_size = window.inner_size().to_physical(dpr);

        // Create renderer
        let mut renderer = QuadRenderer::new()?;

        let (glyph_cache, cell_width, cell_height) =
            Self::new_glyph_cache(dpr, &mut renderer, config)?;

        let mut padding_x = f32::from(config.window.padding.x) * dpr as f32;
        let mut padding_y = f32::from(config.window.padding.y) * dpr as f32;

        if let Some((width, height)) =
            GlyphCache::calculate_dimensions(config, dpr, cell_width, cell_height)
        {
            let PhysicalSize { width: w, height: h } = window.inner_size().to_physical(dpr);
            if (w - width).abs() < f64::EPSILON && (h - height).abs() < f64::EPSILON {
                info!("Estimated DPR correctly, skipping resize");
            } else {
                viewport_size = PhysicalSize::new(width, height);
                window.set_inner_size(viewport_size.to_logical(dpr));
            }
        } else if config.window.dynamic_padding {
            // Make sure additional padding is spread evenly
            padding_x = dynamic_padding(padding_x, viewport_size.width as f32, cell_width);
            padding_y = dynamic_padding(padding_y, viewport_size.height as f32, cell_height);
        }

        padding_x = padding_x.floor();
        padding_y = padding_y.floor();

        info!("Cell Size: {} x {}", cell_width, cell_height);
        info!("Padding: {} x {}", padding_x, padding_y);

        let size_info = SizeInfo {
            dpr,
            width: viewport_size.width as f32,
            height: viewport_size.height as f32,
            cell_width: cell_width as f32,
            cell_height: cell_height as f32,
            padding_x: padding_x as f32,
            padding_y: padding_y as f32,
        };

        // Update OpenGL projection
        renderer.resize(size_info);

        // Clear screen
        let background_color = config.colors.primary.background;
        let background_opacity = config.background_opacity();
        let font_offset = (config.font.offset.x, config.font.offset.y);
        renderer.with_api(&size_info, background_opacity, font_offset, |api| {
            api.clear(background_color);
        });

        // We should call `clear` when window is offscreen, so when `window.show()` happens it
        // would be with background color instead of uninitialized surface.
        context.swap_buffers()?;

        window.set_visible(true);

        // Set window position
        //
        // TODO: replace `set_position` with `with_position` once available
        // Upstream issue: https://github.com/tomaka/winit/issues/806
        if let Some(position) = config.window.position {
            let physical = PhysicalPosition::from((position.x, position.y));
            let logical = physical.to_logical(window.hidpi_factor());
            window.set_outer_position(logical);
        }

        #[allow(clippy::single_match)]
        match config.window.startup_mode() {
            StartupMode::Fullscreen => window.set_fullscreen(true),
            #[cfg(target_os = "macos")]
            StartupMode::SimpleFullscreen => window.set_simple_fullscreen(true),
            #[cfg(not(any(target_os = "macos", windows)))]
            StartupMode::Maximized => window.set_maximized(true),
            _ => (),
        }

        Ok(Display { context, renderer, glyph_cache, meter: Meter::new(), size_info, event_proxy })
    }

    /// Process terminal size changes
    pub fn resize(&mut self, (size_info, font): (SizeInfo, Option<Font>)) {
        if let Some(font) = font {
            let cache = &mut self.glyph_cache;
            self.renderer.with_loader(|mut api| {
                let _ = cache.update_font_size(font, size_info.dpr, &mut api);
            });
        }

        let physical = PhysicalSize::new(f64::from(size_info.width), f64::from(size_info.height));
        self.renderer.resize(size_info);
        self.context.resize(physical);
    }

    /// Draw the screen
    ///
    /// A reference to Term whose state is being drawn must be provided.
    ///
    /// This call may block if vsync is enabled
    pub fn draw(&mut self, render_update: FrameData) {
        let FrameData {
            visual_bell_intensity,
            background_color,
            message_buffer,
            grid_cells,
            size_info,
            background_opacity: bg_opacity,
            font_offset,
            visual_bell_color,
            render_timer,
        } = render_update;
        let metrics = self.glyph_cache.font_metrics();

        self.renderer.with_api(&size_info, bg_opacity, font_offset, |api| {
            api.clear(background_color);
        });

        {
            let glyph_cache = &mut self.glyph_cache;
            let mut lines = RenderLines::new();

            // Draw grid
            self.renderer.with_api(&size_info, bg_opacity, font_offset, |mut api| {
                // Iterate over each contiguous block of text
                for text_run in TextRunIter::new(
                    grid_cells
                        .into_iter()
                        // Logic for WIDE_CHAR is handled internally by TextRun
                        // So we no longer need WIDE_CHAR_SPACER at this point.
                        .filter(|rc| !rc.flags.contains(Flags::WIDE_CHAR_SPACER)),
                ) {
                    // Update underline/strikeout
                    lines.update(&text_run);

                    // Draw text run
                    api.render_text_run(text_run, glyph_cache);
                }
            });

            let mut rects = lines.into_rects(&metrics, &size_info);

            if let Some(message) = message_buffer {
                let text = message.text(&size_info);

                // Create a new rectangle for the background
                let start_line = size_info.lines().0 - text.len();
                let y = size_info.padding_y + size_info.cell_height * start_line as f32;
                rects.push(RenderRect::new(
                    0.,
                    y,
                    size_info.width,
                    size_info.height - y,
                    message.color(),
                ));

                // Draw rectangles including the new background
                self.renderer.draw_rects(
                    &size_info,
                    visual_bell_color,
                    visual_bell_intensity,
                    rects,
                );

                // Relay messages to the user
                let mut offset = 1;
                for message_text in text.iter().rev() {
                    self.renderer.with_api(&size_info, bg_opacity, font_offset, |mut api| {
                        api.render_string(
                            &message_text,
                            Line(size_info.lines().saturating_sub(offset)),
                            glyph_cache,
                            None,
                        );
                    });
                    offset += 1;
                }
            } else {
                // Draw rectangles
                self.renderer.draw_rects(
                    &size_info,
                    visual_bell_color,
                    visual_bell_intensity,
                    rects,
                );
            }

            // Draw render timer
            if render_timer {
                let timing = format!("{:.3} usec", self.meter.average());
                let color = Rgb { r: 0xd5, g: 0x4e, b: 0x53 };
                self.renderer.with_api(&size_info, bg_opacity, font_offset, |mut api| {
                    api.render_string(&timing[..], size_info.lines() - 2, glyph_cache, Some(color));
                });
            }
        }

        self.context.swap_buffers().expect("swap buffers");
    }
}

/// Calculate padding to spread it evenly around the terminal content
#[inline]
pub fn dynamic_padding(padding: f32, dimension: f32, cell_dimension: f32) -> f32 {
    padding + ((dimension - 2. * padding) % cell_dimension) / 2.
}

impl From<Display<PossiblyCurrent>> for Display<NotCurrent> {
    fn from(display: Display<PossiblyCurrent>) -> Self {
        unsafe {
            Display {
                context: display.context.make_not_current().expect("disabling context"),
                renderer: display.renderer,
                glyph_cache: display.glyph_cache,
                meter: display.meter,
                size_info: display.size_info,
                event_proxy: display.event_proxy,
            }
        }
    }
}

impl From<Display<NotCurrent>> for Display<PossiblyCurrent> {
    fn from(display: Display<NotCurrent>) -> Self {
        unsafe {
            Display {
                context: display.context.make_current().expect("enabling context"),
                renderer: display.renderer,
                glyph_cache: display.glyph_cache,
                meter: display.meter,
                size_info: display.size_info,
                event_proxy: display.event_proxy,
            }
        }
    }
}
