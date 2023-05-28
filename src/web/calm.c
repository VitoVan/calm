#include <SDL2/SDL.h>
#include <SDL2/SDL_mixer.h>
#include <cairo/cairo.h>
#include <emscripten.h>
#include <stdbool.h>
#include <stdio.h>

SDL_Window *window;
SDL_Renderer *renderer;
SDL_Texture *texture;
cairo_surface_t *cr_surface = NULL;

int fps = 0;

int window_width = 600;
int window_height = 150;

int renderer_width;
int renderer_height;

double cairo_x_multiplier;
double cairo_y_multiplier;

extern int js_on_keydown(int sym);
extern int js_on_keyup(int sym);
extern int js_on_mousemotion(int x, int y);
extern int js_on_mousebuttonup(int button, int x, int y, int clicks);
extern int js_on_mousebuttondown(int button, int x, int y, int clicks);
extern int js_on_fingermotion(float x, float y, float dx, float dy,
                              float pressure, int fingerId);
extern int js_on_fingerup(float x, float y, float dx, float dy, float pressure,
                          int fingerId);
extern int js_on_fingerdown(float x, float y, float dx, float dy,
                            float pressure, int fingerId);
extern int js_on_windowenter();
extern int js_on_windowleave();
extern int js_on_windowresized(int w, int h);
extern bool js_get_calm_redraw();

extern int js_draw();
extern int js_think();

EM_JS(void, set_cairo_context_to_js, (cairo_t * cr), { window._cr = cr; });

void config(const char *title) { SDL_SetWindowTitle(window, title); }

void draw(cairo_t *cr) {
  cairo_set_source_rgb(cr, 1, 1, 1);
  cairo_paint(cr);

  set_cairo_context_to_js(cr);
  if (js_draw() == 42) {
    return;
  }

  cairo_set_source_rgba(cr, 0.89, 0.12, 0.17, 1);
  cairo_paint(cr);
  cairo_set_source_rgba(cr, 1, 1, 1, 1);
  cairo_move_to(cr, 30, 100);
  cairo_set_font_size(cr, 84);
  cairo_show_text(cr, "DON'T PANIC");
}

void idle_handler() {
  if (cr_surface == NULL || js_get_calm_redraw()) {
    int *pixels = NULL;
    int pitch;
    SDL_LockTexture(texture, NULL, (void **)&pixels, &pitch);

    cr_surface = cairo_image_surface_create_for_data(
        (unsigned char *)pixels, CAIRO_FORMAT_ARGB32, renderer_width,
        renderer_height, pitch);

    cairo_surface_set_device_scale(cr_surface, cairo_x_multiplier,
                                   cairo_y_multiplier);

    cairo_t *cr = cairo_create(cr_surface);

    draw(cr);

    SDL_UnlockTexture(texture);
    SDL_RenderCopy(renderer, texture, NULL, NULL);
    SDL_RenderPresent(renderer);

    cairo_destroy(cr);
    cairo_surface_destroy(cr_surface);
  }
}

void resize_handler() {
  SDL_GetRendererOutputSize(renderer, &renderer_width, &renderer_height);

  cairo_x_multiplier = (double)renderer_width / (double)window_width;
  cairo_y_multiplier = (double)renderer_height / (double)window_height;

  SDL_DestroyTexture(texture);
  texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888,
                              SDL_TEXTUREACCESS_STREAMING, renderer_width,
                              renderer_height);
  SDL_SetTextureBlendMode(texture, SDL_BLENDMODE_BLEND);
}

void handler() {
  SDL_Event event;
  while (SDL_PollEvent(&event)) {
    switch (event.type) {
      case SDL_KEYDOWN:
        js_on_keydown(event.key.keysym.scancode);
        break;
      case SDL_KEYUP:
        js_on_keyup(event.key.keysym.scancode);
        break;
      case SDL_MOUSEMOTION:
        js_on_mousemotion(event.motion.x, event.motion.y);
        break;
      case SDL_MOUSEBUTTONUP:
        js_on_mousebuttonup(event.button.button, event.button.x, event.button.y,
                            event.button.clicks);
        break;
      case SDL_MOUSEBUTTONDOWN:
        js_on_mousebuttondown(event.button.button, event.button.x,
                              event.button.y, event.button.clicks);
        break;
      case SDL_FINGERMOTION:
        js_on_fingermotion(event.tfinger.x, event.tfinger.y, event.tfinger.dx,
                           event.tfinger.dy, event.tfinger.pressure,
                           event.tfinger.fingerId);
        break;
      case SDL_FINGERUP:
        js_on_fingerup(event.tfinger.x, event.tfinger.y, event.tfinger.dx,
                       event.tfinger.dy, event.tfinger.pressure,
                       event.tfinger.fingerId);
        break;
      case SDL_FINGERDOWN:
        js_on_fingerdown(event.tfinger.x, event.tfinger.y, event.tfinger.dx,
                         event.tfinger.dy, event.tfinger.pressure,
                         event.tfinger.fingerId);
        break;
      case SDL_WINDOWEVENT:
        switch (event.window.event) {
          case SDL_WINDOWEVENT_ENTER:
            js_on_windowenter();
            break;
          case SDL_WINDOWEVENT_LEAVE:
            js_on_windowleave();
            break;
          case SDL_WINDOWEVENT_RESIZED:
            js_on_windowresized(event.window.data1, event.window.data2);
            resize_handler();
            break;
        }
        break;
      case SDL_QUIT:
        emscripten_cancel_main_loop();
        break;
      default:
        break;
    }
  }
  js_think();
  idle_handler();
}

void run_main_loop() { emscripten_set_main_loop(handler, fps, true); }

int main(int argc, char *argv[]) {
  // this is of course not useful if we are not using pango (yet)
  setenv("PANGOCAIRO_BACKEND", "fontconfig", 1);
  setenv("CALM_APP_DIR", "/usr/share/", 1);

  char *env_calm_window_width = getenv("CALM_WINDOW_WIDTH");
  char *env_calm_window_height = getenv("CALM_WINDOW_HEIGHT");
  if (env_calm_window_width != NULL || env_calm_window_height != NULL) {
    window_width = atoi(env_calm_window_width);
    window_height = atoi(env_calm_window_height);
  }

  char *env_calm_fps = getenv("CALM_FPS");

  if (env_calm_fps != NULL) {
    fps = atoi(env_calm_fps);
  }

  SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO);

  window =
      SDL_CreateWindow("CALM", 0, 0, window_width, window_height,
                       SDL_WINDOW_SHOWN | SDL_WINDOW_OPENGL |
                           // the following two must be present at the same time
                           // otherwise emscripten canvas will do weird things
                           SDL_WINDOW_RESIZABLE | SDL_WINDOW_ALLOW_HIGHDPI);

  SDL_SetHint(SDL_HINT_RENDER_DRIVER, "opengles2");

  renderer =
      SDL_CreateRenderer(window, -1,
                         SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC |
                             SDL_RENDERER_TARGETTEXTURE);

  SDL_RendererInfo rendererInfo;
  SDL_GetRendererInfo(renderer, &rendererInfo);

  printf("Renderer: %s\n", rendererInfo.name);

  SDL_GetRendererOutputSize(renderer, &renderer_width, &renderer_height);

  cairo_x_multiplier = (double)renderer_width / (double)window_width;
  cairo_y_multiplier = (double)renderer_height / (double)window_height;

  texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888,
                              SDL_TEXTUREACCESS_STREAMING, renderer_width,
                              renderer_height);

  SDL_SetTextureBlendMode(texture, SDL_BLENDMODE_BLEND);

  run_main_loop();

  Mix_CloseAudio();
  SDL_DestroyTexture(texture);

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();
}
