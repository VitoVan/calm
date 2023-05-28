mergeInto(LibraryManager.library, {
  js_get_calm_redraw: function() {
    if (typeof get_calm_redraw === 'function') {
      return get_calm_redraw();
    }
    return false;
  },
  js_draw: function() {
    if (typeof draw === 'function') {
      draw();
      return 42;
    } else {
      return 0;
    }
  },
  js_think: function() {
    if (typeof think === 'function') {
      think();
      return 42;
    } else {
      return 0;
    }
  },
  js_on_windowresized: function(w, h) {
    if (typeof on_windowresized === 'function') {
      on_windowresized(w, h);
      return 42;
    } else {
      return 0;
    }
  },
  js_on_mousemotion: function(x, y) {
    if (typeof on_mousemotion === 'function') {
      on_mousemotion(x, y);
      return 42;
    } else {
      return 0;
    }
  },
  js_on_mousebuttonup: function(button, x, y, clicks) {
    if (typeof on_mousebuttonup === 'function') {
      on_mousebuttonup(button, x, y, clicks);
      return 42;
    } else {
      return 0;
    }
  },
  js_on_mousebuttondown: function(button, x, y, clicks) {
    if (typeof on_mousebuttondown === 'function') {
      on_mousebuttondown(button, x, y, clicks);
      return 42;
    } else {
      return 0;
    }
  },
  js_on_fingermotion: function(x, y, dx, dy, pressure, fingerId) {
    if (typeof on_fingermotion === 'function') {
      on_fingermotion(x, y, dx, dy, pressure, fingerId);
      return 42;
    } else {
      return 0;
    }
  },
  js_on_fingerup: function(x, y, dx, dy, pressure, fingerId) {
    if (typeof on_fingerup === 'function') {
      on_fingerup(x, y, dx, dy, pressure, fingerId);
      return 42;
    } else {
      return 0;
    }
  },
  js_on_fingerdown: function(x, y, dx, dy, pressure, fingerId) {
    if (typeof on_fingerdown === 'function') {
      on_fingerdown(x, y, dx, dy, pressure, fingerId);
      return 42;
    } else {
      return 0;
    }
  },
  js_on_windowenter: function() {
    if (typeof on_windowenter === 'function') {
      on_windowenter();
      return 42;
    } else {
      return 0;
    }
  },
  js_on_windowleave: function() {
    if (typeof on_windowleave === 'function') {
      on_windowleave();
      return 42;
    } else {
      return 0;
    }
  },
  js_on_keydown: function(sym) {
    if (typeof on_keydown === 'function') {
      on_keydown(sym);
      return 42;
    } else {
      return 0;
    }
  },
  js_on_keyup: function(sym) {
    if (typeof on_keyup === 'function') {
      on_keyup(sym);
      return 42;
    } else {
      return 0;
    }
  },
});
