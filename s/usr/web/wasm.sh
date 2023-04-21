######
# compile calm.c to calm.wasm
######

set -x

if [[ -z "${CALM_HOME}" ]]; then
    echo "CALM_HOME not set."
    exit 42
fi

if [[ -z "${CALM_APP_DIR}" ]]; then
    echo "CALM_APP_DIR not set."
    exit 42
fi

cd ${CALM_HOME}

mkdir -p build/web/

check_result() {
    if [ $? -eq 0 ]; then
        echo "SEEMS GOOD."
    else
        echo "BAD THING HAPPENED."
        exit 42
    fi
    echo "PREVIOUS DIR: " $(pwd)
}

check_result

# https://gitlab.com/gitlab-org/gitlab-foss/-/issues/49482
shopt -s expand_aliases

# copy fonts
if [ -d "${CALM_APP_DIR}/fonts" ]; then
    mkdir -p ./build//web/share/
    rm -rf ./build//web/share/fonts
    cp -R ${CALM_APP_DIR}/fonts ./build//web/share/fonts
else
    # get font
    if [ ! -d "./build/web/share/fonts/" ]; then
        mkdir -p ./build//web/share/fonts/
        cd ./build/web/share/fonts/
        curl -LO https://github.com/googlefonts/opensans/raw/main/fonts/ttf/OpenSans-Regular.ttf
        cd ../../
    fi
fi

# copy assets
if [ -d "${CALM_APP_DIR}/assets" ]; then
    mkdir -p ./build//web/share/
    rm -rf ./build//web/share/assets
    cp -R ${CALM_APP_DIR}/assets ./build//web/share/assets
fi

check_result

# compile
alias emcc="docker run --rm --name pcwa -v $(pwd):/app -w=/app vitovan/pango-cairo-wasm emcc"
alias pkg-config="docker run --rm --name pcwa -v $(pwd):/app -w=/app vitovan/pango-cairo-wasm pkg-config"

check_result

# https://www.asciiart.eu/animals/cows
if command -v base64 &> /dev/null; then
    base64 -d <<<"ICAgICAgICAgICAgICAoICAgICAgKQogICAgICAgICAgICAgIH4oXl5eXil+CiAgICAgICAgICAgICAgICkgQEAgXH5fICAgICAgICAgIHxcCiAgICAgICAgICAgICAgLyAgICAgfCBcICAgICAgICBcfiAvCiAgICAgICAgICAgICAoIDAgIDAgICkgXCAgICAgICAgfCB8CiAgICAgICAgICAgICAgLS0tX19fL34gIFwgICAgICAgfCB8CiAgICAgICAgICAgICAgIC8nX18vIHwgICB+LV9fX19fLyB8Cm8gICAgICAgICAgXyAgIH4tLS0tfiAgICAgIF9fXy0tLX4KICBPICAgICAgIC8vICAgICB8ICAgICAgICAgfAogICAgICAgICAoKH5cICBffCAgICAgICAgIC18CiAgIG8gIE8gLy8tXyBcLyB8ICAgICAgICB+ICB8CiAgICAgICAgXiAgIFxfIC8gICAgICAgICB+ICB8CiAgICAgICAgICAgICAgIHwgICAgICAgICAgfiB8CiAgICAgICAgICAgICAgIHwgICAgIC8gICAgIH4gfAogICAgICAgICAgICAgICB8ICAgICAoICAgICAgIHwKICAgICAgICAgICAgICAgIFwgICAgIFwgICAgICAvXAogICAgICAgICAgICAgICAvIC1fX19fXy1cICAgXCB+fi0qCiAgICAgICAgICAgICAgIHwgIC8gICAgICAgXCAgXAogICAgICAgICAgICAgICAvIC8gICAgICAgICAvIC8KICAgICAgICAgICAgIC9+ICB8ICAgICAgIC9+ICB8CiAgICAgICAgICAgICB+fn5+ICAgICAgICB+fn5+Cg=="
fi

echo "compiling... this may take one year, please be patient."


emcc -Oz \
     -sALLOW_MEMORY_GROWTH \
     -sFULL_ES2 \
     -s USE_SDL=2 \
     -s USE_SDL_MIXER=2 \
     -s USE_PTHREADS=0 \
     --shell-file src/web/calm.html \
     --pre-js src/web/pre.js \
     --js-library src/web/bridge.js \
     --preload-file ./build/web/share@/usr/share/ \
     src/web/calm.c -o build/web/calm.html \
     $(pkg-config --libs --cflags glib-2.0, gobject-2.0, cairo, pixman-1, freetype2, fontconfig, cairo, expat, harfbuzz) \
     -s EXPORTED_RUNTIME_METHODS=ccall,cwrap,allocateUTF8,UTF8ToString \
     -s EXPORTED_FUNCTIONS=_main,\
_cairo_create,\
_cairo_reference,\
_cairo_destroy,\
_cairo_status,\
_cairo_save,\
_cairo_restore,\
_cairo_get_target,\
_cairo_push_group,\
_cairo_push_group_with_content,\
_cairo_pop_group,\
_cairo_pop_group_to_source,\
_cairo_get_group_target,\
_cairo_set_source_rgb,\
_cairo_set_source_rgba,\
_cairo_set_source,\
_cairo_set_source_surface,\
_cairo_get_source,\
_cairo_set_antialias,\
_cairo_get_antialias,\
_cairo_set_dash,\
_cairo_get_dash_count,\
_cairo_get_dash,\
_cairo_set_fill_rule,\
_cairo_get_fill_rule,\
_cairo_set_line_cap,\
_cairo_get_line_cap,\
_cairo_set_line_join,\
_cairo_get_line_join,\
_cairo_set_line_width,\
_cairo_get_line_width,\
_cairo_set_miter_limit,\
_cairo_get_miter_limit,\
_cairo_set_operator,\
_cairo_get_operator,\
_cairo_set_tolerance,\
_cairo_get_tolerance,\
_cairo_clip,\
_cairo_clip_preserve,\
_cairo_clip_extents,\
_cairo_in_clip,\
_cairo_reset_clip,\
_cairo_rectangle_list_destroy,\
_cairo_copy_clip_rectangle_list,\
_cairo_fill,\
_cairo_fill_preserve,\
_cairo_fill_extents,\
_cairo_in_fill,\
_cairo_mask,\
_cairo_mask_surface,\
_cairo_paint,\
_cairo_paint_with_alpha,\
_cairo_stroke,\
_cairo_stroke_preserve,\
_cairo_stroke_extents,\
_cairo_in_stroke,\
_cairo_copy_page,\
_cairo_show_page,\
_cairo_get_reference_count,\
_cairo_set_user_data,\
_cairo_get_user_data,\
_cairo_copy_path,\
_cairo_copy_path_flat,\
_cairo_path_destroy,\
_cairo_append_path,\
_cairo_has_current_point,\
_cairo_get_current_point,\
_cairo_new_path,\
_cairo_new_sub_path,\
_cairo_close_path,\
_cairo_arc,\
_cairo_arc_negative,\
_cairo_curve_to,\
_cairo_line_to,\
_cairo_move_to,\
_cairo_rectangle,\
_cairo_glyph_path,\
_cairo_text_path,\
_cairo_rel_curve_to,\
_cairo_rel_line_to,\
_cairo_rel_move_to,\
_cairo_path_extents,\
_cairo_pattern_add_color_stop_rgb,\
_cairo_pattern_add_color_stop_rgba,\
_cairo_pattern_get_color_stop_count,\
_cairo_pattern_get_color_stop_rgba,\
_cairo_pattern_create_rgb,\
_cairo_pattern_create_rgba,\
_cairo_pattern_get_rgba,\
_cairo_pattern_create_for_surface,\
_cairo_pattern_get_surface,\
_cairo_pattern_create_linear,\
_cairo_pattern_get_linear_points,\
_cairo_pattern_create_radial,\
_cairo_pattern_get_radial_circles,\
_cairo_pattern_create_mesh,\
_cairo_mesh_pattern_begin_patch,\
_cairo_mesh_pattern_end_patch,\
_cairo_mesh_pattern_move_to,\
_cairo_mesh_pattern_line_to,\
_cairo_mesh_pattern_curve_to,\
_cairo_mesh_pattern_set_control_point,\
_cairo_mesh_pattern_set_corner_color_rgb,\
_cairo_mesh_pattern_set_corner_color_rgba,\
_cairo_mesh_pattern_get_patch_count,\
_cairo_mesh_pattern_get_path,\
_cairo_mesh_pattern_get_control_point,\
_cairo_mesh_pattern_get_corner_color_rgba,\
_cairo_pattern_reference,\
_cairo_pattern_destroy,\
_cairo_pattern_status,\
_cairo_pattern_set_extend,\
_cairo_pattern_get_extend,\
_cairo_pattern_set_filter,\
_cairo_pattern_get_filter,\
_cairo_pattern_set_matrix,\
_cairo_pattern_get_matrix,\
_cairo_pattern_get_type,\
_cairo_pattern_get_reference_count,\
_cairo_pattern_set_user_data,\
_cairo_pattern_get_user_data,\
_cairo_region_create,\
_cairo_region_create_rectangle,\
_cairo_region_create_rectangles,\
_cairo_region_copy,\
_cairo_region_reference,\
_cairo_region_destroy,\
_cairo_region_status,\
_cairo_region_get_extents,\
_cairo_region_num_rectangles,\
_cairo_region_get_rectangle,\
_cairo_region_is_empty,\
_cairo_region_contains_point,\
_cairo_region_contains_rectangle,\
_cairo_region_equal,\
_cairo_region_translate,\
_cairo_region_intersect,\
_cairo_region_intersect_rectangle,\
_cairo_region_subtract,\
_cairo_region_subtract_rectangle,\
_cairo_region_union,\
_cairo_region_union_rectangle,\
_cairo_region_xor,\
_cairo_region_xor_rectangle,\
_cairo_translate,\
_cairo_scale,\
_cairo_rotate,\
_cairo_transform,\
_cairo_set_matrix,\
_cairo_get_matrix,\
_cairo_identity_matrix,\
_cairo_user_to_device,\
_cairo_user_to_device_distance,\
_cairo_device_to_user,\
_cairo_device_to_user_distance,\
_cairo_select_font_face,\
_cairo_set_font_size,\
_cairo_set_font_matrix,\
_cairo_get_font_matrix,\
_cairo_set_font_options,\
_cairo_get_font_options,\
_cairo_set_font_face,\
_cairo_get_font_face,\
_cairo_set_scaled_font,\
_cairo_get_scaled_font,\
_cairo_show_text,\
_cairo_show_glyphs,\
_cairo_show_text_glyphs,\
_cairo_font_extents,\
_cairo_text_extents,\
_cairo_glyph_extents,\
_cairo_toy_font_face_create,\
_cairo_toy_font_face_get_family,\
_cairo_toy_font_face_get_slant,\
_cairo_toy_font_face_get_weight,\
_cairo_glyph_allocate,\
_cairo_glyph_free,\
_cairo_text_cluster_allocate,\
_cairo_text_cluster_free,\
_cairo_pattern_create_raster_source,\
_cairo_raster_source_pattern_set_callback_data,\
_cairo_raster_source_pattern_get_callback_data,\
_cairo_raster_source_pattern_set_acquire,\
_cairo_raster_source_pattern_get_acquire,\
_cairo_raster_source_pattern_set_snapshot,\
_cairo_raster_source_pattern_get_snapshot,\
_cairo_raster_source_pattern_set_copy,\
_cairo_raster_source_pattern_get_copy,\
_cairo_raster_source_pattern_set_finish,\
_cairo_raster_source_pattern_get_finish,\
_cairo_tag_begin,\
_cairo_tag_end,\
_cairo_image_surface_get_width,\
_cairo_image_surface_get_height,\
_cairo_image_surface_create_from_png,\
_cairo_surface_status,\
_cairo_status,\
_config,\
_Mix_OpenAudio,\
_Mix_CloseAudio,\
_Mix_LoadMUS,\
_Mix_LoadWAV_RW,\
_SDL_RWFromFile,\
_Mix_PlayMusic,\
_Mix_HaltMusic,\
_Mix_PlayChannelTimed,\
_Mix_Playing,\
_Mix_AllocateChannels,\
_SDL_GetTicks,\
_SDL_GetError,\
_free

check_result
