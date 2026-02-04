// cbits/font_stb.c
#define STB_TRUETYPE_IMPLEMENTATION
#include "stb_truetype.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// load font file into memory
unsigned char* stb_load_font (const char* path, int* size_out) {
    FILE* f = fopen(path, "rb");
    if (!f) {
        fprintf(stderr, "Failed to open font file: %s\n", path);
        return NULL;
    }
    // get file size
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);
    // allocate and read
    unsigned char* buffer = (unsigned char*)malloc(size);
    if (!buffer) {
        fclose(f);
        return NULL;
    }
    size_t read = fread(buffer, 1, size, f);
    fclose(f);
    if (read != size) {
        free(buffer);
        return NULL;
    }
    *size_out = (int)size;
    return buffer;
}
// init font
int stb_init_font(const unsigned char* font_data, stbtt_fontinfo* font) {
    return stbtt_InitFont(font, font_data, 0);
}
// get font vertical metrics
void stb_get_font_vmetrics(const stbtt_fontinfo* font, int* ascent, int* descent, int* lineGap) {
    stbtt_GetFontVMetrics(font, ascent, descent, lineGap);
}
// get glyph metrics
void stb_get_glyph_metrics(stbtt_fontinfo* font, int codepoint, float scale,
                           int* width, int* height, 
                           int* xoff, int* yoff, 
                           float* advance) {
    int advanceInt, leftBearing;
    stbtt_GetCodepointHMetrics(font, codepoint, &advanceInt, &leftBearing);
    stbtt_GetCodepointBitmapBox(font, codepoint, scale, scale, xoff, yoff, 
                                width, height);
    *advance = (float)advanceInt * scale;
    
    // Adjust width/height to be relative to offset
    *width = *width - *xoff;
    *height = *height - *yoff;
}

// Render glyph bitmap
unsigned char* stb_render_glyph(stbtt_fontinfo* font, int codepoint, float scale,
                                int* width, int* height,
                                int* xoff, int* yoff) {
    unsigned char* bitmap = stbtt_GetCodepointBitmap(font, 0, scale, codepoint,
                                                      width, height, xoff, yoff);
    return bitmap;
}

// Render glyph as SDF (Signed Distance Field)
unsigned char* stb_render_glyph_sdf(stbtt_fontinfo* font, 
                                     int codepoint, 
                                     float scale,
                                     int padding,
                                     int* width, int* height,
                                     int* xoff, int* yoff) {
    unsigned char onedge_value = 180;  // Edge threshold (0.7 * 255)
    float pixel_dist_scale = (float)onedge_value / (float)padding;
    
    return stbtt_GetCodepointSDF(font, scale, codepoint,
                                  padding, onedge_value, pixel_dist_scale,
                                  width, height, xoff, yoff);
}

// Free SDF bitmap (uses different free function)
void stb_free_sdf(unsigned char* bitmap) {
    stbtt_FreeSDF(bitmap, NULL);
}

// Free bitmap
void stb_free_bitmap(unsigned char* bitmap) {
    stbtt_FreeBitmap(bitmap, NULL);
}

// Free font data
void stb_free_font(unsigned char* font_data) {
    free(font_data);
}

// Calculate scale for pixel height
float stb_scale_for_pixel_height(stbtt_fontinfo* font, float pixels) {
    return stbtt_ScaleForPixelHeight(font, pixels);
}
