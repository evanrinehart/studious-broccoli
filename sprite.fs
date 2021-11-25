#version 150

in vec2 uv;
out vec4 outColor;

uniform sampler2D sheet;
uniform vec2 srcXY;
uniform vec2 srcWH;

/* uv
00  10

01  11
*/

// shade a tile by sampling from a texture
// this will reflect uv vertically which
// is appropriate when drawing from a sprite
// sheet loaded from an image file.

void main() {
  ivec2 sheetWH = textureSize(sheet, 0);
  float sw = float(sheetWH.x);
  float sh = float(sheetWH.y);
  float x = (uv.x * srcWH.x + srcXY.x) / sw;
  float y = ((1 - uv.y) * srcWH.y + srcXY.y) / sh;
  outColor = texture(sheet, vec2(x,y));
}
