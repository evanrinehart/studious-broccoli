#version 150

in vec2 uv;
out vec4 outColor;

uniform sampler2D sheet;
uniform vec4 srcXYWH;

/* uv
01  11

00  10
*/

// shade a tile by sampling from a texture
// maps directly without mirroring.

void main() {
  ivec2 sheetWH = textureSize(sheet, 0);
  float x = srcXYWH.x;
  float y = srcXYWH.y;
  float w = srcXYWH.z;
  float h = srcXYWH.w;
  float sw = float(sheetWH.x);
  float sh = float(sheetWH.y);
  float tx = (uv.x * w + x) / sw;
  float ty = (uv.y * h + y) / sh;
  outColor = texture(sheet, vec2(tx,ty));
}
