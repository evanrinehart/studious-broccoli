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


void main() {
  ivec2 sheetWH = textureSize(sheet, 0);
  float width  = float(sheetWH.x);
  float height = float(sheetWH.y);
  float x = uv.x / width  * srcWH.x + (srcXY.x) / width;
  float y = uv.y / height * srcWH.y + (srcXY.y) / height;
  //vec2 shift = vec2(11,4);
  vec4 c = texture(sheet, vec2(x,y));
  outColor = c;
}

  //float x = gl_FragCoord.x / 640;
  //float y = gl_FragCoord.y / 480;
