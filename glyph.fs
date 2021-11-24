#version 150

in vec2 uv;
out vec4 outColor;

uniform sampler2D sheet;
uniform vec2 srcXY;
uniform vec2 srcWH;

uniform vec3 fgColor;
uniform vec3 bgColor;

in vec2 xy;

/* uv
01  11

00  10
*/


void main() {
  ivec2 sheetWH = textureSize(sheet, 0);
  //outColor = texture(sheet, uv);
  //outColor.r = uv.x;
  //outColor.g = uv.y;
  //outColor.b = 0;
  float width  = float(sheetWH.x);
  float height = float(sheetWH.y);
  float x = uv.x / width  * srcWH.x + (srcXY.x) / width;
  float y = (1 - uv.y) / height * srcWH.y + (srcXY.y) / height;
  //vec2 shift = vec2(11,4);
  vec4 c = texture(sheet, vec2(x,y));
  if(c.r == 1 && c.g == 1 && c.b == 1){
    outColor = vec4(bgColor,1);
  }
  else{
    outColor = vec4(fgColor,1);
  }
}

