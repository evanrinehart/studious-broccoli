#version 150

in vec2 uv;
out vec4 outColor;

uniform sampler2D sheet;
uniform vec4 srcXYWH;

uniform vec3 fgColor;
uniform vec3 bgColor;

/* uv
01  11

00  10
*/


void main() {
  ivec2 sheetWH = textureSize(sheet, 0);
  float width  = float(sheetWH.x);
  float height = float(sheetWH.y);
  float x = srcXYWH.x;
  float y = srcXYWH.y;
  float w = srcXYWH.z;
  float h = srcXYWH.w;
  float tx =     uv.x / width  * w + x / width;
  float ty = (1-uv.y) / height * h + y / height;
  //vec2 shift = vec2(11,4);
  vec4 c = texture(sheet, vec2(tx,ty));
  if(c.r == 1 && c.g == 1 && c.b == 1){
    outColor = vec4(bgColor,1);
  }
  else{
    outColor = vec4(fgColor,1);
  }
}

