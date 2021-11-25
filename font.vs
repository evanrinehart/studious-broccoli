#version 150

in vec2 position;
in vec2 texcoord;

uniform vec2 surfWH;
uniform vec2 dstXY;
uniform vec2 dstWH;

out vec2 uv;
out vec2 xy;

/*
position a tile at xy with size wh
0,0 is bottom left of framebuffer
*/

void main()
{
    gl_Position.w = 1;
    gl_Position.z = 0;
    float sw = surfWH.x;
    float sh = surfWH.y;
    float w = dstWH.x;
    float h = dstWH.y;
    float dx = dstXY.x / (sw / 2);
    float dy = dstXY.y / (sh / 2);
    gl_Position.x = (position.x+1)*(w/sw) - 1 + dx;
    gl_Position.y = (position.y+1)*(h/sh) - 1 + dy;
    uv.x = texcoord.x;
    uv.y = texcoord.y;
    xy = position;
}


