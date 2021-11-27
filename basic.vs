#version 150

in vec2 position;

uniform vec2 winWH;
uniform vec2 dstXY;
uniform vec2 dstWH;

out vec2 uv;

// place a tile *centered* at xy
// 0,0 is bottom left of the screen
// xy spans ranges [0,0] x [winw, winh]
// assumes a tile spanning [-1,1] x [-1,1] is given

void main()
{
    gl_Position.w = 1;
    gl_Position.z = 0;
    float winw = winWH.x;
    float winh = winWH.y;
    float w = dstWH.x;
    float h = dstWH.y;
    float dx = dstXY.x * 2 / winw;
    float dy = dstXY.y * 2 / winh;
    gl_Position.x = position.x * 2 / winw * w - 1 + dx;
    gl_Position.y = position.y * 2 / winh * h - 1 + dy;
    uv = position;
}
