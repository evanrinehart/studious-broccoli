#version 150

in vec2 position;
in vec2 texcoord;

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
    float dx = dstXY.x / (winw / 2) - 1;
    float dy = dstXY.y / (winh / 2) - 1;
    gl_Position.x = position.x * w / winw + dx;
    gl_Position.y = position.y * h / winh + dy;
    uv = texcoord;
}
