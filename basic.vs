#version 150

in vec2 position;

uniform vec2 winWH;
uniform vec2 dstXY;
uniform vec2 dstWH;

out vec2 uv;

void main()
{
    gl_Position.w = 1;
    gl_Position.z = 0;
    float winw = winWH.x;
    float winh = winWH.y;
    float w = dstWH.x;
    float h = dstWH.y;
    float x = dstXY.x;
    float y = dstXY.y;
//    float dx = dstXY.x * 2 / winw;
//    float dy = dstXY.y * 2 / winh;
//    gl_Position.x = position.x * 2 / winw * w - 1 + dx;
//    gl_Position.y = position.y * 2 / winh * h - 1 + dy;
    gl_Position.x = (position.x * w + x) / (winWH.x/2);
    gl_Position.y = (position.y * h + y) / (winWH.y/2);
    uv = position;
}
