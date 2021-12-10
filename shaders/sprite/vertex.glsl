#version 150

in vec2 position;

uniform vec2 winWH;
uniform vec4 dstXYWH;

out vec2 uv;

void main()
{
    float x = dstXYWH.x;
    float y = dstXYWH.y;
    float w = dstXYWH.z;
    float h = dstXYWH.w;
    float sx = winWH.x / 2;
    float sy = winWH.y / 2;
    gl_Position.x = (position.x * w + x) / sx;
    gl_Position.y = (position.y * h + y) / sy;
    gl_Position.z = 0;
    gl_Position.w = 1;
    uv = position;
}
