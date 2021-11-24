#version 150

in vec2 position;
in vec2 texcoord;

uniform vec2 winWH;
uniform vec2 dstXY;
uniform vec2 dstWH;

out vec2 uv;
out vec2 xy;

void main()
{
    gl_Position.w = 1;
    gl_Position.z = 0;
/*    float winw = winWH.x;
    float winh = winWH.y;
    float dx = dstXY.x / (winw / 2);
    float dy = dstXY.y / (winh / 2);
    gl_Position.x = ((position.x+1)/2 - 0) / winw * 2 * dstWH.x - 1 + dx;
    gl_Position.y = ((position.y+1)/2 - 1) / winh * 2 * dstWH.y + 1 - dy;
*/
    float w = dstWH.x;
    float h = dstWH.y;
    float dx = dstXY.x / (800 / 2);
    float dy = dstXY.y / (600 / 2);
    gl_Position.x = (position.x+1)*(w/256)*256/800 - 1 + dx;
    gl_Position.y = (position.y+1)*(h/256)*256/600 - 1 + dy;
    //gl_Position.y += 0.1;
    //gl_Position.y *= -1;
    //gl_Position.y += 0.5;
    uv.x = texcoord.x;
    uv.y = texcoord.y;
    xy = position;
}


