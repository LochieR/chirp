#version 450 core
#pragma pack_matrix(column_major)

layout(location = 0) in vec4 v_Color;
layout(location = 1) in vec2 v_UV;

layout(location = 0) out vec4 o_Color;

layout(set = 0, binding = 0) uniform texture2D u_Texture;
layout(set = 0, binding = 1) uniform sampler u_Sampler;

void main()
{
    vec4 color = texture(sampler2D(u_Texture, u_Sampler), v_UV);
    o_Color = color;
}
