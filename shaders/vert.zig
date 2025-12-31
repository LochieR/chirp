const std = @import("std");

const Mat4 = extern struct {
    c0: @Vector(4, f32),
    c1: @Vector(4, f32),
    c2: @Vector(4, f32),
    c3: @Vector(4, f32),
};

const Camera = extern struct {
    view_proj: Mat4
};

extern const u_camera: Camera addrspace(.push_constant);

extern const a_position: @Vector(4, f32) addrspace(.input);
extern const a_color: @Vector(4, f32) addrspace(.input);
extern const a_uv: @Vector(2, f32) addrspace(.input);

extern var v_color: @Vector(4, f32) addrspace(.output);
extern var v_uv: @Vector(2, f32) addrspace(.output);

export fn main() callconv(.spirv_vertex) void {
    std.gpu.location(&a_position, 0);
    std.gpu.location(&a_color, 1);
    std.gpu.location(&a_uv, 2);
    std.gpu.location(&v_color, 0);
    std.gpu.location(&v_uv, 1);

    v_color = a_color;
    std.gpu.position_out.* = mul(u_camera.view_proj, a_position);
}

fn mul(m: Mat4, v: @Vector(4, f32)) @Vector(4, f32) {
    return m.c0 * @as(@Vector(4, f32), @splat(v[0])) +
           m.c1 * @as(@Vector(4, f32), @splat(v[1])) +
           m.c2 * @as(@Vector(4, f32), @splat(v[2])) +
           m.c3 * @as(@Vector(4, f32), @splat(v[3]));
}
