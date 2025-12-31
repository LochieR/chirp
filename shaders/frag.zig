const std = @import("std");

const Vec4 = @Vector(4, f32);

extern var v_color: Vec4 addrspace(.input);
extern var o_color: Vec4 addrspace(.output);

export fn main() callconv(.spirv_fragment) void {
    std.gpu.location(&v_color, 0);
    std.gpu.location(&o_color, 0);

    o_color = v_color;
}
