const std = @import("std");

extern const a_position: @Vector(4, f32) addrspace(.input);

export fn main() callconv(.spirv_vertex) void {
    std.gpu.location(&a_position, 0);

    std.gpu.position_out.* = a_position;
}
