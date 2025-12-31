const std = @import("std");

const glfw = @import("Renderer/glfw.zig");
const Instance = @import("Renderer/Instance.zig").Instance;
const InstanceInfo = @import("Renderer/Instance.zig").InstanceInfo;
const Device = @import("Renderer/Device.zig").Device;
const Swapchain = @import("Renderer/Swapchain.zig").Swapchain;
const SwapchainInfo = @import("Renderer/Swapchain.zig").SwapchainInfo;
const RenderPass = @import("Renderer/RenderPass.zig").RenderPass;
const RenderPassInfo = @import("Renderer/RenderPass.zig").RenderPassInfo;
const AttachmentInfo = @import("Renderer/RenderPass.zig").AttachmentInfo;
const GraphicsPipeline = @import("Renderer/GraphicsPipeline.zig").GraphicsPipeline;
const GraphicsPipelineInfo = @import("Renderer/GraphicsPipeline.zig").GraphicsPipelineInfo;
const Buffer = @import("Renderer/Buffer.zig").Buffer;
const PushConstantInfo = @import("Renderer/GraphicsPipeline.zig").PushConstantInfo;
const ShaderResourceLayout = @import("Renderer/ShaderResource.zig").ShaderResourceLayout;
const ResourceLayoutItem = @import("Renderer/ShaderResource.zig").ResourceLayoutItem;
const ShaderResource = @import("Renderer/ShaderResource.zig").ShaderResource;
const ShaderResourceSet = @import("Renderer/ShaderResource.zig").ShaderResourceSet;
const VertexInputLayout = @import("Renderer/Shader.zig").VertexInputLayout;
const VertexInputAttributeData = @import("Renderer/Shader.zig").VertexInputAttributeData;
const VertexInputBindingData = @import("Renderer/Shader.zig").VertexInputBindingData;

const vert_spv: []const u32 align(@alignOf(u32)) = @ptrCast(@alignCast(@embedFile("vertex")));
const frag_spv: []const u32 align(@alignOf(u32)) = @ptrCast(@alignCast(@embedFile("fragment")));

var registers: [16]u8 = undefined;

var address_space: [4096]u8 = undefined;

var index_register: u16 = 0;
var program_counter: u16 = 0x0200;

var stack: [16]u16 = undefined;
var stack_pointer: u8 = 0;

var delay_timer: u8 = 0;
var sound_timer: u8 = 0;

var keypad: [16]u8 = undefined;

const framebuffer_width = 64;
const framebuffer_height = 32;
var framebuffer: [framebuffer_width * framebuffer_height]u8 = undefined;
var opcode: u16 = 0;

const fontset = [_]u8 {
    0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
	0x20, 0x60, 0x20, 0x20, 0x70, // 1
	0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
	0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
	0x90, 0x90, 0xF0, 0x10, 0x10, // 4
	0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
	0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
	0xF0, 0x10, 0x20, 0x40, 0x40, // 7
	0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
	0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
	0xF0, 0x90, 0xF0, 0x90, 0x90, // A
	0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
	0xF0, 0x80, 0x80, 0x80, 0xF0, // C
	0xE0, 0x90, 0x90, 0x90, 0xE0, // D
	0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
	0xF0, 0x80, 0xF0, 0x80, 0x80  // F
};
const fontset_start = 0x50;

var prng: std.Random.Xoshiro256 = undefined;
var random: std.Random = undefined;

const Chip8Func = *const fn() void;
var table: [0xF + 1]Chip8Func = undefined;
var table0: [0xE + 1]Chip8Func = undefined;
var table8: [0xE + 1]Chip8Func = undefined;
var tableE: [0xE + 1]Chip8Func = undefined;
var tableF: [0x65 + 1]Chip8Func = undefined;

const Vertex = extern struct {
    position: @Vector(4, f32),
    color: @Vector(4, f32)
};

pub fn main() !void {
    @memset(&registers, 0);
    @memset(&address_space, 0);
    @memset(&stack, 0);
    @memset(&keypad, 0);
    @memset(&framebuffer, 0);

    _ = glfw.c.glfwInit();
    defer glfw.c.glfwTerminate();

    glfw.c.glfwWindowHint(glfw.c.GLFW_CLIENT_API, glfw.c.GLFW_NO_API);
    const window = glfw.c.glfwCreateWindow(1280, 720, "chirp", null, null);
    defer glfw.c.glfwDestroyWindow(window);

    const instance_info = InstanceInfo{
        .app_name = "chirp",
        .allocator = std.heap.c_allocator,
        .window = @ptrCast(window.?)
    };
    var instance = try Instance.init(&instance_info);
    defer instance.deinit();

    var device = try instance.createDevice();
    defer instance.destroyDevice(&device);

    const swapchain_info = SwapchainInfo{
        .attachments = &.{
            .swapchain_color_default
        },
        .present_mode = .mailbox_or_fifo
    };

    const swapchain = try device.createSwapchain(&swapchain_info);
    defer device.destroySwapchain(swapchain);

    const render_pass_info = RenderPassInfo{
        .attachments = &.{
            AttachmentInfo{
                .format = .swapchain_color_default,
                .previous_layout = .undefined,
                .layout = .present,
                .samples = 1,
                .load_op = .clear,
                .store_op = .store,
                .stencil_load_op = .dont_care,
                .stencil_store_op = .dont_care
            }
        }
    };

    var render_pass = try device.createRenderPass(swapchain, &render_pass_info);
    defer device.destroyRenderPass(&render_pass);

    var shader_resource_layout = ShaderResourceLayout{
        .sets = &.{},
        .push_constants = &.{
            PushConstantInfo{
                .size = 4 * @sizeOf(@Vector(4, f32)),
                .offset = 0,
                .shader_type = .vertex
            }
        }
    };

    try device.initShaderResourceLayout(&shader_resource_layout);
    defer device.deinitShaderResourceLayout(&shader_resource_layout);

    var bindings = [_]VertexInputBindingData {
        VertexInputBindingData{
            .binding = 0,
            .input_rate = .per_vertex,
            .stride = @sizeOf(Vertex)
        }
    };

    var attributes = [_]VertexInputAttributeData {
        VertexInputAttributeData{
            .binding = 0,
            .location = 0,
            .format = .r32g32b32a32_sfloat,
            .offset = 0
        },
        VertexInputAttributeData{
            .binding = 0,
            .location = 1,
            .format = .r32g32b32a32_sfloat,
            .offset = @sizeOf(@Vector(4, f32))
        }
    };

    const vertex_input_layout = VertexInputLayout{
        .bindings = bindings[0..],
        .attributes = attributes[0..]
    };

    const pipeline_info = GraphicsPipelineInfo{
        .vertex_shader = vert_spv,
        .pixel_shader = frag_spv,
        .primitive_topology = .triangle_list,
        .render_pass = &render_pass,
        .shader_resource_layout = shader_resource_layout,
        .vertex_input_layout = vertex_input_layout
    };

    var pipeline = try device.createGraphicsPipeline(&pipeline_info);
    defer device.destroyGraphicsPipeline(&pipeline);

    const vertices = [_]@Vector(4, f32) {
        .{ -1.0, -1.0, 0.0, 1.0 },
        .{  3.0, -1.0, 0.0, 1.0 },
        .{ -1.0,  3.0, 0.0, 1.0 }
    };

    var vertex_buffer = try device.createBufferWithData(.vertex_buffer, std.mem.sliceAsBytes(&vertices));
    defer device.destroyBuffer(&vertex_buffer);

    var vertex_buffers = [_]*const Buffer { &vertex_buffer };

    var command_list = device.createCommandList();
    defer device.destroyCommandList(&command_list);

    const push_constant_data = [4]@Vector(4, f32) {
        .{ 1.0, 0.0, 0.0, 0.0 },
        .{ 0.0, 1.0, 0.0, 0.0 },
        .{ 0.0, 0.0, 1.0, 0.0 },
        .{ 0.0, 0.0, 0.0, 1.0 }
    };

    while (glfw.c.glfwWindowShouldClose(@ptrCast(window)) != 1) {
        try device.beginFrame();

        command_list.begin();
        try command_list.beginRenderPass(&render_pass);

        try command_list.bindPipeline(&pipeline);
        try command_list.setViewport(.{ 0, 0 }, .{ @floatFromInt(swapchain.extent.width), @floatFromInt(swapchain.extent.height) }, 0.0, 1.0);
        try command_list.setScissor(.{ 0, 0 }, .{ @floatFromInt(swapchain.extent.width), @floatFromInt(swapchain.extent.height) });
        try command_list.bindVertexBuffers(&vertex_buffers);
        try command_list.pushConstants(std.mem.sliceAsBytes(&push_constant_data), .vertex, 0);
        
        try command_list.draw(3, 0);

        try command_list.endRenderPass();
        try command_list.end();
        
        try device.submitCommandList(&command_list);
        try device.endFrame();

        glfw.c.glfwPollEvents();
    }

    for (0..fontset.len) |i| {
        address_space[fontset_start + i] = fontset[i];
    }

    prng = std.Random.DefaultPrng.init(@intCast(std.time.timestamp()));
    random = prng.random();

    table[0x0] = table0Dispatch;
	table[0x1] = op1xxx;
	table[0x2] = op2xxx;
	table[0x3] = op3xkk;
	table[0x4] = op4xkk;
	table[0x5] = op5xy0;
	table[0x6] = op6xkk;
	table[0x7] = op7xkk;
	table[0x8] = table8Dispatch;
	table[0x9] = op9xy0;
	table[0xA] = opAnnn;
	table[0xB] = opBnnn;
	table[0xC] = opCxkk;
	table[0xD] = opDxyn;
	table[0xE] = tableEDispatch;
	table[0xF] = tableFDispatch;

	for (0..0xE + 1) |i| {
	    table0[i] = opNull;
		table8[i] = opNull;
		tableE[i] = opNull;
	}

	table0[0x0] = op00E0;
	table0[0xE] = op00EE;

	table8[0x0] = op8xy0;
	table8[0x1] = op8xy1;
	table8[0x2] = op8xy2;
	table8[0x3] = op8xy3;
	table8[0x4] = op8xy4;
	table8[0x5] = op8xy5;
	table8[0x6] = op8xy6;
	table8[0x7] = op8xy7;
	table8[0xE] = op8xyE;

	tableE[0x1] = opExA1;
	tableE[0xE] = opEx9E;

	for (0..0x65 + 1) |i| {
	    tableF[i] = opNull;
	}

	tableF[0x07] = opFx07;
	tableF[0x0A] = opFx0A;
	tableF[0x15] = opFx15;
	tableF[0x18] = opFx18;
	tableF[0x1E] = opFx1E;
	tableF[0x29] = opFx29;
	tableF[0x33] = opFx33;
	tableF[0x55] = opFx55;
	tableF[0x65] = opFx65;
}

fn table0Dispatch() void {
    table0[opcode & 0x000F]();
}

fn table8Dispatch() void {
    table8[opcode & 0x000F]();
}

fn tableEDispatch() void {
    tableE[opcode & 0x000F]();
}

fn tableFDispatch() void {
    tableF[opcode & 0x000F]();
}

fn cycle() void {
    // fetch
    opcode = (address_space[program_counter] << 8) | address_space(program_counter + 1);
    program_counter += 2;

    // decode and execute
    table[(opcode & 0xF000) >> 12]();

    if (delay_timer > 0) {
        delay_timer -= 1;
    }

    if (sound_timer > 0) {
        sound_timer -= 1;
    }
}

fn opNull() void {
}

// 00E0 CLS
fn op00E0() void {
    @memset(&framebuffer, 0);
}

// 00EE RET
fn op00EE() void {
    stack_pointer -= 1;
    program_counter = stack[stack_pointer];
}

// 1xxx JP
fn op1xxx() void {
    const address = opcode & 0x0FFF;
    program_counter = address;
}

// 2xxx CALL
fn op2xxx() void {
    const address = opcode & 0x0FFF;

    stack[stack_pointer] = program_counter;
    stack_pointer += 1;
    program_counter = address;
}

// 3xkk SE Vx, byte
fn op3xkk() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);
    const byte: u8 = @intCast(opcode & 0x00FF);

    if (registers[Vx] == byte) {
        program_counter += 2;
    }
}

// 4xkk SNE Vx, byte
fn op4xkk() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);
    const byte: u8 = @intCast(opcode & 0x00FF);

    if (registers[Vx] != byte) {
        program_counter += 2;
    }
}

// 5xy0 SE Vx, Vy
fn op5xy0() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);
    const Vy: u8 = @intCast((opcode & 0x00F0) >> 4);

    if (registers[Vx] == registers[Vy]) {
        program_counter += 2;
    }
}

// 6xkk LD Vx, byte
fn op6xkk() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);
    const byte: u8 = @intCast(opcode & 0x00FF);

    registers[Vx] = byte;
}

// 7xkk ADD Vx, byte
fn op7xkk() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);
    const byte: u8 = @intCast(opcode & 0x00FF);

    registers[Vx] += byte;
}

// 8xy0 LD Vx, Vy
fn op8xy0() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);
	const Vy: u8 = @intCast((opcode & 0x00F0) >> 4);

	registers[Vx] = registers[Vy];
}

// 8xy1 OR Vx, Vy
fn op8xy1() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);
	const Vy: u8 = @intCast((opcode & 0x00F0) >> 4);

	registers[Vx] |= registers[Vy];
}

// 8xy2 AND Vx, Vy
fn op8xy2() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);
	const Vy: u8 = @intCast((opcode & 0x00F0) >> 4);

	registers[Vx] &= registers[Vy];
}

// 8xy3 XOR Vx, Vy
fn op8xy3() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);
	const Vy: u8 = @intCast((opcode & 0x00F0) >> 4);

	registers[Vx] ^= registers[Vy];
}

// 8xy4 ADD Vx, Vy
fn op8xy4() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);
	const Vy: u8 = @intCast((opcode & 0x00F0) >> 4);

	const sum = registers[Vx] + registers[Vy];

	if (sum > 255) {
        registers[0xF] = 1;
	} else {
        registers[0xF] = 0;
	}

	registers[Vx] = sum & 0xFF;
}

// 8xy5 SUB Vx, Vy
fn op8xy5() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);
	const Vy: u8 = @intCast((opcode & 0x00F0) >> 4);

	if (registers[Vx] > registers[Vy]) {
        registers[0xF] = 1;
	} else {
	    registers[0xF] = 0;
	}

	registers[Vx] -= registers[Vy];
}

// 8xy6 SHR Vx
fn op8xy6() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);

    registers[0xF] = registers[Vx] & 0x1;
    registers[Vx] >>= 1;
}

// 8xy7 SUBN Vx, Vy
fn op8xy7() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);
	const Vy: u8 = @intCast((opcode & 0x00F0) >> 4);

	if (registers[Vy] > registers[Vx]) {
        registers[0xF] = 1;
	} else {
        registers[0xF] = 0;
	}

	registers[Vx] = registers[Vy] - registers[Vx];
}

// 8xyE SHL Vx
fn op8xyE() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);

    registers[0xF] = (registers[Vx] & 0x80) >> 7;
    registers[Vx] <<= 1;
}

// 9xy0 SNE Vx, Vy
fn op9xy0() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);
	const Vy: u8 = @intCast((opcode & 0x00F0) >> 4);

	if (registers[Vx] != registers[Vy]) {
        program_counter += 2;
	}
}

// Annn LD I, addr
fn opAnnn() void {
    const address = opcode & 0x0FFF;
    index_register = address;
}

// Bnnn JP V0, addr
fn opBnnn() void {
    const address = opcode & 0x0FFF;
    program_counter = registers[0] + address;
}

// Cxkk RND Vx, byte
fn opCxkk() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);
    const byte: u8 = @intCast(opcode & 0x00FF);

    registers[Vx] = random.intRangeAtMost(u8, 0, 255) & byte;
}

// Dxyn Vx, Vy, nibble
fn opDxyn() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);
	const Vy: u8 = @intCast((opcode & 0x00F0) >> 4);
	const height: u8 = @intCast(opcode & 0x000F);

	const x_pos = registers[Vx] % framebuffer_width;
	const y_pos = registers[Vy] % framebuffer_height;

	registers[0xF] = 0;

	for (0..height) |row| {
	    const sprite_byte = address_space[index_register + row];

		for (0..8) |col| {
		    const sprite_pixel = sprite_byte & (@as(u8, 0x80) >> @as(u3, @intCast(col)));
			const screen_pixel: *u32 = @ptrCast(@alignCast(&framebuffer[(y_pos + row) * framebuffer_width + (x_pos + col)]));

			if (sprite_pixel != 0) {
			    if (screen_pixel.* == 0xFFFFFFFF) {
					registers[0xF] = 1;
				}

				screen_pixel.* ^= 0xFFFFFFFF;
			}
		}
	}
}

// Ex9E SKP Vx
fn opEx9E() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);

    const key = registers[Vx];
    if (keypad[key] != 0) {
        program_counter += 2;
    }
}

// ExA1 SKNP Vx
fn opExA1() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);

    const key = registers[Vx];
    if (keypad[key] == 0) {
        program_counter += 2;
    }
}

// Fx07 LD Vx, DT
fn opFx07() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);

    registers[Vx] = delay_timer;
}

// Fx0A LD Vx, K
fn opFx0A() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);

    if (keypad[0] != 0) {
		registers[Vx] = 0;
	} else if (keypad[1] != 0) {
		registers[Vx] = 1;
	} else if (keypad[2] != 0) {
		registers[Vx] = 2;
	} else if (keypad[3] != 0) {
		registers[Vx] = 3;
	} else if (keypad[4] != 0) {
		registers[Vx] = 4;
	} else if (keypad[5] != 0) {
		registers[Vx] = 5;
	} else if (keypad[6] != 0) {
		registers[Vx] = 6;
	} else if (keypad[7] != 0) {
		registers[Vx] = 7;
	} else if (keypad[8] != 0) {
		registers[Vx] = 8;
	} else if (keypad[9] != 0) {
		registers[Vx] = 9;
	} else if (keypad[10] != 0) {
		registers[Vx] = 10;
	} else if (keypad[11] != 0) {
		registers[Vx] = 11;
	} else if (keypad[12] != 0) {
		registers[Vx] = 12;
	} else if (keypad[13] != 0) {
		registers[Vx] = 13;
	} else if (keypad[14] != 0) {
		registers[Vx] = 14;
	} else if (keypad[15] != 0) {
		registers[Vx] = 15;
	} else {
		program_counter -= 2;
	}
}

// Fx15 LD DT, Vx
fn opFx15() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);

    delay_timer = registers[Vx];
}

// Fx18 LD ST, Vx
fn opFx18() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);

    sound_timer = registers[Vx];
}

// Fx1E ADD I, Vx
fn opFx1E() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);

    index_register += registers[Vx];
}

// Fx29 LD F, Vx
fn opFx29() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);
    const digit: u8 = registers[Vx];

    index_register = fontset_start + (5 * digit);
}

// Fx33 LD B, Vx
fn opFx33() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);
    var value: u8 = registers[Vx];

    address_space[index_register + 2] = value % 10;
    value /= 10;

    address_space[index_register + 1] = value % 10;
    value /= 10;

    address_space[index_register] = value % 10;
}

// Fx55 LD [I], Vx
fn opFx55() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);

    for (0..Vx + 1) |i| {
        registers[i] = address_space[index_register + i];
    }
}

// Fx65 LD Vx, [I]
fn opFx65() void {
    const Vx: u8 = @intCast((opcode & 0x0F00) >> 8);

    for (0..Vx + 1) |i| {
        registers[i] = address_space[index_register + i];
    }
}

fn loadROM(filename: []const u8) void {
    var file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        std.debug.print("failed to open file: {}", .{err});
        return;
    };
    defer file.close();

    file.read(address_space[0x0200..]) catch |err| {
        std.debug.print("failed to read ROM: {}", .{err});
        return;
    };
}
