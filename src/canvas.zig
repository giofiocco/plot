const std = @import("std");
const assert = std.debug.assert;

const c = @import("main.zig").c;

pub const Operator = enum {
    pow,
    sin,
    sub,
    sum,
    mul,
    eq,
    lt,

    const max_args = 2;

    pub fn toString(self: Operator) []const u8 {
        return switch (self) {
            .pow => "^",
            .sin => "sin",
            .sub => "-",
            .sum => "+",
            .mul => "*",
            .eq => "=",
            .lt => "<",
        };
    }

    pub fn argNumber(self: Operator) usize {
        return switch (self) {
            .pow, .sub, .sum, .mul, .eq, .lt => 2,
            .sin => 1,
        };
    }
};

pub const Variable = enum { x, y };

const Anchor = enum { center, south, south_west };

pub const Bytecode = union(enum) {
    num: f64,
    variable: Variable,
    op: Operator,
};

pub const BytecodeList = struct {
    items: std.ArrayList(Bytecode),
    x: bool,
    y: bool,

    pub const empty: BytecodeList = .{ .items = .empty, .x = false, .y = false };

    pub fn deinit(self: *BytecodeList, allocator: std.mem.Allocator) void {
        self.items.deinit(allocator);
    }

    pub fn append(self: *BytecodeList, allocator: std.mem.Allocator, item: Bytecode) std.mem.Allocator.Error!void {
        if (item == .variable) {
            switch (item.variable) {
                .x => self.x = true,
                .y => self.y = true,
            }
        }
        try self.items.append(allocator, item);
    }
};

pub const FloatRange = struct {
    min: f64,
    max: f64,
    step: f64,
    count: usize,
    i: usize,

    pub fn fromStep(min: f64, max: f64, step: f64) FloatRange {
        return .{ .min = min, .max = max, .step = step, .count = 1 + @as(usize, @intFromFloat(@ceil((max - min) / step))), .i = 0 };
    }

    pub fn fromCount(min: f64, max: f64, count: usize) FloatRange {
        return .{ .min = min, .max = max, .step = (max - min) / count, .count = count, .i = 0 };
    }

    fn reset(self: *FloatRange) void {
        self.i = 0;
    }

    fn next(self: *FloatRange) ?f64 {
        const v = self.min + self.step * @as(f64, @floatFromInt(self.i));
        self.i += 1;
        if (self.i > self.count) return null;
        return v;
    }
};

pub const Point = struct {
    x: f64,
    y: f64,

    fn sub(self: Point, other: Point) Point {
        return .{ .x = self.x - other.x, .y = self.y - other.y };
    }

    fn div(self: Point, other: f64) Point {
        return .{ .x = self.x / other, .y = self.y / other };
    }

    pub fn format(self: Point, w: *std.Io.Writer) !void {
        try w.print("({}, {})", .{ self.x, self.y });
    }
};

pub const BytecodeVM = struct {
    stack: std.ArrayList(f64),
    args: [Operator.max_args]f64,

    pub const Error = error{ MissingOpArgs, UnusedValues, NoResultValue } || std.mem.Allocator.Error;

    fn init() BytecodeVM {
        return .{ .stack = .empty, .args = undefined };
    }

    fn deinit(self: *BytecodeVM, allocator: std.mem.Allocator) void {
        self.stack.deinit(allocator);
    }

    fn eval(self: *BytecodeVM, allocator: std.mem.Allocator, bytecodes: BytecodeList, x: f64, y: f64) BytecodeVM.Error!f64 {
        self.stack.clearRetainingCapacity();
        for (bytecodes.items.items) |bc| {
            switch (bc) {
                .num => |n| try self.stack.append(allocator, n),
                .variable => |v| switch (v) {
                    .x => try self.stack.append(allocator, x),
                    .y => try self.stack.append(allocator, y),
                },
                .op => |op| {
                    for (0..op.argNumber()) |i| {
                        self.args[i] = self.stack.pop().?;
                    }

                    switch (op) {
                        .pow => try self.stack.append(allocator, std.math.pow(f64, self.args[0], self.args[1])),
                        .sin => try self.stack.append(allocator, std.math.sin(self.args[0])),
                        .sub => try self.stack.append(allocator, self.args[1] - self.args[0]),
                        .sum => try self.stack.append(allocator, self.args[0] + self.args[1]),
                        .mul => try self.stack.append(allocator, self.args[0] * self.args[1]),
                        .eq => try self.stack.append(allocator, std.math.clamp(1 - 30 * @abs(self.args[0] - self.args[1]), 0, 1)),
                        .lt => try self.stack.append(allocator, std.math.clamp(30 * (self.args[0] - self.args[1]), 0, 1)),
                    }
                },
            }
        }

        if (self.stack.items.len > 1) return BytecodeVM.Error.UnusedValues;
        if (self.stack.items.len == 0) return BytecodeVM.Error.NoResultValue;
        return self.stack.items[0];
    }
};

// simple 2d canvas
// TODO: support raylib and moving and resizing
// TODO: support 3d and surfaces
pub const Canvas = struct {
    allocator: std.mem.Allocator,
    bytecode_vm: BytecodeVM,
    width: usize,
    height: usize,
    scale: f64,
    origin: Point,
    data: []u8,

    pub fn init(allocator: std.mem.Allocator, width: usize, height: usize, scale: f64, origin: union(enum) { point: Point, anchor: Anchor }) std.mem.Allocator.Error!Canvas {
        const canvas = Canvas{
            .allocator = allocator,
            .bytecode_vm = .init(),
            .width = width,
            .height = height,
            .scale = scale,
            .origin = switch (origin) {
                .anchor => |a| switch (a) {
                    .center => .{ .x = @floatFromInt(width / 2), .y = @floatFromInt(height / 2) },
                    .south => .{ .x = @floatFromInt(width / 2), .y = 0 },
                    .south_west => .{ .x = 0, .y = 0 },
                },
                .point => |p| p,
            },
            .data = try allocator.alloc(u8, width * height),
        };
        @memset(canvas.data, 0);
        return canvas;
    }

    pub fn deinit(self: *Canvas) void {
        self.allocator.free(self.data);
        self.bytecode_vm.deinit(self.allocator);
    }

    pub fn plot(self: *Canvas, bytecodes: BytecodeList) BytecodeVM.Error!void {
        var v = if (!bytecodes.x and !bytecodes.y) try self.bytecode_vm.eval(self.allocator, bytecodes, 0, 0) else 0;

        for (0..self.width) |x| {
            const float_x: f64 = (@as(f64, @floatFromInt(x)) - self.origin.x) * self.scale;
            if (bytecodes.x and !bytecodes.y) v = try self.bytecode_vm.eval(self.allocator, bytecodes, float_x, 0);

            for (0..self.height) |y| {
                const float_y: f64 = (@as(f64, @floatFromInt(y)) - self.origin.y) * self.scale;
                if (bytecodes.y)
                    v = try self.bytecode_vm.eval(self.allocator, bytecodes, float_x, float_y);

                const color = if (bytecodes.x and bytecodes.y)
                    v
                else if (bytecodes.x)
                    std.math.clamp(1 - 50 * @abs(v - float_y), 0, 1)
                else if (bytecodes.y)
                    std.math.clamp(1 - 50 * @abs(v - float_x), 0, 1)
                else
                    std.math.clamp(1 - 50 * @abs(v - float_y), 0, 1);

                self.data[y * self.width + x] +%= @intFromFloat(255 * color);
            }
        }
    }

    pub fn saveToFile(self: *Canvas, path: [:0]const u8) void {
        c.stbi_flip_vertically_on_write(1);
        _ = c.stbi_write_png(path, @intCast(self.width), @intCast(self.height), 1, self.data.ptr, 0);
    }
};
