const std = @import("std");
const assert = std.debug.assert;

const c = @import("main.zig").c;

pub const Operator = enum {
    pow,
    sin,
    sub,
    sum,

    fn toString(self: Operator) []u8 {
        return switch (self) {
            .pow => "^",
            .sin => "sin",
            .sub => "-",
            .sub => "+",
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

    pub const Error = error{ MissingOpArgs, UnusedValues, NoResultValue } || std.mem.Allocator.Error;

    fn init() BytecodeVM {
        return .{ .stack = .empty };
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
                .op => |op| switch (op) {
                    .pow => {
                        if (self.stack.items.len < 2) return BytecodeVM.Error.MissingOpArgs;
                        const n = self.stack.pop().?;
                        const m = self.stack.pop().?;
                        try self.stack.append(allocator, std.math.pow(f64, m, n));
                    },
                    .sin => {
                        if (self.stack.items.len < 1) return BytecodeVM.Error.MissingOpArgs;
                        const n = self.stack.pop().?;
                        try self.stack.append(allocator, std.math.sin(n));
                    },
                    .sub => {
                        if (self.stack.items.len < 2) return BytecodeVM.Error.MissingOpArgs;
                        const n = self.stack.pop().?;
                        const m = self.stack.pop().?;
                        try self.stack.append(allocator, m - n);
                    },
                    .sum => {
                        if (self.stack.items.len < 2) return BytecodeVM.Error.MissingOpArgs;
                        const n = self.stack.pop().?;
                        const m = self.stack.pop().?;
                        try self.stack.append(allocator, m + n);
                    },
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

    const with_lines = false;

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

    fn drawLine(self: *Canvas, from: Point, to: Point) void {
        const fromx: usize = @intFromFloat(from.x / self.scale + self.origin.x);
        const tox: usize = @intFromFloat(to.x / self.scale + self.origin.x);
        var fromy: isize = @intFromFloat(from.y / self.scale + self.origin.y);
        var toy: isize = @intFromFloat(to.y / self.scale + self.origin.y);

        if (toy < fromy) {
            const tmp = toy;
            toy = fromy;
            fromy = tmp;
        } else if (toy == fromy) {
            toy += 1;
        }

        if (toy < 0) {
            return;
        } else if (fromy < 0) {
            fromy = 0;
        }

        if (fromx < self.width and fromy < self.height) {
            self.data[@as(usize, @intCast(fromy)) * self.width + fromx] = 255;
        }
        if (tox < self.width and toy < self.height) {
            self.data[@as(usize, @intCast(toy)) * self.width + tox] = 255;
        }

        // for (@intCast(fromy)..@intCast(toy)) |y| {
        //     for (fromx..tox) |x| {
        //         if (x < self.width and y < self.height) {
        //             self.data[y * self.width + x] = 255;
        //         }
        //     }
        // }
    }

    pub fn plot(self: *Canvas, bytecodes: BytecodeList) BytecodeVM.Error!void {
        var last: ?Point = null;
        for (0..self.width) |x| {
            const float_x: f64 = (@as(f64, @floatFromInt(x)) - self.origin.x) * self.scale;
            const float_y = try self.bytecode_vm.eval(self.allocator, bytecodes, float_x, 0);

            const p = Point{ .x = float_x, .y = float_y };
            if (with_lines) {
                if (last) |a| {
                    self.drawLine(a, p);
                }
                last = p;
            } else {
                const y: isize = @intFromFloat(float_y / self.scale + self.origin.y);
                if (0 < y and y < self.height) {
                    self.data[@as(usize, @intCast(y)) * self.width + x] = 255;
                }
            }
        }
    }

    // TODO: line mode
    pub fn plot_intersection(self: *Canvas, bytecodes_a: BytecodeList, bytecodes_b: BytecodeList) BytecodeVM.Error!void {
        var a = if (!bytecodes_a.x and !bytecodes_a.y) try self.bytecode_vm.eval(self.allocator, bytecodes_a, 0, 0) else 0;
        var b = if (!bytecodes_b.x and !bytecodes_b.y) try self.bytecode_vm.eval(self.allocator, bytecodes_b, 0, 0) else 0;

        for (0..self.width) |x| {
            const float_x: f64 = (@as(f64, @floatFromInt(x)) - self.origin.x) * self.scale;

            if (bytecodes_a.x and !bytecodes_a.y) a = try self.bytecode_vm.eval(self.allocator, bytecodes_a, float_x, 0);
            if (bytecodes_b.x and !bytecodes_b.y) b = try self.bytecode_vm.eval(self.allocator, bytecodes_b, float_x, 0);

            for (0..self.height) |y| {
                const float_y: f64 = (@as(f64, @floatFromInt(y)) - self.origin.y) * self.scale;

                if (bytecodes_a.x and bytecodes_a.y) a = try self.bytecode_vm.eval(self.allocator, bytecodes_a, float_x, float_y);
                if (bytecodes_b.x and bytecodes_b.y) b = try self.bytecode_vm.eval(self.allocator, bytecodes_b, float_x, float_y);

                if (@abs(a - b) < self.scale) {
                    self.data[y * self.width + x] = 255;
                }
            }
        }
    }

    pub fn saveToFile(self: *Canvas, path: [:0]const u8) void {
        c.stbi_flip_vertically_on_write(1);
        _ = c.stbi_write_png(path, @intCast(self.width), @intCast(self.height), 1, self.data.ptr, 0);
    }
};
