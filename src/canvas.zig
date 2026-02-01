const std = @import("std");
const assert = std.debug.assert;

const c = @import("main.zig").c;

const Operator = enum { pow };

const Variable = enum { x, y, z, color };

const Anchor = enum { center, south, south_west };

pub const Bytecode = union(enum) {
    num: f64,
    variable: Variable,
    op: Operator,
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

// simple 2d canvas
// TODO: support raylib and moving and resizing
// TODO: support 3d and surfaces
pub const Canvas = struct {
    allocator: std.mem.Allocator,
    width: usize,
    height: usize,
    scale: f64,
    origin: Point,
    data: []u8,

    const Error = error{ MissingOpArgs, UnusedValues, NoResultValue } || std.mem.Allocator.Error;

    pub fn init(allocator: std.mem.Allocator, width: usize, height: usize, scale: f64, origin: union(enum) { point: Point, anchor: Anchor }) std.mem.Allocator.Error!Canvas {
        const canvas = Canvas{
            .allocator = allocator,
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

        for (@intCast(fromy)..@intCast(toy)) |y| {
            for (fromx..tox) |x| {
                if (x < self.width and y < self.height) {
                    self.data[y * self.width + x] = 255;
                }
            }
        }
    }

    pub fn plot(self: *Canvas, bytecodes: []Bytecode) Canvas.Error!void {
        var stack = std.ArrayList(f64).empty;
        defer stack.deinit(self.allocator);

        var last: ?Point = null;
        for (0..self.width) |x| {
            const float_x: f64 = (@as(f64, @floatFromInt(x)) - self.origin.x) * self.scale;

            stack.clearRetainingCapacity();
            for (bytecodes) |bc| {
                switch (bc) {
                    .num => |n| try stack.append(self.allocator, n),
                    .variable => |v| {
                        assert(v == .x);
                        try stack.append(self.allocator, float_x);
                    },
                    .op => |op| switch (op) {
                        .pow => {
                            if (stack.items.len < 2) return Canvas.Error.MissingOpArgs;
                            const n = stack.pop().?;
                            const m = stack.pop().?;
                            try stack.append(self.allocator, std.math.pow(f64, m, n));
                        },
                    },
                }
            }

            if (stack.items.len > 1) return Canvas.Error.UnusedValues;
            if (stack.items.len == 0) return Canvas.Error.NoResultValue;
            const y = stack.items[0];

            // const int_y: usize = @intFromFloat(y / self.scale);

            const p = Point{ .x = float_x, .y = y };
            if (last) |a| {
                self.drawLine(a, p);
            }
            last = p;
        }
    }

    pub fn saveToFile(self: *Canvas, path: [:0]const u8) void {
        c.stbi_flip_vertically_on_write(1);
        _ = c.stbi_write_png(path, @intCast(self.width), @intCast(self.height), 1, self.data.ptr, 0);
    }
};
