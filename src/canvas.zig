const std = @import("std");
const assert = std.debug.assert;

const c = @import("main.zig").c;

const Expr = @import("parser.zig").Expr;

// TODO: expr op and variables to defs

pub const Operator = enum {
    pow,
    sin,
    sub,
    sum,
    mul,
    eq,
    lt,
    and_,
    or_,
    not,

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
            .and_ => "&",
            .or_ => "|",
            .not => "!",
        };
    }

    pub fn argNumber(self: Operator) usize {
        return switch (self) {
            .pow, .sub, .sum, .mul, .eq, .lt, .and_, .or_ => 2,
            .sin, .not => 1,
        };
    }

    pub fn isOutBool(self: Operator) bool {
        return switch (self) {
            .pow, .sin, .sub, .sum, .mul => false,
            .eq, .lt, .and_, .or_, .not => true,
        };
    }
};

pub const Variable = enum { x, y };

const Anchor = enum { center, south, south_west };

pub const Point = struct {
    x: f32,
    y: f32,

    pub fn format(self: Point, w: *std.Io.Writer) !void {
        try w.print("({}, {})", .{ self.x, self.y });
    }
};

// simple 2d canvas
// TODO: support raylib and moving and resizing
// TODO: support 3d and surfaces
// TODO: maybe want the *Allocator
pub const Canvas = struct {
    allocator: std.mem.Allocator,
    width: usize,
    height: usize,
    scale: f32,
    origin: Point,
    data: []u8,

    const epsilon = 0.01;

    pub fn init(allocator: std.mem.Allocator, width: usize, height: usize, scale: f32, origin: union(enum) { point: Point, anchor: Anchor }) std.mem.Allocator.Error!Canvas {
        const self = Canvas{
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
        @memset(self.data, 0);

        // TODO: for bool or xy creates wierd effects or gets deleted
        // const wy = self.width * @as(usize, @intFromFloat(self.origin.y));
        // const wx = @as(usize, @intFromFloat(self.origin.x));
        // for (0..self.width) |x| {
        //     self.data[wy + x] = 128;
        // }
        // for (0..self.height) |y| {
        //     self.data[y * self.width + wx] = 128;
        // }

        return self;
    }

    pub fn deinit(self: *Canvas) void {
        self.allocator.free(self.data);
    }

    fn setAt(self: *Canvas, x: usize, y: usize, v: f32) void {
        self.data[y * self.width + x] = @intFromFloat(std.math.clamp(@as(f32, @floatFromInt(self.data[y * self.width + x])) + 255 * v, 0, 255));
    }

    pub fn saveToFile(self: *Canvas, path: [:0]const u8) void {
        c.stbi_flip_vertically_on_write(1);
        _ = c.stbi_write_png(path, @intCast(self.width), @intCast(self.height), 1, self.data.ptr, 0);
    }

    pub fn plot(self: *Canvas, expr: *Expr) !void {
        var float_y: f32 = 0;
        var float_x: f32 = 0;
        var eval_count: usize = 0;

        switch (expr.outkind) {
            .bool => {
                for (0..self.height) |y| {
                    float_y = (@as(f32, @floatFromInt(y)) - self.origin.y) * self.scale;
                    for (0..self.width) |x| {
                        float_x = (@as(f32, @floatFromInt(x)) - self.origin.x) * self.scale;
                        if (expr.eval(float_x, float_y)) eval_count += 1;
                        self.setAt(x, y, expr.value.?);
                    }
                }
            },
            .xy => {
                for (0..self.height) |y| {
                    float_y = (@as(f32, @floatFromInt(y)) - self.origin.y) * self.scale;
                    for (0..self.width) |x| {
                        float_x = (@as(f32, @floatFromInt(x)) - self.origin.x) * self.scale;
                        if (expr.eval(float_x, float_y)) eval_count += 1;
                        self.setAt(x, y, 1 - @abs(expr.value.?) / epsilon);
                    }
                }
            },
            .x => {
                for (0..self.height) |y| {
                    float_y = (@as(f32, @floatFromInt(y)) - self.origin.y) * self.scale;
                    for (0..self.width) |x| {
                        float_x = (@as(f32, @floatFromInt(x)) - self.origin.x) * self.scale;
                        if (expr.eval(float_x, float_y)) eval_count += 1;
                        self.setAt(x, y, 1 - @abs(float_x - expr.value.?) / epsilon);
                    }
                }
            },
            .y => {
                for (0..self.width) |x| {
                    float_x = (@as(f32, @floatFromInt(x)) - self.origin.x) * self.scale;
                    for (0..self.height) |y| {
                        float_y = (@as(f32, @floatFromInt(y)) - self.origin.y) * self.scale;
                        if (expr.eval(float_x, float_y)) eval_count += 1;
                        self.setAt(x, y, 1 - @abs(float_y - expr.value.?) / epsilon);
                    }
                }
            },
        }

        std.debug.print("eval count: {}\n", .{eval_count});
    }
};
