const std = @import("std");
const assert = std.debug.assert;
const c = @import("main.zig").c;

const Operator = enum {
    pow,
};

const Variable = enum {
    x,
    y,
    z,
    color,
};

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

const Point = struct {
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

// plotter y=f(x)
pub const Plotter = struct {
    allocator: std.mem.Allocator,
    stack: std.ArrayList(f64),
    xrange: FloatRange,
    data: []f64, // TODO: remove data

    const Error = error{ MissingOpArgs, UnusedValues, NoResultValue } || std.mem.Allocator.Error;

    pub fn init(xrange: FloatRange, allocator: std.mem.Allocator) Plotter.Error!Plotter {
        return .{
            .allocator = allocator,
            .stack = .empty,
            .xrange = xrange,
            .data = try allocator.alloc(f64, xrange.count),
        };
    }

    pub fn deinit(self: *Plotter) void {
        self.stack.deinit(self.allocator);
        self.allocator.free(self.data);
    }

    // pub fn plot(self: *Plotter, canvas: Canvas, bytecode: []Bytecode) Plotter.Error!void {
    //     var xrange = canvas.getXRange();
    //
    //     for (xrange.next()) |x| {}
    // }

    pub fn plot(self: *Plotter, canvas: Canvas, bytecode: []Bytecode) Plotter.Error!void {
        self.xrange.reset();
        var last: ?Point = null;

        while (self.xrange.next()) |x| {
            self.stack.clearRetainingCapacity();
            for (bytecode) |b| {
                switch (b) {
                    .num => |n| try self.stack.append(self.allocator, n),
                    .variable => |v| {
                        assert(v == .x);
                        try self.stack.append(self.allocator, x);
                    },
                    .op => |op| switch (op) {
                        .pow => {
                            if (self.stack.items.len < 2) return Plotter.Error.MissingOpArgs;
                            const n = self.stack.pop().?;
                            const m = self.stack.pop().?;
                            try self.stack.append(self.allocator, std.math.pow(f64, m, n));
                        },
                    },
                }
            }

            if (self.stack.items.len > 1) return Plotter.Error.UnusedValues;
            if (self.stack.items.len == 0) return Plotter.Error.NoResultValue;
            const y = self.stack.items[0];
            const p = Point{ .x = x, .y = y };

            if (last) |a| {
                canvas.drawLine(a, p);
            }
            last = p;
        }
    }
};

pub const Canvas = struct {
    context: *anyopaque,
    drawLineFn: *const fn (self: *anyopaque, from: Point, to: Point) void,
    getXRangeFn: *const fn (self: *anyopaque) FloatRange,

    pub fn from(comptime T: type, context: *T) Canvas {
        return .{
            .context = context,
            .drawLineFn = T.drawLine,
            .getXRangeFn = T.getXRange,
        };
    }

    fn drawLine(self: Canvas, a: Point, b: Point) void {
        self.drawLineFn(self.context, a, b);
    }

    fn getXRange(self: Canvas) FloatRange {
        return self.getXRangeFn(self.context);
    }
};

pub const ImageCanvas = struct {
    width: usize,
    height: usize,
    scale: f64,
    origin: Point,
    data: []u8,

    const comp = 1;

    pub fn init(allocator: std.mem.Allocator, width: usize, height: usize, scale: f64, origin: Point) !ImageCanvas {
        const canvas = ImageCanvas{
            .width = width,
            .height = height,
            .scale = scale,
            .origin = origin,
            .data = try allocator.alloc(u8, width * height * comp),
        };
        @memset(canvas.data, 0);
        return canvas;
    }

    pub fn deinit(self: *ImageCanvas, allocator: std.mem.Allocator) void {
        allocator.free(self.data);
    }

    fn getXRange(ctx: *anyopaque) FloatRange {
        const self: *ImageCanvas = @ptrCast(@alignCast(ctx));
        return FloatRange.fromStep(0 - self.origin.x, @as(f64, @floatFromInt(self.width)) - self.origin.x, self.scale);
    }

    fn pointToPixels(self: *ImageCanvas, p: Point) struct { usize, usize } {
        return .{
            @as(usize, @intFromFloat((p.x / self.scale) - self.origin.x)),
            @as(usize, @intFromFloat((p.y / self.scale) - self.origin.y)),
        };
    }

    fn drawLine(ctx: *anyopaque, from: Point, to: Point) void {
        const self: *ImageCanvas = @ptrCast(@alignCast(ctx));
        assert(comp == 1);

        const m = (to.y - from.y) / (to.x - from.x);

        // std.debug.print("{f} -> {f} {}\n", .{ from, to, m });

        var fy: f64 = 0;
        var x: usize = 0;
        var y: usize = 0;
        var xrange = FloatRange.fromStep(from.x, to.x, self.scale);

        std.debug.print("{f} {f} {any}\n", .{ from, to, xrange });

        while (xrange.next()) |fx| {
            fy = from.y + m * fx;

            x = @intFromFloat(@abs(fx / self.scale));
            y = @intFromFloat(@abs(fy / self.scale));

            // std.debug.print("{} {}\n", .{ fx, x });

            // x, y = self.pointToPixels(Point{ .x = fx, .y = fy });
            if (0 <= y and y < self.height and 0 <= x and x < self.width) {
                // std.debug.print("{} {}\n", .{ x, y });
                self.data[y * self.width + x] = 255;
            }
        }
    }

    pub fn saveToFile(self: ImageCanvas, path: [:0]const u8) void {
        _ = c.stbi_write_png(path, @intCast(self.width), @intCast(self.height), comp, self.data.ptr, comp * @sizeOf(u8));
    }
};
