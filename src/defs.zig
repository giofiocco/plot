const std = @import("std");
const assert = std.debug.assert;

// TODO: stdout and stderr

pub const Loc = struct {
    row: usize,
    col: usize,
    line: []const u8, // the whole line for error report
    len: usize, // len of the underline for error report
    filename: ?[]const u8,

    pub fn init(buffer: []const u8, filename: ?[]const u8) Loc {
        var newline: usize = 0;
        while (newline < buffer.len and buffer[newline] != '\n') newline += 1;
        return .{
            .row = 1,
            .col = 1,
            .line = buffer[0..newline],
            .len = 1,
            .filename = filename,
        };
    }

    pub fn extend(self: Loc, other: Loc) Loc {
        if (self.filename != null and other.filename != null) {
            assert(std.mem.eql(u8, self.filename.?, other.filename.?));
        }
        assert(self.row <= other.row);
        assert(self.col <= other.col);

        var new = self;
        if (self.row == other.row) {
            new.len = std.math.clamp(other.col + other.len - self.col, self.len, self.line.len);
        } else {
            new.len = self.line.len;
        }
        return new;
    }
};

pub fn printError(loc: Loc, comptime fmt: [:0]const u8, args: anytype) void {
    if (loc.filename) |filename| std.debug.print("{s}:", .{filename});
    std.debug.print("{}:{}: ERROR: ", .{ loc.row, loc.col });
    std.debug.print(fmt, args);
    std.debug.print("\n", .{});
    std.debug.print("{:5} | {s}\n", .{ loc.row, loc.line });
    std.debug.print("      | {[a]s:[col]}{[b]s:~^[len]}\n", .{ .a = "^", .b = "", .col = loc.col, .len = loc.len - 1 });
}

pub const FloatRange = struct {
    min: f32,
    max: f32,
    step: f32,
    count: usize,
    i: usize,

    pub fn fromStep(min: f32, max: f32, step: f32) FloatRange {
        return .{ .min = min, .max = max, .step = step, .count = 1 + @as(usize, @intFromFloat(@ceil((max - min) / step))), .i = 0 };
    }

    pub fn fromCount(min: f32, max: f32, count: usize) FloatRange {
        return .{ .min = min, .max = max, .step = (max - min) / count, .count = count, .i = 0 };
    }

    fn reset(self: *FloatRange) void {
        self.i = 0;
    }

    fn next(self: *FloatRange) ?f32 {
        const v = self.min + self.step * @as(f32, @floatFromInt(self.i));
        self.i += 1;
        if (self.i > self.count) return null;
        return v;
    }
};
