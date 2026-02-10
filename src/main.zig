const std = @import("std");
const stdout = std.fs.File.stdout();
const stdin = std.fs.File.stdin();
const assert = std.debug.assert;

pub const c = @cImport({
    @cDefine("STB_IMAGE_WRITE_IMPLEMENTATION", "");
    @cDefine("STB_IMAGE_WRITE_STATIC", "");
    @cInclude("stb_image_write.h");
});

const defs = @import("defs.zig");
const printError = defs.printError;

const parser = @import("parser.zig");
const Parser = parser.Parser;
const Expr = parser.Expr;

const canvasmod = @import("canvas.zig");
const Canvas = canvasmod.Canvas;
const Operator = canvasmod.Operator;
const Variable = canvasmod.Variable;

const State = struct {
    vars: std.StringArrayHashMap(*Expr),
    canvas: Canvas,
    allocator: std.mem.Allocator,
    logerror: bool,
    logtime: bool,

    const Error = error{
        Undefined,
        Redefinition,
        Invalid,
        CannotEval,
    } || std.mem.Allocator.Error;

    fn init(allocator: std.mem.Allocator, logerror: bool, logtime: bool) std.mem.Allocator.Error!State {
        return .{
            .vars = .init(allocator),
            .canvas = try .init(allocator, 500, 500, 0.01, .{ .anchor = .center }),
            .allocator = allocator,
            .logerror = logerror,
            .logtime = logtime,
        };
    }

    fn deinit(self: *State) void {
        var iter = self.vars.iterator();
        while (iter.next()) |entry| {
            entry.value_ptr.*.free(self.allocator);
        }
        self.vars.deinit();
        self.canvas.deinit();
    }

    fn eval(self: *State, expr: *Expr) State.Error!void {
        if (expr.x or expr.y) {
            if (self.logerror) printError(expr.loc, "cannot eval variable dependent expression not in plot, maybe wrap in (plot ...)", .{});
            return State.Error.CannotEval;
        }

        switch (expr.expr) {
            .variable => unreachable,
            .sym => |s| {
                if (self.vars.get(s.sym)) |v| {
                    try self.eval(v);
                } else {
                    if (self.logerror) printError(expr.loc, "undefined symbol", .{});
                    return State.Error.Undefined;
                }
            },
            .op, .num, .app => {
                printError(expr.loc, "invalid expression to eval", .{});
                return State.Error.Invalid;
            },
            .def => |def| {
                if (self.vars.get(def.name)) |_| {
                    printError(expr.loc, "redefinition", .{});
                    return State.Error.Redefinition;
                } else {
                    try self.vars.put(def.name, try def.expr.clone(self.allocator));
                }
            },
            .plot => |e| {
                const start = std.time.nanoTimestamp();

                try self.canvas.plot(e);

                if (self.logtime) {
                    const now = std.time.nanoTimestamp();
                    std.debug.print("time plotting: {d:.3} ms\n", .{@as(f32, @floatFromInt(now - start)) * 1e-6});
                }
            },
        }
    }
};

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}).init;
    const allocator = gpa.allocator();
    defer {
        const alloc_status = gpa.deinit();
        std.debug.print("gpa status: {}\n", .{alloc_status});
    }

    var args = std.process.args();
    _ = args.skip();
    const filename = args.next() orelse {
        std.log.err("ERROR: missing arg: file name", .{});
        std.process.exit(1);
    };

    const content = try std.fs.cwd().readFileAlloc(allocator, filename, std.math.maxInt(usize));
    defer allocator.free(content);

    var state = try State.init(allocator, true, true);
    defer state.deinit();

    var par = Parser.init(content, filename, allocator, true);

    while (try par.next()) |expr| {
        std.debug.print("{f}\n", .{expr});
        state.eval(expr) catch {};
        expr.free(allocator);
    }

    state.canvas.saveToFile("x2.png");
}
