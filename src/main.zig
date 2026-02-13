const std = @import("std");
const stdout = std.fs.File.stdout().writer();
const stdin = std.fs.File.stdin().reader();
const assert = std.debug.assert;

pub const c = @cImport({
    @cDefine("STB_IMAGE_WRITE_IMPLEMENTATION", "");
    @cDefine("STB_IMAGE_WRITE_STATIC", "");
    @cInclude("stb_image_write.h");
});

const defs = @import("defs.zig");
const printError = defs.printError;

const Parser = @import("parser.zig").Parser;

const expr_mod = @import("expr.zig");
const Expr = expr_mod.Expr;
const Func = expr_mod.Func;

const canvasmod = @import("canvas.zig");
const Canvas = canvasmod.Canvas;
const Operator = canvasmod.Operator;
const Variable = canvasmod.Variable;

// TODO: prevend (def .. (def ..))

const State = struct {
    vars: std.StringHashMap(Func),
    canvas: Canvas,
    allocator: std.mem.Allocator,
    logerror: bool,
    logtime: bool,

    // TODO: do better State.Error
    const Error = error{
        Undefined,
        Redefinition,
        Invalid,
        CannotEval,
        Shadowing,
        NoArgs,
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
            entry.value_ptr.body.*.free(self.allocator);
            entry.value_ptr.args.deinit(self.allocator);
        }
        self.vars.deinit();
        self.canvas.deinit();
    }

    fn solve_symbols(self: *State, ptr: **Expr) State.Error!void {
        const expr = ptr.*;
        switch (expr.expr) {
            .def, .plot => unreachable,
            .variable, .op, .num => {},
            .sym => |*s| {
                if (self.vars.get(s.sym)) |v| {
                    if (v.args.items.len > 0) {
                        if (self.logerror) printError(expr.loc, "symbol expects {} arguments, try call with ({s} ...)", .{ v.args.items.len, s.sym });
                        return State.Error.NoArgs;
                    }
                    // TODO: sure?
                    ptr.* = try v.body.clone(self.allocator);
                    expr.free(self.allocator);
                    try self.solve_symbols(ptr);
                } else {
                    if (self.logerror) printError(expr.loc, "undefined symbol", .{});
                    return State.Error.Undefined;
                }
            },
            .app => |app| {
                if (app.items[0].expr == .sym) {
                    if (self.vars.get(app.items[0].expr.sym.sym)) |f| {
                        if (f.args.items.len != app.items.len - 1) {
                            if (self.logerror) printError(expr.loc, "symbol expects {} arguments, found {}", .{ f.args.items.len, app.items.len - 1 }); // TODO: curring
                            return State.Error.Invalid;
                        }

                        for (f.args.items, 0..) |arg, i| {
                            if (self.vars.get(arg)) |_| {
                                if (self.logerror) printError(expr.loc, "argument shadows already defined symbol: '{s}'", .{arg});
                                return State.Error.Shadowing;
                            }
                            try self.vars.put(arg, .{ .args = .empty, .body = app.items[i + 1] });
                        }
                        defer for (f.args.items) |arg| {
                            _ = self.vars.remove(arg);
                        };

                        ptr.* = try f.body.clone(self.allocator);
                        try self.solve_symbols(ptr);
                        expr.free(self.allocator);
                        return;
                    }
                }

                for (app.items) |*e| {
                    try self.solve_symbols(e);
                }
            },
        }
    }

    fn eval(self: *State, expr: *Expr) State.Error!void {
        if (expr.x or expr.y) {
            if (self.logerror) printError(expr.loc, "cannot eval variable dependent expression not in plot, maybe wrap in (plot ...)", .{});
            return State.Error.CannotEval;
        }

        // TODO: maybe separate statement and expr
        switch (expr.expr) {
            .sym, .num, .op, .variable, .app => unreachable,
            .def => |def| {
                if (self.vars.get(def.name)) |_| {
                    if (self.logerror) printError(expr.loc, "redefinition", .{});
                    return State.Error.Redefinition;
                } else if (def.func.body.x or def.func.body.y) {
                    if (self.logerror) printError(def.func.body.loc, "invalid sym definition: variable dependent", .{});
                    return State.Error.Invalid;
                }

                try self.vars.put(def.name, try def.func.clone(self.allocator));
            },
            .plot => |*e| {
                const start = std.time.nanoTimestamp();

                try self.solve_symbols(e);
                e.*.compute_dep_out();
                // std.debug.print("plotting: ", .{});
                // e.*.debug();

                try self.canvas.plot(e.*);

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
