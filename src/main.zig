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
const BytecodeList = canvasmod.BytecodeList;
const Operator = canvasmod.Operator;
const Variable = canvasmod.Variable;

const State = struct {
    vars: std.StringArrayHashMap(*Expr),
    canvas: Canvas,
    allocator: std.mem.Allocator,
    logerror: bool,

    const Error = error{
        Undefined,
        Redefinition,
        Invalid,
    } || canvasmod.BytecodeVM.Error || std.mem.Allocator.Error;

    fn init(allocator: std.mem.Allocator, logerror: bool) std.mem.Allocator.Error!State {
        return .{
            .vars = .init(allocator),
            .canvas = try .init(allocator, 500, 500, 0.01, .{ .anchor = .center }),
            .allocator = allocator,
            .logerror = logerror,
        };
    }

    fn deinit(self: *State) void {
        var iter = self.vars.iterator();
        while (iter.next()) |entry| {
            entry.value_ptr.*.deep_free(self.allocator);
        }

        self.vars.deinit();
        self.canvas.deinit();
    }

    fn compile(self: *State, expr: *Expr, bytecodes: *BytecodeList) State.Error!void {
        switch (expr.expr) {
            .op, .def, .plot => {
                printError(expr.loc, "invalid expression to compile", .{});
                return State.Error.Invalid;
            },
            .sym => |sym| {
                if (self.vars.get(sym)) |e| {
                    try self.compile(e, bytecodes);
                } else {
                    if (self.logerror) printError(expr.loc, "undefined symbol", .{});
                    return State.Error.Undefined;
                }
            },
            .num => |n| try bytecodes.append(self.allocator, .{ .num = n }),
            .variable => |v| try bytecodes.append(self.allocator, .{ .variable = v }),
            .app => |app| {
                if (app.items[0].expr == .op) {
                    for (app.items[1..]) |e| {
                        try self.compile(e, bytecodes);
                    }
                    try bytecodes.append(self.allocator, .{ .op = app.items[0].expr.op });
                } else {
                    @panic("TODO: unimplemented");
                }
            },
        }
    }

    fn eval(self: *State, expr: *Expr) State.Error!void {
        switch (expr.expr) {
            .sym, .num, .op, .variable, .app => {
                printError(expr.loc, "invalid expression to eval", .{});
                return State.Error.Invalid;
            },
            .def => {
                if (self.vars.get(expr.expr.def.name)) |_| {
                    printError(expr.loc, "redefinition", .{});
                    return State.Error.Redefinition;
                } else {
                    try self.vars.put(expr.expr.def.name, try expr.expr.def.expr.deep_clone(self.allocator));
                }
            },
            .plot => |plot| {
                var code = BytecodeList.empty;
                try self.compile(plot, &code);
                defer code.deinit(self.allocator);

                try self.canvas.plot(code);

                self.canvas.saveToFile("x2.png");
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

    var state = try State.init(allocator, true);
    defer state.deinit();

    var par = Parser.init(content, filename, allocator, true);

    while (try par.next()) |expr| {
        std.debug.print("{f}\n", .{expr});
        state.eval(expr) catch {};
        expr.free(allocator);
    }
}
