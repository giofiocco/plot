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
    allocator: std.mem.Allocator,
    logerror: bool,

    const Error = error{Redefinition} || canvasmod.BytecodeVM.Error || std.mem.Allocator.Error;

    fn init(allocator: std.mem.Allocator, logerror: bool) State {
        return .{
            .vars = .init(allocator),
            .allocator = allocator,
            .logerror = logerror,
        };
    }

    fn deinit(self: *State) void {
        self.vars.deinit();
    }

    // TODO: error if null
    fn eval(self: *State, expr: *Expr, bytecodes: ?*BytecodeList) State.Error!void {
        switch (expr.expr) {
            .sym => |str| {
                if (self.vars.get(str)) |e| {
                    try self.eval(e, bytecodes);
                } else {
                    inline for (std.meta.fields(Variable)) |v| {
                        if (std.mem.eql(u8, str, v.name)) {
                            try bytecodes.?.append(self.allocator, .{ .variable = @enumFromInt(v.value) });
                            break;
                        }
                    } else {
                        @panic("todo");
                    }
                }
                // } else if (std.mem.eql(u8, str, "x")) {
                //     try bytecodes.?.append(self.allocator, .{ .variable = .x });
                // } else if (std.mem.eql(u8, str, "y")) {
                //     try bytecodes.?.append(self.allocator, .{ .variable = .x });
                // } else {
                //     @panic("todo");
                // }
            },
            .num => |n| try bytecodes.?.append(self.allocator, .{ .num = n }),
            .op => |op| {
                assert(op.items[0].expr == .sym);
                if (std.mem.eql(u8, op.items[0].expr.sym, "^")) {
                    assert(op.items.len == 3);
                    try self.eval(op.items[2], bytecodes);
                    try self.eval(op.items[1], bytecodes);
                    try bytecodes.?.append(self.allocator, .{ .op = .pow });
                } else if (std.mem.eql(u8, op.items[0].expr.sym, "-")) {
                    assert(op.items.len == 3);
                    try self.eval(op.items[2], bytecodes);
                    try self.eval(op.items[1], bytecodes);
                    try bytecodes.?.append(self.allocator, .{ .op = .sub });
                } else if (std.mem.eql(u8, op.items[0].expr.sym, "+")) {
                    assert(op.items.len == 3);
                    try self.eval(op.items[2], bytecodes);
                    try self.eval(op.items[1], bytecodes);
                    try bytecodes.?.append(self.allocator, .{ .op = .sum });
                } else if (std.mem.eql(u8, op.items[0].expr.sym, "sin")) {
                    assert(op.items.len == 2);
                    try self.eval(op.items[1], bytecodes);
                    try bytecodes.?.append(self.allocator, .{ .op = .sin });
                } else {
                    @panic("todo");
                }
            },
            .def => {
                assert(bytecodes == null);
                if (self.vars.get(expr.expr.def.name)) |_| {
                    printError(expr.loc, "redefinition", .{});
                    return State.Error.Redefinition;
                } else {
                    try self.vars.put(expr.expr.def.name, try expr.expr.def.expr.deep_clone(self.allocator));
                }
            },
            .plot => |plot| {
                assert(bytecodes == null);
                var canvas = try Canvas.init(self.allocator, 100, 100, 0.05, .{ .anchor = .center });
                defer canvas.deinit();

                if (plot.expr == .op and plot.expr.op.items[0].expr == .sym and std.mem.eql(u8, plot.expr.op.items[0].expr.sym, "=")) {
                    var code_a = BytecodeList.empty;
                    defer code_a.deinit(self.allocator);
                    try self.eval(plot.expr.op.items[1], &code_a);

                    var code_b = BytecodeList.empty;
                    defer code_b.deinit(self.allocator);
                    try self.eval(plot.expr.op.items[2], &code_b);
                    try canvas.plot_intersection(code_a, code_b);
                } else {
                    var code = BytecodeList.empty;
                    defer code.deinit(self.allocator);
                    try self.eval(plot, &code);
                    try canvas.plot(code);
                }

                canvas.saveToFile("x2.png");
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

    var state = State.init(allocator, true);
    defer state.deinit();

    var par = Parser.init(content, filename, allocator, true);

    while (try par.next()) |expr| {
        std.debug.print("{f}\n", .{expr});
        try state.eval(expr, null);
        expr.free(allocator);
    }

    // var canvas = try Canvas.init(allocator, 100, 100, 0.05, .{ .anchor = .center });
    // defer canvas.deinit();
    //
    // const bc: []canvasmod.Bytecode = @constCast(&[_]canvasmod.Bytecode{
    //     .{ .num = 1 },
    //     .{ .variable = .x },
    //     .{ .num = 2 },
    //     .{ .op = .pow },
    //     .{ .op = .sub },
    //     .{ .num = 0.5 },
    //     .{ .op = .pow },
    // });
    // try canvas.plot(bc);
    //
    // canvas.saveToFile("x2.png");
}
