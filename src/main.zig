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

    const Error = error{Redefinition} || canvasmod.BytecodeVM.Error || std.mem.Allocator.Error;

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
        // TODO: leak of (def f ...)
        // TODO: token variable and operations
        while (iter.next()) |entry| {
            entry.value_ptr.*.deep_free(self.allocator);
        }

        self.vars.deinit();
        self.canvas.deinit();
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
            },
            .num => |n| try bytecodes.?.append(self.allocator, .{ .num = n }),
            .op => @panic("todo"),
            .app => |app| {
                assert(app.items[0].expr == .op);

                if (app.items[0].expr == .op) {
                    const n = app.items[0].expr.op.argNumber();
                    if (app.items.len != n + 1) @panic("TODO: throw error");
                    for (app.items[1..]) |e| {
                        try self.eval(e, bytecodes);
                    }
                    try bytecodes.?.append(self.allocator, .{ .op = app.items[0].expr.op });
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

                var code = BytecodeList.empty;
                try self.eval(plot, &code);
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
