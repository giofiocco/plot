const std = @import("std");
const stdout = std.fs.File.stdout();
const stdin = std.fs.File.stdin();

const defs = @import("defs.zig");
const printError = defs.printError;

const parser = @import("parser.zig");
const Parser = parser.Parser;
const Expr = parser.Expr;

pub const c = @cImport({
    @cDefine("STB_IMAGE_WRITE_IMPLEMENTATION", "");
    @cDefine("STB_IMAGE_WRITE_STATIC", "");
    @cInclude("stb_image_write.h");
});

const canvasmod = @import("canvas.zig");
const Canvas = canvasmod.Canvas;

// const Result = union(enum) {
//     plot: Plot,
//     int: i32,
// };
//
// const StateError = error{
//     Redefinition,
// } || std.mem.Allocator.Error;
//
// const State = struct {
//     vars: std.StringArrayHashMap(*Expr),
//     allocator: std.mem.Allocator,
//     logerror: bool,
//
//     fn init(allocator: std.mem.Allocator, logerror: bool) State {
//         return .{
//             .vars = .init(allocator),
//             .allocator = allocator,
//             .logerror = logerror,
//         };
//     }
//
//     fn deinit(self: *State) void {
//         self.vars.deinit();
//     }
//
//     fn eval(self: *State, expr: *Expr) StateError!Result {
//         switch (expr.expr) {
//             .sym => {
//                 if (self.vars.get(expr.expr.sym)) |e| {
//                     try self.eval(e);
//                 } else {
//                     @panic("todo");
//                 }
//             },
//             .int => @panic("todo"),
//             .op => @panic("todo"),
//             .def => {
//                 if (self.vars.get(expr.expr.def.name)) |_| {
//                     printError(expr.loc, "redefinition", .{});
//                     return StateError.Redefinition;
//                 } else {
//                     try self.vars.put(expr.expr.def.name, try expr.expr.def.expr.deep_clone(self.allocator));
//                 }
//             },
//             .plot => {
//                 std.debug.print("plotting {f}\n", .{expr.expr.plot});
//                 try self.eval(expr.expr.plot);
//             },
//         }
//     }
// };

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}).init;
    const allocator = gpa.allocator();
    defer {
        const alloc_status = gpa.deinit();
        std.debug.print("gpa status: {}\n", .{alloc_status});
    }

    // var args = std.process.args();
    // _ = args.skip();
    // const filename = args.next() orelse {
    //     std.log.err("ERROR: missing arg: file name", .{});
    //     std.process.exit(1);
    // };
    //
    // const content = try std.fs.cwd().readFileAlloc(allocator, filename, std.math.maxInt(usize));
    // defer allocator.free(content);
    //
    // var par = Parser.init(content, filename, allocator, true);
    // var state = State.init(allocator, true);
    // defer state.deinit();
    //
    // while (try par.next()) |expr| {
    //     std.debug.print("{f}\n", .{expr});
    //     try state.eval(expr);
    //     expr.free(allocator);
    // }

    var canvas = try Canvas.init(allocator, 200, 200, 0.1, .{ .anchor = .center });
    defer canvas.deinit();

    const bc: []canvasmod.Bytecode = @constCast(&[_]canvasmod.Bytecode{ .{ .variable = .x }, .{ .num = 2 }, .{ .op = .pow } });
    try canvas.plot(bc);

    canvas.saveToFile("x2.png");

    // var image = try ImageCanvas.init(allocator, 200, 200, 0.01, .{ .x = 100, .y = 0 });
    // defer image.deinit(allocator);
    //
    // const canvas = Canvas.from(ImageCanvas, &image);
    //
    // var plot = try Plotter.init(FloatRange.fromStep(-1, 1, 0.01), allocator);
    // defer plot.deinit();
    // try plot.plot(canvas, bc);
    //
    // image.saveToFile("x2.png");
}
