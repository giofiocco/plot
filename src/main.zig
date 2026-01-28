const std = @import("std");

const parser = @import("parser.zig");
const Parser = parser.Parser;
const Expr = parser.Expr;

const c = @cImport({
    @cDefine("STB_IMAGE_WRITE_IMPLEMENTATION", "");
    @cDefine("STB_IMAGE_WRITE_STATIC", "");
    @cInclude("stb_image_write.h");
});

const Curve = union(enum) {
    function: *Expr,
};

const Plot2D = struct {
    allocator: std.mem.Allocator,
    curves: std.ArrayList(Curve),
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var args = std.process.args();
    _ = args.skip();
    const filename = args.next() orelse {
        std.log.err("ERROR: missing arg: file name", .{});
        std.process.exit(1);
    };

    const content = try std.fs.cwd().readFileAlloc(alloc, filename, std.math.maxInt(usize));

    var par = Parser.init(content, filename, alloc, true);
    std.debug.print("{any}\n", .{try par.next()});

    // const width = 128;
    // const height = 128;
    // const comp = 3;
    // var data: [width * height * comp]u8 = undefined;
    //
    // _ = c.stbi_write_png("prova.png", width, height, comp, &data, comp * @sizeOf(u8));
}
