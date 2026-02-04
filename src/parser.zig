const std = @import("std");
const assert = std.debug.assert;

const defs = @import("defs.zig");
const Loc = defs.Loc;
const printError = defs.printError;

const canvas_mod = @import("canvas.zig");
const Operator = canvas_mod.Operator;
const Variable = canvas_mod.Variable;

pub const Expr = struct {
    loc: Loc,
    expr: union(enum) {
        sym: []const u8,
        num: f64,
        op: Operator,
        app: std.ArrayList(*Expr),
        def: struct { name: []const u8, expr: *Expr },
        plot: *Expr,
    },

    fn alloc(self: Expr, allocator: std.mem.Allocator) !*Expr {
        const ptr = try allocator.create(Expr);
        ptr.* = self;
        return ptr;
    }

    pub fn deep_clone(self: *Expr, allocator: std.mem.Allocator) !*Expr {
        const ptr = try allocator.create(Expr);
        ptr.loc = self.loc;

        switch (self.expr) {
            .sym => |sym| ptr.expr = .{ .sym = try allocator.dupe(u8, sym) },
            .num => |n| ptr.expr = .{ .num = n },
            .op => |op| ptr.expr = .{ .op = op },
            .app => |args| {
                ptr.expr = .{ .app = .empty };
                for (args.items) |e| {
                    try ptr.expr.app.append(allocator, try e.deep_clone(allocator));
                }
            },
            .def => |def| ptr.expr = .{ .def = .{
                .name = try allocator.dupe(u8, def.name),
                .expr = try def.expr.deep_clone(allocator),
            } },
            .plot => |plot| ptr.expr = .{ .plot = try plot.deep_clone(allocator) },
        }

        return ptr;
    }

    pub fn deep_free(self: *Expr, allocator: std.mem.Allocator) void {
        switch (self.expr) {
            .sym => |sym| allocator.free(sym),
            .num => {},
            .op => {},
            .app => |app| {
                for (app.items) |e| {
                    e.free(allocator);
                }
                self.expr.app.deinit(allocator);
            },
            .def => |def| {
                allocator.free(def.name);
                def.expr.free(allocator);
            },
            .plot => |plot| plot.free(allocator),
        }
        allocator.destroy(self);
    }

    pub fn free(self: *Expr, allocator: std.mem.Allocator) void {
        switch (self.expr) {
            .sym => {},
            .num => {},
            .op => {},
            .app => {
                for (self.expr.app.items) |e| {
                    e.free(allocator);
                }
                self.expr.app.deinit(allocator);
            },
            .def => self.expr.def.expr.free(allocator),
            .plot => self.expr.plot.free(allocator),
        }
        allocator.destroy(self);
    }

    pub fn format(self: Expr, w: *std.Io.Writer) !void {
        switch (self.expr) {
            .sym => |sym| try w.print("{s}", .{sym}),
            .num => |n| try w.print("{}", .{n}),
            .op => |op| try w.print("{s}", .{op.toString()}),
            .app => |app| {
                try w.print("(", .{});
                for (app.items, 0..) |e, i| {
                    if (i > 0) try w.print(" ", .{});
                    try w.print("{f}", .{e});
                }
                try w.print(")", .{});
            },
            .def => |def| try w.print("(def {s} {f})", .{ def.name, def.expr }),
            .plot => |plot| try w.print("(plot {f})", .{plot}),
        }
    }
};

const Token = struct {
    loc: Loc,
    token: union(enum) {
        sym: []const u8,
        num: f64,
        op: Operator,
        open,
        close,
        def,
        plot,
    },
};

const TokenKind = std.meta.Tag(@FieldType(Token, "token"));

// TODO: put it in Parser.Error
const ParserError = error{
    InvalidChar,
    UnexpectedToken,
    UnexpectedEOF,
} || std.mem.Allocator.Error || std.fmt.ParseIntError;

const Tokenizer = struct {
    buffer: []const u8,
    loc: Loc,
    last: ?Token,
    logerror: bool,

    fn init(buffer: []const u8, filename: []const u8, logerror: bool) Tokenizer {
        return .{
            .buffer = buffer,
            .loc = .init(buffer, filename),
            .last = null,
            .logerror = logerror,
        };
    }

    fn isSym(c: u8) bool {
        return std.ascii.isPrint(c) and !std.ascii.isWhitespace(c) and c != '(' and c != ')';
    }

    fn consume(self: *Tokenizer, len: usize, t: @FieldType(Token, "token")) Token {
        assert(len >= 1);
        var loc = self.loc;
        loc.len = len;
        self.buffer = self.buffer[len..];
        self.loc.col += len;
        return .{ .loc = loc, .token = t };
    }

    fn next(self: *Tokenizer) ParserError!?Token {
        if (self.last) |last| {
            self.last = null;
            return last;
        }

        if (self.buffer.len == 0) return null;

        const c = self.buffer[0];
        switch (c) {
            ' ' => {
                self.buffer = self.buffer[1..];
                self.loc.col += 1;
                return try self.next();
            },
            '\n' => {
                self.buffer = self.buffer[1..];
                self.loc.col = 1;
                self.loc.row += 1;
                var newline: usize = 0;
                while (newline < self.buffer.len and self.buffer[newline] != '\n') newline += 1;
                self.loc.line = self.buffer[0..newline];
                return try self.next();
            },
            '#' => {
                var i: usize = 1;
                while (i < self.buffer.len and self.buffer[i] != '\n') i += 1;
                self.buffer = self.buffer[i..];
                self.loc.col += i;
                return try self.next();
            },
            '(' => return self.consume(1, .open),
            ')' => return self.consume(1, .close),
            else => {
                if (std.mem.startsWith(u8, self.buffer, "def")) {
                    return self.consume(3, .def);
                } else if (std.mem.startsWith(u8, self.buffer, "plot")) {
                    return self.consume(4, .plot);
                } else if (std.ascii.isDigit(c)) {
                    var i: usize = 1;
                    while (i < self.buffer.len and std.ascii.isDigit(self.buffer[i])) i += 1;
                    if (i < self.buffer.len and self.buffer[i] == '.') {
                        i += 1;
                        while (i < self.buffer.len and std.ascii.isDigit(self.buffer[i])) i += 1;
                    }
                    const num = try std.fmt.parseFloat(f64, self.buffer[0..i]);
                    return self.consume(i, .{ .num = num });
                } else if (isSym(c)) {
                    var i: usize = 1;
                    while (i < self.buffer.len and isSym(self.buffer[i])) i += 1;

                    inline for (std.meta.fields(Operator)) |op| {
                        if (std.mem.eql(u8, self.buffer[0..i], (@as(Operator, @enumFromInt(op.value))).toString())) {
                            return self.consume(i, .{ .op = @enumFromInt(op.value) });
                        }
                    }

                    return self.consume(i, .{ .sym = self.buffer[0..i] });
                } else {
                    if (self.logerror) printError(self.loc, "invalid char '{c}'", .{c});
                    return ParserError.InvalidChar;
                }
            },
        }
    }

    fn peek(self: *Tokenizer) ParserError!?Token {
        if (self.last) |last| {
            return last;
        } else {
            self.last = try self.next();
            return self.last;
        }
    }

    fn expect(self: *Tokenizer, kind: TokenKind) ParserError!Token {
        var loc = self.loc;
        if (try self.next()) |token| {
            if (token.token == kind) {
                return token;
            }
            loc = token.loc;
        }
        if (self.logerror) printError(loc, "unexpected token, expected {}", .{kind});
        return ParserError.UnexpectedToken;
    }

    fn next_if_kind(self: *Tokenizer, kind: TokenKind) ParserError!?Token {
        if (try self.peek()) |token| {
            if (token.token == kind) {
                _ = try self.next();
                return token;
            }
        }
        return null;
    }
};

pub const Parser = struct {
    tok: Tokenizer,
    allocator: std.mem.Allocator,
    logerror: bool,

    pub fn init(buffer: []const u8, filename: []const u8, allocator: std.mem.Allocator, logerror: bool) Parser {
        return .{
            .tok = .init(buffer, filename, logerror),
            .allocator = allocator,
            .logerror = logerror,
        };
    }

    fn next_expect(self: *Parser) ParserError!*Expr {
        const token = (try self.tok.peek()) orelse {
            if (self.logerror) printError(self.tok.loc, "unexpected EOF", .{});
            return ParserError.UnexpectedEOF;
        };

        switch (token.token) {
            .sym => |sym| {
                _ = try self.tok.next();
                return (Expr{ .loc = token.loc, .expr = .{ .sym = sym } }).alloc(self.allocator);
            },
            .num => |n| {
                _ = try self.tok.next();
                return (Expr{ .loc = token.loc, .expr = .{ .num = n } }).alloc(self.allocator);
            },
            .op => |op| {
                _ = try self.tok.next();
                return (Expr{ .loc = token.loc, .expr = .{ .op = op } }).alloc(self.allocator);
            },
            .open => {
                _ = try self.tok.next();
                if (try self.tok.next_if_kind(.def)) |_| {
                    const name = (try self.tok.expect(.sym)).token.sym;
                    const expr = try self.next_expect();
                    errdefer expr.free(self.allocator);

                    const end_loc = (try self.tok.expect(.close)).loc;

                    return (Expr{ .loc = token.loc.extend(end_loc), .expr = .{ .def = .{ .name = name, .expr = expr } } }).alloc(self.allocator);
                } else if (try self.tok.next_if_kind(.plot)) |_| {
                    const expr = try self.next_expect();
                    errdefer expr.free(self.allocator);

                    const end_loc = (try self.tok.expect(.close)).loc;

                    return (Expr{ .loc = token.loc.extend(end_loc), .expr = .{ .plot = expr } }).alloc(self.allocator);
                } else {
                    var args = std.ArrayList(*Expr).empty;
                    errdefer {
                        for (args.items) |e| {
                            e.free(self.allocator);
                        }
                        args.deinit(self.allocator);
                    }
                    try args.append(self.allocator, try self.next_expect());
                    while (((try self.tok.peek()) orelse {
                        if (self.logerror) printError(self.tok.loc, "unexpected EOF", .{});
                        return ParserError.UnexpectedEOF;
                    }).token != .close) {
                        try args.append(self.allocator, try self.next_expect());
                    }

                    const end_loc = (try self.tok.expect(.close)).loc;

                    return (Expr{ .loc = token.loc.extend(end_loc), .expr = .{ .app = args } }).alloc(self.allocator);
                }
            },
            .close, .def, .plot => {
                if (self.logerror) printError(token.loc, "unexpected token", .{});
                return ParserError.UnexpectedToken;
            },
        }
    }

    pub fn next(self: *Parser) ParserError!?*Expr {
        if (try self.tok.peek() == null) return null;
        return try self.next_expect();
    }
};

const expect = std.testing.expect;

test "tokenizer-comment" {
    const buffer: []const u8 = "# 2\n3";
    var tok = Tokenizer.init(buffer, "test", false);
    const token = (try tok.next()).?.token;
    try expect(token.num == 3);
}

test "tokenizer" {
    const buffer: []const u8 = "(def f (^ x 2))";
    var tok = Tokenizer.init(buffer, "test", false);
    try expect((try tok.next()).?.token == .open);
    try expect((try tok.next()).?.token == .def);
    try expect((try tok.next()).?.token == .sym);
    try expect((try tok.next()).?.token == .open);
    try expect((try tok.next()).?.token.op == .pow);
    try expect((try tok.next()).?.token == .sym);
    try expect((try tok.next()).?.token.num == 2);
    try expect((try tok.next()).?.token == .close);
    try expect((try tok.next()).?.token == .close);
    try expect(try tok.next() == null);
}

test "unexpected-token" {
    var par = Parser.init("def", "test", std.testing.allocator, false);
    try expect(par.next() == ParserError.UnexpectedToken);
}

test "parser-def" {
    var par = Parser.init("(def f x)", "test", std.testing.allocator, false);
    const expr = (try par.next()).?;
    defer expr.free(std.testing.allocator);
    try expect(std.mem.eql(u8, expr.expr.def.name, "f"));
    try expect(std.mem.eql(u8, expr.expr.def.expr.expr.sym, "x"));
}

test "parser-def-dont-leak" {
    var par = Parser.init("(def f x", "test", std.testing.allocator, false);
    try expect(par.next() == ParserError.UnexpectedToken);
}

test "parser-app" {
    var par = Parser.init("(^ 2 x)", "test", std.testing.allocator, false);
    const expr = (try par.next()).?;
    defer expr.free(std.testing.allocator);
    try expect(expr.expr.app.items[0].expr.op == .pow);
    try expect(expr.expr.app.items[1].expr.num == 2);
    try expect(std.mem.eql(u8, expr.expr.app.items[2].expr.sym, "x"));
}

test "parser-app-dont-leak" {
    var par = Parser.init("(+ 1 2 3", "test", std.testing.allocator, false);
    try expect(par.next() == ParserError.UnexpectedEOF);
}
