const std = @import("std");
const assert = std.debug.assert;

const defs = @import("defs.zig");
const Loc = defs.Loc;
const printError = defs.printError;

pub const Expr = struct {
    loc: Loc,
    expr: union(enum) {
        sym: []const u8,
        int: i32,
        op: std.ArrayList(*Expr),
        def: struct { name: []const u8, expr: *Expr },
    },

    fn alloc(self: Expr, allocator: std.mem.Allocator) !*Expr {
        const ptr = try allocator.create(Expr);
        ptr.* = self;
        return ptr;
    }

    fn free(self: *Expr, allocator: std.mem.Allocator) void {
        switch (self.expr) {
            .sym => {},
            .int => {},
            .op => {
                for (self.expr.op.items) |e| {
                    e.free(allocator);
                }
                self.expr.op.deinit(allocator);
            },
            .def => self.expr.def.expr.free(allocator),
        }
        allocator.destroy(self);
    }
};

const Token = struct {
    loc: Loc,
    token: union(enum) {
        sym: []const u8,
        int: i32,
        open,
        close,
        def,
    },
};

const TokenKind = std.meta.Tag(@FieldType(Token, "token"));

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
            '(' => return self.consume(1, .open),
            ')' => return self.consume(1, .close),
            else => {
                if (std.mem.startsWith(u8, self.buffer, "def")) {
                    return self.consume(3, .def);
                } else if (std.ascii.isDigit(c)) {
                    var i: usize = 1;
                    while (i < self.buffer.len and std.ascii.isDigit(self.buffer[i])) i += 1;
                    const num = try std.fmt.parseInt(@FieldType(@FieldType(Token, "token"), "int"), self.buffer[0..i], 10);
                    return self.consume(i, .{ .int = num });
                } else if (isSym(c)) {
                    var i: usize = 1;
                    while (i < self.buffer.len and isSym(self.buffer[i])) i += 1;
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
            .sym => {
                _ = try self.tok.next();
                return (Expr{ .loc = token.loc, .expr = .{ .sym = token.token.sym } }).alloc(self.allocator);
            },
            .int => {
                _ = try self.tok.next();
                return (Expr{ .loc = token.loc, .expr = .{ .int = token.token.int } }).alloc(self.allocator);
            },
            .open => {
                _ = try self.tok.next();
                if (try self.tok.next_if_kind(.def)) |_| {
                    const name = (try self.tok.expect(.sym)).token.sym;
                    const expr = try self.next_expect();
                    errdefer expr.free(self.allocator);

                    const end_loc = (try self.tok.expect(.close)).loc;

                    return (Expr{ .loc = token.loc.extend(end_loc), .expr = .{ .def = .{ .name = name, .expr = expr } } }).alloc(self.allocator);
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

                    return (Expr{ .loc = token.loc.extend(end_loc), .expr = .{ .op = args } }).alloc(self.allocator);
                }
            },
            .close, .def => {
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

test "tokenizer" {
    const buffer: []const u8 = "(def f (^ x 2))";
    var tok = Tokenizer.init(buffer, "test", false);
    try expect((try tok.next()).?.token == .open);
    try expect((try tok.next()).?.token == .def);
    try expect((try tok.next()).?.token == .sym);
    try expect((try tok.next()).?.token == .open);
    try expect((try tok.next()).?.token == .sym);
    try expect((try tok.next()).?.token == .sym);
    try expect((try tok.next()).?.token == .int);
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

test "parser-op" {
    var par = Parser.init("(^ x 2)", "test", std.testing.allocator, false);
    const expr = (try par.next()).?;
    defer expr.free(std.testing.allocator);
    try expect(std.mem.eql(u8, expr.expr.op.items[0].expr.sym, "^"));
    try expect(std.mem.eql(u8, expr.expr.op.items[1].expr.sym, "x"));
    try expect(expr.expr.op.items[2].expr.int == 2);
}

test "parser-op-dont-leak" {
    var par = Parser.init("(+ 1 2 3", "test", std.testing.allocator, false);
    try expect(par.next() == ParserError.UnexpectedEOF);
}
