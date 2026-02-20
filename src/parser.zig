const std = @import("std");
const assert = std.debug.assert;

const defs = @import("defs.zig");
const Loc = defs.Loc;
const printError = defs.printError;

const canvas_mod = @import("canvas.zig");
const Operator = canvas_mod.Operator;
const Variable = canvas_mod.Variable;

const Expr = @import("expr.zig").Expr;

const Token = struct {
    loc: Loc,
    token: union(enum) {
        sym: []const u8,
        num: f32,
        op: Operator,
        variable: Variable,
        open,
        close,
        def,
        plot,
    },
};

const TokenKind = std.meta.Tag(@FieldType(Token, "token"));

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

    fn next(self: *Tokenizer) Parser.Error!?Token {
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
                    const num = try std.fmt.parseFloat(f32, self.buffer[0..i]);
                    return self.consume(i, .{ .num = num });
                } else if (isSym(c)) {
                    var i: usize = 1;
                    while (i < self.buffer.len and isSym(self.buffer[i])) i += 1;

                    inline for (std.meta.fields(Operator)) |op| {
                        if (std.mem.eql(u8, self.buffer[0..i], (@as(Operator, @enumFromInt(op.value))).toString())) {
                            return self.consume(i, .{ .op = @enumFromInt(op.value) });
                        }
                    }

                    inline for (std.meta.fields(Variable)) |v| {
                        if (std.mem.eql(u8, self.buffer[0..i], v.name)) {
                            return self.consume(i, .{ .variable = @enumFromInt(v.value) });
                        }
                    }

                    return self.consume(i, .{ .sym = self.buffer[0..i] });
                } else {
                    if (self.logerror) printError(self.loc, "invalid char '{c}'", .{c});
                    return Parser.Error.InvalidChar;
                }
            },
        }
    }

    fn peek(self: *Tokenizer) Parser.Error!?Token {
        if (self.last) |last| {
            return last;
        } else {
            self.last = try self.next();
            return self.last;
        }
    }

    fn expect(self: *Tokenizer, kind: TokenKind) Parser.Error!Token {
        var loc = self.loc;
        if (try self.next()) |token| {
            if (token.token == kind) {
                return token;
            }
            loc = token.loc;
        }
        if (self.logerror) printError(loc, "unexpected token, expected {}", .{kind});
        return Parser.Error.UnexpectedToken;
    }

    fn next_if_kind(self: *Tokenizer, kind: TokenKind) Parser.Error!?Token {
        if (try self.peek()) |token| {
            if (token.token == kind) {
                _ = try self.next();
                return token;
            }
        }
        return null;
    }
};

// TODO: unify errors

pub const Parser = struct {
    tok: Tokenizer,
    allocator: std.mem.Allocator,
    logerror: bool,

    const Error = error{
        InvalidChar,
        UnexpectedToken,
        UnexpectedEOF,
        InvalidDefName,
    } || std.mem.Allocator.Error || std.fmt.ParseIntError;

    pub fn init(buffer: []const u8, filename: []const u8, allocator: std.mem.Allocator, logerror: bool) Parser {
        return .{
            .tok = .init(buffer, filename, logerror),
            .allocator = allocator,
            .logerror = logerror,
        };
    }

    fn next_expect(self: *Parser) Parser.Error!*Expr {
        const token = (try self.tok.peek()) orelse {
            if (self.logerror) printError(self.tok.loc, "unexpected EOF", .{});
            return Parser.Error.UnexpectedEOF;
        };

        switch (token.token) {
            .sym => |s| {
                _ = try self.tok.next();
                return Expr.init(self.allocator, token.loc, .{ .sym = s });
            },
            .num => |n| {
                _ = try self.tok.next();
                return Expr.init(self.allocator, token.loc, .{ .num = n });
            },
            .op => |op| {
                _ = try self.tok.next();
                return Expr.init(self.allocator, token.loc, .{ .op = op });
            },
            .variable => |v| {
                _ = try self.tok.next();
                return Expr.init(self.allocator, token.loc, .{ .variable = v });
            },
            .open => {
                _ = try self.tok.next();
                if (try self.tok.next_if_kind(.def)) |_| {
                    var name: ?[]const u8 = null;
                    var args = std.ArrayList([]const u8).empty;

                    const def = try self.next_expect();
                    defer def.free(self.allocator);
                    if (def.expr == .app) {
                        for (def.expr.app.items, 0..) |e, i| {
                            if (e.expr != .sym) {
                                if (self.logerror) printError(e.loc, "invalid token, expected {}", .{.sym});
                                return Parser.Error.InvalidDefName;
                            }
                            if (i == 0) {
                                name = e.expr.sym;
                            } else {
                                try args.append(self.allocator, e.expr.sym);
                            }
                        }
                    } else if (def.expr == .sym) {
                        name = def.expr.sym;
                    } else {
                        if (self.logerror) printError(def.loc, "invalid def name, expected sym or (sym ...)", .{});
                        return Parser.Error.InvalidDefName;
                    }

                    const expr = try self.next_expect();
                    errdefer expr.free(self.allocator);

                    const end_loc = (try self.tok.expect(.close)).loc;

                    return Expr.init(self.allocator, token.loc.extend(end_loc), .{ .def = .{ .name = name.?, .func = .{ .args = args, .body = expr } } });
                } else if (try self.tok.next_if_kind(.plot)) |_| {
                    const expr = try self.next_expect();
                    errdefer expr.free(self.allocator);

                    const end_loc = (try self.tok.expect(.close)).loc;

                    return Expr.init(self.allocator, token.loc.extend(end_loc), .{ .plot = expr });
                } else {
                    var args = std.ArrayList(*Expr).empty;
                    errdefer {
                        for (args.items) |e| e.free(self.allocator);
                        args.deinit(self.allocator);
                    }
                    try args.append(self.allocator, try self.next_expect());
                    while (((try self.tok.peek()) orelse {
                        if (self.logerror) printError(self.tok.loc, "unexpected EOF", .{});
                        return Parser.Error.UnexpectedEOF;
                    }).token != .close) {
                        try args.append(self.allocator, try self.next_expect());
                    }

                    const end_loc = (try self.tok.expect(.close)).loc;

                    return Expr.init(self.allocator, token.loc.extend(end_loc), .{ .app = args });
                }
            },
            .close, .def, .plot => {
                if (self.logerror) printError(token.loc, "unexpected token", .{});
                return Parser.Error.UnexpectedToken;
            },
        }
    }

    pub fn next(self: *Parser) Parser.Error!?*Expr {
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
    try expect((try tok.next()).?.token.variable == .x);
    try expect((try tok.next()).?.token.num == 2);
    try expect((try tok.next()).?.token == .close);
    try expect((try tok.next()).?.token == .close);
    try expect(try tok.next() == null);
}

test "unexpected-token" {
    var par = Parser.init("def", "test", std.testing.allocator, false);
    try expect(par.next() == Parser.Error.UnexpectedToken);
}

test "parser-def" {
    var par = Parser.init("(def f x)", "test", std.testing.allocator, false);
    const expr = (try par.next()).?;
    defer expr.free(std.testing.allocator);
    try expect(std.mem.eql(u8, expr.expr.def.name, "f"));
    try expect(expr.expr.def.expr.expr.variable == .x);
}

test "parser-def-dont-leak" {
    var par = Parser.init("(def f x", "test", std.testing.allocator, false);
    try expect(par.next() == Parser.Error.UnexpectedToken);
}

test "parser-app" {
    var par = Parser.init("(^ 2 x)", "test", std.testing.allocator, false);
    const expr = (try par.next()).?;
    defer expr.free(std.testing.allocator);
    try expect(expr.expr.app.items[0].expr.op == .pow);
    try expect(expr.expr.app.items[1].expr.num == 2);
    try expect(expr.expr.app.items[2].expr.variable == .x);
}

test "parser-app-dont-leak" {
    var par = Parser.init("(+ 1 2 3", "test", std.testing.allocator, false);
    try expect(par.next() == Parser.Error.UnexpectedEOF);
}
