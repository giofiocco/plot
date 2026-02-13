const std = @import("std");
const assert = std.debug.assert;

const defs = @import("defs.zig");
const Loc = defs.Loc;
const printError = defs.printError;

const canvas_mod = @import("canvas.zig");
const Operator = canvas_mod.Operator;
const Variable = canvas_mod.Variable;

pub const Func = struct {
    args: std.ArrayList([]const u8),
    body: *Expr,

    pub fn clone(self: Func, allocator: std.mem.Allocator) std.mem.Allocator.Error!Func {
        // TODO: maybe clone each args
        return .{
            .args = try self.args.clone(allocator),
            .body = try self.body.clone(allocator),
        };
    }

    pub fn free(self: *Func, allocator: std.mem.Allocator) void {
        self.args.deinit(allocator);
        self.body.free(allocator);
    }
};

pub const Expr = struct {
    loc: Loc,
    expr: union(enum) {
        sym: struct { sym: []const u8, resolve: ?*Expr },
        num: f32,
        op: Operator,
        variable: Variable,
        app: std.ArrayList(*Expr),
        def: struct { name: []const u8, func: Func },
        plot: *Expr,
    },
    value: ?f32,
    lastx: f32,
    lasty: f32,
    outkind: enum(u3) { x, y, xy, bool },
    x: bool,
    y: bool,

    const epsilon = 0.001;

    fn init(allocator: std.mem.Allocator, loc: Loc, expr: @FieldType(Expr, "expr")) std.mem.Allocator.Error!*Expr {
        const self = try allocator.create(Expr);
        self.* = .{
            .loc = loc,
            .expr = expr,
            .value = null,
            .outkind = .y,
            .lastx = 0,
            .lasty = 0,
            .x = false,
            .y = false,
        };
        return self;
    }

    pub fn clone(self: Expr, allocator: std.mem.Allocator) std.mem.Allocator.Error!*Expr {
        const ptr = try allocator.create(Expr);
        ptr.* = self;
        switch (self.expr) {
            .sym, .num, .op, .variable => {},
            .app => |app| {
                ptr.expr.app = try .initCapacity(allocator, app.capacity);
                for (app.items) |e| {
                    ptr.expr.app.appendAssumeCapacity(try e.clone(allocator));
                }
            },
            .def => |def| {
                ptr.expr.def.name = try allocator.dupe(u8, def.name);
                ptr.expr.def.func = try def.func.clone(allocator);
            },
            .plot => |e| ptr.expr.plot = try e.clone(allocator),
        }
        return ptr;
    }

    pub fn free(self: *Expr, allocator: std.mem.Allocator) void {
        switch (self.expr) {
            .sym, .num, .op, .variable => {},
            .app => |*app| {
                for (app.items) |e| e.free(allocator);
                app.deinit(allocator);
            },
            .def => |*def| {
                def.func.free(allocator);
            },
            .plot => |e| e.free(allocator),
        }
        allocator.destroy(self);
    }

    pub fn format(self: Expr, w: *std.Io.Writer) !void {
        switch (self.expr) {
            .sym => |s| try w.print("{s}", .{s.sym}),
            .num => |n| try w.print("{}", .{n}),
            .op => |op| try w.print("{s}", .{op.toString()}),
            .variable => |v| try w.print("{s}", .{@tagName(v)}),
            .app => |app| {
                try w.print("(", .{});
                for (app.items, 0..) |e, i| {
                    if (i > 0) try w.print(" ", .{});
                    try w.print("{f}", .{e});
                }
                try w.print(")", .{});
            },
            .def => |def| {
                try w.print("(def ({s}", .{def.name});
                for (def.func.args.items) |s| {
                    try w.print(" {s}", .{s});
                }
                try w.print(") {f})", .{def.func.body});
            },
            .plot => |plot| try w.print("(plot {f})", .{plot}),
        }
    }

    pub fn debug(self: *Expr) void {
        std.debug.print("{f} x:{} y:{} out:{}\n", .{ self, self.x, self.y, self.outkind });
    }

    pub fn compute_dep_out(self: *Expr) void {
        // TODO: put x y outkind altogheter
        switch (self.expr) {
            .num, .op => {},
            .sym => |s| {
                s.resolve.?.compute_dep_out();
                self.x = s.resolve.?.x;
                self.y = s.resolve.?.y;
                self.outkind = s.resolve.?.outkind;
            },
            .variable => |v| switch (v) {
                .x => self.x = true,
                .y => self.y = true,
            },
            .app => |app| {
                const first = app.items[0].expr;
                if (first == .sym) {
                    first.sym.resolve.?.compute_dep_out();
                    self.x = first.sym.resolve.?.x;
                    self.y = first.sym.resolve.?.y;
                    self.outkind = first.sym.resolve.?.outkind;
                } else {
                    for (app.items) |e| {
                        e.compute_dep_out();
                        self.x |= e.x;
                        self.y |= e.y;
                        if (e.outkind == .bool) self.outkind = .bool;
                    }

                    if (first == .op and first.op.isOutBool()) {
                        self.outkind = .bool;
                    }
                }
            },
            .def, .plot => unreachable,
        }
        if (self.outkind != .bool) {
            if (self.x and self.y)
                self.outkind = .xy
            else if (self.y)
                self.outkind = .x;
        }
    }

    pub fn eval(self: *Expr, x: f32, y: f32) bool {
        if (self.value == null or
            (self.x and @abs(self.lastx - x) > epsilon) or
            (self.y and @abs(self.lasty - y) > epsilon))
        {
            switch (self.expr) {
                .sym => |s| {
                    _ = s.resolve.?.eval(x, y);
                    self.value = s.resolve.?.value;
                },
                .num => |n| self.value = n,
                .variable => |v| switch (v) {
                    .x => self.value = x,
                    .y => self.value = y,
                },
                .app => |app| {
                    const first = app.items[0].expr;
                    if (first == .op) {
                        for (app.items[1..]) |e| {
                            _ = e.eval(x, y);
                        }

                        self.value = switch (first.op) {
                            .pow => std.math.pow(f32, app.items[2].value.?, app.items[1].value.?),
                            .sin => std.math.sin(app.items[1].value.?),
                            .sub => app.items[1].value.? - app.items[2].value.?,
                            .sum => app.items[1].value.? + app.items[2].value.?,
                            .mul => app.items[1].value.? * app.items[2].value.?,
                            .eq => 1 - 30 * @abs(app.items[1].value.? - app.items[2].value.?), // TODO: unify 1 - 30 * and 1 - .../epsilon in canvas.plot
                            .lt => 1 - 30 * (app.items[1].value.? - app.items[2].value.?),
                            .and_ => app.items[1].value.? * app.items[2].value.?,
                            .or_ => app.items[1].value.? + app.items[2].value.?,
                            .not => 1 - 30 * app.items[1].value.?,
                        };
                        if (first.op.isOutBool()) {
                            self.value = std.math.clamp(self.value.?, 0, 1);
                        }
                    } else if (app.items[0].expr == .sym) {
                        _ = eval(app.items[0].expr.sym.resolve.?, x, y);
                        self.value = app.items[0].expr.sym.resolve.?.value;
                    } else {
                        unreachable;
                    }
                },
                .op => unreachable, // TODO: error
                .def => unreachable, // TODO: error
                .plot => unreachable, // TODO: error
            }

            self.lastx = x;
            self.lasty = y;
            return true;
        }
        return false;
    }
};

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

pub const Parser = struct {
    tok: Tokenizer,
    allocator: std.mem.Allocator,
    logerror: bool,

    const Error = error{
        InvalidChar,
        UnexpectedToken,
        UnexpectedEOF,
        MismatchedArgNumber,
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
                return Expr.init(self.allocator, token.loc, .{ .sym = .{ .sym = s, .resolve = null } });
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
                                unreachable; // TODO: error
                            }
                            if (i == 0) {
                                name = e.expr.sym.sym;
                            } else {
                                try args.append(self.allocator, e.expr.sym.sym);
                            }
                        }
                    } else if (def.expr == .sym) {
                        name = def.expr.sym.sym;
                    } else {
                        unreachable; // TODO: error
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
                    const loc = token.loc.extend(end_loc);

                    if (args.items[0].expr == .op and args.items[0].expr.op.argNumber() + 1 != args.items.len) {
                        if (self.logerror) printError(loc, "expected {} args, found {}", .{ args.items[0].expr.op.argNumber(), args.items.len - 1 });
                        return Parser.Error.MismatchedArgNumber;
                    }

                    return Expr.init(self.allocator, loc, .{ .app = args });
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
