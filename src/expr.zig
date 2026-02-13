const std = @import("std");

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

    pub fn init(allocator: std.mem.Allocator, loc: Loc, expr: @FieldType(Expr, "expr")) std.mem.Allocator.Error!*Expr {
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
