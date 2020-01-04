const std = @import("std");
const assert = @import("std").debug.assert;
const c = @import("c.zig");
const Allocator = std.mem.Allocator;

const EvalError = Allocator.Error || error{
    InvalidOp,
    DivideByZero,
    FunctionNoSymbolStart,
    NotANumber,
    TooManyArguments,
    IncorrectTypes,
};

fn eval_error_str(err: EvalError) []const u8 {
    return switch (err) {
        EvalError.InvalidOp => "Invalid Operation",
        EvalError.DivideByZero => "Division by zero",
        EvalError.FunctionNoSymbolStart => "Function doesn't start with symbol",
        EvalError.NotANumber => "Not a number",
        EvalError.TooManyArguments => "Too many arguments",
        EvalError.IncorrectTypes => "Incorrect types",
        Allocator.Error.OutOfMemory => "Out of memory",
    };
}

const Tag = enum {
    Num,
    Sym,
    Sexpr,
    Qexpr,
};

const LValTagged = union(Tag) {
    Num: i64,
    Sym: []u8,
    Sexpr: ?[]*LValTagged,
    Qexpr: ?[]*LValTagged,

    const Self = @This();

    fn free(self: *Self, allocator: *Allocator) void {
        switch (self.*) {
            .Num => {},
            .Sym => |sym| allocator.free(sym),
            .Sexpr, .Qexpr => |cells_opt| if (cells_opt) |cells| {
                for (cells) |child_tagged| {
                    child_tagged.free(allocator);
                }
                allocator.free(cells);
            },
        }
        allocator.destroy(self);
    }

    fn print(self: *Self) void {
        switch (self.*) {
            .Num => |i| std.debug.warn("{}", .{i}),
            .Sym => |s| std.debug.warn("{}", .{s}),
            .Sexpr => |sexpr_opt| if (sexpr_opt) |sexpr| expr_print(sexpr, "(", ")"),
            .Qexpr => |qexpr_opt| if (qexpr_opt) |qexpr| expr_print(qexpr, "{", "}"),
        }
    }

    fn expr_print(cells: []*LValTagged, open: []const u8, close: []const u8) void {
        std.debug.warn("{}", .{open});
        for (cells) |child_tagged, i| {
            child_tagged.print();
            if (i != cells.len - 1) std.debug.warn(" ", .{});
        }
        std.debug.warn("{}", .{close});
    }

    fn eval(self: *Self, allocator: *Allocator) EvalError!*LValTagged {
        switch (self.*) {
            .Num => return self,
            .Sym => return self,
            .Sexpr => |cells_opt| {
                if (cells_opt) |cells| {
                    if (cells.len == 0) return self;

                    for (cells) |*cell| {
                        const new_cell = try cell.*.eval(allocator);
                        if (new_cell != cell.*) cell.*.free(allocator);
                        cell.* = new_cell;
                    }

                    if (cells.len == 1) return pop(allocator, cells, 0);

                    const f = pop(allocator, cells, 0);
                    defer f.free(allocator);

                    switch (f.*) {
                        .Sym => |sym| return try builtin(allocator, self, cells, sym),
                        else => return EvalError.FunctionNoSymbolStart,
                    }
                } else {
                    return self;
                }
            },
            .Qexpr => return self,
        }
    }

    fn pop(allocator: *Allocator, slice: []*LValTagged, index: usize) *LValTagged {
        const value = slice[index];
        std.mem.copy(*LValTagged, slice[index..], slice[index + 1 ..]);
        slice = allocator.shrink(slice, slice.len - 1);
        return value;
    }
};

fn builtin_head(allocator: *Allocator, slice: []*LValTagged) EvalError!*LValTagged {
    if (slice.len != 1) return EvalError.TooManyArguments;

    const v = LValTagged.pop(allocator, slice, 0);
    defer v.free(allocator);

    switch (v.*) {
        .Qexpr => |cells_opt| if (cells_opt) |cells|
            return LValTagged.pop(allocator, cells, 0)
        else
            return EvalError.InvalidOp,
        else => return EvalError.IncorrectTypes,
    }
}

fn builtin_tail(allocator: *Allocator, slice: []*LValTagged) EvalError!*LValTagged {
    if (slice.len != 1) return EvalError.TooManyArguments;

    const tail = LValTagged.pop(allocator, slice, 0);
    errdefer tail.free(allocator);

    switch (tail.*) {
        .Qexpr => |cells_opt| if (cells_opt) |cells| {
            LValTagged.pop(allocator, cells, 0).free(allocator);
            return tail;
        } else {
            return EvalError.InvalidOp;
        },
        else => return EvalError.IncorrectTypes,
    }
}

fn builtin_list(allocator: *Allocator, val: *LValTagged) *LValTagged {
    switch (val.*) {
        .Sexpr => |cells_opt| val.* = LValTagged{ .Qexpr = cells_opt },
        else => {},
    }
    return val;
}

fn builtin_eval(allocator: *Allocator, slice: []*LValTagged) EvalError!*LValTagged {
    if (slice.len != 1) return EvalError.TooManyArguments;
    const v = LValTagged.pop(allocator, slice, 0);
    errdefer v.free(allocator);

    switch (v.*) {
        .Sexpr => {},
        .Qexpr => |cells_opt| v.* = LValTagged{ .Sexpr = cells_opt },
        else => return EvalError.IncorrectTypes,
    }

    const result = try v.eval(allocator);
    if (result != v) v.free(allocator);
    return result;
}

fn builtin_join(allocator: *Allocator, slice: []*LValTagged) EvalError!*LValTagged {
    var x = LValTagged.pop(allocator, slice, 0);
    errdefer x.free(allocator);
    while (slice.len > 0) {
        const y = LValTagged.pop(allocator, slice, 0);
        defer y.free(allocator);
        x = try lval_join(allocator, x, y);
    }
    return x;
}

fn lval_join(allocator: *Allocator, x: *LValTagged, y: *LValTagged) EvalError!*LValTagged {
    var x_temp = x;
    switch (y.*) {
        .Qexpr => |cells_opt| if (cells_opt) |cells| {
            while (cells.len > 0) {
                const cell = LValTagged.pop(allocator, cells, 0);
                errdefer cell.free(allocator);
                x_temp = try lval_add(allocator, x_temp, cell);
            }
        },
        else => return EvalError.IncorrectTypes,
    }
    return x_temp;
}

fn builtin_op(allocator: *Allocator, slice: []*LValTagged, sym: []u8) EvalError!*LValTagged {
    const x = LValTagged.pop(allocator, slice, 0);
    errdefer x.free(allocator);

    switch (x.*) {
        .Num => |*num| {
            if (std.mem.eql(u8, sym, "-") and slice.len == 0) {
                num.* = -num.*;
                return x;
            }

            var i: usize = 0;
            while (i < slice.len) : (i += 1) {
                switch (slice[i].*) {
                    .Num => |other_num| {
                        if (std.mem.eql(u8, sym, "+")) num.* += other_num;
                        if (std.mem.eql(u8, sym, "-")) num.* -= other_num;
                        if (std.mem.eql(u8, sym, "*")) num.* *= other_num;
                        if (std.mem.eql(u8, sym, "/")) {
                            if (other_num == 0)
                                return EvalError.DivideByZero;
                            num.* = @divTrunc(num.*, other_num);
                        }
                    },
                    else => return EvalError.NotANumber,
                }
            }
            return x;
        },
        else => return EvalError.NotANumber,
    }
}

fn builtin(allocator: *Allocator, self: *LValTagged, slice: []*LValTagged, func: []u8) EvalError!*LValTagged {
    if (std.mem.eql(u8, func, "list")) return builtin_list(allocator, self);
    if (std.mem.eql(u8, func, "head")) return builtin_head(allocator, slice);
    if (std.mem.eql(u8, func, "tail")) return builtin_tail(allocator, slice);
    if (std.mem.eql(u8, func, "join")) return builtin_join(allocator, slice);
    if (std.mem.eql(u8, func, "eval")) return builtin_eval(allocator, slice);
    // TODO(DarinM223): throw error if func is not an operator.
    return builtin_op(allocator, slice, func);
}

const LVal = struct {
    tagged: *LValTagged,
    allocator: *Allocator,

    const Self = @This();

    fn num_init(allocator: *Allocator, m: i64) Allocator.Error!Self {
        var lval = try allocator.create(LValTagged);
        lval.* = LValTagged{ .Num = m };
        return LVal{ .tagged = lval, .allocator = allocator };
    }

    fn sym_init(allocator: *Allocator, m: [*c]const u8) Allocator.Error!Self {
        var lval = try allocator.create(LValTagged);
        errdefer allocator.destroy(lval);

        var m_slice = m[0..c.strlen(m)];
        var sym = try allocator.alloc(u8, m_slice.len);
        errdefer allocator.free(sym);

        std.mem.copy(u8, sym, m_slice);
        lval.* = LValTagged{ .Sym = sym };
        return LVal{ .tagged = lval, .allocator = allocator };
    }

    fn sexpr_init(allocator: *Allocator) Allocator.Error!Self {
        var lval = try allocator.create(LValTagged);
        lval.* = LValTagged{ .Sexpr = null };
        return LVal{ .tagged = lval, .allocator = allocator };
    }

    fn qexpr_init(allocator: *Allocator) Allocator.Error!Self {
        var lval = try allocator.create(LValTagged);
        lval.* = LValTagged{ .Qexpr = null };
        return LVal{ .tagged = lval, .allocator = allocator };
    }

    fn deinit(self: Self) void {
        self.tagged.free(self.allocator);
    }

    fn print(self: Self) void {
        self.tagged.print();
        std.debug.warn("\n", .{});
    }

    fn eval(self: *Self) EvalError!void {
        const new_tagged = try self.tagged.eval(self.allocator);
        if (new_tagged != self.tagged) self.tagged.free(self.allocator);
        self.tagged = new_tagged;
    }
};

const ReadError = Allocator.Error || error{InvalidNum};

fn lval_read_num(allocator: *Allocator, node: *c.mpc_ast_t) ReadError!LVal {
    var x: c_long = undefined;
    if (c.parseLong(node.contents, &x) == 0) {
        return ReadError.InvalidNum;
    }
    return LVal.num_init(allocator, x);
}

fn lval_read(allocator: *Allocator, node: *c.mpc_ast_t) ReadError!LVal {
    if (c.strstr(node.tag, "number") != 0)
        return lval_read_num(allocator, node);
    if (c.strstr(node.tag, "symbol") != 0)
        return LVal.sym_init(allocator, node.*.contents);

    var x: ?LVal = null;
    errdefer if (x) |true_x| true_x.deinit();

    if (c.strcmp(node.tag, ">") == 0) x = try LVal.sexpr_init(allocator);
    if (c.strstr(node.tag, "sexpr") != 0) x = try LVal.sexpr_init(allocator);
    if (c.strstr(node.tag, "qexpr") != 0) x = try LVal.qexpr_init(allocator);

    var i: usize = 0;
    while (i < node.children_num) : (i += 1) {
        if (c.strcmp(node.children[i].*.contents, "(") == 0) continue;
        if (c.strcmp(node.children[i].*.contents, ")") == 0) continue;
        if (c.strcmp(node.children[i].*.contents, "{") == 0) continue;
        if (c.strcmp(node.children[i].*.contents, "}") == 0) continue;
        if (c.strcmp(node.children[i].*.tag, "regex") == 0) continue;
        const child = (try lval_read(allocator, node.children[i])).tagged;
        x = LVal{
            .tagged = try lval_add(allocator, x.?.tagged, child),
            .allocator = allocator,
        };
    }
    return x.?;
}

fn lval_add(allocator: *Allocator, v: *LValTagged, x: *LValTagged) Allocator.Error!*LValTagged {
    switch (v.*) {
        .Sexpr, .Qexpr => |*expr_opt| {
            if (expr_opt.*) |*expr| {
                expr.* = try allocator.realloc(expr.*, expr.*.len + 1);
                expr.*[expr.*.len - 1] = x;
            } else {
                expr_opt.* = try allocator.alloc(*LValTagged, 1);
                expr_opt.*.?[0] = x;
            }
        },
        else => {},
    }
    return v;
}

pub fn eval(node: *c.mpc_ast_t) EvalError!i64 {
    if (c.strstr(node.tag, "number") != 0) {
        return c.atoi(node.contents);
    }

    const op = node.children[1].*.contents;
    var result = try eval(node.children[2]);
    var i: usize = 3;
    while (c.strstr(node.children[i].*.tag, "expr") != 0) : (i += 1) {
        result = try eval_op(result, op, try eval(node.children[i]));
    }
    return result;
}

fn eval_op(a: i64, op: *u8, b: i64) EvalError!i64 {
    if (c.strcmp(op, "+") == 0) return a + b;
    if (c.strcmp(op, "-") == 0) return a - b;
    if (c.strcmp(op, "*") == 0) return a * b;
    if (c.strcmp(op, "/") == 0) {
        if (b == 0) {
            return EvalError.DivideByZero;
        } else {
            return @divTrunc(a, b);
        }
    }
    return EvalError.InvalidOp;
}

// Combine errors with ||
const ReplError = ReadError || EvalError || std.os.ReadError;

pub fn repl() ReplError!void {
    const Number = c.mpc_new("number");
    const Symbol = c.mpc_new("symbol");
    const Sexpr = c.mpc_new("sexpr");
    const Qexpr = c.mpc_new("qexpr");
    const Expr = c.mpc_new("expr");
    const Lispy = c.mpc_new("lispy");

    _ = c.mpca_lang(c.MPCA_LANG_DEFAULT,
        \\ number   : /-?[0-9]+/ ;
        \\ symbol   : "list" | "head" | "tail"
        \\          | "join" | "eval" | '+' | '-' | '*' | '/' ;
        \\ sexpr    : '(' <expr>* ')' ;
        \\ qexpr    : '{' <expr>* '}' ;
        \\ expr     : <number> | <symbol> | <sexpr> | <qexpr> ;
        \\ lispy    : /^/ <expr>* /$/ ;
    , Number, Symbol, Sexpr, Qexpr, Expr, Lispy);
    defer c.mpc_cleanup(6, Number, Symbol, Sexpr, Qexpr, Expr, Lispy);

    var input: [2048]u8 = undefined;
    const stdin = std.io.getStdIn();
    while (true) {
        std.debug.warn("lispy> ", .{});
        const bytes_read = try stdin.read(&input);
        // Add null character for C string support.
        if (bytes_read > 0) input[bytes_read - 1] = 0;
        var r: c.mpc_result_t = undefined;
        if (c.mpc_parse("<stdin>", input[0..bytes_read].ptr, Lispy, &r) != 0) {
            var ptr = @ptrCast(*c.mpc_ast_t, @alignCast(@alignOf(c.mpc_ast_t), r.output.?));
            defer c.mpc_ast_delete(ptr);

            var lval = try lval_read(std.heap.direct_allocator, ptr);
            defer lval.deinit();

            lval.eval() catch |e| switch (e) {
                EvalError.InvalidOp, EvalError.DivideByZero, EvalError.FunctionNoSymbolStart, EvalError.NotANumber => {
                    std.debug.warn("{}\n", .{eval_error_str(e)});
                    continue;
                },
                else => |err| return err,
            };
            lval.print();

            //var result = eval(ptr) catch |e| switch (e) {
            //    EvalError.DivideByZero => {
            //        std.debug.warn("Divide by zero\n", .{});
            //        continue;
            //    },
            //    else => |err| return err,
            //};
            //std.debug.warn("{}\n", .{result});
        } else {
            var ptr = @ptrCast([*c]c.mpc_err_t, @field(r, "error").?);
            defer c.mpc_err_delete(ptr);
            c.mpc_err_print(ptr);
        }
    }
}

pub fn main() anyerror!void {
    std.debug.warn("Lispy version 0.0.0.0.1\n", .{});
    std.debug.warn("Press Ctrl+c to Exit\n", .{});
    try repl();
}
