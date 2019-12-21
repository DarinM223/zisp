const std = @import("std");
const c = @import("c.zig");
const Allocator = std.mem.Allocator;

const EvalError = error{
    InvalidOp,
    DivideByZero,
};

const Tag = enum {
    Num,
    Sym,
    Sexpr,
};

const LValTagged = union(Tag) {
    Num: i64,
    Sym: []u8,
    Sexpr: ?[]*LValTagged,

    const Self = @This();

    fn free(self: *Self, allocator: *Allocator) void {
        switch (self.*) {
            .Num => {},
            .Sym => |sym| allocator.free(sym),
            .Sexpr => |cells_opt| if (cells_opt) |cells|
                LValTagged.free_cells(cells, allocator),
        }
        allocator.destroy(self);
    }

    fn free_cells(cells: []*LValTagged, allocator: *Allocator) void {
        for (cells) |child_tagged| {
            child_tagged.free(allocator);
        }
        allocator.free(cells);
    }

    fn print(self: *Self) void {
        switch (self.*) {
            .Num => |i| std.debug.warn("{}", .{i}),
            .Sym => |s| std.debug.warn("{}", .{s}),
            .Sexpr => |sexpr_opt| if (sexpr_opt) |sexpr| {
                std.debug.warn("(", .{});
                for (sexpr) |child_tagged, i| {
                    child_tagged.print();
                    if (i != sexpr.len - 1) std.debug.warn(" ", .{});
                }
                std.debug.warn(")", .{});
            },
        }
    }

    fn destructive_eval(self: *Self, allocator: *Allocator) EvalError!*LValTagged {
        switch (self.*) {
            .Num => return self,
            .Sym => return self,
            .Sexpr => |cells_opt| {
                if (cells_opt) |cells| {
                    if (cells.len == 0) return self;

                    defer self.free(allocator);
                    for (cells) |*cell| {
                        cell.* = try cell.*.destructive_eval(allocator);
                    }

                    if (cells.len == 1) return LValTagged.pop(allocator, cells, 0);

                    var f = LValTagged.pop(allocator, cells, 0);
                    defer f.free(allocator);

                    return undefined;
                } else {
                    return self;
                }
            },
        }
    }

    fn pop(allocator: *Allocator, slice: []*LValTagged, index: usize) *LValTagged {
        const value = slice[index];
        std.mem.copy(*LValTagged, slice[index..], slice[index + 1 ..]);
        slice = allocator.shrink(slice, slice.len - 1);
        return value;
    }

    //fn take(allocator: *Allocator, slice: []*LValTagged, index: usize) *LValTagged {
    //    var x = LValTagged.pop(allocator, slice, index);
    //    defer LValTagged.free_cells(slice, allocator);
    //    return x;
    //}
};

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

    fn deinit(self: Self) void {
        self.tagged.free(self.allocator);
    }

    fn print(self: Self) void {
        self.tagged.print();
        std.debug.warn("\n", .{});
    }

    fn eval(self: *Self) EvalError!void {
        self.tagged = try self.tagged.destructive_eval(self.allocator);
    }
};

const ReadError = Allocator.Error || error{InvalidNum};

fn lval_read_num(allocator: *Allocator, node: *c.mpc_ast_t) ReadError!LVal {
    var x: c_long = undefined;
    if (c.parseLong(node.*.contents, &x) == 0) {
        return ReadError.InvalidNum;
    }
    return LVal.num_init(allocator, x);
}

fn lval_read(allocator: *Allocator, node: *c.mpc_ast_t) ReadError!LVal {
    if (c.strstr(node.*.tag, "number") != 0)
        return lval_read_num(allocator, node);
    if (c.strstr(node.*.tag, "symbol") != 0)
        return LVal.sym_init(allocator, node.*.contents);

    var x: ?LVal = null;
    errdefer if (x) |true_x| true_x.deinit();

    if (c.strcmp(node.*.tag, ">") == 0) x = try LVal.sexpr_init(allocator);
    if (c.strstr(node.*.tag, "sexpr") != 0) x = try LVal.sexpr_init(allocator);

    var i: usize = 0;
    while (i < node.*.children_num) : (i += 1) {
        if (c.strcmp(node.*.children[i].*.contents, "(") == 0) continue;
        if (c.strcmp(node.*.children[i].*.contents, ")") == 0) continue;
        if (c.strcmp(node.*.children[i].*.tag, "regex") == 0) continue;
        x = try lval_add(allocator, x.?, try lval_read(allocator, node.*.children[i]));
    }
    return x.?;
}

fn lval_add(allocator: *Allocator, v: LVal, x: LVal) ReadError!LVal {
    switch (v.tagged.*) {
        .Sexpr => |*sexpr_opt| {
            if (sexpr_opt.*) |*sexpr| {
                sexpr.* = try allocator.realloc(sexpr.*, sexpr.*.len + 1);
                sexpr.*[sexpr.*.len - 1] = x.tagged;
            } else {
                sexpr_opt.* = try allocator.alloc(*LValTagged, 1);
                sexpr_opt.*.?[0] = x.tagged;
            }
        },
        else => {},
    }
    return v;
}

pub fn eval(node: *c.mpc_ast_t) EvalError!i64 {
    if (c.strstr(node.*.tag, "number") != 0) {
        return c.atoi(node.*.contents);
    }

    const op = node.*.children[1].*.contents;
    var result = try eval(node.*.children[2]);
    var i: usize = 3;
    while (c.strstr(node.*.children[i].*.tag, "expr") != 0) : (i += 1) {
        result = try eval_op(result, op, try eval(node.*.children[i]));
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
    const Expr = c.mpc_new("expr");
    const Lispy = c.mpc_new("lispy");

    _ = c.mpca_lang(c.MPCA_LANG_DEFAULT,
        \\ number   : /-?[0-9]+/ ;
        \\ symbol   : '+' | '-' | '*' | '/' ;
        \\ sexpr    : '(' <expr>* ')' ;
        \\ expr     : <number> | <symbol> | <sexpr> ;
        \\ lispy    : /^/ <expr>* /$/ ;
    , Number, Symbol, Sexpr, Expr, Lispy);
    defer c.mpc_cleanup(4, Number, Symbol, Sexpr, Expr, Lispy);

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
            lval.print();

            try lval.eval();

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
