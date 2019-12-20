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

    fn free(tagged: *LValTagged, allocator: *Allocator) void {
        switch (tagged.*) {
            .Num => {},
            .Sym => |sym| allocator.free(sym),
            .Sexpr => |cells_opt| {
                if (cells_opt) |cells| {
                    for (cells) |child_tagged| {
                        LValTagged.free(child_tagged, allocator);
                    }
                    allocator.free(cells);
                }
            },
        }
        allocator.destroy(tagged);
    }
};

const LVal = struct {
    tagged: *LValTagged,
    allocator: *Allocator,

    const Self = @This();

    fn num_init(allocator: *Allocator, m: i64) !Self {
        var lval = try allocator.create(LValTagged);
        lval.* = LValTagged{ .Num = m };
        return LVal{ .tagged = lval, .allocator = allocator };
    }

    fn sym_init(allocator: *Allocator, m: [*c]const u8) !Self {
        var lval = try allocator.create(LValTagged);
        var m_slice = m[0..c.strlen(m)];
        var sym = try allocator.alloc(u8, m_slice.len);
        std.mem.copy(u8, sym, m_slice);
        lval.* = LValTagged{ .Sym = sym };
        return LVal{ .tagged = lval, .allocator = allocator };
    }

    fn sexpr_init(allocator: *Allocator) !Self {
        var lval = try allocator.create(LValTagged);
        lval.* = LValTagged{ .Sexpr = null };
        return LVal{ .tagged = lval, .allocator = allocator };
    }

    fn deinit(self: Self) void {
        LValTagged.free(self.tagged, self.allocator);
    }
};

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
const ReplError = EvalError || std.os.ReadError;

pub fn repl() ReplError!void {
    const Number = c.mpc_new("number");
    const Symbol = c.mpc_new("operator");
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
        if (bytes_read > 0) {
            input[bytes_read - 1] = 0;
        }
        var r: c.mpc_result_t = undefined;
        if (c.mpc_parse("<stdin>", input[0..bytes_read].ptr, Lispy, &r) != 0) {
            var ptr = @ptrCast(*c.mpc_ast_t, @alignCast(@alignOf(c.mpc_ast_t), r.output.?));
            defer c.mpc_ast_delete(ptr);
            var result = eval(ptr) catch |e| switch (e) {
                EvalError.DivideByZero => {
                    std.debug.warn("Divide by zero\n", .{});
                    continue;
                },
                else => |err| return err,
            };
            std.debug.warn("{}\n", .{result});
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
    var lval = try LVal.num_init(std.heap.direct_allocator, 2);
    defer lval.deinit();
    var sym = try LVal.sym_init(std.heap.direct_allocator, "abc");
    defer sym.deinit();
    var sexpr = try LVal.sexpr_init(std.heap.direct_allocator);
    defer sexpr.deinit();
    // try repl();
}
