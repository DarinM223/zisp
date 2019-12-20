const std = @import("std");
const c = @import("c.zig");

const EvalError = error{InvalidOp};

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
    if (c.strcmp(op, "/") == 0) return @divTrunc(a, b);
    return EvalError.InvalidOp;
}

// Combine errors with ||
const ReplError = EvalError || std.os.ReadError;

pub fn repl() ReplError!void {
    const Number = c.mpc_new("number");
    const Operator = c.mpc_new("operator");
    const Expr = c.mpc_new("expr");
    const Lispy = c.mpc_new("lispy");

    _ = c.mpca_lang(c.MPCA_LANG_DEFAULT,
        \\ number   : /-?[0-9]+/ ;
        \\ operator : '+' | '-' | '*' | '/' ;
        \\ expr     : <number> | '(' <operator> <expr>+ ')' ;
        \\ lispy    : /^/ <operator> <expr>+ /$/ ;
    , Number, Operator, Expr, Lispy);
    defer c.mpc_cleanup(4, Number, Operator, Expr, Lispy);

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
            var result = try eval(ptr);
            std.debug.warn("{}\n", .{result});
            c.mpc_ast_delete(ptr);
        } else {
            var ptr = @ptrCast([*c]c.mpc_err_t, @field(r, "error").?);
            c.mpc_err_print(ptr);
            c.mpc_err_delete(ptr);
        }
    }
}

pub fn main() anyerror!void {
    std.debug.warn("Lispy version 0.0.0.0.1\n", .{});
    std.debug.warn("Press Ctrl+c to Exit\n", .{});
    try repl();
}
