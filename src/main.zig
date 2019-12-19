const std = @import("std");
const c = @import("c.zig");

pub fn repl() std.os.ReadError!void {
    var input: [2048]u8 = undefined;
    const stdin = std.io.getStdIn();
    while (true) {
        std.debug.warn("lispy> ", .{});
        const bytes_read = try stdin.read(&input);
        std.debug.warn("No you're a {}", .{input[0..bytes_read]});
    }
}

pub fn main() anyerror!void {
    std.debug.warn("Lispy version 0.0.0.0.1\n", .{});
    std.debug.warn("Press Ctrl+c to Exit\n", .{});
    try repl();
}
