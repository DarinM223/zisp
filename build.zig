const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();
    const exe = b.addExecutable("build_your_own_lisp", "src/main.zig");
    exe.addCSourceFile("mpc/mpc.c", &[_][]const u8{"-std=c99"});
    exe.setBuildMode(mode);
    exe.addIncludeDir("mpc");
    exe.linkSystemLibrary("c");
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
