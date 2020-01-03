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

    const test_step = b.step("test", "Test the app");
    const main_test = b.addTest("./src/main.zig");
    test_step.dependOn(&main_test.step);

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
