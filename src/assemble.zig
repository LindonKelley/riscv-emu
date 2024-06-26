//! Assembling assembly into more executable forms

const std = @import("std");

const TEMP_FILE_TEMPLATE = "riscv-asm-XXXXXXXXXXXX";

/// Assembles the provided assembly code into an ELF file, and returns an absolute path to that file.
/// `arch` is used to specify the ISA naming string, and must be all lowercase, the base values are "rv32i" or "rv64i".
///
/// This function allocates internally, in order to specify the allocator used, use `elf_unmanaged`.
/// This function currently requires a linux system with `riscv64-elf-binutils` installed.
pub fn elf(arch: []const u8, assembly: []const u8) ![absoluteFilePathLength("")] u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    return elf_unmanaged(arch, assembly, allocator);
}

/// Assembles the provided assembly code into an ELF file, and returns an absolute path to that file.
/// `arch` is used to specify the ISA naming string, and must be all lowercase, the base values are "rv32i" or "rv64i".
///
/// If you do not care about which allocator is used, use `elf` instead.
/// This function currently requires a linux system with `riscv64-elf-binutils` (or coreboot-toolchain.riscv on Nix) installed.
pub fn elf_unmanaged(arch: []const u8, assembly: []const u8, allocator: std.mem.Allocator) ![absoluteFilePathLength("")] u8 {
    const assembler_output_file_path = try mktemp(allocator, TEMP_FILE_TEMPLATE ++ ".o");
    defer std.fs.deleteFileAbsolute(&assembler_output_file_path) catch |err| {
        std.log.warn("Unable to delete assembled file '{s}': {s}", .{ assembler_output_file_path, @errorName(err) });
    };
    var assembler_process = std.process.Child.init(&.{"riscv64-elf-as", "-march", arch, "-o", &assembler_output_file_path}, allocator);

    assembler_process.stdin_behavior = .Pipe;
    try assembler_process.spawn();
    try assembler_process.stdin.?.writeAll(assembly);
    assembler_process.stdin.?.close();
    assembler_process.stdin = null;
    const assembler_term = try assembler_process.wait();
    switch (assembler_term) {
        .Exited => |code| if (code != 0) return error.AssemblerFailure,
        else => return error.AssemblerUnknownFailure
    }

    var linker_output_file_path = try mktemp(allocator, TEMP_FILE_TEMPLATE);
    const emulate_machine = if (std.mem.startsWith(u8, arch, "rv32")) "elf32lriscv" else "elf64lriscv";
    var linker_process = std.process.Child.init(&.{"riscv64-elf-ld", &assembler_output_file_path, "-m", emulate_machine, "-o", &linker_output_file_path}, allocator);

    const linker_term = try linker_process.spawnAndWait();
    switch (linker_term) {
        .Exited => |code| if (code != 0) return error.LinkerFailure,
        else => return error.LinkerUnknownFailure
    }

    return linker_output_file_path;
}

/// Assembles the provided assembly code into a file containing it's respective raw machine code, and returns an
/// absolute path to that file.
/// `arch` is used to specify the ISA naming string, and must be all lowercase, the base values are "rv32i" or "rv64i".
///
/// This function allocates internally, in order to specify the allocator used, use `raw_unmanaged`.
/// This function currently requires a linux system with `riscv64-elf-binutils` installed.
pub fn raw(arch: []const u8, assembly: []const u8) ![absoluteFilePathLength(".bin")] u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    return raw_unmanaged(arch, assembly, allocator);
}

/// Assembles the provided assembly code into a file containing it's respective raw machine code, and returns an
/// absolute path to that file.
/// `arch` is used to specify the ISA naming string, and must be all lowercase, the base values are "rv32i" or "rv64i".
///
/// If you do not care about which allocator is used, use `raw` instead.
/// This function currently requires a linux system with `riscv64-elf-binutils` installed.
pub fn raw_unmanaged(arch: []const u8, assembly: []const u8, allocator: std.mem.Allocator) ![absoluteFilePathLength(".bin")] u8 {
    const linker_output_file_path = try elf_unmanaged(arch, assembly, allocator);
    defer std.fs.deleteFileAbsolute(&linker_output_file_path) catch |err| {
        std.log.warn("Unable to delete linked file '{s}': {s}", .{ linker_output_file_path, @errorName(err) });
    };
    const objcopy_output_file_path = try mktemp(allocator, TEMP_FILE_TEMPLATE ++ ".bin");
    var objcopy_process = std.process.Child.init(&.{"riscv64-elf-objcopy", "-O", "binary", &linker_output_file_path, &objcopy_output_file_path}, allocator);

    const objcopy_term = try objcopy_process.spawnAndWait();
    switch (objcopy_term) {
        .Exited => |code| if (code != 0) return error.LinkerFailure,
        else => return error.LinkerUnknownFailure
    }
    return objcopy_output_file_path;
}

fn absoluteFilePathLength(comptime suffix: []const u8) comptime_int {
    return "/tmp/".len + TEMP_FILE_TEMPLATE.len + suffix.len;
}

fn mktemp(allocator: std.mem.Allocator, comptime template: []const u8) !["/tmp/".len + template.len]u8 {
    var mktemp_process = std.process.Child.init(&.{"mktemp", "-p", "/tmp", template}, allocator);
    mktemp_process.stdout_behavior = .Pipe;
    try mktemp_process.spawn();
    const process_out_reader = mktemp_process.stdout.?.reader();
    var temp_file: ["/tmp/".len + template.len]u8 = undefined;
    const read = try process_out_reader.readAll(&temp_file);
    std.debug.assert(read == temp_file.len);
    return temp_file;
}