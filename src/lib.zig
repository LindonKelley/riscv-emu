pub const hart_impls = @import("hart_impls.zig");
pub const instruction = @import("instruction.zig");
pub const inst_format = @import("inst_format.zig");
pub const load = @import("load.zig");
pub const assemble = @import("assemble.zig");
pub const Data = @import("data.zig").Data;

comptime {
    for (@typeInfo(@This()).Struct.decls) |decl| {
        _ = @field(@This(), decl.name);
    }
    const extension = @import("extension.zig");
    std.testing.refAllDecls(extension);
}

const std = @import("std");
const math = std.math;
const Signedness = std.builtin.Signedness;
const Timer = std.time.Timer;

pub fn main() !void {
    try real_test();
}

inline fn speed_test() !void {
    const mmu = @import("mmu.zig");
    var hart = hart_impls.Simple(32, mmu.BasicMmu(32)){ .mmu = .{} };
    const inst = inst_format.I {
        .opcode = instruction.ADDI.ID[0],
        .rd = 3,
        .funct3 = instruction.ADDI.ID[1],
        .rs1 = 3,
        .imm0 = 1
    };
    for (0..100) |inst_addr| {
        try hart.store(.word, @as(u32, @intCast(inst_addr)) * 4, @bitCast(inst));
    }
    const expectEqual = std.testing.expectEqual;
    var times = [_]u64{0} ** 12;
    for (0..12) |lap| {
        var timer = try Timer.start();
        for (0..1000) |_| {
            hart.setPC(0);
            hart.setXRegister(3, @as(u32, 0));
            for (0..100) |i| {
                try expectEqual(i, hart.getXRegister(.unsigned, 3));
                try hart.tick();
            }
        }
        times[lap] = timer.lap();
    }

    var sum: u64 = 0;
    for (times) |time| {
        sum += time;
    }
    const avg = sum / times.len;
    std.debug.print("{}\n", .{ avg });
}

inline fn real_test() !void {
    const mmu = @import("mmu.zig");
    const print = std.debug.print;
    var hart = hart_impls.Simple(32, mmu.BasicMmu(32)){ .mmu = .{} };
    const file_path = try assemble.elf("rv32i",
        \\ .global _start
        \\
        \\ _start:
        \\     li a0, 1
        \\     li a1, 2
        \\     add a0, a0, a1
        \\ loop:
        \\     j loop
        \\
    );
    const file = try std.fs.openFileAbsolute(&file_path, .{});
    try load.elf(&hart, file);
    file.close();
    try hart.store(.halfword, 512, .{ .unsigned = 1 });
    try hart.store(.halfword, 516, .{ .unsigned = 2 });
    for (0..10) |_| {
        print("{X:0>8}\n", .{ hart.getPc() });
        for (0..31) |reg| {
            print("{X:0>8} ", .{ hart.getXRegister(@intCast(reg)).unsigned });
        } else {
            print("{X:0>8}\n\n", .{ hart.getXRegister(31).unsigned });
        }
        if (try hart.tick()) |trap| {
            switch (trap) {
                .ecall => return error.EcallUnsupported,
                .ebreak => {}
            }
        }
    }
}

comptime {
    const builtin = @import("builtin");
    const native_endian = builtin.cpu.arch.endian();
    if (native_endian == .big) {
        // todo big endian host systems are not properly handled
        @panic("this library does not currently support big endian host systems");
        // todo big endian Harts/Mmus are not properly supported
    }
}
