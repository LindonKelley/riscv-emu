//! Integer multiplication and division

const instruction = @import("../instruction.zig");
const opcode = instruction.opcode;
const funct3 = instruction.funct3;
const funct12 = instruction.funct12;
const hart_arch = instruction.hart_arch;
const inst_format = @import("../inst_format.zig");
const Data = @import("../data.zig").Data;
const std = @import("std");
const math = std.math;
const expectEqual = std.testing.expectEqual;

comptime {
    const extension = @import("../extension.zig");
    extension.verifyExtension(@This());
    std.testing.refAllDeclsRecursive(@This());
}

pub const NAME = "M";
pub const VERSION = "2.0";
pub const STATUS = @import("../extension.zig").SpecificationStatus.ratified;
pub const DEPENDENCIES = .{"I"};

pub const INSTRUCTIONS = struct {
    /// MULtiply
    ///
    /// x[`rd`] = x[`rs1`] *% x[`rs2`]
    pub const MUL = struct {
        pub const EXT = .{32};

        pub const ID = .{ opcode.OP, funct3.MUL, 0b000_0001 };

        pub fn execute(context: anytype, inst: inst_format.R) void {
            const a = context.getXRegister(inst.rs1).signed;
            const b = context.getXRegister(inst.rs2).signed;
            const res = .{ .signed = a *% b };
            context.setXRegister(inst.rd, res);
        }
    };

    /// MULtiply High
    ///
    /// x[`rd`] = ((signed)x[`rs1`] * (signed)x[`rs2`]) (signed)>> XLEN
    pub const MULH = struct {
        pub const EXT = .{32};

        pub const ID = .{ opcode.OP, funct3.MUL, 0b000_0001 };

        pub fn execute(context: anytype, inst: inst_format.R) void {
            const arch = hart_arch(@TypeOf(context));
            const a = context.getXRegister(inst.rs1).signExtended(arch.XLEN * 2).signed;
            const b = context.getXRegister(inst.rs2).signExtended(arch.XLEN * 2).signed;
            const res = (Data(arch.XLEN * 2){ .signed = (a * b) >> arch.XLEN }).truncated(arch.XLEN);
            context.setXRegister(inst.rd, res);
        }
    };

    /// MULtiply High Unsigned
    ///
    /// x[`rd`] = ((unsigned)x[`rs1`] * (unsigned)x[`rs2`]) (unsigned)>> XLEN
    pub const MULHU = struct {
        pub const EXT = .{32};

        pub const ID = .{ opcode.OP, funct3.MUL, 0b000_0001 };

        pub fn execute(context: anytype, inst: inst_format.R) void {
            const arch = hart_arch(@TypeOf(context));
            const a = context.getXRegister(inst.rs1).signExtended(arch.XLEN * 2).unsigned;
            const b = context.getXRegister(inst.rs2).signExtended(arch.XLEN * 2).unsigned;
            const res = (Data(arch.XLEN * 2){ .unsigned = (a * b) >> arch.XLEN }).truncated(arch.XLEN);
            context.setXRegister(inst.rd, res);
        }
    };

    /// MULtiply High Signed Unsigned
    ///
    /// x[`rd`] = ((signed)x[`rs1`] * (unsigned)x[`rs2`]) (signed)>> XLEN
    pub const MULHSU = struct {
        pub const EXT = .{32};

        pub const ID = .{ opcode.OP, funct3.MUL, 0b000_0001 };

        pub fn execute(context: anytype, inst: inst_format.R) void {
            const arch = hart_arch(@TypeOf(context));
            const a = context.getXRegister(inst.rs1).signExtended(arch.XLEN * 2).signed;
            const b = context.getXRegister(inst.rs2).signExtended(arch.XLEN * 2).unsigned;
            const res = (Data(arch.XLEN * 2){ .signed = (a * b) >> arch.XLEN }).truncated(arch.XLEN);
            context.setXRegister(inst.rd, res);
        }
    };

    /// DIVide
    ///
    /// if (x[`rs2`] == 0)
    ///     x[`rd`] = -1
    /// else if (x[`rs2`] == -1 and x[`rs1`] == minInt(XLEN))
    ///     x[`rd`] = minInt(XLEN)
    /// else
    ///     x[`rd`] = roundTowardsZero((signed)x[`rs1`] / (signed)x[`rs2`])
    pub const DIV = struct {
        pub const EXT = .{32};

        pub const ID = .{ opcode.OP, funct3.DIV, 0b000_0001 };

        pub fn execute(context: anytype, inst: inst_format.R) void {
            const arch = hart_arch(@TypeOf(context));
            const b = context.getXRegister(inst.rs2).signed;
            const res = blk: {
                if (b == 0) break :blk .{ .signed = -1 };
                const a = context.getXRegister(inst.rs1).signed;
                if (b == -1 and a == math.minInt(arch.XLEN)) {
                    break :blk math.minInt(arch.XLEN);
                } else {
                    break :blk .{ .signed = @divTrunc(a, b) };
                }
            };
            context.setXRegister(inst.rd, res);
        }
    };

    /// DIVide Unsigned
    ///
    /// if (x[`rs2`] == 0)
    ///     x[`rd`] = @bitcast(-1)
    /// else
    ///     x[`rd`] = floor((unsigned)x[`rs1`] / (unsigned)x[`rs2`])
    pub const DIVU = struct {
        pub const EXT = .{32};

        pub const ID = .{ opcode.OP, funct3.DIVU, 0b000_0001 };

        pub fn execute(context: anytype, inst: inst_format.R) void {
            const b = context.getXRegister(inst.rs2).unsigned;
            const res = blk: {
                if (b != 0) {
                    const a = context.getXRegister(inst.rs1).unsigned;
                    break :blk .{ .unsigned = a / b };
                } else {
                    break :blk .{ .signed = -1 };
                }
            };
            context.setXRegister(inst.rd, res);
        }
    };

    /// REMainder
    ///
    /// if (x[`rs2`] == 0)
    ///     x[`rd`] = x[`rs1`]
    /// else if (x[`rs2`] == -1 and x[`rs1`] == minInt(XLEN))
    ///     x[`rd`] = 0
    /// else
    ///     x[`rd`] = (signed)x[`rs1`] % (signed)x[`rs2`]
    pub const REM = struct {
        pub const EXT = .{32};

        pub const ID = .{ opcode.OP, funct3.REM, 0b000_0001 };

        pub fn execute(context: anytype, inst: inst_format.R) void {
            const arch = hart_arch(@TypeOf(context));
            const b = context.getXRegister(inst.rs2).signed;
            const res = blk: {
                const a = context.getXRegister(inst.rs1).signed;
                if (b == 0) {
                    break :blk a;
                } else if (b == -1 and a == math.minInt(arch.XLEN)) {
                    break :blk .{ .unsigned = 0 };
                } else {
                    break :blk .{ .unsigned = @rem(a, b) };
                }
            };
            context.setXRegister(inst.rd, res);
        }

        comptime {
            // Zig spec for @rem: "Caller guarantees denominator > 0"
            // however, I'm unsure if that comment was only for unsigned integers, as for signed integers, it's observed
            // behavior exactly matches the RISC-V test for the REM instruction:
            // https://github.com/riscv-software-src/riscv-tests/blob/master/isa/rv32um/rem.S

            // just in case this behavior is actually unspecified by the Zig spec, this test will ensure that misbehavior
            // at least stops the compilation of this extension

            if (@rem(20, -6) != 2 or @rem(-20, -6) != -2) {
                @compileError("@rem exhibited unexpected behavior, please submit an issue to the GitHub repository with " ++
                    "your Zig version and target triple");
            }
        }
    };

    /// REMainder Unsigned
    ///
    /// if (x[`rs2`] == 0)
    ///     x[`rd`] = x[`rs1`]
    /// else
    ///     x[`rd`] = (unsigned)x[`rs1`] % (unsigned)x[`rs2`]
    pub const REMU = struct {
        pub const EXT = .{32};

        pub const ID = .{ opcode.OP, funct3.REMU, 0b000_0001 };

        pub fn execute(context: anytype, inst: inst_format.R) void {
            const b = context.getXRegister(inst.rs2).unsigned;
            const res = blk: {
                const a = context.getXRegister(inst.rs1).unsigned;
                if (b == 0) {
                    break :blk a;
                } else {
                    break :blk .{ .unsigned = a % b };
                }
            };
            context.setXRegister(inst.rd, res);
        }
    };
};