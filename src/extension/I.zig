//! Base integer ISA, proceeded by RV32 or RV64 depending on register width

const instruction = @import("../instruction.zig");
const opcode = instruction.opcode;
const funct3 = instruction.funct3;
const funct12 = instruction.funct12;
const FenceOperands = instruction.FenceOperands;
const mmu = @import("../mmu.zig");
const inst_format = @import("../inst_format.zig");
const std = @import("std");
const Signedness = std.builtin.Signedness;
const JumpError = instruction.JumpError;
const hart_arch = instruction.hart_arch;
const SimpleHart = @import("../hart_impls.zig").Simple;
const expect = std.testing.expect;
const expectError = std.testing.expectError;
const expectEqual = std.testing.expectEqual;
const Int = std.meta.Int;
const load = @import("../load.zig");
const assemble = @import("../assemble.zig");
const register = @import("../register.zig");
const Data = @import("../data.zig").Data;

comptime {
    const extension = @import("../extension.zig");
    extension.verifyExtensionInstructions(@This());
    std.testing.refAllDeclsRecursive(@This());
}

// todo section all of the instructions off into an internal Instructions struct

/// ADD Immediate
///
/// x[`rd`] = x[`rs1`] +% signExtend(`imm`)
/// `ADDI rd, rs1, 0` is used to implement the `MV rd, rs1` psuedoinstruction
/// `ADDI 0, 0, 0` is used to encode `NOP`
pub const ADDI = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.OP_IMM, funct3.ADDI };

    pub fn execute(context: anytype, inst: inst_format.I) void {
        const arch = hart_arch(@TypeOf(context));
        const src = context.getXRegister(inst.rs1).signed;
        const imm = inst.getImmediate().signExtended(arch.XLEN).signed;
        const res = .{ .signed = src +% imm };
        context.setXRegister(inst.rd, res);
    }

    test "ADDI" {
        inline for (.{ 32, 64 }, .{ "rv32i", "rv64i" }) |XLEN, arch| {
            var hart = SimpleHart(XLEN, mmu.BasicMmu(XLEN)){ .mmu = .{} };
            const file_path = try assemble.raw(arch,
                \\ .global _start
                \\
                \\ _start:
                \\     li a0, 0
                \\     li a1, 0
                \\ loop:
                \\     addi a0, a0, 1
                \\     addi a1, a1, -1
                \\     ebreak
                \\     j loop
                \\
            );
            const file = try std.fs.openFileAbsolute(&file_path, .{});
            try load.raw(&hart, file.reader());
            file.close();
            try std.fs.deleteFileAbsolute(&file_path);

            for (1..100) |i| {
                while (true) {
                    if (try hart.tick()) |trap| {
                        switch (trap) {
                            .ebreak => break,
                            else => unreachable
                        }
                    }
                }
                try expect(i == hart.getXRegister(register.a0).signed);
                try expect(-@as(isize, @intCast(i)) == hart.getXRegister(register.a1).signed);
            }
            // todo overflow testing
        }
    }
};

/// Set Less Than Immediate
///
/// x[`rd`] = if (x[`rs1`] < signExtend(`imm`)) 1 else 0
pub const SLTI = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.OP_IMM, funct3.SLTI };

    pub fn execute(context: anytype, inst: inst_format.I) void {
        const arch = hart_arch(@TypeOf(context));
        const src = context.getXRegister(inst.rs1).signed;
        const imm = inst.getImmediate().signExtended(arch.XLEN).signed;
        const res = .{ .unsigned =  if (src < imm) @as(arch.usize, 1) else @as(arch.usize, 0) };
        context.setXRegister(inst.rd, res);
    }
};

/// Set Less Than Immediate Unsigned
///
/// x[`rd`] = if (x[`rs1`] (unsigned)< signExtend(`imm`)) 1 else 0
pub const SLTIU = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.OP_IMM, funct3.SLTIU };

    pub fn execute(context: anytype, inst: inst_format.I) void {
        const arch = hart_arch(@TypeOf(context));
        const src = context.getXRegister(inst.rs1).unsigned;
        const imm = inst.getImmediate().signExtended(arch.XLEN).unsigned;
        const res = .{ .unsigned =  if (src < imm) @as(arch.usize, 1) else @as(arch.usize, 0) };
        context.setXRegister(inst.rd, res);
    }
};

/// AND Immediate
///
/// x[`rd`] = x[`rs1`] & signExtend(`imm`)
pub const ANDI = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.OP_IMM, funct3.ANDI };

    pub fn execute(context: anytype, inst: inst_format.I) void {
        const arch = hart_arch(@TypeOf(context));
        const src = context.getXRegister(inst.rs1).signed;
        const imm = inst.getImmediate().signExtended(arch.XLEN).signed;
        const res = .{ .signed = src & imm };
        context.setXRegister(inst.rd, res);
    }
};

/// OR Immediate
///
/// x[`rd`] = x[`rs1`] | signExtend(`imm`)
pub const ORI = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.OP_IMM, funct3.ORI };

    pub fn execute(context: anytype, inst: inst_format.I) void {
        const arch = hart_arch(@TypeOf(context));
        const src = context.getXRegister(inst.rs1).signed;
        const imm = inst.getImmediate().signExtended(arch.XLEN).signed;
        const res = .{ .signed = src | imm };
        context.setXRegister(inst.rd, res);
    }
};

/// eXclusive OR Immediate
///
/// x[`rd`] = x[`rs1`] ^ signExtend(`imm`)
/// `XORI rd, rs1, -1` is equivelent to psuedoinstruction `NOT rd, rs1`
pub const XORI = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.OP_IMM, funct3.XORI };

    pub fn execute(context: anytype, inst: inst_format.I) void {
        const arch = hart_arch(@TypeOf(context));
        const src = context.getXRegister(inst.rs1).signed;
        const imm = inst.getImmediate().signExtended(arch.XLEN).signed;
        const res = .{ .signed = src ^ imm };
        context.setXRegister(inst.rd, res);
    }
};

/// Shift Left Logical Immediate
///
/// x[`rd`] = x[`rs1`] << `shamt`
pub const SLLI = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.OP_IMM, funct3.SLLI, 0b000_0000 };

    pub fn execute(context: anytype, inst: inst_format.IS) void {
        const src = context.getXRegister(inst.rs1).unsigned;
        const res = .{ .unsigned = src << inst.shamt };
        context.setXRegister(inst.rd, res);
    }
};

/// Shift Right Logical Immediate
///
/// x[`rd`] = x[`rs1`] (unsigned)>> `shamt`
pub const SRLI = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.OP_IMM, funct3.SRLI, 0b000_0000 };

    pub fn execute(context: anytype, inst: inst_format.IS) void {
        const src = context.getXRegister(inst.rs1).unsigned;
        const res = .{ .unsigned = src >> inst.shamt };
        context.setXRegister(inst.rd, res);
    }
};

/// Shift Right Arithmetic Immediate
///
/// x[`rd`] = x[`rs1`] (signed)>> `shamt`
pub const SRAI = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.OP_IMM, funct3.SRAI, 0b000_0010 };

    pub fn execute(context: anytype, inst: inst_format.IS) void {
        const src = context.getXRegister(inst.rs1).signed;
        const res = .{ .signed = src >> inst.shamt };
        context.setXRegister(inst.rd, res);
    }
};

/// Load Upper Immediate
///
/// x[`rd`] = signExtend(`imm` << 12)
pub const LUI = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{opcode.LUI};

    pub fn execute(context: anytype, inst: inst_format.U) void {
        const imm = inst.getImmediate().unsigned;
        const res = .{ .unsigned = imm };
        context.setXRegister(inst.rd, res);
    }
};

/// Add Upper Immediate to `PC`
///
/// x[`rd`] = `pc` + signExtend(`imm` << 12)
pub const AUIPC = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{opcode.AUIPC};

    pub fn execute(context: anytype, inst: inst_format.U) void {
        const imm = inst.getImmediate().unsigned;
        const pc = context.getPc();
        const res = .{ .unsigned = pc +% imm };
        context.setXRegister(inst.rd, res);
    }
};

/// ADD
///
/// x[`rd`] = x[`rs1`] +% x[`rs2`]
pub const ADD = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.OP, funct3.ADD, 0b000_0000 };

    pub fn execute(context: anytype, inst: inst_format.R) void {
        const arch = hart_arch(@TypeOf(context));
        const src1 = context.getXRegister(inst.rs1).signExtended(arch.XLEN).signed;
        const src2 = context.getXRegister(inst.rs2).signExtended(arch.XLEN).signed;
        const res = .{ .signed = src1 +% src2 };
        context.setXRegister(inst.rd, res);
    }
};

/// Set Less Than
///
/// x[`rd`] = if (x[`rs1`] (signed)< x[`rs2`]) 1 else 0
pub const SLT = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.OP, funct3.SLT, 0b000_0000 };

    pub fn execute(context: anytype, inst: inst_format.R) void {
        const arch = hart_arch(@TypeOf(context));
        const src1 = context.getXRegister(inst.rs1).signed;
        const src2 = context.getXRegister(inst.rs2).signed;
        const res = .{ .unsigned = if (src1 < src2) @as(arch.usize, 1) else @as(arch.usize, 0) };
        context.setXRegister(inst.rd, res);
    }
};

/// Set Less Than Unsigned
///
/// x[`rd`] = if (x[`rs1`] (unsigned)< x[`rs2`]) 1 else 0
/// `SLTU rd, x0, rs1` is equivalent to psuedoinstruction `SNEZ rd, rs`
pub const SLTU = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.OP, funct3.SLTU, 0b000_0000 };

    pub fn execute(context: anytype, inst: inst_format.R) void {
        const arch = hart_arch(@TypeOf(context));
        const src1 = context.getXRegister(inst.rs1).unsigned;
        const src2 = context.getXRegister(inst.rs2).unsigned;
        const res = .{ .unsigned = if (src1 < src2) @as(arch.usize, 1) else @as(arch.usize, 0) };
        context.setXRegister(inst.rd, res);
    }
};

/// AND
///
/// x[`rd`] = x[`rs1`] & x[`rs2`]
pub const AND = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.OP, funct3.AND, 0b000_0000 };

    pub fn execute(context: anytype, inst: inst_format.R) void {
        const src1 = context.getXRegister(inst.rs1).unsigned;
        const src2 = context.getXRegister(inst.rs2).unsigned;
        const res = .{ .unsigned = src1 & src2 };
        context.setXRegister(inst.rd, res);
    }
};

/// OR
///
/// x[`rd`] = x[`rs1`] | x[`rs2`]
pub const OR = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.OP, funct3.OR, 0b000_0000 };

    pub fn execute(context: anytype, inst: inst_format.R) void {
        const src1 = context.getXRegister(inst.rs1).unsigned;
        const src2 = context.getXRegister(inst.rs2).unsigned;
        const res = .{ .unsigned = src1 | src2 };
        context.setXRegister(inst.rd, res);
    }
};

/// eXclusive OR
///
/// x[`rd`] = x[`rs1`] ^ x[`rs2`]
pub const XOR = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.OP, funct3.XOR, 0b000_0000 };

    pub fn execute(context: anytype, inst: inst_format.R) void {
        const src1 = context.getXRegister(inst.rs1).unsigned;
        const src2 = context.getXRegister(inst.rs2).unsigned;
        const res = .{ .unsigned = src1 ^ src2 };
        context.setXRegister(inst.rd, res);
    }
};

/// Shift Left Logical
///
/// x[`rd`] = x[`rs1`] << (x[`rs2`] & 0b1_1111)
pub const SLL = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.OP, funct3.SLL, 0b000_0000 };

    pub fn execute(context: anytype, inst: inst_format.R) void {
        const src1 = context.getXRegister(inst.rs1).unsigned;
        const src2 = context.getXRegister(inst.rs2).truncated(5).unsigned;
        const res = .{ .unsigned = src1 << src2 };
        context.setXRegister(inst.rd, res);
    }
};

/// Shift Right Logical
///
/// x[`rd`] = x[`rs1`] (unsigned)>> (x[`rs2`] & 0b1_1111)
pub const SRL = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.OP, funct3.SRL, 0b000_0000 };

    pub fn execute(context: anytype, inst: inst_format.R) void {
        const src1 = context.getXRegister(inst.rs1).unsigned;
        const src2 = context.getXRegister(inst.rs2).truncated(5).unsigned;
        const res = .{ .unsigned = src1 >> src2 };
        context.setXRegister(inst.rd, res);
    }
};

/// SUBtract
///
/// x[`rd`] = x[`rs1`] -% x[`rs2`]
pub const SUB = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.OP, funct3.SUB, 0b000_0010 };

    pub fn execute(context: anytype, inst: inst_format.R) void {
        const src1 = context.getXRegister(inst.rs1).unsigned;
        const src2 = context.getXRegister(inst.rs2).unsigned;
        const res = .{ .unsigned = src1 -% src2 };
        context.setXRegister(inst.rd, res);
    }
};

/// Shift Right Arithmetic
///
/// x[`rd`] = x[`rs1`] (signed)>> (x[`rs2`] & 0b1_1111)
pub const SRA = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.OP, funct3.SRA, 0b000_0010 };

    pub fn execute(context: anytype, inst: inst_format.R) void {
        const src1 = context.getXRegister(inst.rs1).signed;
        const src2 = context.getXRegister(inst.rs2).truncated(5).unsigned;
        const res = .{ .signed = src1 >> src2 };
        context.setXRegister(inst.rd, res);
    }
};

/// Jump And Link
///
/// x[`rd`] = `pc` +% 4; `pc` +%= signExtend(`imm`)
/// `JAL x0, [whatever] is equivalent to psuedoinstruction `J [whatever]` (plain unconditional jump)
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address however, this is not possible when C-extension is enabled.
pub const JAL = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.JAL };

    pub fn execute(context: anytype, inst: inst_format.J) JumpError!void {
        const arch = hart_arch(@TypeOf(context));
        const imm = inst.getImmediate().signExtended(arch.XLEN).unsigned;
        const pc = context.getPc();
        const pc_set = pc +% imm;
        if (arch.IALIGN != 16 and pc_set % 4 != 0) {
            return error.InstructionAddressMisaligned;
        } else {
            context.setXRegister(inst.rd, .{ .unsigned = pc +% 4 });
            context.setPc(pc_set);
        }
    }
};

/// Jump And Link Register
///
/// x[`rd`] = `pc` +% 4; `pc` = (x[`rs1`] +% signExtend(`imm`)) & signExtend(0b10)
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address however, this is not possible when C-extension is enabled.
pub const JALR = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.JALR, 0 };

    pub fn execute(context: anytype, inst: inst_format.I) JumpError!void {
        const arch = hart_arch(@TypeOf(context));
        const src = context.getXRegister(inst.rs1).signed;
        const imm = inst.getImmediate().signExtended(arch.XLEN).signed;
        const pc = context.getPc();
        const pc_set: arch.usize = @bitCast((src +% imm) & 0b10);
        if (@TypeOf(context).IALIGN != 16 and pc_set % 4 != 0) {
            return error.InstructionAddressMisaligned;
        } else {
            context.setXRegister(inst.rd, .{ .unsigned = pc +% 4 });
            context.setPc(pc_set);
        }
    }
};

inline fn branch_execute(context: anytype, inst: inst_format.B, comptime signedness: Signedness, comptime compare: std.math.CompareOperator) JumpError!void {
    const arch = hart_arch(@TypeOf(context));
    const src1 = switch(signedness) {
        .signed => context.getXRegister(inst.rs1).signed,
        .unsigned => context.getXRegister(inst.rs1).unsigned
    };
    const src2 = switch(signedness) {
        .signed => context.getXRegister(inst.rs2).signed,
        .unsigned => context.getXRegister(inst.rs2).unsigned
    };
    const condition = switch (compare) {
        .lt =>  src1 <  src2,
        .lte => src1 <= src2,
        .eq =>  src1 == src2,
        .gte => src1 >= src2,
        .gt =>  src1 >  src2,
        .neq => src1 != src2
    };
    if (condition) {
        const pc = context.getPc();
        const imm = inst.getImmediate().signExtended(arch.XLEN).unsigned;
        const pc_set = pc +% imm;
        if (arch.IALIGN != 16 and pc_set % 4 != 0) {
            return error.InstructionAddressMisaligned;
        } else {
            context.setPc(pc_set);
        }
    }
}

/// Branch if EQual
///
/// if (x[`rs1`] == x[`rs2`]) `pc` +%= signExtend(`imm`)
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address, however, this is not possible when C-extension is enabled.
pub const BEQ = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.BRANCH, funct3.BEQ };

    pub fn execute(context: anytype, inst: inst_format.B) JumpError!void {
        return branch_execute(context, inst, .signed, .eq);
    }
};

/// Branch if Not eQual
///
/// if (x[`rs1`] != x[`rs2`]) `pc` +%= signExtend(`imm`)
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address, however, this is not possible when C-extension is enabled.
pub const BNQ = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.BRANCH, funct3.BNE };

    pub fn execute(context: anytype, inst: inst_format.B) JumpError!void {
        return branch_execute(context, inst, .signed, .neq);
    }
};

/// Branch if Less Than
///
/// if (x[`rs1`] (signed)< x[`rs2`]) `pc` +%= signExtend(`imm`)
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address, however, this is not possible when C-extension is enabled.
/// psuedoinstruction `BGT` can be implemented by flipping `rs1` and `rs2`
pub const BLT = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.BRANCH, funct3.BLT };

    pub fn execute(context: anytype, inst: inst_format.B) JumpError!void {
        return branch_execute(context, inst, .signed, .lt);
    }
};

/// Branch if Less Than Unsigned
///
/// if (x[`rs1`] (unsigned)< x[`rs2`]) `pc` +%= `imm`
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address, however, this is not possible when C-extension is enabled.
/// psuedoinstruction `BGTU` can be implemented by flipping `rs1` and `rs2`
pub const BLTU = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.BRANCH, funct3.BLTU };

    pub fn execute(context: anytype, inst: inst_format.B) JumpError!void {
        return branch_execute(context, inst, .unsigned, .lt);
    }
};

/// Branch if Greater or Equal
///
/// if (x[`rs1`] (signed)>= x[`rs2`]) `pc` +%= `imm`
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address, however, this is not possible when C-extension is enabled.
/// psuedoinstruction `BLE` can be implemented by flipping `rs1` and `rs2`
pub const BGE = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.BRANCH, funct3.BGE };

    pub fn execute(context: anytype, inst: inst_format.B) JumpError!void {
        return branch_execute(context, inst, .signed, .gte);
    }
};

/// Branch if Greater or Equal Unsigned
///
/// if (x[`rs1`] (unsigned)>= x[`rs2`]) `pc` +%= `imm`
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address, however, this is not possible when C-extension is enabled.
/// psuedoinstruction `BLEU` can be implemented by flipping `rs1` and `rs2`
pub const BGEU = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.BRANCH, funct3.BGEU };

    pub fn execute(context: anytype, inst: inst_format.B) JumpError!void {
        return branch_execute(context, inst, .unsigned, .gte);
    }
};

inline fn load_execute(context: anytype, inst: inst_format.I, comptime width: mmu.MemoryValueWidth, comptime signedness: Signedness) mmu.LoadError!void {
    const arch = hart_arch(@TypeOf(context));
    const base = context.getXRegister(inst.rs1).unsigned;
    const imm = inst.getImmediate().signExtended(arch.XLEN).unsigned;
    const addr = base +% imm;
    const data = try context.load(width, addr);
    const res = switch (signedness) {
        .signed => data.signExtended(arch.XLEN),
        .unsigned => data.zeroExtended(arch.XLEN)
    };
    context.setXRegister(inst.rd, res);
}

/// Load Byte (8 bits)
///
/// x[`rd`] = signExtend(mem.load(8, x[`rs1`] +% signExtend(`imm`)))
pub const LB = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.LOAD, funct3.LB };

    pub fn execute(context: anytype, inst: inst_format.I) mmu.LoadError!void {
        return load_execute(context, inst, .byte, .signed);
    }
};

/// Load Halfword (16 bits)
///
/// x[`rd`] = signExtend(mem.load(16, x[`rs1`] +% signExtend(`imm`)))
pub const LH = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.LOAD, funct3.LH };

    pub fn execute(context: anytype, inst: inst_format.I) mmu.LoadError!void {
        return load_execute(context, inst, .halfword, .signed);
    }
};

/// Load Word (32 bits)
///
/// x[`rd`] = signExtend(mem.load(32, x[`rs1`] +% signExtend(`imm`)))
pub const LW = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.LOAD, funct3.LW };

    pub fn execute(context: anytype, inst: inst_format.I) mmu.LoadError!void {
        return load_execute(context, inst, .word, .signed);
    }
};

/// Load Byte Unsigned (8 bits)
///
/// x[`rd`] = mem.load(8, x[`rs1`] +% signExtend(`imm`))
pub const LBU = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.LOAD, funct3.LBU };

    pub fn execute(context: anytype, inst: inst_format.I) mmu.LoadError!void {
        return load_execute(context, inst, .byte, .unsigned);
    }
};

/// Load Halfword Unsigned (16 bits)
///
/// x[`rd`] = mem.load(16, x[`rs1`] +% signExtend(`imm`))
pub const LHU = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.LOAD, funct3.LHU };

    pub fn execute(context: anytype, inst: inst_format.I) mmu.LoadError!void {
        return load_execute(context, inst, .halfword, .unsigned);
    }
};

inline fn store_execute(context: anytype, inst: inst_format.S, comptime width: mmu.MemoryValueWidth) mmu.StoreError!void {
    const arch = hart_arch(@TypeOf(context));
    const base = context.getXRegister(inst.rs1).unsigned;
    const imm = inst.getImmediate().signExtended(arch.XLEN).unsigned;
    const addr = base +% imm;
    const src = context.getXRegister(inst.rs2).truncated(width.bits());
    try context.store(width, addr, src);
}

/// Store Byte (8 bits)
///
/// mem[x[`rs1`] +% signExtend(`imm`)] = truncate(8, x[`rs2`])
pub const SB = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.STORE, funct3.SB };

    pub fn execute(context: anytype, inst: inst_format.S) mmu.StoreError!void {
        return store_execute(context, inst, .byte);
    }
};

/// Store Halfword (16 bits)
///
/// mem[x[`rs1`] +% signExtend(`imm`)] = truncate(16, x[`rs2`])
pub const SH = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.STORE, funct3.SH };

    pub fn execute(context: anytype, inst: inst_format.S) mmu.StoreError!void {
        return store_execute(context, inst, .halfword);
    }
};

/// Store Word (32 bits)
///
/// mem[x[`rs1`] +% signExtend(`imm`)] = truncate(32, x[`rs2`])
pub const SW = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.STORE, funct3.SW };

    pub fn execute(context: anytype, inst: inst_format.S) mmu.StoreError!void {
        return store_execute(context, inst, .word);
    }
};

/// Fence
pub const FENCE = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.MISC_MEM, funct3.FENCE };

    pub fn execute(context: anytype, inst: inst_format.FENCE) void {
        const succ: FenceOperands = .{
            .writes = inst.sw,
            .reads = inst.sr,
            .outputs = inst.so,
            .inputs = inst.si
        };
        const pred: FenceOperands = .{
            .writes = inst.pw,
            .reads = inst.pr,
            .outputs = inst.po,
            .inputs = inst.pi
        };
        context.fence(inst.fm, pred, succ);
    }
};

/// Environment Call
pub const ECALL = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.SYSTEM, funct3.PRIV, funct12.ECALL };

    pub fn execute(context: anytype, inst: inst_format.ENV) void {
        // nothing we need in the contents of the instruction, but the argument is
        // needed for pattern matching the instruction
        _ = inst;
        context.ecall();
    }
};

/// Environment Break
pub const EBREAK = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.SYSTEM, funct3.PRIV, funct12.EBREAK };

    pub fn execute(context: anytype, inst: inst_format.ENV) void {
        // nothing we need in the contents of the instruction, but the argument is
        // needed for pattern matching the instruction
        _ = inst;
        context.ebreak();
    }
};

// RV64 ----------------------------------------------------------------------------------------------------------------

/// ADD Immediate Word (32 bits)
///
/// x[`rd`] = signExtend(truncate(32, x[`rs1`]) +% signExtend(`imm`))
/// `ADDIW rd, rs1, 0` is used to implement the `SEXT.W pseudoinstruction`
pub const ADDIW = struct {
    pub const Ext = .{ 64 };

    pub const Id = .{ opcode.OP_IMM_32, funct3.ADDIW };

    pub fn execute(context: anytype, inst: inst_format.I) void {
        const arch = hart_arch(@TypeOf(context));
        const src = context.getXRegister(inst.rs1).truncated(32).signed;
        const imm = inst.getImmediate().signExtended(32).signed;
        const res = (Data(32) { .signed = src +% imm }).signExtended(arch.XLEN);
        context.setXRegister(inst.rd, res);
    }
};
