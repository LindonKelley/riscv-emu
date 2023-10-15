/// RISC-V instructions, a Word is 32 bits.

const std = @import("std");
const opcode = @import("opcode.zig");
const inst_format = @import("inst_format.zig");
const instruction = @import("instruction.zig");
const expect = std.testing.expect;
const expectError = std.testing.expectError;
const expectEqual = std.testing.expectEqual;
const SimpleHart = @import("hart.zig").SimpleHart;
const Signedness = std.builtin.Signedness;
const Int = std.meta.Int;
const ext = @import("extension.zig");
const mmu = @import("mmu.zig");
const MemoryValueWidth = mmu.MemoryValueWidth;
const register = @import("register.zig");
const assemble = @import("assemble.zig");
const load = @import("load.zig");

pub const funct3 = struct {
    pub const ADDI: u3 =    0b000;
    pub const SLTI: u3 =    0b010;
    pub const SLTIU: u3 =   0b011;
    pub const ANDI: u3 =    0b111;
    pub const ORI: u3 =     0b110;
    pub const XORI: u3 =    0b100;

    pub const SLLI: u3 =    0b001;
    pub const SRLI: u3 =    0b101;
    pub const SRAI: u3 =    0b101;

    pub const ADD: u3 =     0b000;
    pub const SLT: u3 =     0b010;
    pub const SLTU: u3 =    0b011;
    pub const AND: u3 =     0b111;
    pub const OR: u3 =      0b110;
    pub const XOR: u3 =     0b100;
    pub const SLL: u3 =     0b001;
    pub const SRL: u3 =     0b101;
    pub const SUB: u3 =     0b000;
    pub const SRA: u3 =     0b101;

    pub const BEQ: u3 =     0b000;
    pub const BNE: u3 =     0b001;
    pub const BLT: u3 =     0b100;
    pub const BLTU: u3 =    0b110;
    pub const BGE: u3 =     0b101;
    pub const BGEU: u3 =    0b111;

    pub const LB: u3 =      0b000;
    pub const LH: u3 =      0b001;
    pub const LW: u3 =      0b010;
    pub const LBU: u3 =     0b100;
    pub const LHU: u3 =     0b101;

    pub const SB: u3 =      0b000;
    pub const SH: u3 =      0b001;
    pub const SW: u3 =      0b010;

    pub const FENCE: u3 =   0b000;

    pub const PRIV: u3 =    0b000;

    // Zifencei

    pub const FENCE_I: u3 = 0b001;

    // todo move to RV64 section
    pub const LWU: u3 =     0b110;
    pub const LD: u3 =      0b011;

    pub const SD: u3 =      0b011;
};

pub const funct12 = struct {
    pub const ECALL: u12 =  0b0000_0000_0000;
    pub const EBREAK: u12 = 0b0000_0000_0001;
};

// todo switch to types as values instead of unnecesarily inlining comptime calls

/// ADD Immediate
///
/// x[`rd`] = x[`rs1`] +% signExtend(`imm`)
/// `ADDI rd, rs1, 0` is used to implement the `MV rd, rs1` psuedoinstruction
/// `ADDI 0, 0, 0` is used to encode `NOP`
pub const ADDI = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP_IMM, funct3.ADDI };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) void {
        const arch = hart_arch(@TypeOf(hart_ptr.*));
        const src = hart_ptr.getXRegister(inst.rs1).signed;
        const imm = inst.getImmediate().signExtended(arch.XLEN).signed;
        const res = .{ .signed = src +% imm };
        hart_ptr.setXRegister(inst.rd, res);
    }

    test "ADDI" {
        inline for (.{ 32, 64 }, .{"rv32i", "rv64i"}) |XLEN, arch| {
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
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP_IMM, funct3.SLTI };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) void {
        const arch = hart_arch(@TypeOf(hart_ptr.*));
        const src = hart_ptr.getXRegister(inst.rs1).signed;
        const imm = inst.getImmediate().signExtended(arch.XLEN).signed;
        const res = .{ .unsigned =  if (src < imm) @as(arch.usize, 1) else @as(arch.usize, 0) };
        hart_ptr.setXRegister(inst.rd, res);
    }
};

/// Set Less Than Immediate Unsigned
///
/// x[`rd`] = if (x[`rs1`] (unsigned)< signExtend(`imm`)) 1 else 0
pub const SLTIU = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP_IMM, funct3.SLTIU };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) void {
        const arch = hart_arch(@TypeOf(hart_ptr.*));
        const src = hart_ptr.getXRegister(inst.rs1).unsigned;
        const imm = inst.getImmediate().signExtended(arch.XLEN).unsigned;
        const res = .{ .unsigned =  if (src < imm) @as(arch.usize, 1) else @as(arch.usize, 0) };
        hart_ptr.setXRegister(inst.rd, res);
    }
};

/// AND Immediate
///
/// x[`rd`] = x[`rs1`] & signExtend(`imm`)
pub const ANDI = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP_IMM, funct3.ANDI };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) void {
        const arch = hart_arch(@TypeOf(hart_ptr.*));
        const src = hart_ptr.getXRegister(inst.rs1).signed;
        const imm = inst.getImmediate().signExtended(arch.XLEN).signed;
        const res = .{ .signed = src & imm };
        hart_ptr.setXRegister(inst.rd, res);
    }
};

/// OR Immediate
///
/// x[`rd`] = x[`rs1`] | signExtend(`imm`)
pub const ORI = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP_IMM, funct3.ORI };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) void {
        const arch = hart_arch(@TypeOf(hart_ptr.*));
        const src = hart_ptr.getXRegister(inst.rs1).signed;
        const imm = inst.getImmediate().signExtended(arch.XLEN).signed;
        const res = .{ .signed = src | imm };
        hart_ptr.setXRegister(inst.rd, res);
    }
};

/// eXclusive OR Immediate
///
/// x[`rd`] = x[`rs1`] ^ signExtend(`imm`)
/// `XORI rd, rs1, -1` is equivelent to psuedoinstruction `NOT rd, rs1`
pub const XORI = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP_IMM, funct3.XORI };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) void {
        const arch = hart_arch(@TypeOf(hart_ptr.*));
        const src = hart_ptr.getXRegister(inst.rs1).signed;
        const imm = inst.getImmediate().signExtended(arch.XLEN).signed;
        const res = .{ .signed = src ^ imm };
        hart_ptr.setXRegister(inst.rd, res);
    }
};

/// Shift Left Logical Immediate
///
/// x[`rd`] = x[`rs1`] << `shamt`
pub const SLLI = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP_IMM, funct3.SLLI, 0b000_0000 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.IS) void {
        const src = hart_ptr.getXRegister(inst.rs1).unsigned;
        const res = .{ .unsigned = src << inst.shamt };
        hart_ptr.setXRegister(inst.rd, res);
    }
};

/// Shift Right Logical Immediate
///
/// x[`rd`] = x[`rs1`] (unsigned)>> `shamt`
pub const SRLI = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP_IMM, funct3.SRLI, 0b000_0000 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.IS) void {
        const src = hart_ptr.getXRegister(inst.rs1).unsigned;
        const res = .{ .unsigned = src >> inst.shamt };
        hart_ptr.setXRegister(inst.rd, res);
    }
};

/// Shift Right Arithmetic Immediate
///
/// x[`rd`] = x[`rs1`] (signed)>> `shamt`
pub const SRAI = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP_IMM, funct3.SRAI, 0b000_0010 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.IS) void {
        const src = hart_ptr.getXRegister(inst.rs1).signed;
        const res = .{ .signed = src >> inst.shamt };
        hart_ptr.setXRegister(inst.rd, res);
    }
};

/// Load Upper Immediate
///
/// x[`rd`] = signExtend(`imm` << 12)
pub const LUI = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{opcode.LUI};

    pub fn execute(hart_ptr: anytype, inst: inst_format.U) void {
        const imm = inst.getImmediate().unsigned;
        const res = .{ .unsigned = imm };
        hart_ptr.setXRegister(inst.rd, res);
    }
};

/// Add Upper Immediate to `PC`
///
/// x[`rd`] = `pc` + signExtend(`imm` << 12)
pub const AUIPC = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{opcode.AUIPC};

    pub fn execute(hart_ptr: anytype, inst: inst_format.U) void {
        const imm = inst.getImmediate().unsigned;
        const pc = hart_ptr.getPc();
        const res = .{ .unsigned = pc +% imm };
        hart_ptr.setXRegister(inst.rd, res);
    }
};

/// ADD
///
/// x[`rd`] = x[`rs1`] +% x[`rs2`]
pub const ADD = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP, funct3.ADD, 0b000_0000 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.R) void {
        const arch = hart_arch(@TypeOf(hart_ptr.*));
        const src1 = hart_ptr.getXRegister(inst.rs1).signExtended(arch.XLEN).signed;
        const src2 = hart_ptr.getXRegister(inst.rs2).signExtended(arch.XLEN).signed;
        const res = .{ .signed = src1 +% src2 };
        hart_ptr.setXRegister(inst.rd, res);
    }
};

/// Set Less Than
///
/// x[`rd`] = if (x[`rs1`] (signed)< x[`rs2`]) 1 else 0
pub const SLT = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP, funct3.SLT, 0b000_0000 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.R) void {
        const arch = hart_arch(@TypeOf(hart_ptr.*));
        const src1 = hart_ptr.getXRegister(inst.rs1).signed;
        const src2 = hart_ptr.getXRegister(inst.rs2).signed;
        const res = .{ .unsigned = if (src1 < src2) @as(arch.usize, 1) else @as(arch.usize, 0) };
        hart_ptr.setXRegister(inst.rd, res);
    }
};

/// Set Less Than Unsigned
///
/// x[`rd`] = if (x[`rs1`] (unsigned)< x[`rs2`]) 1 else 0
/// `SLTU rd, x0, rs1` is equivalent to psuedoinstruction `SNEZ rd, rs`
pub const SLTU = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP, funct3.SLTU, 0b000_0000 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.R) void {
        const arch = hart_arch(@TypeOf(hart_ptr.*));
        const src1 = hart_ptr.getXRegister(inst.rs1).unsigned;
        const src2 = hart_ptr.getXRegister(inst.rs2).unsigned;
        const res = .{ .unsigned = if (src1 < src2) @as(arch.usize, 1) else @as(arch.usize, 0) };
        hart_ptr.setXRegister(inst.rd, res);
    }
};

/// AND
///
/// x[`rd`] = x[`rs1`] & x[`rs2`]
pub const AND = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP, funct3.AND, 0b000_0000 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.R) void {
        const src1 = hart_ptr.getXRegister(inst.rs1).unsigned;
        const src2 = hart_ptr.getXRegister(inst.rs2).unsigned;
        const res = .{ .unsigned = src1 & src2 };
        hart_ptr.setXRegister(inst.rd, res);
    }
};

/// OR
///
/// x[`rd`] = x[`rs1`] | x[`rs2`]
pub const OR = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP, funct3.OR, 0b000_0000 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.R) void {
        const src1 = hart_ptr.getXRegister(inst.rs1).unsigned;
        const src2 = hart_ptr.getXRegister(inst.rs2).unsigned;
        const res = .{ .unsigned = src1 | src2 };
        hart_ptr.setXRegister(inst.rd, res);
    }
};

/// eXclusive OR
///
/// x[`rd`] = x[`rs1`] ^ x[`rs2`]
pub const XOR = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP, funct3.XOR, 0b000_0000 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.R) void {
        const src1 = hart_ptr.getXRegister(inst.rs1).unsigned;
        const src2 = hart_ptr.getXRegister(inst.rs2).unsigned;
        const res = .{ .unsigned = src1 ^ src2 };
        hart_ptr.setXRegister(inst.rd, res);
    }
};

/// Shift Left Logical
///
/// x[`rd`] = x[`rs1`] << (x[`rs2`] & 0b1_1111)
pub const SLL = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP, funct3.SLL, 0b000_0000 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.R) void {
        const src1 = hart_ptr.getXRegister(inst.rs1).unsigned;
        const src2 = hart_ptr.getXRegister(inst.rs2).truncated(5).unsigned;
        const res = .{ .unsigned = src1 << src2 };
        hart_ptr.setXRegister(inst.rd, res);
    }
};

/// Shift Right Logical
///
/// x[`rd`] = x[`rs1`] (unsigned)>> (x[`rs2`] & 0b1_1111)
pub const SRL = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP, funct3.SRL, 0b000_0000 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.R) void {
        const src1 = hart_ptr.getXRegister(inst.rs1).unsigned;
        const src2 = hart_ptr.getXRegister(inst.rs2).truncated(5).unsigned;
        const res = .{ .unsigned = src1 >> src2 };
        hart_ptr.setXRegister(inst.rd, res);
    }
};

/// SUB
///
/// x[`rd`] = x[`rs1`] -% x[`rs2`]
pub const SUB = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP, funct3.SUB, 0b000_0010 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.R) void {
        const src1 = hart_ptr.getXRegister(inst.rs1).unsigned;
        const src2 = hart_ptr.getXRegister(inst.rs2).unsigned;
        const res = .{ .unsigned = src1 -% src2 };
        hart_ptr.setXRegister(inst.rd, res);
    }
};

/// Shift Right Arithmetic
///
/// x[`rd`] = x[`rs1`] (signed)>> (x[`rs2`] & 0b1_1111)
pub const SRA = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP, funct3.SRA, 0b000_0010 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.R) void {
        const src1 = hart_ptr.getXRegister(inst.rs1).signed;
        const src2 = hart_ptr.getXRegister(inst.rs2).truncated(5).unsigned;
        const res = .{ .signed = src1 >> src2 };
        hart_ptr.setXRegister(inst.rd, res);
    }
};

pub const JumpError = error {
    InstructionAddressMisaligned
};

/// Jump And Link
///
/// x[`rd`] = `pc` +% 4; `pc` +%= signExtend(`imm`)
/// `JAL x0, [whatever] is equivalent to psuedoinstruction `J [whatever]` (plain unconditional jump)
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address however, this is not possible when C-extension is enabled.
pub const JAL = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.JAL };

    pub fn execute(hart_ptr: anytype, inst: inst_format.J) JumpError!void {
        const arch = hart_arch(@TypeOf(hart_ptr.*));
        const imm = inst.getImmediate().signExtended(arch.XLEN).unsigned;
        const pc = hart_ptr.getPc();
        const pc_set = pc +% imm;
        if (arch.IALIGN != 16 and pc_set % 4 != 0) {
            return error.InstructionAddressMisaligned;
        } else {
            // todo not sure if this should happen irregardless of the above exception
            hart_ptr.setXRegister(inst.rd, .{ .unsigned = pc +% 4 });
            hart_ptr.setPc(pc_set);
        }
    }
};

/// Jump And Link Register
///
/// x[`rd`] = `pc` +% 4; `pc` = (x[`rs1`] +% signExtend(`imm`)) & signExtend(0b10)
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address however, this is not possible when C-extension is enabled.
pub const JALR = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.JALR, 0 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) JumpError!void {
        const arch = hart_arch(@TypeOf(hart_ptr.*));
        const src = hart_ptr.getXRegister(inst.rs1).signed;
        const imm = inst.getImmediate().signExtended(arch.XLEN).signed;
        const pc = hart_ptr.getPc();
        const pc_set: arch.usize = @bitCast((src +% imm) & 0b10);
        if (@TypeOf(hart_ptr.*).IALIGN != 16 and pc_set % 4 != 0) {
            return error.InstructionAddressMisaligned;
        } else {
            // todo not sure if this should happen irregardless of the above exception
            hart_ptr.setXRegister(inst.rd, .{ .unsigned = pc +% 4 });
            hart_ptr.setPc(pc_set);
        }
    }

    test {
        var hart = SimpleHart(32, mmu.BasicMmu(32)){ .mmu = .{} };
        var inst: inst_format.I = @bitCast(@as(u32, 0));
        inst.setImmediate(.{ .unsigned = 0 });
        try JALR.execute(&hart, inst);
        inst.setImmediate(.{ .unsigned = 1 }); // lowest bit cleared
        try JALR.execute(&hart, inst);
        inst.setImmediate(.{ .unsigned = 2 });
        try expectError(error.InstructionAddressMisaligned, JALR.execute(&hart, inst));
        inst.setImmediate(.{ .unsigned = 3 });
        try expectError(error.InstructionAddressMisaligned, JALR.execute(&hart, inst));
    }
};

inline fn branch_execute(hart_ptr: anytype, inst: inst_format.B, comptime signedness: Signedness, comptime compare: std.math.CompareOperator) JumpError!void {
    const arch = hart_arch(@TypeOf(hart_ptr.*));
    const src1 = switch(signedness) {
        .signed => hart_ptr.getXRegister(inst.rs1).signed,
        .unsigned => hart_ptr.getXRegister(inst.rs1).unsigned
    };
    const src2 = switch(signedness) {
        .signed => hart_ptr.getXRegister(inst.rs2).signed,
        .unsigned => hart_ptr.getXRegister(inst.rs2).unsigned
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
        const pc = hart_ptr.getPc();
        const imm = inst.getImmediate().signExtended(arch.XLEN).unsigned;
        const pc_set = pc +% imm;
        if (arch.IALIGN != 16 and pc_set % 4 != 0) {
            return error.InstructionAddressMisaligned;
        } else {
            hart_ptr.setPc(pc_set);
        }
    }
}

/// Branch if EQual
///
/// if (x[`rs1`] == x[`rs2`]) `pc` +%= signExtend(`imm`)
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address, however, this is not possible when C-extension is enabled.
pub const BEQ = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.BRANCH, funct3.BEQ };

    pub fn execute(hart_ptr: anytype, inst: inst_format.B) JumpError!void {
        return branch_execute(hart_ptr, inst, .signed, .eq);
    }
};

/// Branch if Not eQual
///
/// if (x[`rs1`] != x[`rs2`]) `pc` +%= signExtend(`imm`)
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address, however, this is not possible when C-extension is enabled.
pub const BNQ = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.BRANCH, funct3.BNE };

    pub fn execute(hart_ptr: anytype, inst: inst_format.B) JumpError!void {
        return branch_execute(hart_ptr, inst, .signed, .neq);
    }
};

/// Branch if Less Than
///
/// if (x[`rs1`] (signed)< x[`rs2`]) `pc` +%= signExtend(`imm`)
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address, however, this is not possible when C-extension is enabled.
/// psuedoinstruction `BGT` can be implemented by flipping `rs1` and `rs2`
pub const BLT = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.BRANCH, funct3.BLT };

    pub fn execute(hart_ptr: anytype, inst: inst_format.B) JumpError!void {
        return branch_execute(hart_ptr, inst, .signed, .lt);
    }
};

/// Branch if Less Than Unsigned
///
/// if (x[`rs1`] (unsigned)< x[`rs2`]) `pc` +%= `imm`
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address, however, this is not possible when C-extension is enabled.
/// psuedoinstruction `BGTU` can be implemented by flipping `rs1` and `rs2`
pub const BLTU = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.BRANCH, funct3.BLTU };

    pub fn execute(hart_ptr: anytype, inst: inst_format.B) JumpError!void {
        return branch_execute(hart_ptr, inst, .unsigned, .lt);
    }
};

/// Branch if Greater or Equal
///
/// if (x[`rs1`] (signed)>= x[`rs2`]) `pc` +%= `imm`
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address, however, this is not possible when C-extension is enabled.
/// psuedoinstruction `BLE` can be implemented by flipping `rs1` and `rs2`
pub const BGE = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.BRANCH, funct3.BGE };

    pub fn execute(hart_ptr: anytype, inst: inst_format.B) JumpError!void {
        return branch_execute(hart_ptr, inst, .signed, .gte);
    }
};

/// Branch if Greater or Equal Unsigned
///
/// if (x[`rs1`] (unsigned)>= x[`rs2`]) `pc` +%= `imm`
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address, however, this is not possible when C-extension is enabled.
/// psuedoinstruction `BLEU` can be implemented by flipping `rs1` and `rs2`
pub const BGEU = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.BRANCH, funct3.BGEU };

    pub fn execute(hart_ptr: anytype, inst: inst_format.B) JumpError!void {
        return branch_execute(hart_ptr, inst, .unsigned, .gte);
    }
};

inline fn load_execute(hart_ptr: anytype, inst: inst_format.I, comptime width: MemoryValueWidth, comptime signedness: Signedness) mmu.LoadError!void {
    const arch = hart_arch(@TypeOf(hart_ptr.*));
    const base = hart_ptr.getXRegister(inst.rs1).unsigned;
    const imm = inst.getImmediate().signExtended(arch.XLEN).unsigned;
    const addr = base +% imm;
    const data = try hart_ptr.load(width, addr);
    const res = switch (signedness) {
        .signed => data.signExtended(arch.XLEN),
        .unsigned => data.zeroExtended(arch.XLEN)
    };
    hart_ptr.setXRegister(inst.rd, res);
}

/// Load Byte (8 bits)
///
/// x[`rd`] = signExtend(mem.load(8, x[`rs1`] +% signExtend(`imm`)))
pub const LB = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.LOAD, funct3.LB };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) mmu.LoadError!void {
        return load_execute(hart_ptr, inst, .byte, .signed);
    }
};

/// Load Halfword (16 bits)
///
/// x[`rd`] = signExtend(mem.load(16, x[`rs1`] +% signExtend(`imm`)))
pub const LH = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.LOAD, funct3.LH };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) mmu.LoadError!void {
        return load_execute(hart_ptr, inst, .halfword, .signed);
    }
};

/// Load Word (32 bits)
///
/// x[`rd`] = signExtend(mem.load(32, x[`rs1`] +% signExtend(`imm`)))
pub const LW = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.LOAD, funct3.LW };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) mmu.LoadError!void {
        return load_execute(hart_ptr, inst, .word, .signed);
    }
};

/// Load Byte Unsigned (8 bits)
///
/// x[`rd`] = mem.load(8, x[`rs1`] +% signExtend(`imm`))
pub const LBU = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.LOAD, funct3.LBU };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) mmu.LoadError!void {
        return load_execute(hart_ptr, inst, .byte, .unsigned);
    }
};

/// Load Halfword Unsigned (16 bits)
///
/// x[`rd`] = mem.load(16, x[`rs1`] +% signExtend(`imm`))
pub const LHU = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.LOAD, funct3.LHU };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) mmu.LoadError!void {
        return load_execute(hart_ptr, inst, .halfword, .unsigned);
    }
};

inline fn store_execute(hart_ptr: anytype, inst: inst_format.S, comptime width: MemoryValueWidth) mmu.StoreError!void {
    const arch = hart_arch(@TypeOf(hart_ptr.*));
    const base = hart_ptr.getXRegister(inst.rs1).unsigned;
    const imm = inst.getImmediate().signExtended(arch.XLEN).unsigned;
    const addr = base +% imm;
    const src = hart_ptr.getXRegister(inst.rs2).truncated(width.bits());
    try hart_ptr.store(width, addr, src);
}

/// Store Byte (8 bits)
///
/// mem[x[`rs1`] +% signExtend(`imm`)] = truncate(8, x[`rs2`])
pub const SB = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.STORE, funct3.SB };

    pub fn execute(hart_ptr: anytype, inst: inst_format.S) mmu.StoreError!void {
        return store_execute(hart_ptr, inst, .byte);
    }
};

/// Store Halfword (16 bits)
///
/// mem[x[`rs1`] +% signExtend(`imm`)] = truncate(16, x[`rs2`])
pub const SH = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.STORE, funct3.SH };

    pub fn execute(hart_ptr: anytype, inst: inst_format.S) mmu.StoreError!void {
        return store_execute(hart_ptr, inst, .halfword);
    }
};

/// Store Word (32 bits)
///
/// mem[x[`rs1`] +% signExtend(`imm`)] = truncate(32, x[`rs2`])
pub const SW = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.STORE, funct3.SW };

    pub fn execute(hart_ptr: anytype, inst: inst_format.S) mmu.StoreError!void {
        return store_execute(hart_ptr, inst, .word);
    }
};

/// Fence
pub const FENCE = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.MISC_MEM, funct3.FENCE };

    pub fn execute(hart_ptr: anytype, inst: inst_format.FENCE) void {
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
        hart_ptr.fence(inst.fm, pred, succ);
    }
};

/// Environment Call
pub const ECALL = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.SYSTEM, funct3.PRIV, funct12.ECALL };

    pub fn execute(hart_ptr: anytype, inst: inst_format.ENV) void {
        // nothing we need in the contents of the instruction, but the argument is
        // needed for pattern matching the instruction
        _ = inst;
        hart_ptr.ecall();
    }
};

/// Environment Break
pub const EBREAK = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.SYSTEM, funct3.PRIV, funct12.EBREAK };

    pub fn execute(hart_ptr: anytype, inst: inst_format.ENV) void {
        // nothing we need in the contents of the instruction, but the argument is
        // needed for pattern matching the instruction
        _ = inst;
        hart_ptr.ebreak();
    }
};

/// Instruction Fence
pub const FENCE_I = struct {
    pub const Ext = .{ 32, ext.Zifencei };

    pub const ID = .{ opcode.MISC_MEM, funct3.FENCE_I };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) void {
        hart_ptr.fence_i(@as(u32, @bitCast(inst)));
    }
};

fn hart_arch(comptime HartType: type) type {
    return struct {
        const XLEN = HartType.XLEN;
        const @"usize" = Int(.unsigned, HartType.XLEN);
        const @"isize" = Int(.signed, HartType.XLEN);
        const IALIGN = HartType.IALIGN;
        const ILEN = HartType.ILE;
    };
}

pub const FenceOperands = packed struct {
    writes: bool,
    reads: bool,
    outputs: bool,
    inputs: bool
};

comptime {
    for (@typeInfo(instruction).Struct.decls) |decl| {
        const ignored = .{ "funct3", "funct12", "FenceOperands", "JumpError" };
        if (arrayContains(ignored, decl.name)) continue;
        const field = @field(instruction, decl.name);
        if (!@hasDecl(field, "Ext") or !@hasDecl(field, "ID")) {
            @compileError("Instructions must have Ext and ID declared: " ++ decl.name);
        }
        const Instruction = field;
        if (Instruction.Ext.len < 2) {
            @compileError("An Instruction's Ext field must contain a minimum XLEN {32, 64} followed by the " ++
            "extension that implements them: " ++ decl.name);
        }
        if (!arrayContains(.{32, 64}, Instruction.Ext[0])) {
            @compileError("An Instruction's first Ext field must start with either 32 or 64: " ++ decl.name);
        }
        if (Instruction.ID.len > 3) {
            @compileError("An Instruction's ID must have at most 3 elements: " ++ decl.name);
        }
    }
}

fn arrayContains(array: anytype, value: anytype) bool {
    for (array) |v| {
        switch (@typeInfo(@TypeOf(value))) {
            .Array, .Pointer => if (std.mem.eql(u8, v, value)) {
                return true;
            },
            else => if (v == value) {
                return true;
            }
        }
    }
    return false;
}

comptime {
    for (@typeInfo(@This()).Struct.decls) |decl| {
        const field = @field(@This(), decl.name);
        if (@TypeOf(field) == type and @typeInfo(field) != .ErrorSet) {
            std.testing.refAllDecls(field);
        }
    }
}
