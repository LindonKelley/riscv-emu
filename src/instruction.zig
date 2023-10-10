/// RISC-V instructions, all integers are signed unless otherwise specified, a Word is 32 bits.

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

// todo switch docs to using x[`rs1`] instead of `rs1`
// todo switch to types as values instead of unnecesarily inlining comptime calls
// todo use hart_arch instead of const XLEN, Usize, Isize

/// ADD Immediate
///
/// `rd` = `rs1` +% `imm`
/// `ADDI rd, rs1, 0` is used to implement the `MV rd, rs1` psuedoinstruction
/// `ADDI 0, 0, 0` is used to encode `NOP`
pub const ADDI = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP_IMM, funct3.ADDI };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) void {
        const rs1 = hart_ptr.getXRegister(.signed, inst.rs1);
        const imm = inst.getImmediate();
        hart_ptr.setXRegister(inst.rd, rs1 +% imm);
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
                try expect(i == hart.getXRegister(.signed, register.a0));
                try expect(-@as(isize, @intCast(i)) == hart.getXRegister(.signed, register.a1));
            }
            // todo overflow testing
        }
    }
};

/// Set Less Than Immediate
///
/// `rd` = if (`rs1` < `imm`) 1 else 0
pub const SLTI = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP_IMM, funct3.SLTI };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) void {
        const rs1 = hart_ptr.getXRegister(.signed, inst.rs1);
        const imm = inst.getImmediate();
        const arch = hart_arch(@TypeOf(hart_ptr.*));
        hart_ptr.setXRegister(inst.rd, if (rs1 < imm) @as(arch.usize, 1) else @as(arch.isize, 0));
    }
};

/// Set Less Than Immediate Unsigned
///
/// `rd` = if (`rs1` < `imm`) 1 else 0 // all unsigned; imm is sign extended to XLEN, then treated as unsigned
pub const SLTIU = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP_IMM, funct3.SLTIU };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) void {
        const rs1 = hart_ptr.getXRegister(.unsigned, inst.rs1);
        const arch = hart_arch(@TypeOf(hart_ptr.*));
        const imm: arch.usize = @bitCast(@as(arch.isize, inst.getImmediate()));
        hart_ptr.setXRegister(inst.rd, if (rs1 < imm) @as(arch.usize, 1) else @as(arch.usize, 0));
    }
};

/// AND Immediate
///
/// `rd` = `rs1` & `imm`
pub const ANDI = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP_IMM, funct3.ANDI };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) void {
        const rs1 = hart_ptr.getXRegister(.signed, inst.rs1);
        const imm = inst.getImmediate();
        hart_ptr.setXRegister(inst.rd, rs1 & imm);
    }
};

/// OR Immediate
///
/// `rd` = `rs1` | `imm`
pub const ORI = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP_IMM, funct3.ORI };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) void {
        const rs1 = hart_ptr.getXRegister(.signed, inst.rs1);
        const imm = inst.getImmediate();
        hart_ptr.setXRegister(inst.rd, rs1 | imm);
    }
};

/// XOR Immediate
///
/// `rd` = `rs1` ^ `imm`
/// `XORI rd, rs1, -1` is equivelent to psuedoinstruction `NOT rd, rs1`
pub const XORI = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP_IMM, funct3.XORI };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) void {
        const rs1 = hart_ptr.getXRegister(.signed, inst.rs1);
        const imm = inst.getImmediate();
        hart_ptr.setXRegister(inst.rd, rs1 ^ imm);
    }
};

/// Shift Left Logical Immediate
///
/// `rd` = `rs1` << `shamt` // all unsigned
pub const SLLI = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP_IMM, funct3.SLLI, 0b000_0000 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.IS) void {
        const rs1 = hart_ptr.getXRegister(.unsigned, inst.rs1);
        hart_ptr.setXRegister(inst.rd, rs1 << inst.shamt);
    }
};

/// Shift Right Logical Immediate
///
/// `rd` = `rs1` >> `shamt` // all unsigned
pub const SRLI = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP_IMM, funct3.SRLI, 0b000_0000 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.IS) void {
        const rs1 = hart_ptr.getXRegister(.unsigned, inst.rs1);
        hart_ptr.setXRegister(inst.rd, rs1 >> inst.shamt);
    }
};

/// Shift Right Arithmetic Immediate
///
/// `rd` = `rs1` >> `shamt` // shamt unsigned
pub const SRAI = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP_IMM, funct3.SRAI, 0b000_0010 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.IS) void {
        const rs1 = hart_ptr.getXRegister(.signed, inst.rs1);
        hart_ptr.setXRegister(inst.rd, rs1 >> inst.shamt);
    }
};

/// Load Upper Immediate
///
/// `rd` = `imm` << 20 // all unsigned
pub const LUI = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{opcode.LUI};

    pub fn execute(hart_ptr: anytype, inst: inst_format.U) void {
        const imm = inst.getImmediate();
        hart_ptr.setXRegister(inst.rd, imm);
    }
};

/// Add Upper Immediate to `PC`
///
/// `rd` = `pc` + `imm` << 20 // all unsigned
pub const AUIPC = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{opcode.AUIPC};

    pub fn execute(hart_ptr: anytype, inst: inst_format.U) void {
        const imm = inst.getImmediate();
        const XLEN = @TypeOf(hart_ptr.*).XLEN;
        const Isize = Int(.signed, XLEN);
        hart_ptr.setXRegister(inst.rd, @as(Isize, @bitCast(hart_ptr.getPc())) +% imm);
    }
};

/// ADD
///
/// `rd` = `rs1` +% `rs2`
pub const ADD = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP, funct3.ADD, 0b000_0000 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.R) void {
        const rs1 = hart_ptr.getXRegister(.signed, inst.rs1);
        const rs2 = hart_ptr.getXRegister(.signed, inst.rs2);
        hart_ptr.setXRegister(inst.rd, rs1 + rs2);
    }
};

/// Set Less Than
///
/// `rd` = if (`rs1` < `rs2`) 1 else 0
pub const SLT = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP, funct3.SLT, 0b000_0000 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.R) void {
        const rs1 = hart_ptr.getXRegister(.signed, inst.rs1);
        const rs2 = hart_ptr.getXRegister(.signed, inst.rs2);
        const arch = hart_arch(@TypeOf(hart_ptr.*));
        hart_ptr.setXRegister(inst.rd, if (rs1 < rs2) @as(arch.usize, 1) else @as(arch.usize, 0));
    }
};

/// Set Less Than Unsigned
///
/// `rd` = if (`rs1` < `rs2`) 1 else 0 // all unsigned
/// `SLTU rd, x0, rs1` is equivalent to psuedoinstruction `SNEZ rd, rs`
pub const SLTU = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP, funct3.SLTU, 0b000_0000 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.R) void {
        const rs1 = hart_ptr.getXRegister(.unsigned, inst.rs1);
        const rs2 = hart_ptr.getXRegister(.unsigned, inst.rs2);
        const arch = hart_arch(@TypeOf(hart_ptr.*));
        hart_ptr.setXRegister(inst.rd, if (rs1 < rs2) @as(arch.usize, 1) else @as(arch.usize, 0));
    }
};

/// AND
///
/// `rd` = `rs1` & `rs2`
pub const AND = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP, funct3.AND, 0b000_0000 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.R) void {
        const rs1 = hart_ptr.getXRegister(.unsigned, inst.rs1);
        const rs2 = hart_ptr.getXRegister(.unsigned, inst.rs2);
        hart_ptr.setXRegister(inst.rd, rs1 & rs2);
    }
};

/// OR
///
/// `rd` = `rs1` | `rs2`
pub const OR = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP, funct3.OR, 0b000_0000 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.R) void {
        const rs1 = hart_ptr.getXRegister(.unsigned, inst.rs1);
        const rs2 = hart_ptr.getXRegister(.unsigned, inst.rs2);
        hart_ptr.setXRegister(inst.rd, rs1 | rs2);
    }
};

/// XOR
///
/// `rd` = `rs1` ^ `rs2`
pub const XOR = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP, funct3.XOR, 0b000_0000 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.R) void {
        const rs1 = hart_ptr.getXRegister(.unsigned, inst.rs1);
        const rs2 = hart_ptr.getXRegister(.unsigned, inst.rs2);
        hart_ptr.setXRegister(inst.rd, rs1 ^ rs2);
    }
};

/// Shift Left Logical
///
/// `rd` = `rs1` << `rs2` & 0b1_1111 // all unsigned
pub const SLL = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP, funct3.SLL, 0b000_0000 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.R) void {
        const rs1 = hart_ptr.getXRegister(.unsigned, inst.rs1);
        const rs2: u5 = @truncate(hart_ptr.getXRegister(.unsigned, inst.rs2));
        hart_ptr.setXRegister(inst.rd, rs1 << rs2);
    }
};

/// Shift Right Logical
///
/// `rd` = `rs1` >> `rs2` & 0b1_1111 // all unsigned
pub const SRL = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP, funct3.SRL, 0b000_0000 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.R) void {
        const rs1 = hart_ptr.getXRegister(.unsigned, inst.rs1);
        const rs2: u5 = @truncate(hart_ptr.getXRegister(.unsigned, inst.rs2));
        hart_ptr.setXRegister(inst.rd, rs1 >> rs2);
    }
};

/// SUB
///
/// `rd` = `rs1` -% `rs2`
pub const SUB = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP, funct3.SUB, 0b000_0010 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.R) void {
        const rs1 = hart_ptr.getXRegister(.signed, inst.rs1);
        const rs2 = hart_ptr.getXRegister(.signed, inst.rs2);
        hart_ptr.setXRegister(inst.rd, rs1 -% rs2);
    }
};

/// Shift Right Arithmetic
///
/// `rd` = `rs1` >> `rs2` & 0b1_1111 // rs2 is unsigned
pub const SRA = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.OP, funct3.SRA, 0b000_0010 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.R) void {
        const rs1 = hart_ptr.getXRegister(.signed, inst.rs1);
        const rs2: u5 = @truncate(hart_ptr.getXRegister(.unsigned, inst.rs2));
        hart_ptr.setXRegister(inst.rd, rs1 >> rs2);
    }
};

pub const JumpError = error {
    InstructionAddressMisaligned
};

/// Jump And Link
///
/// `rd` = `pc` +% 4; `pc` = `pc` +% `imm` // pc is unsigned, imm is sign extended to XLEN, then treated as unsigned
/// `JAL x0, [whatever] is equivalent to psuedoinstruction `J [whatever]` (plain unconditional jump)
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address however, this is not possible when C-extension is enabled.
pub const JAL = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.JAL };

    pub fn execute(hart_ptr: anytype, inst: inst_format.J) JumpError!void {
        const pc = hart_ptr.getPc();
        const imm: Int(.signed, @TypeOf(hart_ptr.*).XLEN) = inst.getImmediate();
        const pc_set = pc +% @as(Int(.unsigned, @TypeOf(hart_ptr.*).XLEN), @bitCast(imm));
        // todo is bit checking faster? if so update JALR and all branch instructions
        if (@TypeOf(hart_ptr.*).IALIGN != 16 and pc_set % 4 != 0) {
            return error.InstructionAddressMisaligned;
        } else {
            // todo not sure if this should happen irregardless of the above exception
            hart_ptr.setXRegister(inst.rd, pc +% 4);
            hart_ptr.setPc(pc_set);
        }
    }
};

/// Jump And Link Register
///
/// `rd` = `pc` +% 4; `pc` = @bitCast((`rs1` +% `imm`) & 0b10) // pc is unsigned, 0b10 must be sign extended
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address however, this is not possible when C-extension is enabled.
pub const JALR = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.JALR, 0 };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) JumpError!void {
        const pc = hart_ptr.getPc();
        const rs1 = hart_ptr.getXRegister(.signed, inst.rs1);
        const imm = inst.getImmediate();
        // todo not sure if 0b10 is sign extended in the coercion
        const pc_set: Int(.unsigned, @TypeOf(hart_ptr.*).XLEN) = @bitCast((rs1 +% imm) & 0b10);
        if (@TypeOf(hart_ptr.*).IALIGN != 16 and pc_set % 4 != 0) {
            return error.InstructionAddressMisaligned;
        } else {
            // todo not sure if this should happen irregardless of the above exception
            hart_ptr.setXRegister(inst.rd, pc +% 4);
            hart_ptr.setPc(pc_set);
        }
    }

    test {
        var hart = SimpleHart(32, mmu.BasicMmu(32)){ .mmu = .{} };
        var inst: inst_format.I = @bitCast(@as(u32, 0));
        inst.setImmediate(0);
        try JALR.execute(&hart, inst);
        inst.setImmediate(1); // lowest bit cleared
        try JALR.execute(&hart, inst);
        inst.setImmediate(2);
        try expectError(error.InstructionAddressMisaligned, JALR.execute(&hart, inst));
        inst.setImmediate(3);
        try expectError(error.InstructionAddressMisaligned, JALR.execute(&hart, inst));
    }
};

/// Branch if Equal
///
/// if (`rs1` == `rs2`) `pc` = `pc` +% `imm` // pc is unsigned, imm is sign extended to XLEN, then treated as unsigned
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address, however, this is not possible when C-extension is enabled.
pub const BEQ = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.BRANCH, funct3.BEQ };

    pub fn execute(hart_ptr: anytype, inst: inst_format.B) JumpError!void {
        const rs1 = hart_ptr.getXRegister(.signed, inst.rs1);
        const rs2 = hart_ptr.getXRegister(.signed, inst.rs2);
        const pc = hart_ptr.getPc();
        const imm: Int(.signed, @TypeOf(hart_ptr.*).XLEN) = inst.getImmediate();
        if (rs1 == rs2) {
            const pc_set = pc +% @as(Int(.unsigned, @TypeOf(hart_ptr.*).XLEN), @bitCast(imm));
            if (@TypeOf(hart_ptr.*).IALIGN != 16 and pc_set % 4 != 0) {
                return error.InstructionAddressMisaligned;
            } else {
                hart_ptr.setPc(pc_set);
            }
        }
    }
};

/// Branch if Not Equal
///
/// if (`rs1` != `rs2`) `pc` = `pc` +% `imm` // pc is unsigned, imm is sign extended to XLEN, then treated as unsigned
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address, however, this is not possible when C-extension is enabled.
pub const BNQ = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.BRANCH, funct3.BNE };

    pub fn execute(hart_ptr: anytype, inst: inst_format.B) JumpError!void {
        const rs1 = hart_ptr.getXRegister(.signed, inst.rs1);
        const rs2 = hart_ptr.getXRegister(.signed, inst.rs2);
        const pc = hart_ptr.getPc();
        const imm: Int(.signed, @TypeOf(hart_ptr.*).XLEN) = inst.getImmediate();
        if (rs1 != rs2) {
            const pc_set = pc +% @as(Int(.unsigned, @TypeOf(hart_ptr.*).XLEN), @bitCast(imm));
            if (@TypeOf(hart_ptr.*).IALIGN != 16 and pc_set % 4 != 0) {
                return error.InstructionAddressMisaligned;
            } else {
                hart_ptr.setPc(pc_set);
            }
        }
    }
};

/// Branch if Less Than
///
/// if (`rs1` < `rs2`) `pc` = `pc` +% `imm` // pc is unsigned, imm is sign extended to XLEN, then treated as unsigned
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address, however, this is not possible when C-extension is enabled.
/// psuedoinstruction `BGT` can be implemented by flipping `rs1` and `rs2`
pub const BLT = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.BRANCH, funct3.BLT };

    pub fn execute(hart_ptr: anytype, inst: inst_format.B) JumpError!void {
        const rs1 = hart_ptr.getXRegister(.signed, inst.rs1);
        const rs2 = hart_ptr.getXRegister(.signed, inst.rs2);
        const pc = hart_ptr.getPc();
        const imm: Int(.signed, @TypeOf(hart_ptr.*).XLEN) = inst.getImmediate();
        if (rs1 < rs2) {
            const pc_set = pc +% @as(Int(.unsigned, @TypeOf(hart_ptr.*).XLEN), @bitCast(imm));
            if (@TypeOf(hart_ptr.*).IALIGN != 16 and pc_set % 4 != 0) {
                return error.InstructionAddressMisaligned;
            } else {
                hart_ptr.setPc(pc_set);
            }
        }
    }
};

/// Branch if Less Than Unsigned
///
/// if (`rs1` < `rs2`) `pc` = `pc` +% `imm` // all unsigned; imm is sign extended to XLEN, then treated as unsigned
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address, however, this is not possible when C-extension is enabled.
/// psuedoinstruction `BGTU` can be implemented by flipping `rs1` and `rs2`
pub const BLTU = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.BRANCH, funct3.BLTU };

    pub fn execute(hart_ptr: anytype, inst: inst_format.B) JumpError!void {
        const rs1 = hart_ptr.getXRegister(.unsigned, inst.rs1);
        const rs2 = hart_ptr.getXRegister(.unsigned, inst.rs2);
        const pc = hart_ptr.getPc();
        const imm: Int(.signed, @TypeOf(hart_ptr.*).XLEN) = inst.getImmediate();
        if (rs1 < rs2) {
            const pc_set = pc +% @as(Int(.unsigned, @TypeOf(hart_ptr.*).XLEN), @bitCast(imm));
            if (@TypeOf(hart_ptr.*).IALIGN != 16 and pc_set % 4 != 0) {
                return error.InstructionAddressMisaligned;
            } else {
                hart_ptr.setPc(pc_set);
            }
        }
    }
};

/// Branch if Greater or Equal
///
/// if (`rs1` >= `rs2`) `pc` = `pc` +% `imm` // pc is unsigned, imm is sign extended to XLEN, then treated as unsigned
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address, however, this is not possible when C-extension is enabled.
/// psuedoinstruction `BLE` can be implemented by flipping `rs1` and `rs2`
pub const BGE = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.BRANCH, funct3.BGE };

    pub fn execute(hart_ptr: anytype, inst: inst_format.B) JumpError!void {
        const rs1 = hart_ptr.getXRegister(.signed, inst.rs1);
        const rs2 = hart_ptr.getXRegister(.signed, inst.rs2);
        const pc = hart_ptr.getPc();
        const imm: Int(.signed, @TypeOf(hart_ptr.*).XLEN) = inst.getImmediate();
        if (rs1 >= rs2) {
            const pc_set = pc +% @as(Int(.unsigned, @TypeOf(hart_ptr.*).XLEN), @bitCast(imm));
            if (@TypeOf(hart_ptr.*).IALIGN != 16 and pc_set % 4 != 0) {
                return error.InstructionAddressMisaligned;
            } else {
                hart_ptr.setPc(pc_set);
            }
        }
    }
};

/// Branch if Greater or Equal Unsigned
///
/// if (`rs1` >= `rs2`) `pc` = `pc` +% `imm` // all unsigned; imm is sign extended to XLEN, then treated as unsigned
/// Can raise the `InstructionAddressMisaligned` exception if execution attempts
/// to set PC to an invalid address, however, this is not possible when C-extension is enabled.
/// psuedoinstruction `BLEU` can be implemented by flipping `rs1` and `rs2`
pub const BGEU = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.BRANCH, funct3.BGEU };

    pub fn execute(hart_ptr: anytype, inst: inst_format.B) JumpError!void {
        const rs1 = hart_ptr.getXRegister(.unsigned, inst.rs1);
        const rs2 = hart_ptr.getXRegister(.unsigned, inst.rs2);
        const pc = hart_ptr.getPc();
        const imm: Int(.signed, @TypeOf(hart_ptr.*).XLEN) = inst.getImmediate();
        if (rs1 >= rs2) {
            const pc_set = pc +% @as(Int(.unsigned, @TypeOf(hart_ptr.*).XLEN), @bitCast(imm));
            if (@TypeOf(hart_ptr.*).IALIGN != 16 and pc_set % 4 != 0) {
                return error.InstructionAddressMisaligned;
            } else {
                hart_ptr.setPc(pc_set);
            }
        }
    }
};

inline fn load_execute(hart_ptr: anytype, inst: inst_format.I, comptime width: MemoryValueWidth, comptime signedness: Signedness) mmu.LoadError!void {
    const rs1 = hart_ptr.getXRegister(.signed, inst.rs1);
    const XLEN = @TypeOf(hart_ptr.*).XLEN;
    const Isize = Int(.signed, XLEN);
    // todo why did I put Isize here, run 64 bit and verify if this is needed in any instructions
    const imm: Isize = inst.getImmediate();
    const addr = rs1 +% imm;
    const value: Int(signedness, @intFromEnum(width) * 8) = @bitCast(try hart_ptr.load(width, @bitCast(addr)));
    hart_ptr.setXRegister(inst.rd, @as(Int(signedness, XLEN), value));
}

/// Load Byte (8 bits)
///
/// `rd` = mem[`rs1` +% `imm`]
pub const LB = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.LOAD, funct3.LB };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) mmu.LoadError!void {
        return load_execute(hart_ptr, inst, .byte, .signed);
    }
};

/// Load Halfword (16 bits)
///
/// `rd` = mem[`rs1` +% `imm`]
pub const LH = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.LOAD, funct3.LH };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) mmu.LoadError!void {
        return load_execute(hart_ptr, inst, .halfword, .signed);
    }
};

/// Load Word (32 bits)
///
/// `rd` = mem[`rs1` +% `imm`]
pub const LW = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.LOAD, funct3.LW };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) mmu.LoadError!void {
        return load_execute(hart_ptr, inst, .word, .signed);
    }
};

/// Load Byte Unsigned (8 bits)
///
/// `rd` = mem[`rs1` +% `imm`] // the value returned by mem is unsigned
pub const LBU = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.LOAD, funct3.LBU };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) mmu.LoadError!void {
        return load_execute(hart_ptr, inst, .byte, .unsigned);
    }
};

/// Load Halfword Unsigned (16 bits)
///
/// `rd` = mem[`rs1` +% `imm`] // the value returned by mem is unsigned
pub const LHU = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.LOAD, funct3.LHU };

    pub fn execute(hart_ptr: anytype, inst: inst_format.I) mmu.LoadError!void {
        return load_execute(hart_ptr, inst, .halfword, .unsigned);
    }
};

inline fn store_execute(hart_ptr: anytype, inst: inst_format.S, comptime width: MemoryValueWidth) mmu.StoreError!void {
    const rs1 = hart_ptr.getXRegister(.signed, inst.rs1);
    const Isize = Int(.signed, @TypeOf(hart_ptr.*).XLEN);
    // todo why did I put Isize here
    const imm: Isize = inst.getImmediate();
    const addr = rs1 +% imm;
    const rs2 = hart_ptr.getXRegister(.unsigned, inst.rs2);
    std.debug.print("copying {} (reg {}) to mem[{}(reg {}) +% {}]\n", .{ rs2, inst.rs2, rs1, inst.rs1, imm });
    try hart_ptr.store(width, @bitCast(addr), @as(width.Unsigned(), @truncate(rs2)));
}

/// Store Byte (8 bits)
///
/// mem[`rs1` +% `imm`] = `rs2` // rs2 is unsigned
pub const SB = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.STORE, funct3.SB };

    pub fn execute(hart_ptr: anytype, inst: inst_format.S) mmu.StoreError!void {
        return store_execute(hart_ptr, inst, .byte);
    }
};

/// Store Halfword (16 bits)
///
/// mem[`rs1` +% `imm`] = `rs2` // rs2 is unsigned
pub const SH = struct {
    pub const Ext = .{ 32, ext.I };

    pub const ID = .{ opcode.STORE, funct3.SH };

    pub fn execute(hart_ptr: anytype, inst: inst_format.S) mmu.StoreError!void {
        return store_execute(hart_ptr, inst, .halfword);
    }
};

/// Store Word (32 bits)
///
/// mem[`rs1` +% `imm`] = `rs2` // rs2 is unsigned
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
