//! RISC-V instruction helpers

const std = @import("std");
const Int = std.meta.Int;
const inst_format = @import("inst_format.zig");
const mmu = @import("mmu.zig");
const MemoryValueWidth = mmu.MemoryValueWidth;
const Data = @import("data.zig").Data;
const extension = @import("extension.zig");

/// RV32G and RV64G opcodes.
pub const opcode = struct {
    // The opcodes are in order, with comments where there's gaps that are either
    // reserved for future developement of the spec or for custom use.
    // Some reserved opcodes are reserved for RV128 but can be used for
    // custom use extensions in RV32 and RV64.

    pub const LOAD: u7 =        0b00_000_11;
    pub const LOAD_FP: u7 =     0b00_001_11;
    // custom-0
    pub const MISC_MEM: u7 =    0b00_011_11;
    pub const OP_IMM: u7 =      0b00_100_11;
    pub const AUIPC: u7 =       0b00_101_11;
    pub const OP_IMM_32: u7 =   0b00_110_11;
    pub const STORE: u7 =       0b01_000_11;
    pub const STORE_FP: u7 =    0b01_001_11;
    // custom-1
    pub const AMO: u7 =         0b01_011_11;
    pub const OP: u7 =          0b01_100_11;
    pub const LUI: u7 =         0b01_101_11;
    pub const OP_32: u7 =       0b01_110_11;
    pub const MADD: u7 =        0b10_000_11;
    pub const MSUB: u7 =        0b10_001_11;
    pub const NMSUB: u7 =       0b10_010_11;
    pub const NMADD: u7 =       0b10_011_11;
    pub const OP_FP: u7 =       0b10_100_11;
    // reserved
    // custom-2/rv128
    pub const BRANCH: u7 =      0b11_000_11;
    pub const JALR: u7 =        0b11_001_11;
    // reserved
    pub const JAL: u7 =         0b11_011_11;
    pub const SYSTEM: u7 =      0b11_100_11;
    // reserved
    // custom-3/rv128
};

pub const funct3 = struct {
    // RV32I

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

    // RV64I

    pub const ADDIW: u3 =   0b111;

    pub const LWU: u3 =     0b110;
    pub const LD: u3 =      0b011;

    pub const SD: u3 =      0b011;

    // Zifencei

    pub const FENCE_I: u3 = 0b001;

    // RV32M

    pub const MUL: u3 =     0b000;
    pub const MULH: u32 =   0b001;
    pub const MULHSU: u32 = 0b010;
    pub const MULHU: u32 =  0b011;

    pub const DIV: u32 =    0b100;
    pub const DIVU: u32 =   0b101;
    pub const REM: u32 =    0b110;
    pub const REMU: u32 =   0b111;

    // RV64M

    pub const MULW: u3 =    0b000;

    pub const DIVW: u3 =    0b100;
    pub const DIVUW: u3 =   0b101;
    pub const REMW: u3 =    0b110;
    pub const REMUW: u3 =   0b111;
};

pub const funct12 = struct {
    pub const ECALL: u12 =  0b0000_0000_0000;
    pub const EBREAK: u12 = 0b0000_0000_0001;
};

pub fn ContextFunctions(comptime HartContext: type) type {
    return struct {
        pub const XLEN: comptime_int = HartContext.XLEN;

        getXRegister: *const fn(hart_ptr: *HartContext, rs: u5) Data(XLEN),
        setXRegister: *const fn(hart_ptr: *HartContext, rs: u5, v: Data(XLEN)) void,
        getPc: *const fn(self: *HartContext) Int(.unsigned, XLEN),
        setPc: *const fn(self: *HartContext, v: Int(.unsigned, XLEN)) void,
        // need to use a dummy and @TypeOf to get around a compiler bug: https://github.com/ziglang/zig/issues/15409
        load: *const @TypeOf(loadDummy),
        store: *const @TypeOf(storeDummy),
        fence: *const fn(self: *HartContext, fm: u4, pred: FenceOperands, succ: FenceOperands) void,
        ecall: *const fn(self: *HartContext) void,
        ebreak: *const fn(self: *HartContext) void,
        // extra_functions: HartContext.EXTRA_FUNCTIONS_TYPE,
        // fenceI: fn(self: *HartContext, inst: Data(32)) void, todo part of Zifencei

        fn loadDummy(self: *HartContext, comptime width: MemoryValueWidth, address: Int(.unsigned, XLEN)) mmu.LoadError!width.Data() {
            _ = self;
            _ = address;
            return 0;
        }

        fn storeDummy(self: *HartContext, comptime width: MemoryValueWidth, address: Int(.unsigned, XLEN), data: width.Data()) mmu.StoreError!void {
            _ = self;
            _ = address;
            _ = data;
        }
    };
}

pub fn Context(comptime HartContext: type, comptime Functions: ContextFunctions(HartContext)) type {
    return struct {
        pub const XLEN: comptime_int = HartContext.XLEN;
        pub const IALIGN: comptime_int = HartContext.IALIGN;
        pub const ILEN: comptime_int = HartContext.ILEN;
        //pub const EXTS = HartContext.EXTS;
        // todo some way to publically expose the functions from Functions with the convenience API provided by @this, usingnamespace?

        hart_ptr: *HartContext,

        pub fn init(hart_ptr: *HartContext) @This() {
            return .{ .hart_ptr = hart_ptr };
        }

        /// executes the provided instruction, does not increment the program counter, jump/branch instructions
        /// will still change the program counter when appropriate.
        pub fn execute(context: @This(), inst: Data(32)) !void {
            const Masking = packed struct { compressed: u2, op: u5, _: u25 };
            const cir: Masking = @bitCast(inst);
            if (cir.compressed != 0b11) return error.IllegalInstruction;
            const opcode_map = extension.getOpcodeMap();
            inline for (opcode_map, 0..) |Instructions, op| {
                if (cir.op == op) {
                    inline for (Instructions) |Instruction_optional| {
                        match: {
                            if (Instruction_optional == null) break;
                            const Instruction = Instruction_optional.?;
                            const InstructionFormat = @typeInfo(@TypeOf(Instruction.execute)).Fn.params[1].type.?;
                            const functs = InstructionFormat.getFuncts();
                            inline for (functs, 1..) |funct_optional, id_index| {
                                if (funct_optional == null) break;
                                const funct = funct_optional.?;
                                const cir_cast: InstructionFormat = @bitCast(cir);
                                if (Instruction.Id[id_index] != @field(cir_cast, funct.name)) break :match;
                            }
                            //std.debug.print("executing instruction: {s:<20} {b:0>32}\n", .{ @typeName(Instruction), @as(u32, @bitCast(cir))});
                            if (@typeInfo(@TypeOf(Instruction.execute)).Fn.return_type == void) {
                                Instruction.execute(context, @bitCast(cir));
                            } else {
                                try Instruction.execute(context, @bitCast(cir));
                            }
                            return;
                        }
                    }
                }
            }
        }

        pub fn getXRegister(self: @This(), rs: u5) Data(XLEN) {
            return Functions.getXRegister(self.hart_ptr, rs);
        }

        pub fn setXRegister(self: @This(), rs: u5, v: Data(XLEN)) void {
            return Functions.setXRegister(self.hart_ptr, rs, v);
        }

        pub fn getPc(self: @This()) Int(.unsigned, XLEN) {
            return Functions.getPc(self.hart_ptr);
        }

        pub fn setPc(self: @This(), v: Int(.unsigned, XLEN)) void {
            Functions.setPc(self.hart_ptr, v);
        }

        pub fn load(self: @This(), comptime width: MemoryValueWidth, address: Int(.unsigned, XLEN)) mmu.LoadError!width.Data() {
            return Functions.load(self.hart_ptr, width, address);
        }

        pub fn store(self: @This(), comptime width: MemoryValueWidth, address: Int(.unsigned, XLEN), data: width.Data()) mmu.StoreError!void {
            return Functions.store(self.hart_ptr, width, address, data);
        }

        pub fn fence(self: @This(), fm: u4, pred: FenceOperands, succ: FenceOperands) void {
            Functions.fence(self.hart_ptr, fm, pred, succ);
        }

        pub fn ecall(self: @This()) void {
            Functions.ecall(self.hart_ptr);
        }

        pub fn ebreak(self: @This()) void {
            Functions.ebreak(self.hart_ptr);
        }

        // the instruction is passed in as a u32 to allow forward compatibility with
        // future extensions that expand upon FENCE.I, currently the value of inst
        // isn't important and can be ignored
        //pub fn fenceI(self: *@This(), inst: u32) void {
        //    self.fenceI(self.hart_ptr, inst);
        //}
    };
}

pub const JumpError = error {
    InstructionAddressMisaligned
};

pub fn hart_arch(comptime HartContext: type) type {
    return struct {
        pub const XLEN = HartContext.XLEN;
        pub const @"usize" = Int(.unsigned, XLEN);
        pub const @"isize" = Int(.signed, XLEN);
        pub const IALIGN = HartContext.IALIGN;
        pub const ILEN = HartContext.ILEN;
    };
}

pub const FenceOperands = packed struct {
    writes: bool,
    reads: bool,
    outputs: bool,
    inputs: bool
};
