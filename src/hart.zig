const std = @import("std");
const instruction = @import("instruction.zig");
const inst_format = @import("inst_format.zig");
const opcode = @import("opcode.zig");
const FenceOperands = instruction.FenceOperands;
const Signedness = std.builtin.Signedness;
const Int = std.meta.Int;
const mmu = @import("mmu.zig");
const MemoryValueWidth = mmu.MemoryValueWidth;
const Struct = std.builtin.Type.Struct;
const Data = @import("data.zig").Data;
const register = @import("register");

/// The interface for a RISC-V Hart, or hardware thread, this is to be followed by anyone wishing to implement their
///  own Hart implementation and is used for `verifyHartImplementation`
/// `P_XLEN` is the Register and Program Counter width, and is the limiting factor on how much memory the
///  Hart can immediately address
/// `P_ILEN` is the maximum instruction length, always a multiple of IALIGN and is 32 in the default ISA
pub fn Hart(comptime Impl: type) type {
    //const P_IALIGN = if (@hasDecl(Extensions, "C")) 16 else 32;
    comptime {
        if (Impl.XLEN != 32 and Impl.XLEN != 64) {
            var buf: [32]u8 = undefined;
            const XLEN_string = try std.fmt.bufPrint(&buf, "{d}", .{ Impl.XLEN });
            @compileError("Hart XLEN must be either 32 or 64, was " ++ XLEN_string);
        }
        if (Impl.ILEN % Impl.IALIGN != 0) {
            var buf: [32]u8 = undefined;
            const ILEN_string = try std.fmt.bufPrint(&buf, "{d}", .{ Impl.ILEN });
            @compileError("Hart ILEN (" ++ ILEN_string ++ ") must be a multiple of IALIGN ( " ++ Impl.IALIGN ++ ")");
        }
    }

    return struct {
        /// Register and Program Counter width, also the limiting factor on
        /// how much memory the Hart can immediately address.
        pub const XLEN: comptime_int = Impl.XLEN;
        /// Instruction alignment must be 32 or is relaxed to 16 in the presence of some extensions.
        pub const IALIGN: comptime_int = Impl.IALIGN;
        /// Maximum instruction length, always a multiple of IALIGN and is 32 in the default ISA.
        pub const ILEN: comptime_int = Impl.ILEN;

        pub const tick = fn(self: *Impl) TickError!?RequestedTrap;

        pub const getXRegister = fn(self: Impl, rs: u5) Data(XLEN);

        pub const setXRegister = fn(self: *Impl, rs: u5, v: Data(XLEN)) void;

        pub const getPc = fn(self: Impl) Int(.unsigned, XLEN);

        pub const setPc = fn(self: *Impl, v: Int(.unsigned, XLEN)) void;

        // need to use a dummy and @TypeOf to get around a compiler bug: https://github.com/ziglang/zig/issues/15409
        pub const load = @TypeOf(loadDummy);

        fn loadDummy(self: *Impl, comptime width: MemoryValueWidth, address: Int(.unsigned, XLEN)) mmu.LoadError!width.Data() {
            _ = self;
            _ = address;
            return 0;
        }

        // need to use a dummy and @TypeOf to get around a compiler bug: https://github.com/ziglang/zig/issues/15409
        pub const store = @TypeOf(storeDummy);

        fn storeDummy(self: *Impl, comptime width: MemoryValueWidth, address: Int(.unsigned, XLEN), data: width.Data()) mmu.StoreError!void {
            _ = self;
            _ = address;
            _ = data;
        }
    };
}

// todo will be a function, has to deal with errors coming from extensions
pub const TickError = mmu.LoadError || mmu.StoreError || instruction.JumpError || @TypeOf(error.IllegalInstruction);

pub fn verifyHartImplementation(comptime Impl: type) void {
    const HartInterface = Hart(Impl);

    inline for (@typeInfo(HartInterface).Struct.decls) |decl| {
        const field = @field(HartInterface, decl.name);
        if (@TypeOf(field) == type) {
            verifyHartImplDeclType(Impl, decl.name, field);
        }
    }
}

fn verifyHartImplDeclType(comptime Impl: type, comptime name: []const u8, comptime ExpectedType: type) void {
    if (@hasDecl(Impl, name)) {
        const ActualType = @TypeOf(@field(Impl, name));
        if (ActualType == ExpectedType) {
            return;
        } else {
            const hart_impl_prefix = "Hart implementation '" ++ @typeName(Impl) ++ "." ++ name ++ "' ";
            const expected_type_info = @typeInfo(ExpectedType);
            const actual_type_info = @typeInfo(ActualType);
            if (@intFromEnum(actual_type_info) == @intFromEnum(expected_type_info)) {
                switch (expected_type_info) {
                    .Fn => |expected_fn| {
                        const actual_fn = actual_type_info.Fn;
                        // ignoring everything but params and return type, not sure if calling_convetion and alignment
                        //  differing would even cause a problem
                        var param_problem = false;
                        if (actual_fn.params.len != expected_fn.params.len) {
                            param_problem = true;
                        } else {
                            for (actual_fn.params, expected_fn.params) |actual_param, expected_param| {
                                if (actual_param.type != expected_param.type) {
                                    param_problem = true;
                                    break;
                                }
                            }
                        }
                        if (param_problem) {
                            // readability here sucks, but I'd rather leave this in it's still useful state and get
                            //  going with other parts of this project and maybe in the future someone else will
                            //  make a library to do this sort of interface verification and helpful compile logging
                            @compileLog(hart_impl_prefix ++ "has incorrect parameter types, should be '",
                             expected_fn.params, "' but is actually '", actual_fn.params, "'");
                        }
                        if (actual_fn.return_type != expected_fn.return_type) {
                            //const ActualReturn = actual_fn.return_type orelse void;
                            const ExpectedReturn = expected_fn.return_type orelse void;
                            @compileLog(hart_impl_prefix ++ "has an incorrect return type, should be '"
                             ++ @typeName(ExpectedReturn) ++ "'");
                            // potential compiler crash if the below is used, upon very little inspection, I believe it
                            // is caused by passing something with the inferred error set of a function into @typeName
                            //@compileLog(hart_impl_prefix ++ "has an incorrect return type, should be '"
                            // ++ @typeName(ExpectedReturn) ++ "' was '" ++ @typeName(ActualReturn) ++ "'");
                        }
                    },
                    else => @compileLog("Hart interface has unexpected type '" ++ @typeName(ExpectedType)
                     ++ "', this likely indicates a bug with `verifyHartImplDeclType`")
                }
            } else {
                @compileLog(hart_impl_prefix ++ "must have type '" ++ @typeName(ExpectedType)
                 ++ "' but actually has type '" ++ @typeName(ActualType));
            }
        }
    } else {
        @compileLog("Hart implementation '" ++ @typeName(Impl) ++ "' missing declaration '" ++ name ++ "' of type '"
         ++ @typeName(ExpectedType) ++ "'");
    }
}

fn verifyHartImplDeclTypeTest(comptime Impl: type, comptime name: []const u8, comptime T: type) void {
    if (@hasDecl(Impl, name)) {
        const ImplFieldType = @TypeOf(@field(Impl, name));
        const impl_field_type_name = @typeName(ImplFieldType);
        if (ImplFieldType == T) {
            return;
        } else {
            @compileLog(impl_field_type_name);
            //const wrongTypeName = @typeName(WrongType);
            @compileLog("Hart implementation '" ++ @typeName(Impl) ++ "." ++ name ++ "' should have type '" ++ @typeName(T)
             ++ "' but actually has type '" ++ impl_field_type_name);
        }
    } else {
        @compileLog("Hart implementation '" ++ @typeName(Impl) ++ "' missing declaration '" ++ name ++ "' of type '"
         ++ @typeName(T) ++ "'");
    }
}

comptime {
    // todo verifyHartImplementation(SimpleHart(32, mmu.BasicMmu(32)));
}

// P_XLEN would just be XLEN if the shadowing rules allowed it
pub fn SimpleHart(comptime P_XLEN: comptime_int, comptime Mmu: type) type {
    comptime {
        if (P_XLEN != 32 and P_XLEN != 64) {
            var buf: [32]u8 = undefined;
            const XLEN_string = try std.fmt.bufPrint(&buf, "{d}", .{P_XLEN});
            @compileError("Hart XLEN must be either 32 or 64, was " ++ XLEN_string);
        }
    }

    // Word is 32 bits
    return struct {
        /// Register and Program Counter width, also the limiting factor on
        /// how much memory the Hart can immediately address
        pub const XLEN = P_XLEN;
        /// Instruction alignment must be 32 or is relaxed to 16 in the presence of some extensions
        pub const IALIGN = 32;
        /// Maximum instruction length, always a multiple of IALIGN and is 32 in the default ISA
        /// (only 16 and 32 bit encodings are currently frozen, so 32 is really the only option at this time.)
        pub const ILEN = 32;

        /// Program Counter
        pc: Int(.unsigned, XLEN) = 0,
        // access has 1 subtracted from it it, register 0 is hardwired as 0
        x_registers: [31]Data(XLEN) = [_]Data(XLEN){.{ .unsigned = 0 }} ** 31,
        pc_set: bool = false,
        ecall_set: bool = false,
        ebreak_set: bool = false,
        mmu: Mmu,

        /// Completes one instruction cycle, errors returned are intended to be treated as either
        ///  invisible or fatal traps. This function assumes that requested traps only come from within the execution
        ///  environment, any calls to Hart.ecall and Hart.ebreak outside of executing instructions have no guaranteed
        ///  effects.
        pub fn tick(self: *@This()) TickError!?RequestedTrap {
            // current instruction register
            // for when something sets these outside of instructions
            self.pc_set = false;
            self.ebreak_set = false;
            self.ecall_set = false;
            // todo not sure about the try, in terms of if that's the correct error for a hart to return on a failed fetch
            const cir: Masking = @bitCast(try self.load(.word, self.pc));
            try self.tick_fast(cir);
            if (self.ecall_set) return .ecall;
            if (self.ebreak_set) return .ebreak;
            return null;
        }

        inline fn tick_slow(self: *@This(), cir: Masking) !void {
            if (cir.compressed != 0b11) return error.IllegalInstruction;
            inline for (@typeInfo(instruction).Struct.decls) |decl| {
                const field = @field(instruction, decl.name);
                if (@typeInfo(field) != .Struct) continue;
                if (@hasDecl(field, "Ext") and @hasDecl(field, "ID")) {
                    // todo does not restrict the Instruction search to available extensions
                    const Instruction = field;
                    const actual: OpcodeActual = @bitCast(Instruction.ID[0]);
                    match: {
                        if (actual.op != cir.op) break :match;
                        if (Instruction.ID.len > 1 and Instruction.ID[1] != cir.funct3) break :match;
                        if (cir.op == @as(OpcodeActual, @bitCast(opcode.SYSTEM)).op) {
                            // todo something around here is wrong, this function is currently broken
                            if (Instruction.ID[1] == instruction.funct3.PRIV) {
                                @compileLog(@typeName(Instruction), Instruction.ID, actual.op, @as(OpcodeActual, @bitCast(opcode.SYSTEM)).op);
                            }
                            if (Instruction.ID[1] == instruction.funct3.PRIV and Instruction.ID[2] != @as(inst_format.ENV, @bitCast(cir)).funct12) {
                                break :match;
                            }
                        } else {
                            if (Instruction.ID.len > 2 and Instruction.ID[2] != cir.funct7) break :match;
                        }
                        if (@typeInfo(@TypeOf(Instruction.execute)).Fn.return_type == void) {
                            Instruction.execute(self, @bitCast(cir));
                        } else {
                            try Instruction.execute(self, @bitCast(cir));
                        }
                        if (!self.pc_set) {
                            self.pc += 4;
                        } else {
                            self.pc_set = false;
                        }
                        return;
                    }
                }
            }
        }

        inline fn tick_fast(self: *@This(), cir: Masking) !void {
            if (cir.compressed != 0b11) return error.IllegalInstruction;
            inline for (opcode_map, 0..) |Instructions, op| {
                if (cir.op == op) {
                    instructions_loop: inline for (Instructions) |Instruction_optional| {
                        match: {
                            // compiler tracks undefined so should be safe, right?
                            if (Instruction_optional == null) break :instructions_loop;
                            const Instruction = Instruction_optional.?;
                            if (Instruction.ID.len > 1 and Instruction.ID[1] != cir.funct3) break :match;
                            if (op == @as(OpcodeActual, @bitCast(opcode.SYSTEM)).op) {
                                if (Instruction.ID[1] == instruction.funct3.PRIV and Instruction.ID[2] != @as(inst_format.ENV, @bitCast(cir)).funct12) {
                                    break :match;
                                }
                            } else {
                                if (Instruction.ID.len > 2 and Instruction.ID[2] != cir.funct7) break :match;
                            }
                            //std.debug.print("executing instruction: {s} {b:0>32}\n", .{ @typeName(Instruction), @as(u32, @bitCast(cir))});
                            if (@typeInfo(@TypeOf(Instruction.execute)).Fn.return_type == void) {
                                Instruction.execute(self, @bitCast(cir));
                            } else {
                                try Instruction.execute(self, @bitCast(cir));
                            }
                            if (!self.pc_set) {
                                self.pc += 4;
                            } else {
                                self.pc_set = false;
                            }
                            return;
                        }
                    }
                }
            }
        }

        const opcode_map = blk: {
            // compiler won't really let me do slice stuff at compile time, so opcode_map will be a rectangle instead
            var opcode_map_var: [32][largest_opcode_space]?type = .{.{null} ** largest_opcode_space} ** 32;
            var indices = [_]usize{0} ** 32;
            for (@typeInfo(instruction).Struct.decls) |decl| {
                const field = @field(instruction, decl.name);
                if (@typeInfo(field) != .Struct) continue;
                if (@hasDecl(field, "Ext") and @hasDecl(field, "ID")) {
                    // todo does not restrict the Instruction search to available extensions
                    const Instruction = field;
                    const actual: OpcodeActual = @bitCast(Instruction.ID[0]);
                    opcode_map_var[actual.op][indices[actual.op]] = Instruction;
                    indices[actual.op] += 1;
                }
            }
            break :blk opcode_map_var;
        };

        const largest_opcode_space = blk: {
            var instructions_using_opcode = [_]u32{0} ** 32;
            for (@typeInfo(instruction).Struct.decls) |decl| {
                const field = @field(instruction, decl.name);
                if (@typeInfo(field) != .Struct) continue;
                if (@hasDecl(field, "Ext") and @hasDecl(field, "ID")) {
                    // todo does not restrict the Instruction search to available extensions
                    const Instruction = field;
                    const actual: OpcodeActual = @bitCast(Instruction.ID[0]);
                    instructions_using_opcode[actual.op] += 1;
                }
            }

            var largest_opcode_space_var = 0;
            for (instructions_using_opcode) |v| {
                largest_opcode_space_var = @max(largest_opcode_space_var, v);
            }
            break :blk largest_opcode_space_var;
        };

        // inline candidate
        pub fn getXRegister(self: *@This(), rs: u5) Data(XLEN) {
            if (rs == 0) {
                return .{ .unsigned = 0 };
            } else {
                return self.x_registers[rs - 1];
            }
        }

        // inline candidate
        pub fn setXRegister(self: *@This(), rd: u5, v: Data(XLEN)) void {
            if (rd != 0) {
                self.x_registers[rd - 1] = v;
            }
        }

        // must be implemented such that an instruction can call this to get it's own location,
        // in an implementation with a pipeline, this may be more complicated than it is here,
        // the value returned must be aligned by IALIGN
        pub fn getPc(self: *@This()) Int(.unsigned, XLEN) {
            return self.pc;
        }

        // callers guarrentee that v is properly aligned
        pub fn setPc(self: *@This(), v: Int(.unsigned, XLEN)) void {
            self.pc_set = true;
            self.pc = v;
        }

        pub fn load(self: *@This(), comptime width: MemoryValueWidth, address: Int(.unsigned, XLEN)) mmu.LoadError!width.Data() {
            return self.mmu.load(width, address);
        }

        pub fn store(self: *@This(), comptime width: MemoryValueWidth, address: Int(.unsigned, XLEN), data: width.Data()) mmu.StoreError!void {
            return self.mmu.store(width, address, data);
        }

        pub fn fence(self: *@This(), fm: u4, pred: FenceOperands, succ: FenceOperands) void {
            self.mmu.fence(fm, pred, succ);
        }

        // the instruction is passed in as a u32 to allow forward compatibility with
        // future extensions that expand upon FENCE.I, currently the value of inst
        // isn't important and can be ignored
        pub fn fence_i(self: *@This(), inst: u32) void {
            // intentional no-op on this simple implementation
            _ = self;
            _ = inst;
        }

        pub fn ecall(self: *@This()) void {
            self.ecall_set = true;
        }

        pub fn ebreak(self: *@This()) void {
            self.ebreak_set = true;
        }
    };
}

pub const RequestedTrap = enum {
    ecall,
    ebreak,
};

const Masking = packed struct { compressed: u2, op: u5, _1: u5, funct3: u3, _2: u10, funct7: u7 };

const OpcodeActual = packed struct { _0: u2, op: u5 };
