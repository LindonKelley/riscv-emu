const std = @import("std");
const inst_format = @import("../inst_format.zig");
const instruction = @import("../instruction.zig");
const opcode = instruction.opcode;
const FenceOperands = instruction.FenceOperands;
const Signedness = std.builtin.Signedness;
const Int = std.meta.Int;
const mmu = @import("../mmu.zig");
const MemoryValueWidth = mmu.MemoryValueWidth;
const Struct = std.builtin.Type.Struct;
const Data = @import("../data.zig").Data;
const extension = @import("../extension.zig");
const TickError = extension.TickError;
const RequestedTrap = extension.RequestedTrap;

/// A simple implementation of a RISC-V Hart, or hardware thread
/// `P_XLEN` is the Register and Program Counter width, and is the limiting factor on how much memory the
///  Hart can immediately address
// P_XLEN would just be XLEN if the shadowing rules allowed it
// todo needs to receive potential implementation overrides for functions from extensions
// todo above unlikely, instead Hart implementations don't have their own ticking function, and just expose all functions needed to be called by any external one
// todo needs function that exposes which extensions are supported, and likely also their versions
// todo above may not be necessary if the exposed functions that are needed to implement an extension are standard across all hart impls
pub fn Hart(comptime P_XLEN: comptime_int, comptime Mmu: type) type {
    comptime {
        if (P_XLEN != 32 and P_XLEN != 64) {
            var buf: [32]u8 = undefined;
            const XLEN_string = try std.fmt.bufPrint(&buf, "{d}", .{ P_XLEN });
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
        /// Extensions available for use
        // pub const EXTS = ExtensionSet(.{ extension.I, extension.M });
        // pub const EXTRA_FUNCTIONS_TYPE = getExtraFunctions(EXTS);

        /// Program Counter
        pc: Int(.unsigned, XLEN) = 0,
        // access has 1 subtracted from it it, register 0 is hardwired as 0
        x_registers: [31]Data(XLEN) = [_]Data(XLEN){.{ .unsigned = 0 }} ** 31,
        mmu: Mmu,
        /// Extra fields from other extensions
        // extra_fields = getExtraFields(EXTS),

        /// Completes one instruction cycle, errors returned are intended to be treated as either
        ///  invisible or fatal traps.
        pub fn tick(self: *@This()) TickError!?RequestedTrap {
            // todo not sure about the try, in terms of if that's the correct error for a hart to return on a failed fetch
            const inst = try self.load(.word, self.pc);
            const ThisHart = *@This();
            const pc_state = struct {
                var called = false;

                fn call(hart: ThisHart, v: Int(.unsigned, XLEN)) void {
                    called = true;
                    hart.pc = v;
                }
            };
            const ecall_state = struct {
                var called = false;

                fn set(_: ThisHart) void {
                    called = true;
                }
            };
            const ebreak_state = struct {
                var called = false;

                fn set(_: ThisHart) void {
                    called = true;
                }
            };

            const Functions = .{
                .getXRegister = getXRegister,
                .setXRegister = setXRegister,
                .getPc = getPc,
                .setPc = pc_state.call,
                .load = load,
                .store = store,
                .fence = fence,
                .ecall = ecall_state.set,
                .ebreak = ebreak_state.set,
            };
            var context = instruction.Context(@This(), Functions).init(self);
            try context.execute(inst);
            if (!pc_state.called) {
                // todo compressed instructions
                self.pc += 4;
            } else {
                pc_state.called = false;
            }
            if (ecall_state.called) {
                ecall_state.called = false;
                return .ecall;
            }
            if (ebreak_state.called) {
                ebreak_state.called = false;
                return .ebreak;
            }
            return null;
        }

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
    };
}

fn getExtraFunctions(comptime extension_set: anytype) type {
    _ = extension_set;
    // todo returns a struct of extension functions (like ifence)
    return void;
}

// todo ifence function implementation within a struct, the struct will be directly usable by the function above
