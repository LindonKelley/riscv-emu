//! RISC-V extensions, a Word is 32 bits.

const std = @import("std");
const mem = std.mem;
const Data = @import("data.zig").Data;
const instruction = @import("instruction.zig");
const opcode = instruction.opcode;
const mmu = @import("mmu.zig");
const inst_format = @import("inst_format.zig");

// todo each extension file will internally list it's required extensions, and functions otherwise individually requiring extensions will list them

/// Base integer ISA, proceeded by RV32 or RV64 depending on register width
pub const I = @import("extension/I.zig");

/// Instruction fetch fencing
pub const Zifencei = @import("extension/Zifencei.zig");

/// Integer multiplication and division
pub const M = @import("extension/M.zig");

/// Atomic memory operations
pub const A = @import("extension/A.zig");

/// Control and Status registers
pub const Zicsr = @import("extension/Zicsr.zig");

/// Floating point registers and single precision floating point operations
/// The width of the FP registers is determined by FLEN, which other extensions can expand, F sets FLEN=32
pub const F = @import("extension/F.zig");

/// Adds double precision floating point operations, and expands the FP registers: FLEN=64
pub const D = @import("extension/D.zig");

/// Adds quadruple precision floating point operations, and expands the FP registers: FLEN=128
pub const Q = @import("extension/Q.zig");

/// Compressed instructions
pub const C = @import("extension/C.zig");

/// General purpose ISA
pub const G = .{ I, M, A, F, D, Zicsr, Zifencei };

// todo will be a function, has to deal with errors coming from extensions
pub const TickError = mmu.LoadError || mmu.StoreError || instruction.JumpError || @TypeOf(error.IllegalInstruction);

// todo same as above
pub const RequestedTrap = enum {
    ecall,
    ebreak,
};

pub const OpcodeActual = packed struct { _0: u2, op: u5 };

// todo a parameter listing all available extensions
pub fn getOpcodeMap() [32][getLargestOpcodeSpace()]?type {
    const largest_opcode_space = getLargestOpcodeSpace();
    // compiler won't really let me do slice stuff at compile time, so opcode_map will be a rectangle instead
    var opcode_map: [32][largest_opcode_space]?type = .{.{null} ** largest_opcode_space} ** 32;
    var indices = [_]usize{0} ** 32;
    // todo should iterate over all available extensions
    const ExtensionStruct = I;
    for (@typeInfo(ExtensionStruct).Struct.decls) |decl| {
        if (mem.eql(u8, decl.name, "Dependencies")) continue;
        const field = @field(ExtensionStruct, decl.name);
        if (@typeInfo(field) != .Struct) continue;
        // todo does not restrict the Instruction search to available extensions
        const Instruction = field;
        const actual: OpcodeActual = @bitCast(Instruction.Id[0]);
        opcode_map[actual.op][indices[actual.op]] = Instruction;
        indices[actual.op] += 1;
    }
    return opcode_map;
}

// todo a parameter listing all available extensions
pub fn getLargestOpcodeSpace() comptime_int {
    var instructions_using_opcode = [_]u32{0} ** 32;
    // todo should iterate over all available extensions
    const ExtensionStruct = I;
    for (@typeInfo(ExtensionStruct).Struct.decls) |decl| {
        if (mem.eql(u8, decl.name, "Dependencies")) continue;
        const field = @field(I, decl.name);
        if (@typeInfo(field) != .Struct) continue;
        // todo does not restrict the Instruction search to available extensions
        const Instruction = field;
        const actual: OpcodeActual = @bitCast(Instruction.Id[0]);
        instructions_using_opcode[actual.op] += 1;
    }

    var largest_opcode_space = 0;
    for (instructions_using_opcode) |v| {
        largest_opcode_space = @max(largest_opcode_space, v);
    }
    return largest_opcode_space;
}

/// verifies the instructions listed in a file, ensuring they fit the format assumed by other functions in this
/// library
pub fn verifyExtensionInstructions(extension_instructions: anytype) void {
    for (@typeInfo(extension_instructions).Struct.decls) |decl| {
        const field = @field(extension_instructions, decl.name);
        if (mem.eql(u8, decl.name, "Dependencies")) {
            const Dependencies = field;
            if (Dependencies.len == 0) {
                @compileError("Dependencies list must be non-empty: " ++ extension_instructions);
            }
            for (Dependencies, 0..) |dependency, i| {
                const type_info = @typeInfo(@TypeOf(dependency));
                var buf: [64]u8 = undefined;
                const dep_string = try std.fmt.bufPrint(&buf, "Dependencies[{d}] ", .{ i });
                if (type_info != .Pointer or @typeInfo(type_info.Pointer.child).Array.child != u8) {
                    @compileError(dep_string ++ "is not a string, is " ++ @typeName(@TypeOf(dependency)));
                }
                if (dependency.len == 0) {
                    @compileError(dep_string ++ "must be non-empty");
                }
                if (dependency[0] < 'A' or dependency[0] > 'Z') {
                    // not actually part of the spec (the spec actually says the opposite "The ISA naming strings are
                    // case insensitive."), makes reading the extension names a lot easier however
                    @compileError(dep_string ++ "must begin with one of [A-Z]");
                }
            }
        } else {
            if (!@hasDecl(field, "Ext") or !@hasDecl(field, "Id")) {
                @compileError("Instructions must have Ext and Id declared: " ++ decl.name);
            }
            const Instruction = field;
            if (Instruction.Ext.len < 1) {
                @compileError("An Instruction's Ext field must contain a minimum XLEN {32, 64} followed by any " ++
                    "other extensions that are required to implement them: " ++ decl.name);
            }
            if (!isOneOf(Instruction.Ext[0], .{ 32, 64 })) {
                @compileError("An Instruction's first Ext field must start with either 32 or 64: " ++ decl.name);
            }
            if (Instruction.Id.len > 3) {
                @compileError("An Instruction's Id must have at most 3 elements: " ++ decl.name);
            }
        }
    }
}

fn isOneOf(value: anytype, array: anytype) bool {
    for (array) |v| {
        switch (@typeInfo(@TypeOf(value))) {
            .Array, .Pointer => if (std.mem.eql(u8, v, value)) {
                return true;
            },
            else => if (v == value) {
                return true;
            },
        }
    }
    return false;
}
