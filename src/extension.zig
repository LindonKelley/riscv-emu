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

pub const SpecificationStatus = enum { draft, frozen, ratified };

pub const OpcodeActual = packed struct { _0: u2, op: u5 };

// todo a parameter listing all available extensions
// todo move this function to ExtensionSet, as well as the other three functions in this file that need information from extensions
pub fn getOpcodeMap() [32][getLargestOpcodeSpace()]?type {
    // todo can be updated to match recursively over functs, for some potential speed gain
    const largest_opcode_space = getLargestOpcodeSpace();
    // compiler won't really let me do slice stuff at compile time, so opcode_map will be a rectangle instead
    var opcode_map: [32][largest_opcode_space]?type = .{.{null} ** largest_opcode_space} ** 32;
    var indices = [_]usize{0} ** 32;
    // todo should iterate over all available extensions
    const ExtensionStruct = I;
    if (@hasDecl(ExtensionStruct, "INSTRUCTIONS")) {
        for (@typeInfo(ExtensionStruct.INSTRUCTIONS).@"struct".decls) |inst_decl| {
            const Instruction = @field(ExtensionStruct.INSTRUCTIONS, inst_decl.name);
            const actual: OpcodeActual = @bitCast(Instruction.ID[0]);
            opcode_map[actual.op][indices[actual.op]] = Instruction;
            indices[actual.op] += 1;
        }
    }
    return opcode_map;
}

// todo a parameter listing all available extensions
pub fn getLargestOpcodeSpace() comptime_int {
    var instructions_using_opcode = [_]u32{0} ** 32;
    // todo should iterate over all available extensions
    const ExtensionStruct = I;
    if (@hasDecl(ExtensionStruct, "INSTRUCTIONS")) {
        for (@typeInfo(ExtensionStruct.INSTRUCTIONS).@"struct".decls) |inst_decl| {
            const Instruction = @field(ExtensionStruct.INSTRUCTIONS, inst_decl.name);
            const actual: OpcodeActual = @bitCast(Instruction.ID[0]);
            instructions_using_opcode[actual.op] += 1;
        }
    }

    var largest_opcode_space = 0;
    for (instructions_using_opcode) |v| {
        largest_opcode_space = @max(largest_opcode_space, v);
    }
    return largest_opcode_space;
}

/// verifies the extension in a file, ensuring they fit the format assumed by other functions in this
/// library
pub fn verifyExtension(extension: anytype) void {
    if (!@hasDecl(extension, "NAME")) {
        @compileError("extensions must have NAME declared");
    }
    if (@typeInfo(@TypeOf(extension.NAME)) != .pointer or @typeInfo(@typeInfo(@TypeOf(extension.NAME)).pointer.child).array.child != u8) {
        @compileError("NAME must be a string");
    }
    if (extension.NAME.len < 1) {
        @compileError("NAME must be non-empty");
    }
    if (extension.NAME[0] < 'A' or extension.NAME[0] > 'Z') {
        // not actually part of the spec (the spec actually says the opposite "The ISA naming strings are
        // case insensitive."), makes reading the extension names a lot easier however
        @compileError("NAME must begin with one of [A-Z]");
    }

    if (@hasDecl(extension, "DEPENDENCIES")) {
        const DEPENDENCIES = extension.DEPENDENCIES;
        if (DEPENDENCIES.len == 0) {
            @compileError("DEPENDENCIES list must be non-empty");
        }
        for (DEPENDENCIES, 0..) |dependency, i| {
            const type_info = @typeInfo(@TypeOf(dependency));
            var buf: [64]u8 = undefined;
            const dep_string = try std.fmt.bufPrint(&buf, "DEPENDENCIES[{d}] ", .{i});
            if (type_info != .Pointer or @typeInfo(type_info.Pointer.child).Array.child != u8) {
                @compileError(dep_string ++ "must be a string, is " ++ @typeName(@TypeOf(dependency)));
            }
        }
    }

    if (@hasDecl(extension, "INSTRUCTIONS")) {
        for (@typeInfo(extension.INSTRUCTIONS).@"struct".decls) |inst_decl| {
            const Instruction = @field(extension.INSTRUCTIONS, inst_decl.name);
            const inst_name = @typeName(Instruction);
            if (!@hasDecl(Instruction, "EXT") or !@hasDecl(Instruction, "ID")) {
                @compileError("INSTRUCTIONS must have EXT and ID declared: " ++ inst_name);
            }
            if (Instruction.EXT.len < 1) {
                @compileError("An Instruction's EXT field must contain a minimum XLEN {32, 64} followed by any " ++
                    "other extensions that are required to implement them: " ++ inst_name);
            }
            if (!isOneOf(Instruction.EXT[0], .{ 32, 64 })) {
                @compileError("An Instruction's first EXT field must start with either 32 or 64: " ++ inst_name);
            }
            if (!@hasDecl(Instruction, "execute")) {
                @compileError("All INSTRUCTIONS must have an `execute` function: " ++ inst_name);
            }
            const instruction_execute_info = @typeInfo(@TypeOf(Instruction.execute));
            if (instruction_execute_info != .@"fn") {
                @compileError("Instruction has declaration `execute` that is not a function: " ++ inst_name);
            }
            const instruction_execute = instruction_execute_info.@"fn";
            if (instruction_execute.params.len != 2) {
                @compileError("Instruction.execute must have exactly two parameters: " ++ inst_name);
            }
            if (!instruction_execute.params[0].is_generic) {
                @compileError("Instruction.execute's first parameter type must be `anytype`: " ++ inst_name);
            }
            if (instruction_execute.params[1].is_generic) {
                @compileError("Instruction.execute's second parameter type must not be generic: " ++ inst_name);
            }
            if (instruction_execute.params[1].type == null) {
                // I believe this can only be true if the parameter is generic, but there's no documentation there so
                // I'll err on the side of caution
                // also if that's the only time Param.type can be generic, I wonder why Zig didn't use a tagged union
                @compileError("Instruction.execute's second parameter type must not be null: " ++ inst_name);
            }
            const InstructionFormat = @typeInfo(@TypeOf(Instruction.execute)).@"fn".params[1].type.?;
            if (!@hasDecl(InstructionFormat, "Functs")) {
                @compileError("Instruction.execute's second parameter must be a valid instruction format, it's " ++
                    "missing the `Functs` namespace, see `inst_format.zig` for examples: " ++ inst_name);
            }
            if (!@hasDecl(InstructionFormat.Functs, "get")) {
                @compileError("Instruction.execute's second parameter must be a valid instruction format, it's " ++
                    "missing the `Functs` namespace is missing a `get` function, see `inst_format.zig` for examples: " ++ inst_name);
            }
            const functs = InstructionFormat.Functs.get();
            var functs_len = 0;
            for (functs) |funct_optional| {
                if (funct_optional == null) break;
                functs_len += 1;
            }
            if (functs_len != Instruction.ID.len - 1) {
                @compileError("InstructionFormat (as specified by Instruction.execute's second parameter) does not " ++
                    "match Instruction.ID. Instruction.ID should have an opcode, followed by the values specified by " ++
                    "the instruction corresponding to it's format, see `extension/I.zig` for examples: " ++ inst_name);
            }
            for (functs, 1..) |funct_optional, id_index| {
                if (funct_optional == null) break;
                const funct = funct_optional.?;
                @setEvalBranchQuota(2000);
                const cast = std.math.cast(funct.type, Instruction.ID[id_index]);
                if (cast == null) {
                    var buf: [32]u8 = undefined;
                    const id_index_string = try std.fmt.bufPrint(&buf, "{d}", .{id_index});
                    @compileError("Instruction.ID[" ++ id_index_string ++ "] does not fit inside corresponding " ++
                        "InstructionFormat funct type (" ++ @typeName(funct.type) ++ "): " ++ inst_name);
                }
            }
            const instruction_execute_return_type_info = @typeInfo(instruction_execute.return_type.?);
            if (instruction_execute_return_type_info != .void) {
                if (instruction_execute_return_type_info != .error_union) {
                    @compileError("Instruction.execute must return either `void` or `!void`: " ++ inst_name);
                } else if (instruction_execute_return_type_info.error_union.payload != void) {
                    @compileError("Instruction.execute must return `void` as the error union payload: " ++ inst_name);
                }
            }
        }
    }
}

fn isOneOf(value: anytype, array: anytype) bool {
    for (array) |v| {
        switch (@typeInfo(@TypeOf(value))) {
            .array, .pointer => if (std.mem.eql(u8, v, value)) {
                return true;
            },
            else => if (v == value) {
                return true;
            },
        }
    }
    return false;
}
