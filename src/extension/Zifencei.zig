//! Instruction fence

const instruction = @import("../instruction.zig");
const opcode = instruction.opcode;
const funct3 = instruction.funct3;
const inst_format = @import("../inst_format.zig");

comptime {
    const extension = @import("../extension.zig");
    extension.verifyExtensionInstructions(@This());
    const std = @import("std");
    std.testing.refAllDeclsRecursive(@This());
}

/// Instruction Fence
pub const FENCE_I = struct {
    pub const Ext = .{ 32 };

    pub const Id = .{ opcode.MISC_MEM, funct3.FENCE_I };

    pub fn execute(context: anytype, inst: inst_format.I) void {
        context.fence_i(@as(u32, @bitCast(inst)));
    }
};
