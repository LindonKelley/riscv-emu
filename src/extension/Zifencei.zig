//! Instruction fence

const instruction = @import("../instruction.zig");
const opcode = instruction.opcode;
const funct3 = instruction.funct3;
const inst_format = @import("../inst_format.zig");
const Data = @import("../data.zig").Data;


comptime {
    const extension = @import("../extension.zig");
    extension.verifyExtension(@This());
    const std = @import("std");
    std.testing.refAllDeclsRecursive(@This());
}

pub const NAME = "Zifencei";
pub const VERSION = "2.0";
pub const STATUS = @import("../extension.zig").SpecificationStatus.ratified;
// todo this will be changed once I figure out how I want to go about it
pub fn Requirements(comptime HartContext: type) type {
    return struct { fenceI: *const fn (self: *HartContext, inst: Data(32)) void };
}

pub const INSTRUCTIONS = struct {
    /// Instruction Fence
    pub const FENCE_I = struct {
        pub const EXT = .{32};

        pub const ID = .{ opcode.MISC_MEM, funct3.FENCE_I };

        pub fn execute(context: anytype, inst: inst_format.I) void {
            context.fenceI(@as(u32, @bitCast(inst)));
        }
    };
};
