//! Adds double precision floating point operations, and expands the FP registers: FLEN=64

pub const Dependencies = .{ "F" };

comptime {
    const extension = @import("../extension.zig");
    extension.verifyExtensionInstructions(@This());
    const std = @import("std");
    std.testing.refAllDeclsRecursive(@This());
}
