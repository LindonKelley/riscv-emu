//! Adds quadruple precision floating point operations, and expands the FP registers: FLEN=128

pub const Dependencies = .{ "D" };

comptime {
    const extension = @import("../extension.zig");
    extension.verifyExtensionInstructions(@This());
    const std = @import("std");
    std.testing.refAllDeclsRecursive(@This());
}
