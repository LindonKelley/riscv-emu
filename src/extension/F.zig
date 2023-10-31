//! Floating point registers and single precision floating point operations
//! The width of the FP registers is determined by FLEN, which other extensions can expand, F sets FLEN=32

pub const Dependencies = .{ "Zicsr" };

comptime {
    const extension = @import("../extension.zig");
    extension.verifyExtensionInstructions(@This());
    const std = @import("std");
    std.testing.refAllDeclsRecursive(@This());
}
