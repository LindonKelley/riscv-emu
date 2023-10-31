//! Control and Status registers

comptime {
    const extension = @import("../extension.zig");
    extension.verifyExtensionInstructions(@This());
    const std = @import("std");
    std.testing.refAllDeclsRecursive(@This());
}
