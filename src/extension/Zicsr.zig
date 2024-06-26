//! Control and Status registers

comptime {
    const extension = @import("../extension.zig");
    extension.verifyExtension(@This());
    const std = @import("std");
    std.testing.refAllDeclsRecursive(@This());
}

pub const NAME = "Ziscr";
pub const VERSION = "2.0";
pub const STATUS = @import("../extension.zig").SpecificationStatus.ratified;
