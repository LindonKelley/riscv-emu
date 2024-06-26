//! Atomic memory operations

comptime {
    const extension = @import("../extension.zig");
    extension.verifyExtension(@This());
    const std = @import("std");
    std.testing.refAllDeclsRecursive(@This());
}

pub const NAME = "A";
pub const VERSION = "2.1";
pub const STATUS = @import("../extension.zig").SpecificationStatus.ratified;
