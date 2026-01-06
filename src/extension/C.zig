//! Compressed instructions

comptime {
    const extension = @import("../extension.zig");
    extension.verifyExtension(@This());
    const std = @import("std");
    std.testing.refAllDeclsRecursive(@This());
}

pub const NAME = "C";
pub const VERSION = "2.0";
pub const STATUS = @import("../extension.zig").SpecificationStatus.ratified;
pub const DEPENDENCIES = .{"I"};
