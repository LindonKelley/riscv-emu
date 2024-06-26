//! Adds quadruple precision floating point operations, and expands the FP registers: FLEN=128

comptime {
    const extension = @import("../extension.zig");
    extension.verifyExtension(@This());
    const std = @import("std");
    std.testing.refAllDeclsRecursive(@This());
}

pub const NAME = "Q";
pub const VERSION = "2.2";
pub const STATUS = @import("../extension.zig").SpecificationStatus.ratified;
pub const DEPENDENCIES = .{"D"};
