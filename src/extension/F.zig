//! Floating point registers and single precision floating point operations
//! The width of the FP registers is determined by FLEN, which other extensions can expand, F sets FLEN=32


comptime {
    const extension = @import("../extension.zig");
    extension.verifyExtension(@This());
    const std = @import("std");
    std.testing.refAllDeclsRecursive(@This());
}

pub const NAME = "F";
pub const VERSION = "2.2";
pub const STATUS = @import("../extension.zig").SpecificationStatus.ratified;
pub const DEPENDENCIES = .{"Zicsr"};
