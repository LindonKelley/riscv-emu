/// Simple Hart implementation, reference implementation with extendability and readability as primary goals
pub const std = @import("std");
pub const Simple = @import("hart_impls/simple.zig").Hart;

// todo dependency checking
// todo allow overrides, such that E can override I, but externally the Hart has both (other than in Zicsr if those have to be shown differently)
/// Recieves a tuple of extensions and returns a type that acts as a map of the extensions, using their typenames as keys
/// ```
/// const extension_set = ExtensionSet(.{extension.I, extension.M});
/// _ = extension_set.I;
/// ```
pub fn ExtensionSet(comptime extensions: anytype) type {
    const Type = std.builtin.Type;
    var fields = [_]Type.StructField { undefined } ** extensions.len;
    for (extensions, &fields) |ext, *field| {
        // todo replace with extention.Name
        var splitter = std.mem.splitBackwardsScalar(u8, @typeName(ext), '.');
        field.* = .{
            .name = splitter.next().?,
            .type = type,
            .default_value = @alignCast(@ptrCast(&ext)),
            .is_comptime = true,
            .alignment = @alignOf(type)
        };
    }
    const Exts = .{ .Struct = .{
        .layout = .Auto,
        .backing_integer = null,
        .fields = &fields,
        .decls = &[_]Type.Declaration {},
        .is_tuple = false
    }};
    return @Type(Exts);
}

// todo a verification function that ensures the required functions for a given set of extensions are present
//  receives a hart_impl and ExtensionSet, returns true if the hart implementation contains all the necessary functions
//  todo this should really be on the Context made by the hart_impl in question
//  todo alternatively, check hart_impl.EXTS like Context will, and perhaps add a function into hart implementations that executes a provided function that takes the Context as a parameter
