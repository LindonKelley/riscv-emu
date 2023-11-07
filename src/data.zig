const Int = @import("std").meta.Int;

pub fn Data(comptime width: comptime_int) type {
    return packed union {
        signed: Int(.signed, width),
        unsigned: Int(.unsigned, width),

        pub fn zeroExtended(self: @This(), comptime len: comptime_int) Data(len) {
            return .{ .unsigned = self.unsigned };
        }

        pub fn signExtended(self: @This(), comptime len: comptime_int) Data(len) {
            return .{ .signed = self.signed };
        }

        pub fn truncated(self: @This(), comptime len: comptime_int) Data(len) {
            return .{ .unsigned = @truncate(self.unsigned) };
        }
    };
}
