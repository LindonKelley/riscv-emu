const Int = @import("std").meta.Int;

pub fn Data(comptime width: comptime_int) type {
    return packed union {
        unsigned: Int(.unsigned, width),
        signed: Int(.signed, width),

        pub fn unsigned(self: @This()) Int(.unsigned, width) {
            return self.unsigned;
        }

        pub fn signExtended(self: @This(), comptime len: comptime_int) Int(.signed, len) {
            return self.signed;
        }
    };
}