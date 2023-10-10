const std = @import("std");
const Int = std.meta.Int;
const FenceOperands = @import("instruction.zig").FenceOperands;
const raw_data = @import("data.zig");

pub const LoadError = error {
    LoadAddressMisaligned,
    LoadAccessFault,
};

pub const StoreError = error {
    StoreAddressMisaligned,
    StoreAccessFault,
};

// todo make a proper simple example, stack based and allocation based
/// A basic MMU implementation
///
/// Does not have Memory Mapped IO, virtual addresses are the same as physical addresses.
pub fn BasicMmu(comptime XLEN: comptime_int) type {
    comptime {
        if (XLEN != 32 and XLEN != 64) {
            var buf: [32]u8 = undefined;
            const XLEN_string = try std.fmt.bufPrint(&buf, "{d}", .{XLEN});
            @compileError("MMU XLEN must be either 32 or 64, was " ++ XLEN_string);
        }
    }
    return struct {
        // todo speed test explicit align vs implicit
        memory: [1 << 17]u8 align(@intFromEnum(MemoryValueWidth.doubleword)) = undefined,

        // todo there must be an error missing for out of bounds access
        pub fn load(self: *@This(), comptime width: MemoryValueWidth, address: Int(.unsigned, XLEN)) LoadError!width.Unsigned() {
            if (address % @intFromEnum(width) == 0) {
                const ptr = self.memory[address..];
                return std.mem.readIntNative(width.Unsigned(), ptr[0..@intFromEnum(width)]);
            } else {
                return error.LoadAddressMisaligned;
            }
        }

        pub fn store(self: *@This(), comptime width: MemoryValueWidth, address: Int(.unsigned, XLEN), data: width.Unsigned()) StoreError!void {
            if (address % @intFromEnum(width) == 0) {
                const ptr = self.memory[address..];
                std.mem.writeIntNative(width.Unsigned(), ptr[0..@intFromEnum(width)], data);
            } else {
                return error.StoreAddressMisaligned;
            }
        }
        
        // todo fn store_slice, a function should exist in this file that naively implements this via repeat store calls

        pub fn fence(self: *@This(), fm: u4, pred: FenceOperands, succ: FenceOperands) void {
            // intentional no-op on this MMU
            _ = self;
            _ = succ;
            _ = pred;
            _ = fm;
        }
    };
}

pub const MemoryValueWidth = enum(u8) {
    byte = 1,
    halfword = 2,
    word = 4,
    doubleword = 8,

    pub fn Unsigned(comptime self: @This()) type {
        return Int(.unsigned, @intFromEnum(self) * 8);
    }

    pub fn Signed(comptime self: @This()) type {
        return Int(.signed, @intFromEnum(self) * 8);
    }

    pub fn Data(comptime self: @This()) type {
        return raw_data.Data(@intFromEnum(self) * 8);
    }
};
