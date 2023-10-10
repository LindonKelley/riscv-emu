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
        // todo needs to allow arbitrary additions to the error set:
        //  "The EEI will define what portions of the address space are legal to access with
        // which instructions (e.g., some addresses might be read only, or support word access only)."
        pub fn load(self: *@This(), comptime width: MemoryValueWidth, address: Int(.unsigned, XLEN)) LoadError!width.Data() {
            if (address % width.bytes() == 0) {
                const ptr = self.memory[address..];
                return .{ .unsigned = std.mem.readIntNative(width.Unsigned(), ptr[0..width.bytes()]) };
            } else {
                return error.LoadAddressMisaligned;
            }
        }

        pub fn store(self: *@This(), comptime width: MemoryValueWidth, address: Int(.unsigned, XLEN), data: width.Data()) StoreError!void {
            if (address % width.bytes() == 0) {
                const ptr = self.memory[address..];
                std.mem.writeIntNative(width.Unsigned(), ptr[0..width.bytes()], data.unsigned);
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
        return Int(.unsigned, self.bits());
    }

    pub fn Signed(comptime self: @This()) type {
        return Int(.signed, self.bits());
    }

    pub fn Data(comptime self: @This()) type {
        return raw_data.Data(self.bits());
    }

    pub fn bits(comptime self: @This()) comptime_int {
        return self.bytes() * 8;
    }

    pub fn bytes(comptime self: @This()) comptime_int {
        return @intFromEnum(self);
    }
};
