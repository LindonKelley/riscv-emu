const std = @import("std");
const FixedBufferStream = std.io.FixedBufferStream;

/// Immediately loads an ELF file into the given Hart, and sets the program counter to the file's entry point.
/// The caller must ensure the ELF file is an executable.
///
/// This function does not close the file.
// making it a personal goal to avoid allocating in this function, later I may add an allocating version
pub fn elf(hart_ptr: anytype, file: anytype) !void {
    const header = try std.elf.Header.read(file);
    if (header.endian != .Little) {
        // todo I don't want to require this, and hart implementations should be allowed to specify supported endianness
        return error.ElfNotLittleEndian;
    }
    if (header.machine != .RISCV) {
        return error.ElfNotRiscV;
    }
    if (header.is_64 and @TypeOf(hart_ptr.*).XLEN < 64) {
        return error.ElfHeaderHartIncompatibleXlen;
    }

    var program_header_iter = header.program_header_iterator(file);
    while (try program_header_iter.next()) |program_header| {
        switch (program_header.p_type) {
            std.elf.PT_LOAD => {
                try file.seekableStream().seekTo(program_header.p_offset);
                for (0..program_header.p_filesz) |offset| {
                    // todo should probably try loading more of the file at once
                    var data: u8 = undefined;
                    try file.reader().readNoEof(std.mem.asBytes(&data));
                    try hart_ptr.store(.byte, @intCast(program_header.p_vaddr + offset), .{ .unsigned = data });
                }
                for (program_header.p_filesz..program_header.p_memsz) |offset| {
                    try hart_ptr.store(.byte, @intCast(program_header.p_vaddr + offset), .{ .unsigned = 0 });
                }
                // todo modeset Hart memory according to program header flags
                //  program_header.p_flags & elf.PF_R != 0; // readable, same for W and X
            },
            // todo PT_RISCV_ATTRIBUTES: https://github.com/riscv-non-isa/riscv-elf-psabi-doc/blob/master/riscv-elf.adoc#program-header-table
            0x70000003 => {},
            std.elf.PT_NULL, std.elf.PT_NOTE, std.elf.PT_PHDR, std.elf.PT_NUM => {},
            else => {
                return error.ElfUnsupportedProgramHeader;
            },
        }
    }

    // todo need to ensure header.entry is properly aligned
    hart_ptr.setPc(@intCast(header.entry));
}

/// Immediately loads a raw binary from the given reader into the given Hart, starting at address 0, and sets
///  the program counter to 0
/// The caller must ensure the reader is reading raw machine code.
///
/// This function does not close the file.
pub fn raw(hart_ptr: anytype, reader: anytype) !void {
    var address: std.meta.Int(.unsigned, @TypeOf(hart_ptr.*).XLEN) = 0;
    while (true) {
        var data: [@sizeOf(u64)]u8 = undefined;
        const read = try reader.readAll(&data);
        // todo this forces the endianess of Hart.store to be little, likely going to add a storeBig to remedy
        if (read < data.len) {
            for (0..read) |i| {
                try hart_ptr.store(.byte, address, .{ .unsigned = data[i] });
                address += @sizeOf(u8);
            }
            break;
        } else {
            try hart_ptr.store(.doubleword, address, .{ .unsigned = std.mem.bytesToValue(u64, &data) });
            address += @sizeOf(u64);
        }
    }

    hart_ptr.setPc(0);
}

// it is likely that std has a slice reader but I could not find it
const ByteSliceReader = struct {
    slice: []u8,
    index: *usize,

    fn init(slice: []u8, index: *usize) @This() {
        return .{ .slice = slice, .index = index };
    }

    fn remaining(self: @This()) usize {
        return self.slice.len - self.index.*;
    }

    fn read(self: @This(), dst: []u8) !usize {
        const len = @min(self.remaining(), dst.len);
        @memcpy(dst[0..len], self.slice[self.index.* .. self.index.* + len]);
        self.index.* += len;
        return len;
    }

    const Reader = std.io.Reader(@This(), error{}, read);

    fn reader(self: @This()) Reader {
        return .{ .context = self };
    }
};
