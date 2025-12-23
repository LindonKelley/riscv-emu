const std = @import("std");
const math = std.math;
const assert = std.debug.assert;
const expect = std.testing.expect;
const Signedness = std.builtin.Signedness;
const Int = std.meta.Int;
const Data = @import("data.zig").Data;

// all instruction formats must be safely bit castable to an integer of
// equal length, in the case of the base instruction set, that is 32, in the
// C-extension formats, that is 16

pub const R = packed struct {
    opcode: u7,
    rd: u5,
    funct3: u3,
    rs1: u5,
    rs2: u5,
    funct7: u7,

    immediate: ImmediateMixin(@This()),

    pub const Functs = FunctMixin(@This());
};

pub const I = packed struct {
    opcode: u7,
    rd: u5,
    funct3: u3,
    rs1: u5,
    imm0: u12,

    immediate: ImmediateMixin(@This()) = .{},

    pub const Functs = FunctMixin(@This());
    pub const Conversion = packed struct { imm0: u12 };
};

/// specialization of the I format for shift immediate instructions such as SLLI, SRLI, and SRAI
// the spec doesn't actually name this format, I find it reasonably unlikely
// that it will attempt to use this format name in the future
pub const IS = packed struct {
    opcode: u7,
    rd: u5,
    funct3: u3,
    rs1: u5,
    shamt: u5,
    funct7: u7, // the spec actually names this imm[11:5] but it's usage matches funct7

    immediate: ImmediateMixin(@This()),

    pub const Functs = FunctMixin(@This());
};

pub const S = packed struct {
    opcode: u7,
    imm0: u5,
    funct3: u3,
    rs1: u5,
    rs2: u5,
    imm5: u7,

    immediate: ImmediateMixin(@This()),

    pub const Functs = FunctMixin(@This());
    pub const Conversion = packed struct { imm0: u5, imm5: u7 };
};

pub const B = packed struct {
    opcode: u7,
    imm11: u1,
    imm1: u4,
    funct3: u3,
    rs1: u5,
    rs2: u5,
    imm5: u6,
    imm12: u1,

    immediate: ImmediateMixin(@This()),

    pub const Functs = FunctMixin(@This());
    pub const Conversion = packed struct { imm0: u1 = 0, imm1: u4, imm5: u6, imm11: u1, imm12: u1 };
};

pub const U = packed struct {
    opcode: u7,
    rd: u5,
    imm12: u20,

    immediate: ImmediateMixin(@This()),

    pub const Functs = FunctMixin(@This());
    pub const Conversion = packed struct { imm0: u12 = 0, imm12: u20 };
};

pub const J = packed struct {
    opcode: u7,
    rd: u5,
    imm12: u8,
    imm11: u1,
    imm1: u10,
    imm20: u1,

    immediate: ImmediateMixin(@This()),

    pub const Functs = FunctMixin(@This());
    pub const Conversion = packed struct { imm0: u1 = 0, imm1: u10, imm11: u1, imm12: u8, imm20: u1 };
};

// the spec doesn't actually name this format, choosing a full word to be safe
pub const FENCE = packed struct {
    opcode: u7,
    rd: u5, // currently unused, reserved for future finer grain control
    funct3: u3,
    rs1: u5, // currently unused, reserved for future finer grain control
    sw: bool,
    sr: bool,
    so: bool,
    si: bool,
    pw: bool,
    pr: bool,
    po: bool,
    pi: bool,
    fm: u4,

    immediate: ImmediateMixin(@This()),

    pub const Functs = FunctMixin(@This());
};

// ECALL and EBREAK use this, although the spec defines their format as I, this
// will be more helpful to me for pattern matching
pub const ENV = packed struct {
    opcode: u7,
    rd: u5,
    funct3: u3,
    rs1: u5,
    funct12: u12,

    immediate: ImmediateMixin(@This()),

    pub const Functs = FunctMixin(@This());
};

pub fn ImmediateMixin(comptime Format: type) type {
    return packed struct {
        pub fn get(self: *const @This()) Data(@bitSizeOf(Format.Conversion)) {
            const Conversion = Format.Conversion;
            var conv: Conversion = @bitCast(@as(std.meta.Int(.unsigned, @bitSizeOf(Conversion)), 0));
            const format: *const Format = @alignCast(@fieldParentPtr("immediate", self));
            structCopyFields(&conv, format.*);
            return .{ .unsigned = @bitCast(conv) };
        }

        // some instruction formats ignore some number of lower bits,
        // in those cases the aforementioned lower bits of the given `imm`
        // argument are silently ignored
        pub fn set(self: *@This(), imm: Data(@bitSizeOf(Format.Conversion))) void {
            const conv: Format.Conversion = @bitCast(imm);
            const format: *Format = @alignCast(@fieldParentPtr("immediate", self));
            structCopyFields(format, conv);
        }
    };
}

pub fn FunctMixin(comptime Format: type) type {
    return packed struct {
        pub fn get() [8]?std.builtin.Type.StructField {
            var functs: [8]?std.builtin.Type.StructField = .{ null } ** 8;
            var index = 0;
            inline for (@typeInfo(Format).@"struct".fields) |field| {
                const eql = std.mem.eql;
                if (field.name.len > "funct".len and eql(u8, field.name[0.."funct".len], "funct")) {
                    functs[index] = field;
                    index += 1;
                }
            }
            return functs;
        }
    };
}

inline fn structCopyFields(dst_ptr: anytype, src: anytype) void {
    inline for (@typeInfo(@TypeOf(dst_ptr.*)).@"struct".fields) |field| {
        if (@hasField(@TypeOf(src), field.name)) {
            @field(dst_ptr, field.name) = @field(src, field.name);
        }
    }
}

test "instruction type width sanity check" {
    inline for (@typeInfo(@This()).@"struct".decls) |decl| {
        const field = @field(@This(), decl.name);
        if (@TypeOf(field) == type) {
            try expect(@bitSizeOf(field) == 32 or @bitSizeOf(field) == 16);
        }
    }
}

test "instruction immediates properly signed" {
    inline for (@typeInfo(@This()).@"struct".decls) |decl| {
        const field = @field(@This(), decl.name);
        if (@TypeOf(field) == type and @hasField(field, "Conversion")) {
            const Format = field;
            const Integer = Int(.signed, @bitSizeOf(Format.Conversion));
            const first_field = @typeInfo(Format.Conversion).@"struct".fields[0];
            assert(std.mem.eql(u8, first_field.name, "imm0"));
            const zero_mask: Integer = if (first_field.default_value) |default| blk: {
                assert(@as(*const first_field.type, @ptrCast(@alignCast(default))).* == 0);
                break :blk math.maxInt(first_field.type);
            } else blk: {
                break :blk 0;
            };

            var any_instruction: Int(.signed, @bitSizeOf(Format)) = 0;
            const instruction_ptr: *Format = @ptrCast(&any_instruction);
            const max = math.maxInt(Integer);
            const min = math.minInt(Integer);
            for ([_]Integer{ min, min / 2, 0, max / 2, max }) |i| {
                instruction_ptr.*.setImmediate(i);
                try expect(instruction_ptr.*.getImmediate() == @as(Integer, i) & ~zero_mask);
            }
        }
    }
}
