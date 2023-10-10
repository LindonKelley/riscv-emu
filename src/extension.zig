/// Base integer ISA, proceeded by RV32 or RV64 depending on register width
pub const I = .{};

/// Instruction fetch fencing
pub const Zifencei = .{};

/// Integer multiplication and division
pub const M = .{};

/// Atomic memory operations
pub const A = .{};

/// Control and Status registers
pub const Zicsr = .{};

/// Floating point registers and single precision floating point operations
/// The width of the FP registers is determined by FLEN, which other extensions can expand, F sets FLEN=32
pub const F = .{ Zicsr };

/// Adds double precision floating point operations, and expands the FP registers: FLEN=64
pub const D = .{ F };

/// Adds quadruple precision floating point operations, and expands the FP registers: FLEN=128
pub const Q = .{ D };

/// Compressed instructions
pub const C = .{};

/// General purpose ISA
pub const G = .{ I, M, A, F, D, Zicsr, Zifencei };
