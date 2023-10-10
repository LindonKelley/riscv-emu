/// RV32G and RV64G opcodes.


// The opcodes are in order, with comments where there's gaps that are either
// reserved for future developement of the spec or for custom use.
// Some reserved opcodes are reserved for RV128 but can be used for
// custom use extensions in RV32 and RV64.

pub const LOAD: u7 =        0b00_000_11;
pub const LOAD_FP: u7 =     0b00_001_11;
// custom-0
pub const MISC_MEM: u7 =    0b00_011_11;
pub const OP_IMM: u7 =      0b00_100_11;
pub const AUIPC: u7 =       0b00_101_11;
pub const OP_IMM_32: u7 =   0b00_110_11;
pub const STORE: u7 =       0b01_000_11;
pub const STORE_FP: u7 =    0b01_001_11;
// custom-1
pub const AMO: u7 =         0b01_011_11;
pub const OP: u7 =          0b01_100_11;
pub const LUI: u7 =         0b01_101_11;
pub const OP_32: u7 =       0b01_110_11;
pub const MADD: u7 =        0b10_000_11;
pub const MSUB: u7 =        0b10_001_11;
pub const NMSUB: u7 =       0b10_010_11;
pub const NMADD: u7 =       0b10_011_11;
pub const OP_FP: u7 =       0b10_100_11;
// reserved
// custom-2/rv128
pub const BRANCH: u7 =      0b11_000_11;
pub const JALR: u7 =        0b11_001_11;
// reserved
pub const JAL: u7 =         0b11_011_11;
pub const SYSTEM: u7 =      0b11_100_11;
// reserved
// custom-3/rv128
