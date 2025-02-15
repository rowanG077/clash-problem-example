"""

Copyright (c) 2020 Alex Forencich

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

"""

import enum


# Ethernet frame
class EthPre(enum.IntEnum):
    PRE = 0x55
    SFD = 0xD5


ETH_PREAMBLE = b'\x55\x55\x55\x55\x55\x55\x55\xd5'


# XGMII control characters
class XgmiiCtrl(enum.IntEnum):
    IDLE   = 0x07
    LPI    = 0x06
    START  = 0xfb
    TERM   = 0xfd
    ERROR  = 0xfe
    SEQ_OS = 0x9c
    RES_0  = 0x1c
    RES_1  = 0x3c
    RES_2  = 0x7c
    RES_3  = 0xbc
    RES_4  = 0xdc
    RES_5  = 0xf7
    SIG_OS = 0x5c


# BASE-R control characters
class BaseRCtrl(enum.IntEnum):
    IDLE  = 0x00
    LPI   = 0x06
    ERROR = 0x1e
    RES_0 = 0x2d
    RES_1 = 0x33
    RES_2 = 0x4b
    RES_3 = 0x55
    RES_4 = 0x66
    RES_5 = 0x78


# BASE-R O codes
class BaseRO(enum.IntEnum):
    SEQ_OS = 0x0
    SIG_OS = 0xf


# BASE-R sync header
class BaseRSync(enum.IntEnum):
    DATA = 0b10
    CTRL = 0b01


# BASE-R block type field
class BaseRBlockType(enum.IntEnum):
    CTRL     = 0x1e  # C7 C6 C5 C4 C3 C2 C1 C0 BT
    OS_4     = 0x2d  # D7 D6 D5 O4 C3 C2 C1 C0 BT
    START_4  = 0x33  # D7 D6 D5    C3 C2 C1 C0 BT
    OS_START = 0x66  # D7 D6 D5    O0 D3 D2 D1 BT
    OS_04    = 0x55  # D7 D6 D5 O4 O0 D3 D2 D1 BT
    START_0  = 0x78  # D7 D6 D5 D4 D3 D2 D1    BT
    OS_0     = 0x4b  # C7 C6 C5 C4 O0 D3 D2 D1 BT
    TERM_0   = 0x87  # C7 C6 C5 C4 C3 C2 C1    BT
    TERM_1   = 0x99  # C7 C6 C5 C4 C3 C2    D0 BT
    TERM_2   = 0xaa  # C7 C6 C5 C4 C3    D1 D0 BT
    TERM_3   = 0xb4  # C7 C6 C5 C4    D2 D1 D0 BT
    TERM_4   = 0xcc  # C7 C6 C5    D3 D2 D1 D0 BT
    TERM_5   = 0xd2  # C7 C6    D4 D3 D2 D1 D0 BT
    TERM_6   = 0xe1  # C7    D5 D4 D3 D2 D1 D0 BT
    TERM_7   = 0xff  #    D6 D5 D4 D3 D2 D1 D0 BT


xgmii_ctrl_to_baser_mapping = {
    XgmiiCtrl.IDLE:   BaseRCtrl.IDLE,
    XgmiiCtrl.LPI:    BaseRCtrl.LPI,
    XgmiiCtrl.ERROR:  BaseRCtrl.ERROR,
    XgmiiCtrl.RES_0:  BaseRCtrl.RES_0,
    XgmiiCtrl.RES_1:  BaseRCtrl.RES_1,
    XgmiiCtrl.RES_2:  BaseRCtrl.RES_2,
    XgmiiCtrl.RES_3:  BaseRCtrl.RES_3,
    XgmiiCtrl.RES_4:  BaseRCtrl.RES_4,
    XgmiiCtrl.RES_5:  BaseRCtrl.RES_5,
}


baser_ctrl_to_xgmii_mapping = {
    BaseRCtrl.IDLE:   XgmiiCtrl.IDLE,
    BaseRCtrl.LPI:    XgmiiCtrl.LPI,
    BaseRCtrl.ERROR:  XgmiiCtrl.ERROR,
    BaseRCtrl.RES_0:  XgmiiCtrl.RES_0,
    BaseRCtrl.RES_1:  XgmiiCtrl.RES_1,
    BaseRCtrl.RES_2:  XgmiiCtrl.RES_2,
    BaseRCtrl.RES_3:  XgmiiCtrl.RES_3,
    BaseRCtrl.RES_4:  XgmiiCtrl.RES_4,
    BaseRCtrl.RES_5:  XgmiiCtrl.RES_5,
}


block_type_term_lane_mapping = {
    BaseRBlockType.TERM_0:  0,
    BaseRBlockType.TERM_1:  1,
    BaseRBlockType.TERM_2:  2,
    BaseRBlockType.TERM_3:  3,
    BaseRBlockType.TERM_4:  4,
    BaseRBlockType.TERM_5:  5,
    BaseRBlockType.TERM_6:  6,
    BaseRBlockType.TERM_7:  7,
}

# Burst types
# AWBURST/ARBURST
class AxiBurstType(enum.IntEnum):
    FIXED = 0b00
    INCR  = 0b01
    WRAP  = 0b10


# Burst sizes (per beat)
# AWSIZE/ARSIZE
class AxiBurstSize(enum.IntEnum):
    SIZE_1   = 0b000
    SIZE_2   = 0b001
    SIZE_4   = 0b010
    SIZE_8   = 0b011
    SIZE_16  = 0b100
    SIZE_32  = 0b101
    SIZE_64  = 0b110
    SIZE_128 = 0b111


# Lock types
# AWLOCK/ARLOCK
class AxiLockType(enum.IntEnum):
    NORMAL    = 0b0
    EXCLUSIVE = 0b1


# Cache control
# AWCACHE/ARCACHE
class AxiCacheBit(enum.IntFlag):
    B  = 0b0001
    M  = 0b0010
    RA = 0b0100
    WA = 0b1000


# ARCACHE
ARCACHE_DEVICE_NON_BUFFERABLE = 0b0000
ARCACHE_DEVICE_BUFFERABLE = 0b0001
ARCACHE_NORMAL_NON_CACHEABLE_NON_BUFFERABLE = 0b0010
ARCACHE_NORMAL_NON_CACHEABLE_BUFFERABLE = 0b0011
ARCACHE_WRITE_THROUGH_NO_ALLOC = 0b1010
ARCACHE_WRITE_THROUGH_READ_ALLOC = 0b1110
ARCACHE_WRITE_THROUGH_WRITE_ALLOC = 0b1010
ARCACHE_WRITE_THROUGH_READ_AND_WRITE_ALLOC = 0b1110
ARCACHE_WRITE_BACK_NO_ALLOC = 0b1011
ARCACHE_WRITE_BACK_READ_ALLOC = 0b1111
ARCACHE_WRITE_BACK_WRITE_ALLOC = 0b1011
ARCACHE_WRITE_BACK_READ_AND_WRITE_ALLOC = 0b1111

# AWCACHE
AWCACHE_DEVICE_NON_BUFFERABLE = 0b0000
AWCACHE_DEVICE_BUFFERABLE = 0b0001
AWCACHE_NORMAL_NON_CACHEABLE_NON_BUFFERABLE = 0b0010
AWCACHE_NORMAL_NON_CACHEABLE_BUFFERABLE = 0b0011
AWCACHE_WRITE_THROUGH_NO_ALLOC = 0b0110
AWCACHE_WRITE_THROUGH_READ_ALLOC = 0b0110
AWCACHE_WRITE_THROUGH_WRITE_ALLOC = 0b1110
AWCACHE_WRITE_THROUGH_READ_AND_WRITE_ALLOC = 0b1110
AWCACHE_WRITE_BACK_NO_ALLOC = 0b0111
AWCACHE_WRITE_BACK_READ_ALLOC = 0b0111
AWCACHE_WRITE_BACK_WRITE_ALLOC = 0b1111
AWCACHE_WRITE_BACK_READ_AND_WRITE_ALLOC = 0b1111


# Protection bits
# AWPROT/ARPROT
class AxiProt(enum.IntFlag):
    PRIVILEGED  = 0b001
    NONSECURE   = 0b010
    INSTRUCTION = 0b100


# Operation status responses
# BRESP/RRESP
class AxiResp(enum.IntEnum):
    OKAY   = 0b00
    EXOKAY = 0b01
    SLVERR = 0b10
    DECERR = 0b11
