import logging 

from dataclasses import dataclass, field
from typing import List, Optional

import cocotb
from cocotb.queue import Queue, QueueFull
from cocotb.handle import ModifiableObject
from cocotb.triggers import RisingEdge, Event

from cocotb_lib.reset import Reset
from cocotb_lib.axis import AxiStreamSink, AxiStreamSource, AxiStreamBus
from cocotb_lib.ip_ep import IpFrame
import cocotb_lib.utils.formatters as formatters


@dataclass
class TcpFrame:
    payload: bytes
    eth_src_mac: int
    eth_dest_mac: int
    eth_type: int
    ip_version: int
    ip_ihl: int
    ip_dscp: int
    ip_ecn: int
    ip_length: int
    ip_identification: int
    ip_flags: int
    ip_fragment_offset: int
    ip_ttl: int
    ip_protocol: int
    ip_header_checksum: int
    ip_source_ip: int
    ip_dest_ip: int
    tcp_source_port: int
    tcp_dest_port: int
    tcp_seq_num: int
    tcp_ack_num: int
    tcp_data_offset: int
    tcp_cwr: bool
    tcp_ece: bool
    tcp_urg: bool
    tcp_ack: bool
    tcp_psh: bool
    tcp_rst: bool
    tcp_syn: bool
    tcp_fin: bool
    tcp_window_size: int
    tcp_checksum: int
    tcp_urgent_pointer: int
    tcp_options: List[bytes]
    tcp_padding: bytes

    @classmethod
    def with_defaults(
        cls,
        payload = b'',
        eth_dest_mac = 1,
        eth_src_mac = 2,
        eth_type = 0,
        ip_version = 4,
        ip_ihl = 5,
        ip_dscp = 0,
        ip_ecn = 0,
        ip_identification = 0,
        ip_flags = 2,
        ip_fragment_offset = 0,
        ip_ttl = 64,
        ip_protocol = 0x11,
        ip_source_ip = 0xc0a80164,
        ip_dest_ip = 0xc0a80165,
        tcp_source_port = 11111,
        tcp_dest_port = 22222,
        tcp_seq_num = 0,
        tcp_ack_num = 0,
        tcp_cwr = False,
        tcp_ece = False,
        tcp_urg = False,
        tcp_ack = False,
        tcp_psh = False,
        tcp_rst = False,
        tcp_syn = False,
        tcp_fin = False,
        tcp_window_size = 200,
        tcp_urgent_pointer = 0,
        tcp_options = [],
        tcp_padding = b'',
    ):
        ip_length = IpFrame.get_ip_length_from_fields(payload)
        ip_header_checksum = IpFrame.get_ip_header_checksum_from_fields(
            ip_version,
            ip_ihl,
            ip_dscp,
            ip_ecn,
            ip_length,
            ip_identification,
            ip_flags,
            ip_fragment_offset,
            ip_ttl,
            ip_protocol,
            ip_source_ip,
            ip_dest_ip,
        )
        tcp_data_offset = TcpFrame.get_data_offset_from_fields(tcp_options)
        tcp_checksum = TcpFrame.get_checksum_from_fields(
            payload,
            ip_source_ip,
            ip_dest_ip,
            tcp_source_port,
            tcp_dest_port,
            tcp_seq_num,
            tcp_ack_num,
            tcp_window_size,
            tcp_cwr,
            tcp_ece,
            tcp_urg,
            tcp_ack,
            tcp_psh,
            tcp_rst,
            tcp_syn,
            tcp_fin,
            tcp_urgent_pointer,
            tcp_options,
            tcp_padding,
        )
        frame = cls(
            payload = payload,
            eth_dest_mac = eth_dest_mac,
            eth_src_mac = eth_src_mac,
            eth_type = eth_type,
            ip_version = ip_version,
            ip_ihl = ip_ihl,
            ip_dscp = ip_dscp,
            ip_ecn = ip_ecn,
            ip_length = ip_length,
            ip_identification = ip_identification,
            ip_flags = ip_flags,
            ip_fragment_offset = ip_fragment_offset,
            ip_ttl = ip_ttl,
            ip_protocol = ip_protocol,
            ip_header_checksum = ip_header_checksum,
            ip_source_ip = ip_source_ip,
            ip_dest_ip = ip_dest_ip,
            tcp_source_port = tcp_source_port,
            tcp_dest_port = tcp_dest_port,
            tcp_seq_num = tcp_seq_num,
            tcp_ack_num = tcp_ack_num,
            tcp_data_offset = tcp_data_offset,
            tcp_cwr = tcp_cwr,
            tcp_ece = tcp_ece,
            tcp_urg = tcp_urg,
            tcp_ack = tcp_ack,
            tcp_psh = tcp_psh,
            tcp_rst = tcp_rst,
            tcp_syn = tcp_syn,
            tcp_fin = tcp_fin,
            tcp_window_size = tcp_window_size,
            tcp_checksum = tcp_checksum,
            tcp_urgent_pointer = tcp_urgent_pointer,
            tcp_options = tcp_options,
            tcp_padding = tcp_padding,
        )
        return frame

    def __len__(self) -> int:
        return len(self.payload) + self.get_data_offset()

    # The length of the header, in 32-bit words
    @staticmethod
    def get_data_offset_from_fields(
        tcp_options: List[bytes],
    ) -> int:
        basic_header_len = 20
        if len(tcp_options) != 0:
            raise NotImplementedError()
        else:
            header_len = basic_header_len
        if header_len % 4 != 0:
            raise ValueError("Header length is not divisible by 32 bits")
        if (header_len // 4) > 15:
            raise ValueError("Header length is greater than 4-bit limit")
        return header_len // 4

    @staticmethod
    def get_tcp_header_from_fields(
        tcp_source_port: int,
        tcp_dest_port: int,
        tcp_seq_num: int,
        tcp_ack_num: int,
        tcp_data_offset: int,
        tcp_cwr: bool,
        tcp_ece: bool,
        tcp_urg: bool,
        tcp_ack: bool,
        tcp_psh: bool,
        tcp_rst: bool,
        tcp_syn: bool,
        tcp_fin: bool,
        tcp_urgent_pointer: int,
        tcp_checksum: int,
        tcp_window_size: int,
        tcp_options: List[bytes],
        tcp_padding: bytes,
    ) -> bytes:
        offset_reserved = ((tcp_data_offset << 4) & 0xF0)
        control_bits = \
            tcp_cwr << 7 \
            | tcp_ece << 6 \
            | tcp_urg << 5 \
            | tcp_ack << 4 \
            | tcp_psh << 3 \
            | tcp_rst << 2 \
            | tcp_syn << 1 \
            | tcp_fin << 0 

        return tcp_source_port.to_bytes(2, 'big') \
            + tcp_dest_port.to_bytes(2, 'big') \
            + tcp_seq_num.to_bytes(4, 'big') \
            + tcp_ack_num.to_bytes(4, 'big') \
            + offset_reserved.to_bytes(1, 'big') \
            + control_bits.to_bytes(1, 'big') \
            + tcp_window_size.to_bytes(2, 'big') \
            + tcp_checksum.to_bytes(2, 'big') \
            + tcp_urgent_pointer.to_bytes(2, 'big')

    @staticmethod
    def get_checksum_from_fields(
        payload: bytes,
        ip_source_ip: int,
        ip_dest_ip: int,
        tcp_source_port: int,
        tcp_dest_port: int,
        tcp_seq_num: int,
        tcp_ack_num: int,
        tcp_window_size: int,
        tcp_cwr: bool,
        tcp_ece: bool,
        tcp_urg: bool,
        tcp_ack: bool,
        tcp_psh: bool,
        tcp_rst: bool,
        tcp_syn: bool,
        tcp_fin: bool,
        tcp_urgent_pointer: int,
        tcp_options: List[bytes],
        tcp_padding: bytes,
    ) -> int:
        tcp_data_offset = TcpFrame.get_data_offset_from_fields(tcp_options)
        frame_len = len(payload) + tcp_data_offset
        pseudo_header = ip_source_ip.to_bytes(4, 'big') \
            + ip_dest_ip.to_bytes(4, 'big') \
            + b'\x00' \
            + frame_len.to_bytes() 
        tcp_header = TcpFrame.get_tcp_header_from_fields(
            tcp_source_port,
            tcp_dest_port,
            tcp_seq_num,
            tcp_ack_num,
            tcp_data_offset,
            tcp_cwr,
            tcp_ece,
            tcp_urg,
            tcp_ack,
            tcp_psh,
            tcp_rst,
            tcp_syn,
            tcp_fin,
            tcp_urgent_pointer,
            0,
            tcp_window_size,
            tcp_options,
            tcp_padding,
        )
        byte_stream = pseudo_header + tcp_header + payload
        checksum = 0
        if len(byte_stream) % 2 != 0:
            byte_stream += b'\x00'
        for i in range(0, len(byte_stream), 2):
            word = (byte_stream[i] << 8) + byte_stream[i + 1]
            checksum += word
        checksum = (checksum >> 16) + (checksum & 0xFFFF)
        checksum += (checksum >> 16)
        checksum = ~checksum & 0xFFFF
        return checksum
    
    def __repr__(self):
        return (
            f"TcpFrame(payload={self.payload}, "
            f"eth_src_mac={formatters.as_mac_address(self.eth_src_mac)}, "
            f"eth_dest_mac={formatters.as_mac_address(self.eth_dest_mac)}, "
            f"eth_type={self.eth_type}, "
            f"ip_version={self.ip_version}, "
            f"ip_ihl={self.ip_ihl}, "
            f"ip_dscp = {self.ip_dscp}, "
            f"ip_ecn = {self.ip_ecn}, "
            f"ip_length = {self.ip_length}, "
            f"ip_identification = {self.ip_identification}, "
            f"ip_flags = {self.ip_flags}, "
            f"ip_fragment_offset = {self.ip_fragment_offset}, "
            f"ip_ttl = {self.ip_ttl}, "
            f"ip_protocol = {self.ip_protocol}, "
            f"ip_header_checksum = {self.ip_header_checksum}, "
            f"ip_source_ip = {formatters.as_ip_address(self.ip_source_ip)}, "
            f"ip_dest_ip = {formatters.as_ip_address(self.ip_dest_ip)}, "
            f"tcp_source_port = {self.tcp_source_port}, "
            f"tcp_dest_port = {self.tcp_dest_port}, "
            f"tcp_seq_num = {self.tcp_seq_num}, "
            f"tcp_ack_num = {self.tcp_ack_num}, "
            f"tcp_data_offset = {self.tcp_data_offset}, "
            f"tcp_cwr = {self.tcp_cwr}, "
            f"tcp_ece = {self.tcp_ece}, "
            f"tcp_urg = {self.tcp_urg}, "
            f"tcp_ack = {self.tcp_ack}, "
            f"tcp_psh = {self.tcp_psh}, "
            f"tcp_rst = {self.tcp_rst}, "
            f"tcp_syn = {self.tcp_syn}, "
            f"tcp_fin = {self.tcp_fin}, "
            f"tcp_window_size = {self.tcp_window_size}, "
            f"tcp_checksum = {self.tcp_checksum}, "
            f"tcp_urgent_pointer = {self.tcp_urgent_pointer}, "
            f"tcp_options = {self.tcp_options}, "
            f"tcp_padding = {self.tcp_padding}"
        )

@dataclass
class TcpFrameSource(Reset):
    clock: ModifiableObject
    reset: ModifiableObject
    hdr_valid: ModifiableObject
    hdr_ready: ModifiableObject
    eth_dest_mac: ModifiableObject
    eth_src_mac: ModifiableObject
    eth_type: ModifiableObject
    ip_version: ModifiableObject
    ip_ihl: ModifiableObject
    ip_dscp: ModifiableObject
    ip_ecn: ModifiableObject
    ip_length: ModifiableObject
    ip_identification: ModifiableObject
    ip_flags: ModifiableObject
    ip_fragment_offset: ModifiableObject
    ip_ttl: ModifiableObject
    ip_protocol: ModifiableObject
    ip_header_checksum: ModifiableObject
    ip_source_ip: ModifiableObject
    ip_dest_ip: ModifiableObject
    tcp_source_port: ModifiableObject
    tcp_dest_port: ModifiableObject
    tcp_seq_num: ModifiableObject
    tcp_ack_num: ModifiableObject
    tcp_data_offset: ModifiableObject
    tcp_cwr: ModifiableObject
    tcp_ece: ModifiableObject
    tcp_urg: ModifiableObject
    tcp_ack: ModifiableObject
    tcp_psh: ModifiableObject
    tcp_rst: ModifiableObject
    tcp_syn: ModifiableObject
    tcp_fin: ModifiableObject
    tcp_window_size: ModifiableObject
    tcp_checksum: ModifiableObject
    tcp_urgent_pointer: ModifiableObject
    tcp_payload_bus: AxiStreamBus

    def __post_init__(self):
        self.log = logging.getLogger(f"cocotb.{self.tcp_payload_bus.tdata._path}")
        self._run_coroutine = None
        self.queue = Queue()
        self.payload_source = AxiStreamSource(self.tcp_payload_bus, self.clock, self.reset)
        self.reset_active_level = True
        self.current_frame: Optional[TcpFrame] = None

        self._init_reset(self.reset, self.reset_active_level)

    async def send(self, frame: TcpFrame):
        self.queue.put_nowait(frame)

    # In case you want to get the bus first, before instantiating this
    @classmethod
    def from_entity_plus_payload_bus(
        cls,
        entity,
        prefix: str,
        tcp_payload_bus: AxiStreamBus,
    ):
        if prefix == "" or prefix is None:
            prefix_ = ""
        elif prefix[-1] == "_":
            prefix_ = f"{prefix}"
        else:
            prefix_ = f"{prefix}_"
        return cls(
            clock = getattr(entity, f"clk"),
            reset = getattr(entity, f"rst"),
            hdr_valid = getattr(entity, f"{prefix_}hdr_valid"),
            hdr_ready = getattr(entity, f"{prefix_}hdr_ready"),
            eth_dest_mac = getattr(entity, f"{prefix_}eth_dest_mac"),
            eth_src_mac = getattr(entity, f"{prefix_}eth_src_mac"),
            eth_type = getattr(entity, f"{prefix_}eth_type"),
            ip_version = getattr(entity, f"{prefix_}ip_version"),
            ip_ihl = getattr(entity, f"{prefix_}ip_ihl"),
            ip_dscp = getattr(entity, f"{prefix_}ip_dscp"),
            ip_ecn = getattr(entity, f"{prefix_}ip_ecn"),
            ip_length = getattr(entity, f"{prefix_}ip_length"),
            ip_identification = getattr(entity, f"{prefix_}ip_identification"),
            ip_flags = getattr(entity, f"{prefix_}ip_flags"),
            ip_fragment_offset = getattr(entity, f"{prefix_}ip_fragment_offset"),
            ip_ttl = getattr(entity, f"{prefix_}ip_ttl"),
            ip_protocol = getattr(entity, f"{prefix_}ip_protocol"),
            ip_header_checksum = getattr(entity, f"{prefix_}ip_header_checksum"),
            ip_source_ip = getattr(entity, f"{prefix_}ip_source_ip"),
            ip_dest_ip = getattr(entity, f"{prefix_}ip_dest_ip"),
            tcp_source_port = getattr(entity, f"{prefix_}tcp_source_port"),
            tcp_dest_port = getattr(entity, f"{prefix_}tcp_dest_port"),
            tcp_seq_num = getattr(entity, f"{prefix_}tcp_seq_num"),
            tcp_ack_num = getattr(entity, f"{prefix_}tcp_ack_num"),
            tcp_data_offset = getattr(entity, f"{prefix_}tcp_data_offset"),
            tcp_cwr = getattr(entity, f"{prefix_}tcp_cwr"),
            tcp_ece = getattr(entity, f"{prefix_}tcp_ece"),
            tcp_urg = getattr(entity, f"{prefix_}tcp_urg"),
            tcp_ack = getattr(entity, f"{prefix_}tcp_ack"),
            tcp_psh = getattr(entity, f"{prefix_}tcp_psh"),
            tcp_rst = getattr(entity, f"{prefix_}tcp_rst"),
            tcp_syn = getattr(entity, f"{prefix_}tcp_syn"),
            tcp_fin = getattr(entity, f"{prefix_}tcp_fin"),
            tcp_window_size = getattr(entity, f"{prefix_}tcp_window_size"),
            tcp_checksum =  getattr(entity, f"{prefix_}tcp_checksum"),
            tcp_urgent_pointer =  getattr(entity, f"{prefix_}tcp_urgent_pointer"),
            tcp_payload_bus = tcp_payload_bus,
        )

    def _handle_reset(self, state):
        self.payload_source._handle_reset(state)
        if state:
            self.log.info("Reset asserted")
            if self._run_coroutine is not None:
                self._run_coroutine.kill()
                self._run_coroutine = None

            if self.current_frame is not None:
                self.log.warning("Flushed transmit frame during reset: %s", self.current_frame)
                self.current_frame = None

        else:
            self.log.info("Reset de-asserted")
            if self._run_coroutine is None:
                self._run_coroutine = cocotb.start_soon(self._run())

    async def _run(self):
        while True:
            await RisingEdge(self.clock)

            if (not self.queue.empty()):
                self.current_frame = self.queue.get_nowait() 
                await self._send_header(self.current_frame)
                await self.payload_source.finish_sending(self.current_frame.payload)

    async def _send_header(self, frame: TcpFrame):
        header_has_been_sent = False

        self.hdr_valid.value = True
        self._set_header_wires_from_frame(frame)

        while not header_has_been_sent:
            await RisingEdge(self.clock)
            header_has_been_sent = self.hdr_ready.value
            self.hdr_valid.value = not header_has_been_sent

    def _set_header_wires_from_frame(self, frame: TcpFrame):
        self.eth_src_mac.value = frame.eth_src_mac
        self.eth_dest_mac.value = frame.eth_dest_mac
        self.eth_type.value = frame.eth_type
        self.ip_version.value = frame.ip_version
        self.ip_ihl.value = frame.ip_ihl
        self.ip_dscp.value = frame.ip_dscp
        self.ip_ecn.value = frame.ip_ecn
        self.ip_length.value = frame.ip_length
        self.ip_identification.value = frame.ip_identification
        self.ip_flags.value = frame.ip_flags
        self.ip_fragment_offset.value = frame.ip_fragment_offset
        self.ip_ttl.value = frame.ip_ttl
        self.ip_protocol.value = frame.ip_protocol
        self.ip_header_checksum.value = frame.ip_header_checksum
        self.ip_source_ip.value = frame.ip_source_ip
        self.ip_dest_ip.value = frame.ip_dest_ip
        self.tcp_source_port.value = frame.tcp_source_port
        self.tcp_dest_port.value = frame.tcp_dest_port
        self.tcp_seq_num.value = frame.tcp_seq_num
        self.tcp_ack_num.value = frame.tcp_ack_num
        self.tcp_data_offset.value = frame.tcp_data_offset
        self.tcp_cwr.value = frame.tcp_cwr
        self.tcp_ece.value = frame.tcp_ece
        self.tcp_urg.value = frame.tcp_urg
        self.tcp_ack.value = frame.tcp_ack
        self.tcp_psh.value = frame.tcp_psh
        self.tcp_rst.value = frame.tcp_rst
        self.tcp_syn.value = frame.tcp_syn
        self.tcp_fin.value = frame.tcp_fin
        self.tcp_window_size.value = frame.tcp_window_size
        self.tcp_checksum.value = frame.tcp_checksum
        self.tcp_urgent_pointer.value = frame.tcp_urgent_pointer
        
@dataclass
class TcpFrameSink(Reset):
    clock: ModifiableObject
    reset: ModifiableObject
    hdr_valid: ModifiableObject
    hdr_ready: ModifiableObject
    eth_dest_mac: ModifiableObject
    eth_src_mac: ModifiableObject
    eth_type: ModifiableObject
    ip_version: ModifiableObject
    ip_ihl: ModifiableObject
    ip_dscp: ModifiableObject
    ip_ecn: ModifiableObject
    ip_length: ModifiableObject
    ip_identification: ModifiableObject
    ip_flags: ModifiableObject
    ip_fragment_offset: ModifiableObject
    ip_ttl: ModifiableObject
    ip_protocol: ModifiableObject
    ip_header_checksum: ModifiableObject
    ip_source_ip: ModifiableObject
    ip_dest_ip: ModifiableObject
    tcp_source_port: ModifiableObject
    tcp_dest_port: ModifiableObject
    tcp_seq_num: ModifiableObject
    tcp_ack_num: ModifiableObject
    tcp_data_offset: ModifiableObject
    tcp_cwr: ModifiableObject
    tcp_ece: ModifiableObject
    tcp_urg: ModifiableObject
    tcp_ack: ModifiableObject
    tcp_psh: ModifiableObject
    tcp_rst: ModifiableObject
    tcp_syn: ModifiableObject
    tcp_fin: ModifiableObject
    tcp_window_size: ModifiableObject
    tcp_checksum: ModifiableObject
    tcp_urgent_pointer: ModifiableObject
    tcp_payload_bus: AxiStreamBus

    def __post_init__(self):
        self.log = logging.getLogger(f"cocotb.{self.tcp_payload_bus.tdata._path}")
        self._run_coroutine = None
        self.queue = Queue()
        self.payload_sink = AxiStreamSink(self.tcp_payload_bus, self.clock, self.reset)
        self.reset_active_level = True
        self.current_frame: Optional[TcpFrame] = None

        self._init_reset(self.reset, self.reset_active_level)

    # In case you want to get the bus first, before instantiating this
    @classmethod
    def from_entity_plus_payload_bus(
        cls,
        entity,
        prefix: str,
        tcp_payload_bus: AxiStreamBus,
    ):
        if prefix == "" or prefix is None:
            prefix_ = ""
        elif prefix[-1] == "_":
            prefix_ = f"{prefix}"
        else:
            prefix_ = f"{prefix}_"
        return cls(
            clock = getattr(entity, f"clk"),
            reset = getattr(entity, f"rst"),
            hdr_valid = getattr(entity, f"{prefix_}hdr_valid"),
            hdr_ready = getattr(entity, f"{prefix_}hdr_ready"),
            eth_dest_mac = getattr(entity, f"{prefix_}eth_dest_mac"),
            eth_src_mac = getattr(entity, f"{prefix_}eth_src_mac"),
            eth_type = getattr(entity, f"{prefix_}eth_type"),
            ip_version = getattr(entity, f"{prefix_}ip_version"),
            ip_ihl = getattr(entity, f"{prefix_}ip_ihl"),
            ip_dscp = getattr(entity, f"{prefix_}ip_dscp"),
            ip_ecn = getattr(entity, f"{prefix_}ip_ecn"),
            ip_length = getattr(entity, f"{prefix_}ip_length"),
            ip_identification = getattr(entity, f"{prefix_}ip_identification"),
            ip_flags = getattr(entity, f"{prefix_}ip_flags"),
            ip_fragment_offset = getattr(entity, f"{prefix_}ip_fragment_offset"),
            ip_ttl = getattr(entity, f"{prefix_}ip_ttl"),
            ip_protocol = getattr(entity, f"{prefix_}ip_protocol"),
            ip_header_checksum = getattr(entity, f"{prefix_}ip_header_checksum"),
            ip_source_ip = getattr(entity, f"{prefix_}ip_source_ip"),
            ip_dest_ip = getattr(entity, f"{prefix_}ip_dest_ip"),
            tcp_source_port = getattr(entity, f"{prefix_}tcp_source_port"),
            tcp_dest_port = getattr(entity, f"{prefix_}tcp_dest_port"),
            tcp_seq_num = getattr(entity, f"{prefix_}tcp_seq_num"),
            tcp_ack_num = getattr(entity, f"{prefix_}tcp_ack_num"),
            tcp_data_offset = getattr(entity, f"{prefix_}tcp_data_offset"),
            tcp_cwr = getattr(entity, f"{prefix_}tcp_cwr"),
            tcp_ece = getattr(entity, f"{prefix_}tcp_ece"),
            tcp_urg = getattr(entity, f"{prefix_}tcp_urg"),
            tcp_ack = getattr(entity, f"{prefix_}tcp_ack"),
            tcp_psh = getattr(entity, f"{prefix_}tcp_psh"),
            tcp_rst = getattr(entity, f"{prefix_}tcp_rst"),
            tcp_syn = getattr(entity, f"{prefix_}tcp_syn"),
            tcp_fin = getattr(entity, f"{prefix_}tcp_fin"),
            tcp_window_size = getattr(entity, f"{prefix_}tcp_window_size"),
            tcp_checksum =  getattr(entity, f"{prefix_}tcp_checksum"),
            tcp_urgent_pointer =  getattr(entity, f"{prefix_}tcp_urgent_pointer"),
            tcp_payload_bus = tcp_payload_bus,
        )

    async def recv(self):
        return await self.queue.get()

    def _handle_reset(self, state):
        self.payload_sink._handle_reset(state)
        if state:
            self.log.info("Reset asserted")
            if self._run_coroutine is not None:
                self._run_coroutine.kill()
                self._run_coroutine = None
            if self.current_frame is not None:
                self.log.warning("Flushed transmit frame during reset: %s", self.current_frame)
                self.current_frame = None
        else:
            self.log.info("Reset de-asserted")
            if self._run_coroutine is None:
                self._run_coroutine = cocotb.start_soon(self._run())

    async def _run(self):
        self.hdr_ready.value = True

        while True:
            await RisingEdge(self.clock)

            if self.hdr_valid.value and self.hdr_ready.value:
                self.current_frame = self._instantiate_frame_from_header_wires()
                self.hdr_ready.value = False
                self.current_frame.payload = bytes((await self.payload_sink.recv()).tdata)
                self.queue.put_nowait(self.current_frame)
                self.current_frame = None
                self.hdr_ready.value = True

    def _instantiate_frame_from_header_wires(self) -> TcpFrame:
        return TcpFrame(
            payload = b'',
            eth_dest_mac = int(self.eth_dest_mac.value),
            eth_src_mac = int(self.eth_src_mac.value),
            eth_type = int(self.eth_type.value),
            ip_version = int(self.ip_version.value),
            ip_ihl = int(self.ip_ihl.value),
            ip_dscp = int(self.ip_dscp.value),
            ip_ecn = int(self.ip_ecn.value),
            ip_length = int(self.ip_length.value),
            ip_identification = int(self.ip_identification.value),
            ip_flags = int(self.ip_flags.value),
            ip_fragment_offset = int(self.ip_fragment_offset.value),
            ip_ttl = int(self.ip_ttl.value),
            ip_protocol = int(self.ip_protocol.value),
            ip_header_checksum = int(self.ip_header_checksum.value),
            ip_source_ip = int(int(self.ip_source_ip.value)),
            ip_dest_ip = int(int(self.ip_dest_ip.value)),
            tcp_source_port = int(self.tcp_source_port.value),
            tcp_dest_port = int(self.tcp_dest_port.value),
            tcp_seq_num = int(self.tcp_seq_num.value),
            tcp_ack_num = int(self.tcp_ack_num.value),
            tcp_data_offset = int(self.tcp_data_offset),
            tcp_cwr = bool(self.tcp_cwr.value),
            tcp_ece = bool(self.tcp_ece.value),
            tcp_urg = bool(self.tcp_urg.value),
            tcp_ack = bool(self.tcp_ack.value),
            tcp_psh = bool(self.tcp_psh.value),
            tcp_rst = bool(self.tcp_rst.value),
            tcp_syn = bool(self.tcp_syn.value),
            tcp_fin = bool(self.tcp_fin.value),
            tcp_window_size = int(self.tcp_window_size.value),
            tcp_checksum = int(self.tcp_checksum),
            tcp_urgent_pointer = int(self.tcp_urgent_pointer),
            tcp_options = [],
            tcp_padding = b'',
        )
