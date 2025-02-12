from dataclasses import dataclass
from typing import Optional

import cocotb
import logging 

from cocotb.queue import Queue, QueueFull
from cocotb.handle import ModifiableObject
from cocotb.triggers import RisingEdge, Event

from cocotb_lib.reset import Reset
from cocotb_lib.axis import AxiStreamSink, AxiStreamSource, AxiStreamBus

@dataclass 
class IpFrame:
    payload: bytes
    eth_dest_mac: int
    eth_src_mac: int
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
        frame = cls(
            payload,
            eth_dest_mac,
            eth_src_mac,
            eth_type,
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
            ip_header_checksum,
            ip_source_ip,
            ip_dest_ip,
        )
        return frame
        
    @staticmethod
    def get_ip_length_from_fields(
        payload: bytes, 
    ) -> int:
        return len(payload) + 20

    @staticmethod
    def get_ip_header_checksum_from_fields(
        ip_version: int,
        ip_ihl: int,
        ip_dscp: int,
        ip_ecn: int,
        ip_length: int,
        ip_identification: int,
        ip_flags: int,
        ip_fragment_offset: int,
        ip_ttl: int,
        ip_protocol: int,
        ip_source_ip: int,
        ip_dest_ip: int,
    ) -> int:
        cksum = ip_version << 12 | ip_ihl << 8 | ip_dscp << 2 | ip_ecn
        cksum += ip_length
        cksum += ip_identification
        cksum += ip_flags << 13 | ip_fragment_offset
        cksum += ip_ttl << 8 | ip_protocol
        cksum += ip_source_ip & 0xffff
        cksum += (ip_source_ip >> 16) & 0xffff
        cksum += ip_dest_ip & 0xffff
        cksum += (ip_dest_ip >> 16) & 0xffff
        cksum = (cksum & 0xffff) + (cksum >> 16)
        cksum = (cksum & 0xffff) + (cksum >> 16)
        return ~cksum & 0xffff

    def get_checksum(self) -> int:
        return IpFrame.get_ip_header_checksum_from_fields(
            self.ip_version,
            self.ip_ihl,
            self.ip_dscp,
            self.ip_ecn,
            self.ip_length,
            self.ip_identification,
            self.ip_flags,
            self.ip_fragment_offset,
            self.ip_ttl,
            self.ip_protocol,
            self.ip_source_ip,
            self.ip_dest_ip,
        )

@dataclass
class IpFrameSource(Reset):
    clock: ModifiableObject
    reset: ModifiableObject
    ip_hdr_valid: ModifiableObject
    ip_hdr_ready: ModifiableObject
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
    ip_payload_bus: AxiStreamBus

    @classmethod
    def from_entity(
        cls,
        entity,
        prefix: str,
    ):
        if prefix == "" or prefix is None:
            prefix_ = ""
        elif prefix[-1] == "_":
            prefix_ = f"{prefix}"
        else:
            prefix_ = f"{prefix}_"

        ip_payload_bus = AxiStreamBus.from_prefix(dut, prefix)

        return cls(
            clock = getattr(entity, f"{prefix_}clk"),
            reset = getattr(entity, f"{prefix_}rst"),
            ip_hdr_valid = getattr(entity, f"{prefix_}ip_hdr_valid"),
            ip_hdr_ready = getattr(entity, f"{prefix_}ip_hdr_ready"),
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
            ip_payload_axis_tdata = getattr(entity, f"{prefix_}ip_payload_axis_tdata"),
            ip_payload_axis_tkeep = getattr(entity, f"{prefix_}ip_payload_axis_tkeep"),
            ip_payload_axis_tvalid = getattr(entity, f"{prefix_}ip_payload_axis_tvalid"),
            ip_payload_axis_tready = getattr(entity, f"{prefix_}ip_payload_axis_tready"),
            ip_payload_axis_tlast = getattr(entity, f"{prefix_}ip_payload_axis_tlast"),
            ip_payload_axis_tuser = getattr(entity, f"{prefix_}ip_payload_axis_tuser"),    
            ip_payload_bus = ip_payload_bus,
        )

    # In case you want to get the bus first, before instantiating this
    @classmethod
    def from_entity_plus_payload_bus(
        cls,
        entity,
        prefix: str,
        ip_payload_bus: AxiStreamBus,
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
            ip_hdr_valid = getattr(entity, f"{prefix_}ip_hdr_valid"),
            ip_hdr_ready = getattr(entity, f"{prefix_}ip_hdr_ready"),
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
            ip_payload_bus = ip_payload_bus,
        )

    def __post_init__(self):
        self.log = logging.getLogger(f"cocotb.{self.ip_payload_bus.tdata._path}")
        self._run_coroutine = None
        self.queue = Queue()
        self.payload_source = AxiStreamSource(self.ip_payload_bus, self.clock, self.reset)
        self.reset_active_level = True
        self.current_frame: Optional[IpFrame] = None

        self._init_reset(self.reset, self.reset_active_level)

    async def send(self, frame: IpFrame):
        self.queue.put_nowait(frame)

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
        self.active = False
        frame = None
        
        while True:
            await RisingEdge(self.clock)

            if (not self.queue.empty()):
                self.current_frame = self.queue.get_nowait() 
                await self._send_header(self.current_frame)
                await self.payload_source.finish_sending(self.current_frame.payload)

    async def _send_header(self, frame: IpFrame):
        header_has_been_sent = False

        self.ip_hdr_valid.value = True
        self._set_header_wires_from_frame(frame)

        while not header_has_been_sent:
            await RisingEdge(self.clock)
            header_has_been_sent = self.ip_hdr_ready.value
            self.ip_hdr_valid.value = not header_has_been_sent

    def _set_header_wires_from_frame(self, frame: IpFrame):
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
        
@dataclass
class IpFrameSink(Reset):
    clock: ModifiableObject
    reset: ModifiableObject
    ip_hdr_valid: ModifiableObject
    ip_hdr_ready: ModifiableObject
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
    ip_payload_bus: AxiStreamBus
    
    def __post_init__(self):
        self.log = logging.getLogger(f"cocotb.{self.ip_payload_bus.tdata._path}")
        self._run_coroutine = None
        self.queue = Queue()
        self.payload_sink = AxiStreamSink(self.ip_payload_bus, self.clock, self.reset)
        self.reset_active_level = True
        self.current_frame: Optional[IpFrame] = None

        self._init_reset(self.reset, self.reset_active_level)

    # In case you want to get the bus first, before instantiating this
    @classmethod
    def from_entity_plus_payload_bus(
        cls,
        entity,
        prefix: str,
        ip_payload_bus: AxiStreamBus,
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
            ip_hdr_valid = getattr(entity, f"{prefix_}ip_hdr_valid"),
            ip_hdr_ready = getattr(entity, f"{prefix_}ip_hdr_ready"),
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
            ip_payload_bus = ip_payload_bus,
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
        self.ip_hdr_ready.value = True

        while True:
            await RisingEdge(self.clock)

            if self.ip_hdr_valid.value and self.ip_hdr_ready.value:
                self.current_frame = self._instantiate_frame_from_header_wires()
                self.ip_hdr_ready.value = False
                self.current_frame.payload = bytes((await self.payload_sink.recv()).tdata)
                self.queue.put_nowait(self.current_frame)
                self.current_frame = None
                self.ip_hdr_ready.value = True

    def _instantiate_frame_from_header_wires(self) -> IpFrame:
        return IpFrame(
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
        )


