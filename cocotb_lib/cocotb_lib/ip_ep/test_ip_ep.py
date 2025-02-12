#!/usr/bin/env python

import logging
import os
import sys

import cocotb_test.simulator
import pytest

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge

this_dir = os.path.abspath(os.path.dirname(__file__))
sys.path.insert(0, this_dir)
import ip_ep

from cocotb_lib.axis import AxiStreamBus, AxiStreamMonitor
from ip_ep import IpFrameSource, IpFrameSink, IpFrame

class TB:
    def __init__(self, dut):
        self.dut = dut

        self.log = logging.getLogger("cocotb.tb")
        self.log.setLevel(logging.DEBUG)

        cocotb.start_soon(Clock(dut.clk, 2, units="ns").start())

        source_ip_payload_bus = AxiStreamBus.from_prefix(dut, "s_ip_payload_axis")
        self.source = IpFrameSource.from_entity_plus_payload_bus(
            dut, "s_", source_ip_payload_bus,
        )

        sink_ip_payload_bus = AxiStreamBus.from_prefix(dut, "o_ip_payload_axis")
        self.sink = IpFrameSink.from_entity_plus_payload_bus(
            dut, "o_", sink_ip_payload_bus,
        )

    async def reset(self):
        self.dut.rst.setimmediatevalue(0)
        await RisingEdge(self.dut.clk)
        await RisingEdge(self.dut.clk)
        self.dut.rst.value = 1
        await RisingEdge(self.dut.clk)
        await RisingEdge(self.dut.clk)
        self.dut.rst.value = 0
        await RisingEdge(self.dut.clk)
        await RisingEdge(self.dut.clk)

@cocotb.test()
async def ip_source_and_sink_can_be_used_to_pass_ip_frame(dut):

    tb = TB(dut)
    await tb.reset()

    test_ip_frame = IpFrame.with_defaults(
        payload = b'1234',
    )

    tb.log.info(f"Sending test IP frame:\n{test_ip_frame} ...")

    await tb.source.send(test_ip_frame)
    tb.log.info(f"... test IP frame sent")
    tb.log.info(f"Attempting to receive IP frame ...")
    received_frame = await tb.sink.recv()
    tb.log.info(f"... IP frame received")
    assert test_ip_frame == received_frame

    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)

@cocotb.test()
async def ip_source_and_sink_can_be_used_to_pass_multiple_short_ip_frames(dut):

    tb = TB(dut)
    await tb.reset()

    test_ip_frames = [ 
        IpFrame.with_defaults(f"{x}".encode('ascii'), x, x) for x in range(1, 7) 
    ]

    for frame in test_ip_frames:
        await tb.source.send(frame)

    received_ip_frames = [ await tb.sink.recv() for _frame in test_ip_frames ]
    assert test_ip_frames == received_ip_frames

    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)

@cocotb.test()
async def ip_source_and_sink_can_be_used_to_pass_long_ip_frame(dut):

    tb = TB(dut)
    await tb.reset()

    payload = bytes([x % 256 for x in range(16)])
    test_ip_frame = IpFrame.with_defaults(
        payload = payload,
    )

    tb.log.info(f"Test IP frame:\n{test_ip_frame}")

    await tb.source.send(test_ip_frame)
    received_frame = await tb.sink.recv()
    assert test_ip_frame == received_frame

    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)

@cocotb.test()
async def ip_source_and_sink_can_be_used_to_pass_multiple_long_ip_frames(dut):

    tb = TB(dut)
    await tb.reset()

    test_ip_frames = [ 
        IpFrame.with_defaults((f"{x}" * 20).encode('ascii'), x, x) 
        for x in range(1,3)
    ]

    for frame in test_ip_frames:
        await tb.source.send(frame)

    received_ip_frames = [ await tb.sink.recv() for _frame in test_ip_frames ]

    assert test_ip_frames == received_ip_frames

    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)

def test_ip_ep(request):

    dut = "test_ip_ep"
    module = os.path.splitext(os.path.basename(__file__))[0]
    toplevel = dut

    verilog_sources = [
        os.path.join(this_dir, f"{dut}.v"),
    ]

    sim_build = os.path.join(this_dir, "sim_build", "build")

    cocotb_test.simulator.run(
        python_search=[this_dir],
        verilog_sources=verilog_sources,
        toplevel=toplevel,
        module=module,
        sim_build=sim_build,
    )

if __name__ == "__main__":
    test_ip_ep(None)

