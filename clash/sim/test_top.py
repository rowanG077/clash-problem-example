#!/usr/bin/env python
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

import itertools
import logging
import os
import subprocess

from scapy.all import Ether, IP, UDP, Raw

import pytest
import cocotb_test.simulator

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge
from cocotb.regression import TestFactory

from cocotb_lib.axis import AxiStreamBus, AxiStreamSource, AxiStreamSink
from cocotb_lib.stream import define_stream


EthHdrBus, EthHdrTransaction, EthHdrSource, EthHdrSink, EthHdrMonitor = define_stream("EthHdr",
    signals=["hdr_valid", "hdr_ready", "dest_mac", "src_mac", "type"]
)


class TB:
    def __init__(self, dut):
        self.dut = dut

        self.log = logging.getLogger("cocotb.tb")
        self.log.setLevel(logging.DEBUG)

        cocotb.start_soon(Clock(dut.clk, 8, units="ns").start())

        self.source = AxiStreamSource(AxiStreamBus.from_prefix(dut, "rx"), dut.clk, dut.rst)
        self.sink = AxiStreamSink(AxiStreamBus.from_prefix(dut, "tx"), dut.clk, dut.rst)

    def set_idle_generator(self, generator=None):
        if generator:
            self.source.set_pause_generator(generator())

    def set_backpressure_generator(self, generator=None):
        if generator:
            self.sink.set_pause_generator(generator())

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

    async def send(self, pkt):
        await self.source.send(bytes(pkt))

    async def recv(self):
        rx_frame = await self.sink.recv()
        eth = Ether(bytes(rx_frame))
        return eth

def to_axis(test_pkt, data_width):
    chunks = [
        zip(fill(True, data_width), data[i:i+data_width])
        for i in range(0, len(test_pkt), data_width)
    ]
    if len(chunks[-1]) < data_width:
        chunks[-1] = chunks[-1].ljust(data_width, (False, 0))  # Pad with \x00
    return chunks

async def run_test(dut, payload_lengths=None, payload_data=None, idle_inserter=None, backpressure_inserter=None):

    tb = TB(dut)

    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)

    test_pkts = []

    eth_hdr = Ether(
        src="0A:0A:0A:0A:0A:0A",  # MAC source: 10:10:10:10:10:10
        dst="0B:0B:0B:0B:0B:0B",  # MAC destination: 11:11:11:11:11:11
        type=0x0C0C               # EtherType: 12:12
    )

    # Define IP Header
    ip_hdr = IP(
        version=4,          # IP Version
        ihl=5,              # Internet Header Length
        tos=20,             # Type of Service: 20
        len=21,             # Total Length: 21
        id=0x1616,          # Identification: 22:22
        flags=0,            # No fragmentation flags
        frag=0,             # Fragment Offset: 23:23
        ttl=24,             # Time to Live: 24
        proto=25,           # Protocol: 25
        chksum=0x1A1A,      # Header Checksum: 26:26
        src="27.27.27.27",  # Source IP: 27:27:27:27
        dst="28.28.28.28"   # Destination IP: 28:28:28:28
    )

    # Define UDP Header
    udp_hdr = UDP(
        sport=0x1E1E,  # Source Port: 30:30
        dport=0x1F1F,  # Destination Port: 31:31
        len=0x2020,    # Length: 32:32
        chksum=0x2121  # Checksum: 33:33
    )

    # Define Payload
    payload = Raw(load=b"EXAMPLE_PAYLOAD")

    # Create the complete packet
    test_pkt = eth_hdr / ip_hdr / udp_hdr # / payload

    tb.log.info(f"Total packet length is {len(bytes(test_pkt))}")

    test_pkts.append(test_pkt.copy())
    print(" ".join(f"0x{b:02X}" for b in bytes(test_pkt)))

    await tb.send(test_pkt)
    tb.log.info("Sent packet: %s", repr(test_pkt))

    for x in range(1, 20):
        await RisingEdge(dut.clk)

def cycle_pause():
    return itertools.cycle([1, 1, 1, 0])

def size_list():
    return list(range(1, 128)) + [512, 1500, 9200] + [60-14]*10

def incrementing_payload(length):
    return bytes(itertools.islice(itertools.cycle(range(256)), length))


if cocotb.SIM_NAME:

    factory = TestFactory(run_test)
    factory.add_option("payload_lengths", [size_list])
    factory.add_option("payload_data", [incrementing_payload])
    factory.add_option("idle_inserter", [None, cycle_pause])
    factory.add_option("backpressure_inserter", [None, cycle_pause])
    factory.generate_tests()


# cocotb-test

sim_dir = os.path.abspath(os.path.dirname(__file__))
project_base_dir = os.path.join(sim_dir, '..')
clash_verilog_dir = os.path.abspath(os.path.join(project_base_dir, "verilog"))

def test_eth_axis_rx(request):
    clash_src = "Top"
    dut = clash_src.lower()
    module = os.path.splitext(os.path.basename(__file__))[0]
    toplevel = dut

    verilog_sources = [
        os.path.join(project_base_dir, "verilog", f"{clash_src}.topEntity", f"{dut}.v"),
    ]

    # sim_build = os.path.join(tests_dir, "sim_build",
    #     request.node.name.replace('[', '-').replace(']', ''))

    # subprocess.run(
    #     ["cabal", "run", "clash", "--", f"lib/{clash_src}.hs", "--verilog"],
    #     cwd=project_base_dir,
    #     check=True
    # )

    cocotb_test.simulator.run(
        python_search=[sim_dir],
        verilog_sources=verilog_sources,
        toplevel=toplevel,
        module=module,
        # sim_build=sim_build,
        # extra_env=extra_env,
    )

if __name__ == "__main__":
    test_eth_axis_rx(None)
