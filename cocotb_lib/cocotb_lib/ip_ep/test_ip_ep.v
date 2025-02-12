`timescale 1ns / 1ns

/*
 * Exists purely to test the ip_ep package for cocotb tests
 */
module test_ip_ep (
    input  wire                   clk,
    input  wire                   rst,

    // Connects to the IpSource
    input  wire        s_ip_hdr_valid,
    output wire        s_ip_hdr_ready,
    input  wire [47:0] s_eth_dest_mac,
    input  wire [47:0] s_eth_src_mac,
    input  wire [15:0] s_eth_type,
    input  wire [3:0]  s_ip_version,
    input  wire [3:0]  s_ip_ihl,
    input  wire [5:0]  s_ip_dscp,
    input  wire [1:0]  s_ip_ecn,
    input  wire [15:0] s_ip_length,
    input  wire [15:0] s_ip_identification,
    input  wire [2:0]  s_ip_flags,
    input  wire [12:0] s_ip_fragment_offset,
    input  wire [7:0]  s_ip_ttl,
    input  wire [7:0]  s_ip_protocol,
    input  wire [15:0] s_ip_header_checksum,
    input  wire [31:0] s_ip_source_ip,
    input  wire [31:0] s_ip_dest_ip,
    input  wire [63:0] s_ip_payload_axis_tdata,
    input  wire [7:0]  s_ip_payload_axis_tkeep,
    input  wire        s_ip_payload_axis_tvalid,
    output wire        s_ip_payload_axis_tready,
    input  wire        s_ip_payload_axis_tlast,
    input  wire        s_ip_payload_axis_tuser,

    // Connects to the IpSink
    output  wire        o_ip_hdr_valid,
    input wire          o_ip_hdr_ready,
    output  wire [47:0] o_eth_dest_mac,
    output  wire [47:0] o_eth_src_mac,
    output  wire [15:0] o_eth_type,
    output  wire [3:0]  o_ip_version,
    output  wire [3:0]  o_ip_ihl,
    output  wire [5:0]  o_ip_dscp,
    output  wire [1:0]  o_ip_ecn,
    output  wire [15:0] o_ip_length,
    output  wire [15:0] o_ip_identification,
    output  wire [2:0]  o_ip_flags,
    output  wire [12:0] o_ip_fragment_offset,
    output  wire [7:0]  o_ip_ttl,
    output  wire [7:0]  o_ip_protocol,
    output  wire [15:0] o_ip_header_checksum,
    output  wire [31:0] o_ip_source_ip,
    output  wire [31:0] o_ip_dest_ip,
    output  wire [63:0] o_ip_payload_axis_tdata,
    output  wire [7:0]  o_ip_payload_axis_tkeep,
    output  wire        o_ip_payload_axis_tvalid,
    input wire          o_ip_payload_axis_tready,
    output  wire        o_ip_payload_axis_tlast,
    output  wire        o_ip_payload_axis_tuser
);

assign s_ip_hdr_ready = o_ip_hdr_ready;
assign s_ip_payload_axis_tready = o_ip_payload_axis_tready;
assign o_ip_hdr_valid = s_ip_hdr_valid;
assign o_eth_dest_mac = s_eth_dest_mac;
assign o_eth_src_mac = s_eth_src_mac;
assign o_eth_type = s_eth_type;
assign o_ip_version = s_ip_version;
assign o_ip_ihl = s_ip_ihl;
assign o_ip_dscp = s_ip_dscp;
assign o_ip_ecn = s_ip_ecn;
assign o_ip_length = s_ip_length;
assign o_ip_identification = s_ip_identification;
assign o_ip_flags = s_ip_flags;
assign o_ip_fragment_offset = s_ip_fragment_offset;
assign o_ip_ttl = s_ip_ttl;
assign o_ip_protocol = s_ip_protocol;
assign o_ip_header_checksum = s_ip_header_checksum;
assign o_ip_source_ip = s_ip_source_ip;
assign o_ip_dest_ip = s_ip_dest_ip;
assign o_ip_payload_axis_tdata = s_ip_payload_axis_tdata;
assign o_ip_payload_axis_tkeep = s_ip_payload_axis_tkeep;
assign o_ip_payload_axis_tvalid = s_ip_payload_axis_tvalid;
assign o_ip_payload_axis_tlast = s_ip_payload_axis_tlast;
assign o_ip_payload_axis_tuser = s_ip_payload_axis_tuser;


// reg        ip_hdr_ready_next;
// reg        ip_payload_axis_tready_next;
// reg        ip_hdr_valid_next;
// reg [47:0] eth_dest_mac_next;
// reg [47:0] eth_src_mac_next;
// reg [15:0] eth_type_next;
// reg [3:0]  ip_version_next;
// reg [3:0]  ip_ihl_next;
// reg [5:0]  ip_dscp_next;
// reg [1:0]  ip_ecn_next;
// reg [15:0] ip_length_next;
// reg [15:0] ip_identification_next;
// reg [2:0]  ip_flags_next;
// reg [12:0] ip_fragment_offset_next;
// reg [7:0]  ip_ttl_next;
// reg [7:0]  ip_protocol_next;
// reg [15:0] ip_header_checksum_next;
// reg [31:0] ip_source_ip_next;
// reg [31:0] ip_dest_ip_next;
// reg [63:0] ip_payload_axis_tdata_next;
// reg [7:0]  ip_payload_axis_tkeep_next;
// reg        ip_payload_axis_tvalid_next;
// reg        ip_payload_axis_tlast_next;
// reg        ip_payload_axis_tuser_next;

// assign o_ip_hdr_valid = ip_hdr_valid_next;
// assign s_ip_hdr_ready = ip_hdr_ready_next;
// assign o_eth_dest_mac = eth_dest_mac_next;
// assign o_eth_src_mac = eth_src_mac_next;
// assign o_eth_type = eth_type_next;
// assign o_ip_version = ip_version_next;
// assign o_ip_ihl = ip_ihl_next;
// assign o_ip_dscp = ip_dscp_next;
// assign o_ip_ecn = ip_ecn_next;
// assign o_ip_length = ip_length_next;
// assign o_ip_identification = ip_identification_next;
// assign o_ip_flags = ip_flags_next;
// assign o_ip_fragment_offset = ip_fragment_offset_next;
// assign o_ip_ttl = ip_ttl_next;
// assign o_ip_protocol = ip_protocol_next;
// assign o_ip_header_checksum = ip_header_checksum_next;
// assign o_ip_source_ip = ip_source_ip_next;
// assign o_ip_dest_ip = ip_dest_ip_next;
// assign o_ip_payload_axis_tdata = ip_payload_axis_tdata_next;
// assign o_ip_payload_axis_tkeep = ip_payload_axis_tkeep_next;
// assign o_ip_payload_axis_tvalid = ip_payload_axis_tvalid_next;
// assign s_ip_payload_axis_tready = ip_payload_axis_tready_next;
// assign o_ip_payload_axis_tlast = ip_payload_axis_tlast_next;
// assign o_ip_payload_axis_tuser = ip_payload_axis_tuser_next;

// always @(posedge clk) begin
//     ip_hdr_valid_next <= s_ip_hdr_valid;
//     ip_hdr_ready_next <= o_ip_hdr_ready;
//     eth_dest_mac_next <= s_eth_dest_mac;
//     eth_src_mac_next <= s_eth_src_mac;
//     eth_type_next <= s_eth_type;
//     ip_version_next <= s_ip_version;
//     ip_ihl_next <= s_ip_ihl;
//     ip_dscp_next <= s_ip_dscp;
//     ip_ecn_next <= s_ip_ecn;
//     ip_length_next <= s_ip_length;
//     ip_identification_next <= s_ip_identification;
//     ip_flags_next <= s_ip_flags;
//     ip_fragment_offset_next <= s_ip_fragment_offset;
//     ip_ttl_next <= s_ip_ttl;
//     ip_protocol_next <= s_ip_protocol;
//     ip_header_checksum_next <= s_ip_header_checksum;
//     ip_source_ip_next <= s_ip_source_ip;
//     ip_dest_ip_next <= s_ip_dest_ip;
//     ip_payload_axis_tdata_next <= s_ip_payload_axis_tdata;
//     ip_payload_axis_tkeep_next <= s_ip_payload_axis_tkeep;
//     ip_payload_axis_tvalid_next <= s_ip_payload_axis_tvalid;
//     ip_payload_axis_tready_next <= o_ip_payload_axis_tready;
//     ip_payload_axis_tlast_next <= s_ip_payload_axis_tlast;
//     ip_payload_axis_tuser_next <= s_ip_payload_axis_tuser;
// end

endmodule
