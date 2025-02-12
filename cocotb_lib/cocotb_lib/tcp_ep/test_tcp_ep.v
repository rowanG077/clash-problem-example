`timescale 1ns / 1ns

/*
 * Exists purely to test the tcp_ep package for cocotb tests
 */
module test_tcp_ep (
    input  wire                   clk,
    input  wire                   rst,

    // Connects to the TcpSource
    input  wire        s_hdr_valid,
    output wire        s_hdr_ready,
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
    input  wire [15:0] s_tcp_source_port,
    input  wire [15:0] s_tcp_dest_port,
    input  wire [31:0] s_tcp_seq_num,
    input  wire [31:0] s_tcp_ack_num,
    input  wire [3:0]  s_tcp_data_offset,
    input  wire        s_tcp_cwr,
    input  wire        s_tcp_ece,
    input  wire        s_tcp_urg,
    input  wire        s_tcp_ack,
    input  wire        s_tcp_psh,
    input  wire        s_tcp_rst,
    input  wire        s_tcp_syn,
    input  wire        s_tcp_fin,
    input  wire [15:0] s_tcp_window_size,
    input  wire [15:0] s_tcp_checksum,
    input  wire [15:0] s_tcp_urgent_pointer,
    input  wire [63:0] s_tcp_payload_axis_tdata,
    input  wire [7:0]  s_tcp_payload_axis_tkeep,
    input  wire        s_tcp_payload_axis_tvalid,
    output wire        s_tcp_payload_axis_tready,
    input  wire        s_tcp_payload_axis_tlast,
    input  wire        s_tcp_payload_axis_tuser,

    // Connects to the TcpSink
    output  wire        o_hdr_valid,
    input   wire        o_hdr_ready,
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
    output  wire [15:0] o_tcp_source_port,
    output  wire [15:0] o_tcp_dest_port,
    output  wire [31:0] o_tcp_seq_num,
    output  wire [31:0] o_tcp_ack_num,
    output  wire [3:0]  o_tcp_data_offset,
    output  wire        o_tcp_cwr,
    output  wire        o_tcp_ece,
    output  wire        o_tcp_urg,
    output  wire        o_tcp_ack,
    output  wire        o_tcp_psh,
    output  wire        o_tcp_rst,
    output  wire        o_tcp_syn,
    output  wire        o_tcp_fin,
    output  wire [15:0] o_tcp_window_size,
    output  wire [15:0] o_tcp_checksum,
    output  wire [15:0] o_tcp_urgent_pointer,
    output  wire [63:0] o_tcp_payload_axis_tdata,
    output  wire [7:0]  o_tcp_payload_axis_tkeep,
    output  wire        o_tcp_payload_axis_tvalid,
    input   wire        o_tcp_payload_axis_tready,
    output  wire        o_tcp_payload_axis_tlast,
    output  wire        o_tcp_payload_axis_tuser
);

assign s_hdr_ready = o_hdr_ready;
assign s_tcp_payload_axis_tready = o_tcp_payload_axis_tready;
assign o_hdr_valid = s_hdr_valid;
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
assign o_tcp_source_port = s_tcp_source_port;
assign o_tcp_dest_port = s_tcp_dest_port;
assign o_tcp_seq_num = s_tcp_seq_num;
assign o_tcp_ack_num = s_tcp_ack_num;
assign o_tcp_data_offset = s_tcp_data_offset;
assign o_tcp_cwr = s_tcp_cwr;
assign o_tcp_ece = s_tcp_ece;
assign o_tcp_urg = s_tcp_urg;
assign o_tcp_ack = s_tcp_ack;
assign o_tcp_psh = s_tcp_psh;
assign o_tcp_rst = s_tcp_rst;
assign o_tcp_syn = s_tcp_syn;
assign o_tcp_fin = s_tcp_fin;
assign o_tcp_window_size = s_tcp_window_size;
assign o_tcp_checksum = s_tcp_checksum;
assign o_tcp_urgent_pointer = s_tcp_urgent_pointer;
assign o_tcp_payload_axis_tdata = s_tcp_payload_axis_tdata;
assign o_tcp_payload_axis_tkeep = s_tcp_payload_axis_tkeep;
assign o_tcp_payload_axis_tvalid = s_tcp_payload_axis_tvalid;
assign o_tcp_payload_axis_tlast = s_tcp_payload_axis_tlast;
assign o_tcp_payload_axis_tuser = s_tcp_payload_axis_tuser;

endmodule
