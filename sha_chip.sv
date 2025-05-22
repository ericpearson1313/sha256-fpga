// FIPS 180-4  SHA256 FPGA

`timescale 1ns / 1ps
module sha_chip
(
	// Input Buttons
	input  logic arm_button,
	input  logic fire_button,

	// Output LED/SPK
	output logic arm_led_n,
	output logic cont_led_n,
	output logic speaker,
	output logic speaker_n,
	
	// Bank 1A: Analog Inputs / IO
	output [8:1] anain,
	
	// Bank 7, future serial port
	inout [6:0] digio,
	
	// Bank 1B Rs232
	input 		rx232,
	output 		tx232,
	
	// High Voltage 
	output logic lt3420_charge,
	input  logic lt3420_done,
	output logic pwm,	
	output logic dump,
	input  logic cont_n,
	
	// External A/D Converters (2.5v)
	output logic        ad_cs,
	output logic		  ad_sclk,
	input  logic  [1:0] ad_sdata_a,
	input  logic  [1:0] ad_sdata_b,
	input  logic        CIdiag,
	input  logic        CVdiag,
	input  logic        LIdiag,
	input  logic 		  LVdiag,
	
	// External Current Control Input
	input	 logic  [2:0] iset, // Current target in unit amps  
	
	// SPI8 Bus
	inout  wire [7:0]  spi8_data_pad,   //   pad_io.export
	inout  wire spi_clk0,
	inout  wire spi_ncs,
	inout  wire spi_ds,
	inout  wire spi_nrst,
	
	// HDMI Output 1 (Tru LVDS)
	output logic		hdmi_d0,
	output logic		hdmi_d1,
	output logic		hdmi_d2,
	output logic      hdmi_ck,

	// HDMI Output 2 (Tru LVDS)
	output logic		hdmi2_d0,
	output logic		hdmi2_d1,
	output logic		hdmi2_d2,
	output logic      hdmi2_ck,
	
	// Input clock, reset
	output logic clk_out, // Differential output
	input logic clk_in,	// Reference 48Mhz or other
	input logic reset_n
);

/////////////////////
//
// Unused IO Tie-off/Turn off
//		Extdev may/may_not be present
//
/////////////////////

	// Turn off leds speaker
	assign arm_led_n	= 1'b0; 
	assign cont_led_n	= 1'b0;
	//assign speaker		= 1'b0;
	//assign speaker_n	= 1'b0;
	
	// Float future comm port
	assign digio = 7'bzzz_zzzz;
	
	// Rs232
	assign tx232 = rx232; // wire through
	
	// Safe the High Voltage 
	assign lt3420_charge = 1'b0;
	assign  pwm 			= 1'b0;
	assign  dump 			= 1'b1; // turn on dump for safety
	
	// Tie off Turn off A/D Converters 
	assign ad_cs 	= 1'b0;
	assign ad_sclk = 1'b0;
	
	// Tie off/turn off SPI8 Bus
	assign spi8_data_pad = 8'bzzzz_zzzz;
	assign spi_clk0 	= 1'b0;
	assign spi_ncs 	= 1'b1;
	assign spi_ds 		= 1'bz;
	assign spi_nrst 	= 1'b0;


/////////////////////
//
// Clock and Reset
//
/////////////////////


// PLL (only 1 PLL in E144 package!)

logic clk;	// global 48Mhz clock
logic clk4; // global 192MhZ spi8 clk
logic hdmi_clk; 	// Pixel clk, apparentlyi can support 720p
logic hdmi_clk5;  // 5x pixel clk clock for data xmit, 10b*3=30/3lanes=10ddr=5 

trial_pll _spll(
	.inclk0 (clk_in),		// External clock input
	.c0     (clk_out), 	// Flash Clock 6Mhz, also External clock output differential
	.c1	  (clk),			// Global Clock ADC rate 48 Mhz
	.c2	  (clk4),		// Global Clock SPI8 rate 192 Mhz
	.c3	  (hdmi_clk),	// HDMI pixel clk
	.c4	  (hdmi_clk5)  // HDMI ddr clock 5x
	);
	
// assign ad_sclk  = !clk;		// TODO: Ren-enable if ADC is used. Impotant that its inverterted!!!!

// delayed from fpga config and external reset d-assert

logic [3:0] reset_shift = 0; // initial value upon config
always @(posedge clk) begin
		if( !reset_n ) begin
			reset_shift <= 4'h0;
		end else begin
			if( reset_shift != 4'HF ) begin
				reset_shift[3:0] <= reset_shift[3:0] + 4'h1;
			end else begin
				reset_shift[3:0] <= reset_shift[3:0];
			end
		end
end

logic reset;
assign reset = (reset_shift[3:0] != 4'hF) ? 1'b1 : 1'b0; // reset de-asserted after all bit shifted in 


/////////////////////
//
// Debug LEDs anain[8:1]
//
/////////////////////	

assign anain[3:1] = iset[2:0]; // active low switch inputs
assign anain[4] = !reset;
logic [24:0] count;
always @(posedge clk4) begin
	count <= count + 1;
end
assign anain[8:5] = count[24:21];
assign anain[8]=count[24];

// Fire Button

logic fire_button_debounce;
logic fbd_delay;
logic short_fire;
logic long_fire; // fire button held down >1 wsec

debounce _firedb ( .clk( clk ), .reset( reset ), .in( fire_button ), .out( fire_button_debounce ), .long( long_fire ));

always @(posedge clk) begin
	fbd_delay <= fire_button_debounce;
	short_fire <= fire_button_debounce & !fbd_delay;
end

// Speaker C5 to C6
logic [15:0] tone_cnt;
logic cont_tone;
logic spk_toggle;

logic beep;
logic [19:0] beep_cnt; // 1/50th-ish sec
always @(posedge clk)
	if( beep_cnt == 0 ) 
		beep_cnt <= ( beep ) ? 1 : 0;
	else
		beep_cnt <= beep_cnt + 1;


always @(posedge clk) begin
	if( tone_cnt == 0 ) begin
		spk_toggle <= !spk_toggle;
		tone_cnt   <= ( fire_button_debounce  ) ? { 16'h2CCA } /* C5 */ : 
								   //( key == 5'h12 ) ? { 16'h27E7 } /* D5 */ :
								   //( key == 5'h13 ) ? { 16'h238D } /* E5 */ :
								   //( key == 5'h14 ) ? { 16'h218E } /* F5 */ :
								   //( key == 5'h15 ) ? { 16'h1DE5 } /* G5 */ :
								   //( key == 5'h16 ) ? { 16'h1AA2 } /* A5 */ :
								   ( beep_cnt != 0 ) ? { 16'h17BA } /* B5 */ :
								   //( key == 5'h18 ) ? { 16'h1665 } /* C6 */ : 
														                0; // mute
	end else begin
		tone_cnt <= tone_cnt - 1;
		spk_toggle <= spk_toggle;
	end
end

assign speaker = spk_toggle; 
assign speaker_n = !speaker;

/////////////////////
//
// TOP Level CHIP code
//
/////////////////////	







	// State machine
	logic get_msg; // get a new message 
	logic ld_msg;	// Load message into sha unit
	logic	msg_idx; 
	logic sha_go;
	logic [1:0] mode; 
	logic hit;	// tags when hash2 hits
	logic ld_hit; // loads nonce register  with delayed nonce
	
	
	localparam MODE_INIT = 1;	// Will init with H* at start (both input and REG)
	localparam MODE_HASH = 0;	// starts with REG and will Update reg at END (Normal steady state(
	localparam MODE_REDO = 3;  // starts with Reg, but discards value at end (keeping REG unaltered for REDO
		
	// Press and hold (long push) turns on continuous mode and the button can be released.
	// Press button again to stop
	logic continuous;
	logic long_fire_del1;
	always_ff @(posedge clk) begin
		long_fire_del1 <= long_fire;
		continuous <=  ( reset ) ? 1'b0 : 
							( long_fire && !long_fire_del1 ) ? 1'b1 : // rising edge of longfire starts continuous
							( short_fire ) ? 1'b0 : 
							          !hit & continuous;
	end

	assign beep = hit & continuous;										
	assign sha_go = short_fire || continuous;
	
`define PIPE12
`ifdef PIPE12	
	// Control Logic
	localparam BL = 6;   // Burst Length
	localparam PD = BL*6; // Pipe Delay
	// PD cycles to do BL rounds
	logic [7:0] state_count;
	always_ff @(posedge clk) begin
		if( reset ) begin
			state_count <= 0;
		end else begin
			if( state_count == 0 ) begin
				state_count <= ( sha_go ) ? 1 : 0;
			end else if( state_count == (3*PD) ) begin
				state_count <= ( sha_go ) ? (2*PD+1) : 0; // redo or last pass after hit
			end else begin
				state_count <= state_count + 1;
			end
		end
	end
	
	assign  ld_msg = ( ( state_count >= (0*PD+1) && state_count <= (0*PD+BL) ) ||
							 ( state_count >= (1*PD+1) && state_count <= (1*PD+BL) ) ||
							 ( state_count >= (2*PD+1) && state_count <= (2*PD+BL) ) ) ? 1'b1 : 1'b0;
	assign mode    =   ( state_count >= (0*PD+1) && state_count <= (0*PD+BL) ) ? MODE_INIT :
							 ( state_count >= (1*PD+1) && state_count <= (1*PD+BL) ) ? MODE_HASH :
							 ( state_count >= (2*PD+1) && state_count <= (2*PD+BL) ) ? MODE_REDO : 0;
	assign msg_idx =   ( state_count >=       1  && state_count <=       BL  ) ? 1'b0 : 1'b1;
`endif // pipe12

`ifdef PIPE6	
	logic [7:0] state_count;
	always_ff @(posedge clk) begin
		if( reset ) begin
			state_count <= 0;
		end else begin
			if( state_count == 0 ) begin
				state_count <= ( sha_go ) ? 1 : 0;
			end else if( state_count == 108 ) begin
				state_count <= ( sha_go ) ? 73 : 0; // redo or last pass after hit
			end else begin
				state_count <= state_count + 1;
			end
		end
	end
	
	assign  ld_msg = ( ( state_count >=  1 && state_count <=  6 ) ||
							 ( state_count >= 37 && state_count <= 42 ) ||
							 ( state_count >= 73 && state_count <= 78 ) ) ? 1'b1 : 1'b0;
	assign mode    =   ( state_count >=  1 && state_count <=  6 ) ? MODE_INIT :
							 ( state_count >= 37 && state_count <= 42 ) ? MODE_HASH :
							 ( state_count >= 73 && state_count <= 78 ) ? MODE_REDO : 0;
	assign msg_idx =   ( state_count >=  1 && state_count <=  6 ) ? 1'b0 : 1'b1;
`endif // pipe6

	// Padded Input Message (2 blocks)
	logic [0:1][0:63][7:0] bc_msg; // endian byte stram
	logic [0:1][0:15][31:0] in_msg;	// SHA input format
	assign bc_msg[0] = 512'h000000014cc2c57c7905fd399965282c87fe259e7da366e035dc087a0000141f000000006427b6492f2b052578fb4bc23655ca4e8b9e2b9b69c88041b2ac8c77;
	assign bc_msg[1] = 512'h1571d1be4de695931a2694217a33330e000000800000000000000000000000000000000000000000000000000000000000000000000000000000000080020000;

	always_comb begin
		for( int bb = 0; bb < 2; bb++ ) // block
			for( int ww = 0; ww < 16; ww++ ) // 32 bit words
				in_msg[bb][ww][31:0] = { bc_msg[bb][ww*4+3], bc_msg[bb][ww*4+2], bc_msg[bb][ww*4+1], bc_msg[bb][ww*4+0] };
	end
	// Extract difficulty
	logic [255:0] difficulty;
	assign difficulty = bc_msg[1][9:11]<<((bc_msg[1][8]-3)<<3);
	
	// Nonce counter generator
	reg [31:0] nonce = 0;//32'h7a33330e; // orig endian hit;
	always_ff @(posedge clk) 
		nonce <= ( ld_msg ) ? nonce + 1 : nonce;

	
	logic ovalid;
	logic [255:0] sha_out;	
	//sha_core _sha_core (
	//	.clk ( clk ),
	//	.reset( reset ),
	//	// Input strobe and message
	//	.in_valid( ld_msg ),
	//	.mode( mode ),
	//	.message( (msg_idx) ? ibuf[1] : ibuf[0] ),
	//	// Output 
	//	.out_valid( ovalid ),
	//	.hash( sha_out )
	//);

	sha_11_6_core _sha_core (
//	sha_11_12_core _sha_core (
//	sha_11_11_core _sha_core (
		.clk ( clk ),
		.reset( 1'b0 ), // use por defaults
		// Input strobe and message
		.i_valid( ld_msg ),
		.i_mode( mode ),
		.i_data( (msg_idx) ? 
						{ in_msg[1][0:2], 
						  nonce[7:0], nonce[15:8], nonce[23:16], nonce[31:24], 
						  in_msg[1][4:15] } : in_msg[0] ),
		// Output 
		.o_valid( ovalid ),
		.o_data( sha_out )
	);
	

	// Latch output hash for display
	// 256 bit sha2 message hash
	reg [255:0] hash = {256{1'b1}};
	always_ff @(posedge clk) 
		hash <= ( ovalid ) ? sha_out : hash;

	logic ovalid2;
	logic [255:0] sha_out2;
	//sha_core _sha_core2 (
	//	.clk ( clk ),
	//	.reset( reset ),
	//	// Input strobe and message
	//	.in_valid( ovalid ),
	//	.mode ( MODE_INIT ),
	//	.message( { sha_out, 256'h80000000_00000000_00000000_00000000_00000000_00000000_00000000_00000100 } ),
	//	// Output 
	//	.out_valid( ovalid2 ),
	//	.hash( sha_out2 )
	//);	

	sha_11_6_core _sha_core2 (
//	sha_11_12_core _sha_core2 (
//	sha_11_11_core _sha_core2 (
		.clk ( clk ),
		.reset( 1'b0 ), // use por defaults
		// Input strobe and message
		.i_valid( ovalid ),
		.i_mode( MODE_INIT ),
		.i_data( { sha_out, 256'h80000000_00000000_00000000_00000000_00000000_00000000_00000000_00000100 } ),
		// Output 
		.o_valid( ovalid2 ),
		.o_data( sha_out2 )
	);	
	
	// Latch output hash for display
	logic [0:7][31:0] hash2 = {256{1'b1}};
	always_ff @(posedge clk) 
		hash2 <= ( ovalid2 ) ? sha_out2 : hash2;
		
	logic [7:0][31:0] hash_word;
	
	always_comb 
		for( int ii = 0; ii < 8; ii++ )
			hash_word[ii] = hash2[ii];  // swaps word order to create the protocol number [255:0] word
	
	
	assign hit = ( ovalid2 && sha_out2[31:0] == 0 ) ? 1'b1 : 1'b0; 
	
	logic [31:0] hit_nonce;
	logic hit_flag;
	always_ff @(posedge clk) begin
		if( reset ) begin
			hit_flag <= 0;
			hit_nonce <= 0;
		end else begin
			hit_flag <= ( hit & continuous ) ? 1'b1 : hit_flag;
			hit_nonce <= ( hit_flag ) ? hit_nonce : nonce - 12;
		end
	end

	///////////////////////////////////////
	// Stat counter timer and rate counters
	///////////////////////////////////////
	
	logic inc_stat;
	assign inc_stat = ovalid2; //(state_count == 100); // doing 1 has calc, guaranteed
	logic [47:0] op_count;
	always_ff@( posedge clk ) begin
		op_count <= ( inc_stat ) ? op_count + 1 : op_count;
	end
	
	logic [25:0] second_count;	// clk = 48Mhz osc
	logic 	    second_tick; // 1 pulse / sec
	always_ff @(posedge clk) begin
		second_count <= ( second_count == 26'd48_000_000 - 1 ) ? 26'd0 : second_count + 1;
		second_tick <= ( second_count == 26'd0 ) ? 1'b1 : 1'b0;
	end
	
	logic [31:0] oppersec_latch;
	logic [31:0] oppersec_count;
	logic [3:0] sec_del;
	always_ff @(posedge clk) begin
		sec_del[3:0] <= { sec_del[2:0], second_tick };
		if( sec_del[2] && !sec_del[3] ) begin // second pulse rising edge
			oppersec_latch <= oppersec_count;
			oppersec_count <= ( inc_stat ) ? 1 : 0;
		end else begin
			oppersec_latch <= oppersec_latch;
			oppersec_count <= ( inc_stat ) ? oppersec_count + 1 : oppersec_count;
		end
	end
	
	/////////////////////////////////
	////
	////       VIDEO
	////
	//////////////////////////////////
`define WVGA
`ifdef WVGA
	// HDMI reset
	logic [3:0] hdmi_reg;
	always @(posedge hdmi_clk) begin
		hdmi_reg[3:0] <= { hdmi_reg[2:0], reset };
	end
	logic hdmi_reset;
	assign hdmi_reset = hdmi_reg[3];
	
	logic video_preamble;
	logic data_preamble;
	logic video_guard;
	logic data_guard;
	logic data_island;
	
	// XVGA 800x480x60hz sych generator
	logic blank, hsync, vsync;
	vga_800x480_sync _sync
	(
		.clk(   hdmi_clk   ),	
		.reset( reset ),
		.blank( blank ),
		.hsync( hsync ),
		.vsync( vsync ),
		// HDMI encoding controls
		.video_preamble( video_preamble ),
		.data_preamble ( data_preamble  ),
		.video_guard   ( video_guard    ),
		.data_guard    ( data_guard     ),
		.data_island   ( data_island    )
	);
	

	// Font Generator
	logic [7:0] char_x, char_y;
	logic [255:0] ascii_char;
	logic [15:0] hex_char;
	logic [1:0] bin_char;
	ascii_font57 _font
	(
		.clk( hdmi_clk ),
		.reset( reset ),
		.blank( blank ),
		.hsync( hsync ),
		.vsync( vsync ),
		.char_x( char_x ), // 0 to 105 chars horizontally
		.char_y( char_y ), // o to 59 rows vertically
		.hex_char   ( hex_char ),
		.binary_char( bin_char ),
		.ascii_char ( ascii_char )	
	);

	// test pattern gen
	logic [7:0] test_red, test_green, test_blue;
	test_pattern _testgen 
	(
		.clk( hdmi_clk  ),
		.reset( reset ),
		.blank( blank ),
		.hsync( hsync ),
		.vsync( vsync ),
		.red	( test_red   ),
		.green( test_green ),
		.blue	( test_blue  )
	);	
	
	// Flash Memory interface (init font and text overlay)
	// the serial interface runs at 6 Mhz (max 7 Mhz!)
	// we assigned c0 the output diff pair clock to this interface.
	
	logic [11:0] 	flash_addr; // 32 bit word address, 16Kbytes total flash for M04
	logic 			flash_read;
	logic				flash_data;
	logic 			flash_wait;
	logic 			flash_valid;
	ufm_flash _flash (
		.clock						( clk_out 			 ), // 6 Mhz
		.avmm_data_addr			( flash_addr[11:0] ), // word address 
		.avmm_data_read			( flash_read 		 ),
		.avmm_data_readdata		( flash_data 		 ),
		.avmm_data_waitrequest	( flash_wait 		 ),
		.avmm_data_readdatavalid( flash_valid 		 ),
		.avmm_data_burstcount	( 128 * 32 			 ), // 4K bit burst
		.reset_n						( !reset 			 )
	);	
	
	// Text Overlay (from flash rom)
	// Important to put commit hash in flash, 
	// otherwise influences the build reproduction logic
	
	logic text_ovl;
	logic [3:0] text_color;
	text_overlay _text
	(
		.clk( hdmi_clk  ),
		.reset( reset ),
		.blank( blank ),
		.hsync( hsync ),
		.vsync( vsync ),
		// Overlay output bit for ORing
		.overlay( text_ovl ),
		.color( text_color ),
		// Avalon bus to init font and text rams
		.flash_clock( clk_out 			 ), // 6 Mhz
		.flash_addr ( flash_addr[11:0] ), // word address 
		.flash_read ( flash_read 		 ),
		.flash_data ( flash_data 		 ),
		.flash_wait ( flash_wait 		 ),
		.flash_valid( flash_valid 		 )
	);


	// Overlay Text - Dynamic
	logic [31:0] id_str;
	//string_overlay #(.LEN(18)) _id0(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.ascii_char(ascii_char), .x('h48), .y('h09), .out( id_str[0]), .str( "FIPS 180-4 SHA-256" ) );
	//hex_overlay    #(.LEN(12 )) _id1(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.hex_char(hex_char), .x('h50),.y('d58), .out( id_str[1]), .in( op_count[47:0] ) );
   //bin_overlay    #(.LEN(1 )) _id2(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.bin_char(bin_char), .x('h46),.y('h09), .out( id_str[2]), .in( disp_id == 32'h0E96_0001 ) );
	//string_overlay #(.LEN(14)) _id3(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.ascii_char(ascii_char), .x('d119),.y('d58), .out( id_str[3]), .str( "commit 0123abc" ) );
	//hex_overlay    #(.LEN(8 )) _id4(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.hex_char(hex_char), .x('h50),.y('d54), .out( id_str[4]), .in( oppersec_latch[31:0] ) );
	//string_overlay #(.LEN(16)) _id5(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.ascii_char(ascii_char), .x('h48), .y('d56), .out( id_str[5]), .str( "Total Operations" ) );
	//string_overlay #(.LEN(14)) _id6(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.ascii_char(ascii_char), .x('h48), .y('d52), .out( id_str[6]), .str( "Operations/sec" ) );

	// Display two 512 bit message blocks and 256 bit output hash
	//hex_overlay #(.LEN(128)) _id7(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.hex_char(hex_char), .x('d1 ), .y('d16), .out( id_str[7]), .in( ibuf[0] ) );
	//hex_overlay #(.LEN(128)) _id8(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.hex_char(hex_char), .x('d1 ), .y('d18), .out( id_str[8]), .in( ibuf[1] ) );
	//hex_overlay #(.LEN(64 )) _id9(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.hex_char(hex_char), .x('d1 ), .y('d20), .out( id_str[9]), .in( hash    ) );
	//hex_overlay #(.LEN(64 )) _id10(.clk(hdmi_clk),.reset(reset), .char_x(char_x), .char_y(char_y),.hex_char(hex_char), .x('d1 ), .y('d22), .out( id_str[10]),.in( hash2    ) );
	//hex_overlay #(.LEN(64 )) _id11(.clk(hdmi_clk),.reset(reset), .char_x(char_x), .char_y(char_y),.hex_char(hex_char), .x('d1 ), .y('d24), .out( id_str[11]),.in( hash_word  ) );
	//hex_overlay #(.LEN(64 )) _id12(.clk(hdmi_clk),.reset(reset), .char_x(char_x), .char_y(char_y),.hex_char(hex_char), .x('d1 ), .y('d26), .out( id_str[12]),.in( difficulty  ) );
	
	//hex_overlay #(.LEN( 8 )) _id13(.clk(hdmi_clk),.reset(reset), .char_x(char_x), .char_y(char_y),.hex_char(hex_char), .x('d68), .y('d20), .out( id_str[13]),.in( nonce_pipe[1]  ) );
	hex_overlay #(.LEN( 8 )) _id14(.clk(hdmi_clk),.reset(reset), .char_x(char_x), .char_y(char_y),.hex_char(hex_char), .x('d68), .y('d24), .out( id_str[14]),.in( nonce ) );//nonce_pipe[2]  ) );
	//bin_overlay #(.LEN( 1 )) _id15(.clk(hdmi_clk),.reset(reset), .char_x(char_x), .char_y(char_y),.bin_char(bin_char), .x('d78), .y('d24), .out( id_str[15]),.in( hit  ) );

	//string_overlay #(.LEN(7)) _id16(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.ascii_char(ascii_char), .x('d68), .y('d20), .out( id_str[16]), .str( "1st SHA" ) );
	//string_overlay #(.LEN(7)) _id17(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.ascii_char(ascii_char), .x('d68), .y('d22), .out( id_str[17]), .str( "2nd SHA" ) );
	//string_overlay #(.LEN(10)) _id18(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.ascii_char(ascii_char), .x('d68), .y('d26), .out( id_str[18]), .str( "difficulty" ) );
	
	logic overlay;
	assign overlay = ( text_ovl && text_color == 0 ) | // normal text
						  (|id_str  ) ;
	
	// Overlay Color
	logic [7:0] overlay_red, overlay_green, overlay_blue;
	assign { overlay_red, overlay_green, overlay_blue } =
			( overlay ) ? 24'hFFFFFF :
			//( life_fg ) ? 24'h00c0c0 /* smpte_turquise_surf */ :
			//( life_bg ) ? 24'h1d1d1d /* smpte_eerie_black   */ :
			( text_ovl && text_color == 4'h1 ) ? 24'hf00000 :
			( text_ovl && text_color == 4'h2 ) ? 24'hFFFFFF :
			( text_ovl && text_color == 4'h3 ) ? 24'hff0000 :			
			( text_ovl && text_color == 4'h4 ) ? 24'h00ff00 :
			( text_ovl && text_color == 4'h5 ) ? 24'h0000ff :
			( text_ovl && text_color == 4'h6 ) ? 24'hc0c0c0 :
			( text_ovl && text_color == 4'h7 ) ? 24'h0000c0 :
			( text_ovl && text_color == 4'h8 ) ? 24'h00c0c0 :
			( text_ovl && text_color == 4'h9 ) ? 24'h00c000 : 
			( text_ovl && text_color == 4'hA ) ? 24'hc0c000 : 
			( text_ovl                       ) ? 24'hf0f000 : 
															 24'h000000 ;

	// video encoder
	// Simultaneous HDMI and DVI
	logic [7:0] hdmi2_data;
	logic [7:0] dvi_data;
	video_encoder _encode2
	(
		.clk  ( hdmi_clk  ),
		.clk5 ( hdmi_clk5 ),
		.reset( reset | charge ),  // battery limit during charging
		.blank( blank ),
		.hsync( hsync ),
		.vsync( vsync ),
		// HDMI encoding control
		.video_preamble( video_preamble ),
		.data_preamble ( data_preamble  ),
		.video_guard   ( video_guard    ),
		.data_guard    ( data_guard     ),
		.data_island   ( data_island    ),	
		// YUV mode input
		.yuv_mode		( 0 ), // use YUV2 mode, cheap USb capture devices provice lossless YUV2 capture mode 
		// RBG Data
		.red   ( test_red   | overlay_red   ),
		.green ( test_green | overlay_green ),
		.blue  ( test_blue  | overlay_blue  ),
		// HDMI and DVI encoded video
		.hdmi_data( hdmi2_data ),
		.dvi_data( dvi_data )
	);
		
	// HDMI 2 Output, DVI outputs
	hdmi_out _hdmi2_out ( // LDVS DDR outputs
		.outclock( hdmi_clk5 ),
		.din( dvi_data ),
		.pad_out( {hdmi2_d2, hdmi2_d1, hdmi2_d0, hdmi2_ck} ), 
		.pad_out_b( )  // true differential, _b not req
	);
	
	// HDMI 1 output, HDMI outputs, with YUV2 support
	hdmi_out _hdmi_out ( // LDVS DDR outputs
		.outclock( hdmi_clk5 ),
		.din( hdmi2_data ),
		.pad_out( {hdmi_d2, hdmi_d1, hdmi_d0, hdmi_ck} ), 
		.pad_out_b( )  // true differential, _b not req
	);
`endif
endmodule







// Debounce of pushbutton
module debounce(
	input clk,
	input reset,
	input in,
	output out,	// fixed pulse 15ms after 5ms pressure
	output long // after fire held for > 2/3 sec, until release
	);
	
	logic [25:0] count1; // total 1.3 sec
	logic [22:0] count0;
	logic [2:0] state;
	logic [2:0] meta;
	logic       inm;

	
	always @(posedge clk) { inm, meta } <= { meta, in };
	
	// State Machine	
	localparam S_IDLE 		= 0;
	localparam S_WAIT_PRESS	= 1;
	localparam S_WAIT_PULSE	= 2;
	localparam S_WAIT_LONG	= 3;
	localparam S_LONG			= 4;
	localparam S_WAIT_OFF	= 5;
	localparam S_WAIT_LOFF	= 6;
	
	always @(posedge clk) begin
		if( reset ) begin
			state <= S_IDLE;
		end else begin
			case( state )
				S_IDLE 		 :	state <= ( inm ) ? S_WAIT_PRESS : S_IDLE;
				S_WAIT_PRESS :	state <= (!inm ) ? S_IDLE       : (count1 == ( 5  * 48000 )) ? S_WAIT_PULSE : S_WAIT_PRESS;
				S_WAIT_PULSE :	state <=                          (count1 == ( 25 * 48000 )) ? S_WAIT_LONG  : S_WAIT_PULSE; 
				S_WAIT_LONG	 :	state <= (!inm ) ? S_WAIT_OFF   : (count1 >= 26'h20_00000  ) ? S_LONG       : S_WAIT_LONG;
				S_LONG		 :	state <= (!inm ) ? S_WAIT_LOFF  :  S_LONG;
				S_WAIT_OFF	 :	state <= ( inm ) ? S_WAIT_LONG  : (count0 == ( 100 * 48000)) ? S_IDLE       : S_WAIT_OFF;
				S_WAIT_LOFF	 :	state <= ( inm ) ? S_LONG       : (count0 == ( 100 * 48000)) ? S_IDLE       : S_WAIT_LOFF;
				default: state <= S_IDLE;
			endcase
		end
	end
	
	assign out = (state == S_WAIT_PULSE) ? 1'b1 : 1'b0;
	assign long = (state == S_LONG || state == S_WAIT_LOFF) ? 1'b1 : 1'b0;
	
	// Counters
	always @(posedge clk) begin
		if( reset ) begin
			count0 <= 0;
			count1 <= 0;
		end else begin
			count0 <= ( state == S_WAIT_OFF  || 
			            state == S_WAIT_LOFF ) ? (count0 + 1) : 0; // count when low waiting
			count1 <= ( state == S_IDLE      ) ? 0            : (count1 + 1); 
		end
	end

endmodule