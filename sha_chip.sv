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
	assign tx2323 = rx232; // wire through
	
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

always @(posedge clk) begin
	if( tone_cnt == 0 ) begin
		spk_toggle <= !spk_toggle;
		tone_cnt   <= ( fire_button_debounce  ) ? { 16'h2CCA } /* C5 */ : 
								   //( key == 5'h12 ) ? { 16'h27E7 } /* D5 */ :
								   //( key == 5'h13 ) ? { 16'h238D } /* E5 */ :
								   //( key == 5'h14 ) ? { 16'h218E } /* F5 */ :
								   //( key == 5'h15 ) ? { 16'h1DE5 } /* G5 */ :
								   //( key == 5'h16 ) ? { 16'h1AA2 } /* A5 */ :
								   //( key == 5'h17 ) ? { 16'h17BA } /* B5 */ :
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


	// Key data structures
	// two 512 bit message blocks
	logic [0:1][0:63][7:0] ibuf;
	// 256 bit sha2 message hash
	logic [255:0] hash;


	// State machine
	logic get_msg; // get a new message 
	logic ld_msg;	// Load message into sha unit
	logic	msg_idx; 
	logic sha_go;
	
	assign sha_go = short_fire || long_fire;
	
	logic [7:0] state_count;
	always_ff @(posedge clk) begin
		if( reset ) begin
			state_count <= 0;
		end else begin
			if( state_count == 0 ) begin
				state_count <= ( sha_go ) ? 1 : 0;
			end else if( state_count == 129 ) begin
				state_count <= ( sha_go ) ? 65 : 0;
			end else begin
				state_count <= state_count + 1;
			end
		end
	end
	
	assign get_msg = ( state_count == 1 || ( state_count == 129 && sha_go ) ) ? 1'b1 : 1'b0;
	assign  ld_msg = ( state_count == 2 || state_count == 66 ) ? 1'b1 : 1'b0;
	assign msg_idx = ( state_count == 66 ) ? 1'b1 : 1'b0;
	
	// Load input buffer and increment nonce
	logic [31:0] nonce;
	always_ff @(posedge clk) begin
		if( reset ) begin
			ibuf <= 0;
			nonce <= 0;
		end else if( get_msg ) begin
			nonce <= nonce + 1;
			ibuf[0] <= 512'h000000014cc2c57c7905fd399965282c87fe259e7da366e035dc087a0000141f000000006427b6492f2b052578fb4bc23655ca4e8b9e2b9b69c88041b2ac8c77;
			ibuf[1] <= { 96'h1571d1be4de695931a269421, 
								nonce[31:0],  
			            128'h00000080000000000000000000000000,
							128'h00000000000000000000000000000000,
							 64'h0000000000000000,
							 64'h0000000080020000 };		
		end
	end
	
	logic ovalid;
	logic [255:0] sha_out;	
	sha_core _sha_core (
		.clk ( clk ),
		.reset( reset ),
		// Input strobe and message
		.in_valid( ld_msg ),
		.message( (msg_idx)?ibuf[1]:ibuf[0] ),
		// Output 
		.out_valid( ovalid ),
		.hash( sha_out )
	);

	// Latch output hash for display
	always_ff @(posedge clk) 
		hash <= ( ovalid ) ? sha_out : hash;
	
	
	///////////////////////////////////////
	// Stat counter timer and rate counters
	///////////////////////////////////////
	
	logic inc_stat;
	assign inc_stat = (state_count == 129); // finished a hash
	logic [47:0] op_count;
	always_ff@( posedge clk4 ) begin
		op_count <= ( inc_stat ) ? op_count + 1 : op_count;
	end
	
	logic [25:0] second_count;	// clk = 48Mhz osc
	logic 	    second_tick; // 1 pulse / sec
	always_ff @(posedge clk) begin
		second_count <= ( second_count == 26'd48_000_000 - 1 ) ? 26'd0 : second_count + 1;
		second_tick <= ( second_count == 26'd0 ) ? 1'b1 : 1'b0;
	end
	
	logic [23:0] oppersec_latch;
	logic [23:0] oppersec_count;
	logic [3:0] sec_del;
	always_ff @(posedge clk4) begin
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
	logic [9:0] id_str;
	string_overlay #(.LEN(18)) _id0(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.ascii_char(ascii_char), .x('h48), .y('h09), .out( id_str[0]), .str( "FIPS 180-4 SHA-256" ) );
	hex_overlay    #(.LEN(12 )) _id1(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.hex_char(hex_char), .x('h50),.y('d58), .out( id_str[1]), .in( op_count[47:0] ) );
   //bin_overlay    #(.LEN(1 )) _id2(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.bin_char(bin_char), .x('h46),.y('h09), .out( id_str[2]), .in( disp_id == 32'h0E96_0001 ) );
	//string_overlay #(.LEN(14)) _id3(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.ascii_char(ascii_char), .x('d119),.y('d58), .out( id_str[3]), .str( "commit 0123abc" ) );
	hex_overlay    #(.LEN(6 )) _id4(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.hex_char(hex_char), .x('h50),.y('d54), .out( id_str[4]), .in( oppersec_latch[23:0] ) );
	string_overlay #(.LEN(16)) _id5(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.ascii_char(ascii_char), .x('h48), .y('d56), .out( id_str[5]), .str( "Total Operations" ) );
	string_overlay #(.LEN(14)) _id6(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.ascii_char(ascii_char), .x('h48), .y('d52), .out( id_str[6]), .str( "Operations/sec" ) );

	// Display two 512 bit message blocks and 256 bit output hash
	hex_overlay #(.LEN(128)) _id7(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.hex_char(hex_char), .x('d1 ), .y('d16), .out( id_str[7]), .in( ibuf[0] ) );
	hex_overlay #(.LEN(128)) _id8(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.hex_char(hex_char), .x('d1 ), .y('d18), .out( id_str[8]), .in( ibuf[1] ) );
	hex_overlay #(.LEN(64 )) _id9(.clk(hdmi_clk), .reset(reset), .char_x(char_x), .char_y(char_y),.hex_char(hex_char), .x('d1 ), .y('d20), .out( id_str[9]), .in( hash    ) );
	
	

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
endmodule

module sha_core (
	input logic clk,
	input logic reset,
	// Input strobe and message
	input logic 		  in_valid,
	input logic [511:0] message,
	// Output 
	output logic         out_valid,
	output logic [255:0] hash
	);
	
	// Control logic
	logic kt_shift;	
	logic wt_shift;
	logic wt_load;	
	logic init_hash;	
	
	logic [64:0] t;
	always_ff @(posedge clk)
		t <= { t[63:0], in_valid };
	
	assign out_valid = t[63];
	assign kt_shift = |t[63:0];
	assign wt_load = in_valid;
	assign wt_shift = |t[62:0];
	assign init_hash = t[0];
	
	///////
	// Kt
	///////

	logic [0:63][31:0] kt_reg;
	logic	[31:0] kt;
	assign kt = kt_reg[0];
	
	always_ff @(posedge clk) begin
		if( reset ) begin
			kt_reg <= {	 32'h428a2f98, 32'h71374491, 32'hb5c0fbcf, 32'he9b5dba5,   
							 32'h3956c25b, 32'h59f111f1, 32'h923f82a4, 32'hab1c5ed5,  
							 32'hd807aa98, 32'h12835b01, 32'h243185be, 32'h550c7dc3,   
							 32'h72be5d74, 32'h80deb1fe, 32'h9bdc06a7, 32'hc19bf174,  
							 32'he49b69c1, 32'hefbe4786, 32'h0fc19dc6, 32'h240ca1cc,   
							 32'h2de92c6f, 32'h4a7484aa, 32'h5cb0a9dc, 32'h76f988da,  
							 32'h983e5152, 32'ha831c66d, 32'hb00327c8, 32'hbf597fc7,   
							 32'hc6e00bf3, 32'hd5a79147, 32'h06ca6351, 32'h14292967,  
							 32'h27b70a85, 32'h2e1b2138, 32'h4d2c6dfc, 32'h53380d13,   
							 32'h650a7354, 32'h766a0abb, 32'h81c2c92e, 32'h92722c85,  
							 32'ha2bfe8a1, 32'ha81a664b, 32'hc24b8b70, 32'hc76c51a3,   
							 32'hd192e819, 32'hd6990624, 32'hf40e3585, 32'h106aa070,  
							 32'h19a4c116, 32'h1e376c08, 32'h2748774c, 32'h34b0bcb5,   
							 32'h391c0cb3, 32'h4ed8aa4a, 32'h5b9cca4f, 32'h682e6ff3,  
							 32'h748f82ee, 32'h78a5636f, 32'h84c87814, 32'h8cc70208,   
							 32'h90befffa, 32'ha4506ceb, 32'hbef9a3f7, 32'hc67178f2 };	
		end else if( kt_shift ) begin
			kt_reg <= { kt_reg[1:63], kt_reg[0] }; // shift/Rotate through the values kt. ROM maybe?
		end else begin
			kt_reg <= kt_reg;
		end
	end
	
	///////
	// Wt
	///////

	logic [0:15][31:0] wt_reg;
	logic	[31:0] Wt;
	logic [31:0] s0, s1;
	logic [31:0] w2, w15, w7, w16;
	logic [31:0] wt_next;
	
	assign wt = wt_reg[0];		
	assign w2 = wt_reg[16-2];
	assign w7 = wt_reg[16-7];
	assign w15= wt_reg[16-15];
	assign w16= wt_reg[16-16];
	
   assign  s1 = {  w2[16:0],  w2[31:17] } ^ {  w2[18:0],  w2[31:19] } ^ { 10'b0,  w2[31:10] }; // section 1.1.2, eqn 4.7
   assign  s0 = { w15[ 6:0], w15[31: 7] } ^ { w15[17:0], w15[31:18] } ^ {  3'b0, w15[31: 3] }; // section 1.1.2, eqn 4.6
   assign wt_next = ( s1 + w7 ) + ( s0 + w16 ); // section 6.2.2 step 1		
	
	
	always_ff @(posedge clk) begin
		if( wt_load ) begin
			wt_reg <= message;						// Initally load with message 16 words
		end else if( wt_shift ) begin
			wt_reg <= { wt_reg[1:15], wt_next };// Create remaining 48 words as we shift wt
		end else begin
			wt_reg <= wt_reg;
		end
	end	
	
	//////////
	// Hash 
	//////////

	logic [0:7][31:0] hash_reg;
		
	// Round logic
	reg   [31:0]  ch_e_f_g, maj_a_b_c, sig1_e, sig0_a;
	reg   [31:0]  f1 ;    // roudn variable for sha-1
	reg   [31:0]  da, db, dc, dd, de, df, dg, dh;
	reg   [31:0]  qa, qb, qc, qd, qe, qf, qg, qh;

	always_comb	begin
		ch_e_f_g = 0;
		maj_a_b_c = 0;
		sig1_e = 0;
		sig0_a = 0;

		if( init_hash ) begin
			{ da, db, dc, dd, de, df, dg, dh } = { 
				32'h6a09e667, 
				32'hbb67ae85, 
				32'h3c6ef372, 
				32'ha54ff53a, 
				32'h510e527f, 
				32'h9b05688c, 
				32'h1f83d9ab, 
				32'h5be0cd19 };   // Step 2 for 6.1.2 and 6.2.2
		end else begin
			{ da, db, dc, dd, de, df, dg, dh } = hash_reg;
		end
			 
      begin : _sha_logic  // step 3 of 6.2.2
			ch_e_f_g = (de & df) ^ (~de & dg);
			maj_a_b_c= (da & db) ^ ( da & dc) ^ (db & dc);
			sig1_e   = { de[5:0], de[31:6] } ^ { de[10:0], de[31:11] } ^ { de[24:0], de[31:25] };
			sig0_a   = { da[1:0], da[31:2] } ^ { da[12:0], da[31:13] } ^ { da[21:0], da[31:22] };
			qa = ((dh + wt) + kt) + ( (ch_e_f_g + sig1_e) + (sig0_a + maj_a_b_c) );
			qb = da;
			qc = db;
			qd = dc;
			qe = ((dh + wt) + (kt + dd)) + (ch_e_f_g + sig1_e);
			qf = de;
			qg = df;
			qh = dg;
      end
	end	
	
	always_ff @(posedge clk) begin
		hash_reg <= { da+qa, db+qb, dc+qc, dd+qd, de+qe, df+qf, dg+qg, dh+qh };
	end
	
	assign hash = hash_reg; // Output
	
endmodule



// A Combinatorial Portion for SHA-256
//----- Module definition ---------------------------------
module sha_core_comb
	(
	input logic [5:0]   n,    // wt itteration counter, must step from  0 to 63 for sha-256
	input logic [5:0]   n2,   // round itteration counter, must step from  0 to 63 for sha-256
	input logic [31:0]  m,    // curreent message word input (used for 1st 16 cycles)
	input logic [31:0]  w2,   // curreent message word (from storage) delayed by 2 cyc
	input logic [31:0]  w7,   // curreent message word (from storage) delayed by 7
	input logic [31:0]  w15,  // curreent message word (from storage) delayed by 15
	input logic [31:0]  w16,  // curreent message word (from storage) delayed by 16
	output logic[31:0]  kt,   // current roudn constant
	input logic [31:0]  kt_in, // round constant input of kt_out (optionally registered)
	output logic[31:0]  wt,   // currently used message word (for storage)
	input logic [31:0]  wt_in,   // copy of wt_out fed back in (optional external delay)
	input logic [255:0] round_in,    // Round register input
	output logic [255:0] round_out,   // round register output
	output logic digest_we,
	input logic [255:0] digest_in    // digest register input
	); 

reg   [31:0]  s0, s1; // temp key expansion variable
reg   [31:0]  ch_e_f_g, maj_a_b_c, sig1_e, sig0_a;
reg   [31:0]  f1 ;    // roudn variable for sha-1


reg   [31:0]  da, db, dc, dd, de, df, dg, dh;
reg   [31:0]  qa, qb, qc, qd, qe, qf, qg, qh;

// Perform Wt message scheduling

always_comb 
  begin
    s1 = 0;
    s0 = 0;
    if( n[6:4] == 0 ) // n < 15 are directly from input
      begin
        wt = m;
      end
    else begin
      s1 = {  w2[16:0],  w2[31:17] } ^ {  w2[18:0],  w2[31:19] } ^ { 10'b0,  w2[31:10] }; // section 1.1.2, eqn 4.7
      s0 = { w15[ 6:0], w15[31: 7] } ^ { w15[17:0], w15[31:18] } ^ {  3'b0, w15[31: 3] }; // section 1.1.2, eqn 4.6
      wt = ( s1 + w7 ) + ( s0 + w16 ); // section 6.2.2 step 1
    end
  end

// Calculate round constants (kt)

always_comb
  begin
    case( n )
    // SHA-256
    6'h00:kt=32'h428a2f98;  6'h01:kt=32'h71374491;  6'h02:kt=32'hb5c0fbcf;  6'h03:kt=32'he9b5dba5;  
    6'h04:kt=32'h3956c25b;  6'h05:kt=32'h59f111f1;  6'h06:kt=32'h923f82a4;  6'h07:kt=32'hab1c5ed5; 
    6'h08:kt=32'hd807aa98;  6'h09:kt=32'h12835b01;  6'h0A:kt=32'h243185be;  6'h0B:kt=32'h550c7dc3;  
    6'h0C:kt=32'h72be5d74;  6'h0D:kt=32'h80deb1fe;  6'h0E:kt=32'h9bdc06a7;  6'h0F:kt=32'hc19bf174; 
    6'h10:kt=32'he49b69c1;  6'h11:kt=32'hefbe4786;  6'h12:kt=32'h0fc19dc6;  6'h13:kt=32'h240ca1cc;  
    6'h14:kt=32'h2de92c6f;  6'h15:kt=32'h4a7484aa;  6'h16:kt=32'h5cb0a9dc;  6'h17:kt=32'h76f988da; 
    6'h18:kt=32'h983e5152;  6'h19:kt=32'ha831c66d;  6'h1A:kt=32'hb00327c8;  6'h1B:kt=32'hbf597fc7;  
    6'h1C:kt=32'hc6e00bf3;  6'h1D:kt=32'hd5a79147;  6'h1E:kt=32'h06ca6351;  6'h1F:kt=32'h14292967; 
    6'h20:kt=32'h27b70a85;  6'h21:kt=32'h2e1b2138;  6'h22:kt=32'h4d2c6dfc;  6'h23:kt=32'h53380d13;  
    6'h24:kt=32'h650a7354;  6'h25:kt=32'h766a0abb;  6'h26:kt=32'h81c2c92e;  6'h27:kt=32'h92722c85; 
    6'h28:kt=32'ha2bfe8a1;  6'h29:kt=32'ha81a664b;  6'h2A:kt=32'hc24b8b70;  6'h2B:kt=32'hc76c51a3;  
    6'h2C:kt=32'hd192e819;  6'h2D:kt=32'hd6990624;  6'h2E:kt=32'hf40e3585;  6'h2F:kt=32'h106aa070; 
    6'h30:kt=32'h19a4c116;  6'h31:kt=32'h1e376c08;  6'h32:kt=32'h2748774c;  6'h33:kt=32'h34b0bcb5;  
    6'h34:kt=32'h391c0cb3;  6'h35:kt=32'h4ed8aa4a;  6'h36:kt=32'h5b9cca4f;  6'h37:kt=32'h682e6ff3; 
    6'h38:kt=32'h748f82ee;  6'h39:kt=32'h78a5636f;  6'h3A:kt=32'h84c87814;  6'h3B:kt=32'h8cc70208;  
    6'h3C:kt=32'h90befffa;  6'h3D:kt=32'ha4506ceb;  6'h3E:kt=32'hbef9a3f7;  6'h3F:kt=32'hc67178f2; 
    default: kt = 32'b0;
    endcase
  end

// Round logic

always_comb
  begin
    f1 = 0;
    ch_e_f_g = 0;
    maj_a_b_c = 0;
    sig1_e = 0;
    sig0_a = 0;

    if( n2 == 0 )
          { da, db, dc, dd, de, df, dg, dh } = digest_in;   // Step 2 for 6.1.2 and 6.2.2
              else
          { da, db, dc, dd, de, df, dg, dh } = round_in;

      begin  // step 3 of 6.2.2
        
        ch_e_f_g = (de & df) ^ (~de & dg);
        maj_a_b_c= (da & db) ^ ( da & dc) ^ (db & dc);
        sig1_e   = { de[5:0], de[31:6] } ^ { de[10:0], de[31:11] } ^ { de[24:0], de[31:25] };
        sig0_a   = { da[1:0], da[31:2] } ^ { da[12:0], da[31:13] } ^ { da[21:0], da[31:22] };

        qa = ((dh + wt_in) + kt_in) + ( (ch_e_f_g + sig1_e) + (sig0_a + maj_a_b_c) );
        qb = da;
        qc = db;
        qd = dc;
        qe = ((dh + wt_in) + (kt_in + dd)) + (ch_e_f_g + sig1_e);
        qf = de;
        qg = df;
        qh = dg;
      end
  end

  assign round_out = { qa, qb, qc, qd, qe, qf, qg, qh };

  assign digest_we = ( n2 == 63 );

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