// top level launcher sim.
`timescale 1ns / 1ps
module testbench( );

`define SHA2_TEST
//`define HDMI_INFOFRAME
//`define FONT_GEN
//`define DIV_TEST
//`define PSRAM_TEST
//`define BLASTER_TEST


///////////////////////////////
`ifdef SHA2_TEST

    //////////////////////
    // Let there be Clocks!
    //////////////////////
    
    logic clk, clk4;
    initial begin
        clk = 1'b1;
		  clk4 = 1'b1;
        forever begin
				#(2.5ns) clk4 = 0;
				#(2.5ns) clk4 = 1;
				#(2.5ns) clk4 = 0;
				#(2.5ns) begin clk4 = 1; clk = ~clk; end
        end 
    end
	 
	 // Reset generation
	 
	 logic reset; // active high
	 initial begin
			reset = 1'b1;
			for( int ii = 0; ii < 10; ii++ ) begin
				@(posedge clk);
			end
			@(negedge clk);
			reset = 1'b0;
	 end
			
	 // Simulation e-stop.
	 
	 initial begin
        for( int ii = 0; ii < 1000; ii++ ) 
				@(posedge clk);
        $stop;
	 end

	 ////////////////
	 // UUT
	 ////////////////
	 
		logic [0:7][31:0] hash;
		logic ovalid;
		logic [511:0] msg;
		logic ivalid;
		logic [1:0] mode;
		
	localparam MODE_INIT = 1;	// Will init with H* at start (both input and REG)
	localparam MODE_HASH = 0;	// starts with REG and will Update reg at END (Normal steady state(
	localparam MODE_REDO = 3;  // starts with Reg, but discards value at end (keeping REG unaltered for REDO
		
	 	//sha_core _uut (
		//	.clk ( clk ),
		//	.reset( reset ),
		//// Input strobe and message
		//	.in_valid( ivalid ),
		//	.mode( mode ),
		//	.message( msg ),
		//// Output 
		//	.out_valid( ovalid ),
		//	.hash( hash )
		//);

	 	sha_11_12_core _uut (
			.clk ( clk ),
			.reset( reset ),
		// Input strobe and message
			.i_valid( ivalid ),
			.i_mode( mode ),
			.i_data( msg ),
		// Output 
			.o_valid( ovalid ),
			.o_data( hash )
		);
		
		logic [0:3][0:7][31:0] hash_out;
		always_ff @(posedge clk) 
			if( ovalid ) 
				hash_out <= { hash_out[1:3], hash };

	logic [0:3][0:63][7:0] bc_msg; 
	logic [0:2][0:15][31:0] in_msg;				

	// Test sequence
	initial begin
		// reset signals
		msg = 0;
		ivalid = 0;
		mode = 0;
		
      // wwait for reset de-assert
		while( reset ) @(posedge clk);
		
		// and some clock cycles
      for( int ii = 0; ii < 100; ii++ ) @(posedge clk); // wait pipeline fill cycles		
		
		// Test #1 - Single block NIST message sample

			msg = {	32'h61626380,	// "abc"
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000018  };
		mode = MODE_INIT; // first cycle		
		for( int ii = 0; ii < 72; ii++ ) begin
			ivalid = ( ii == 0 ) ? 1'b1 : 1'b0;
			msg = ( ii != 0 ) ? 0 :  
			      {	32'h61626380,	// "abc"
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000018  };
			@( posedge clk );
		end
		

		
		// Test #2 - Two block NIST message sample, with 2nd block repeated via REDO
		
		// and some clock cycles
      for( int ii = 0; ii < 15; ii++ ) @(posedge clk); // +15 cycles		
				

		for( int ii = 0; ii < (3*72+1); ii++ ) begin
			ivalid = ( ii == 0 || ii == 72 || ii == 144 ) ? 1'b1 : 1'b0;
			if( ii == 0 ) begin
			mode = MODE_INIT;			
			msg = {	32'h61626364,	// "abc"
						32'h62636465,
						32'h63646566,
						32'h64656667,
						32'h65666768,
						32'h66676869,
						32'h6768696A,
						32'h68696A6B,
						32'h696A6B6C,
						32'h6A6B6C6D,
						32'h6B6C6D6E,
						32'h6C6D6E6F,
						32'h6D6E6F70,
						32'h6E6F7071,
						32'h80000000,
						32'h00000000  };
			end else if ( ii == 72 || ii == 144 ) begin
			mode = ( ii == 72 ) ? MODE_HASH : MODE_REDO; // swapped for pipelined version
			msg = {	32'h00000000,	// "abc"
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h00000000,
						32'h000001C0  };
			end		
			@( posedge clk );
		end

		// and some clock cycles
      for( int ii = 0; ii < 15; ii++ ) @(posedge clk); // +15 cycles		

		for( int ii = 0; ii < 4; ii++ ) 
			$display("hash_out[%d] = %h\n", ii, hash_out[ii] );
		
		// Evaluate Results
		if( hash_out[0] != { 32'hBA7816BF, 
									32'h8F01CFEA, 
									32'h414140DE, 
									32'h5DAE2223, 
									32'hB00361A3, 
									32'h96177A9C, 
									32'hB410FF61, 
									32'hF20015AD } ) 
				$display("ERROR: single buffer message\n");	
		else
				$display("Passed 1 \n");	
		
				
		if( hash_out[1] != { 32'h85E655D6, 
									32'h417A1795, 
									32'h3363376A, 
									32'h624CDE5C, 
									32'h76E09589, 
									32'hCAC5F811, 
									32'hCC4B32C1, 
									32'hF20E533A } ) 
				$display("ERROR: two buffer message, intermediate hash\n");	
		else
				$display("Passed 2 \n");	
				
		if( hash_out[2] != { 32'h248D6A61, 
									32'hD20638B8, 
									32'hE5C02693, 
									32'h0C3E6039, 
									32'hA33CE459, 
									32'h64FF2167, 
									32'hF6ECEDD4, 
									32'h19DB06C1 } ) 
				$display("ERROR: two buffer message, final hash\n");	
		else
				$display("Passed 3 \n");					


		if( hash_out[3] != { 32'h248D6A61, // shoudl be same if REDO
									32'hD20638B8, 
									32'hE5C02693, 
									32'h0C3E6039, 
									32'hA33CE459, 
									32'h64FF2167, 
									32'hF6ECEDD4, 
									32'h19DB06C1 } ) 
				$display("ERROR: two buffer message, REDO final hash\n");	
		else
				$display("Passed 4 \n");					
				
		/////////////////////////////////////////////////////////////		
		// Test #3 - Two block sample 80 byte message with nonce
		/////////////////////////////////////////////////////////////		
		/////////////////////////////////////////////////////////////		
		//	Midstate: 90f741afb3ab06f1a582c5c85ee7a561912b25a7cd09c060a89b3c2a73a48e22
		//	Data: 000000014cc2c57c7905fd399965282c87fe259e7da366e035dc087a0000141f000000006427b6492f2b052578fb4bc23655ca4e8b9e2b9b69c88041b2ac8c771571d1be4de695931a2694217a33330e000000800000000000000000000000000000000000000000000000000000000000000000000000000000000080020000
		//	NONCE: 32'h0e33337a == 238,236,538
		//	Verilog values:
		//	Data: 512'h000002800000000000000000000000000000000000000000000000000000000000000000000000000000000080000000000000002194261a9395e64dbed17115
		//	Midstate: 256'h228ea4732a3c9ba860c009cda7252b9161a5e75ec8c582a5f106abb3af41f790

	
		bc_msg[0] = 512'h000000014cc2c57c7905fd399965282c87fe259e7da366e035dc087a0000141f000000006427b6492f2b052578fb4bc23655ca4e8b9e2b9b69c88041b2ac8c77;
		bc_msg[1] = 512'h1571d1be4de695931a2694217a33330e000000800000000000000000000000000000000000000000000000000000000000000000000000000000000080020000;
		
		// Difficulty
		// exp = bc_msg[1][8] 
		// Thresh = bc_msg[1][9:11]
		
		// Resort into correct byte packing

		for( int bb = 0; bb < 3; bb++ ) // block
			for( int ww = 0; ww < 16; ww++ ) // 32 bit words
				in_msg[bb][ww][31:0] = { bc_msg[bb][ww*4+3], bc_msg[bb][ww*4+2], bc_msg[bb][ww*4+1], bc_msg[bb][ww*4+0] };
		

		// and some clock cycles
      for( int ii = 0; ii < 15; ii++ ) @(posedge clk); // +15 cycles						
	
		// Run through 2 blocks 
		msg = in_msg[0];
		mode = MODE_INIT;
		for( int ii = 0; ii < 72; ii++ ) begin
			ivalid = ( ii == 0 ) ? 1'b1 : 1'b0;
			@( posedge clk );
		end
		
		msg = in_msg[1];
		mode = MODE_HASH;
		for( int ii = 0; ii < 72; ii++ ) begin
			ivalid = ( ii == 0 ) ? 1'b1 : 1'b0;
			@( posedge clk );
		end
	
		// and some clock cycles
      for( int ii = 0; ii < 15; ii++ ) @(posedge clk); // +15 cycles						
		
		// Take last hash and feed it into SHA as a message
		
		in_msg[2] = { hash_out[3][0], 
						  hash_out[3][1],
						  hash_out[3][2],
						  hash_out[3][3],
						  hash_out[3][4],
						  hash_out[3][5],
						  hash_out[3][6],
  						  hash_out[3][7],
						  256'h80000000_00000000_00000000_00000000_00000000_00000000_00000000_00000100 };
		
		
		msg = in_msg[2]; 
		mode = MODE_INIT;
		for( int ii = 0; ii < 72; ii++ ) begin
			ivalid = ( ii == 0 ) ? 1'b1 : 1'b0;
			@( posedge clk );
		end	

		// and some clock cycles
      for( int ii = 0; ii < 15; ii++ ) @(posedge clk); // +15 cycles					

		$display ("blk1 = %h\n", in_msg[0] );
		$display ("blk2 = %h\n", in_msg[1] );
		$display ("blk3 = %h\n", in_msg[2] );			
		
		$display ("hash_out[0] = %h\n", hash_out[1] );
		$display ("hash_out[1] = %h\n", hash_out[2] );
		$display ("hash_out[2] = %h\n", hash_out[3] );	

		// check midstate is expected
		if( hash_out[1] != { 32'haf41f790, 32'hf106abb3, 32'hc8c582a5, 32'h61a5e75e, 
									32'ha7252b91, 32'h60c009cd, 32'h2a3c9ba8, 32'h228ea473 } ) 
				$display("ERROR: Wronge expected midstate\n");
		else
				$display("Pass: correct midstate\n");
			

		// Check HASH passes difficulty
		if( hash_out[3][7] != 32'h00000000 ) 
				$display("ERROR: failed difficulty\n");
		else
				$display("Pass: passes difficulty\n");		
		
		// Difficulty
		// exp = bc_msg[1][8] = 8'h1a
		// Thresh = bc_msg[1][9:11] = 12'h269421		
		
		$display("Diffculty exp: %h, thresh %h\n", bc_msg[1][8], bc_msg[1][9:11] );
		
		for( int ii = bc_msg[1][8]+1; ii < 32; ii++ )
			$display( "zero byte %d = %h\n", ii, hash_out[3][ii>>2][8*(ii&3)+7-:8] );		
		
		
		// Stop Sim in a bit
      for( int ii = 0; ii < 100; ii++ ) @(posedge clk); // 16 cycles		
		 $stop;
		
	end // initial
	
`endif // SHA2_TEST


///////////////////////////////
`ifdef HDMI_INFOFRAME

    //////////////////////
    // Let there be Clocks!
    //////////////////////
    
    logic clk, clk4;
    initial begin
        clk = 1'b1;
		  clk4 = 1'b1;
        forever begin
				#(2.5ns) clk4 = 0;
				#(2.5ns) clk4 = 1;
				#(2.5ns) clk4 = 0;
				#(2.5ns) begin clk4 = 1; clk = ~clk; end
        end 
    end
	 
	 // Reset generation
	 
	 logic reset; // active high
	 initial begin
			reset = 1'b1;
			for( int ii = 0; ii < 10; ii++ ) begin
				@(posedge clk);
			end
			@(negedge clk);
			reset = 1'b0;
	 end
			
	 // Simulation stop.
	 
	 initial begin
        for( int ii = 0; ii < 100; ii++ ) 
				@(posedge clk);
        $stop;
	 end

	 ////////////////
	 // UUT
	 ////////////////
	 
	 packet_test _uut (
		.clk( clk ),
		.reset( reset )
	 );

`endif // HDMI_INFOFRAME

///////////////////////////////
`ifdef FONT_GEN
	
	logic clk, reset;
	logic blank, hsync, vsync;
	logic [7:0] char_x, char_y;
	logic [255:0] ascii_char;
	logic [15:0] hex_char;
	logic [1:0] bin_char;

	ascii_font57 _font
	(
		.clk( 0 ),
		.reset( 0 ),
		.blank( 0 ),
		.hsync( 0 ),
		.vsync( 0 ),
		.char_x( char_x ), // 0 to 105 chars horizontally
		.char_y( char_y ), // o to 59 rows vertically
		.hex_char   ( hex_char ),
		.binary_char( bin_char ),
		.ascii_char ( ascii_char )	
	);

	 initial begin
        #(2.5ns)
        $stop;
	 end
`endif // FONT_GEN

///////////////////////////////
`ifdef DIV_TEST

    //////////////////////
    // Let there be Clocks!
    //////////////////////
    
    logic clk, clk4;
    initial begin
        clk = 1'b1;
		  clk4 = 1'b1;
        forever begin
				#(2.5ns) clk4 = 0;
				#(2.5ns) clk4 = 1;
				#(2.5ns) clk4 = 0;
				#(2.5ns) begin clk4 = 1; clk = ~clk; end
        end 
    end
	 
	 // Reset generation
	 
	 logic reset; // active high
	 initial begin
			reset = 1'b1;
			for( int ii = 0; ii < 10; ii++ ) begin
				@(posedge clk);
			end
			@(negedge clk);
			reset = 1'b0;
	 end
			
	 // Simulation stop.
	 
	 initial begin
        for( int ii = 0; ii < 1000; ii++ ) 
				@(posedge clk);
        $stop;
	 end

    //////////////////////
    // UUT Unit Under Test
    //////////////////////

	 logic [11:0] voltage, current, resistance;
	 logic [11:0] vin, iin, rout;
	 logic in_valid, out_valid;
 	 ohm_div _uut (
		// Input clock
		.clk( clk ),
		.reset( reset ),
		// Votlage and Current Inputs
		.valid_in( in_valid ),
		.v_in( vin ), // ADC Vout
		.i_in( iin ), // ADC Iout
		// Resistance Output
		.valid_out( out_valid ),
		.r_out( rout )
	);
	
	// covert adc to signed, and vice versa
	assign vin = voltage ^ 12'h7ff; // .2005 V/DN
	assign iin = current ^ 12'h7ff; // 205 DN/A
	assign resistance = rout ^ 12'h7ff; // 32 DN/ohm
	
	// test

	
	initial begin
		// reset signals
		voltage = 0;
		current = 0;
		in_valid = 0;
      // startup delay (for future reset)
      for( int ii = 0; ii < 15; ii++ ) @(posedge clk); // 15 cycles
		
	// Test Case #1
	// 2.5 ohms 5 volts, 2 amps
		@(posedge clk);
		voltage = 5.0 / 0.2005;
		current = 2 * 209;
		in_valid = 1;
		@(posedge clk);
		voltage = 0;
		current = 0;
		in_valid = 0;
		// wait for data output
		while( !out_valid ) @(posedge clk);			
		
		// Observe 6.5 format = '04E , about 2.4 ohms


	// Test Case #2
	// 30 ohms 90 volts, 3 amps
		@(posedge clk);	
		voltage = 90 / 0.2005;
		current = 3 * 209;
		in_valid = 1;
		@(posedge clk);
		voltage = 0;
		current = 0;
		in_valid = 0;
		// wait for data output
		while( !out_valid ) @(posedge clk);			
		
		// Observe '3AD, about 29.4 ohms
		
	// Test Case #3 - Clipping
	// 70 ohms 140 volts, 2 amps
		@(posedge clk);	
		voltage = 140 / 0.2005;
		current = 2 * 209;
		in_valid = 1;
		@(posedge clk);
		voltage = 0;
		current = 0;
		in_valid = 0;
		// wait for data output
		while( !out_valid ) @(posedge clk);			
		
		// Observe '7FF Clipped value
		
		// Stop Sim in a bit
      for( int ii = 0; ii < 100; ii++ ) @(posedge clk); // 16 cycles		
		 $stop;
		
	end
	
	
	
	
	
`endif // DIV_TEST

///////////////////////////////
`ifdef PSRAM_TEST


    //////////////////////
    // Let there be Clocks!
    //////////////////////
    
    logic clk, clk4;
    initial begin
        clk = 1'b1;
		  clk4 = 1'b1;
        forever begin
				#(2.5ns) clk4 = 0;
				#(2.5ns) clk4 = 1;
				#(2.5ns) clk4 = 0;
				#(2.5ns) begin clk4 = 1; clk = ~clk; end
        end 
    end
	 
	 // Reset generation
	 
	 logic reset; // active high
	 initial begin
			reset = 1'b1;
			for( int ii = 0; ii < 10; ii++ ) begin
				@(posedge clk);
			end
			@(negedge clk);
			reset = 1'b0;
	 end
			
	 // Simulation stop.
	 
	 initial begin
        for( int ii = 0; ii < 1000; ii++ ) 
				@(posedge clk);
        $stop;
	 end
	 


    //////////////////////
    // UUT Unit Under Test
    //////////////////////


	logic [7:0] 	spi_data_out;
	logic   			spi_data_oe;
	logic [1:0]   	spi_le_out; // match delay
	logic [7:0] 	spi_data_in;
	logic [1:0]		spi_le_in; // match IO registering
	logic				spi_clk;
	logic				spi_cs;
	logic				spi_rwds_out;
	logic				spi_rwds_oe;
	logic				spi_rwds_in;
	
	// SPI Controller
	
	logic psram_ready;
	logic [17:0] rdata;
	logic rvalid;
	
	// Loopback SPI I/O
	
	assign spi_data_in = spi_data_out;
	assign spi_le_in = spi_le_out;
	assign spi_rwds_in = spi_rwds_out;
	
	psram_ctrl _uut (
		// System
		.clk		( clk ),
		.clk4		( clk4 ),
		.reset	( reset ),
		// Psram spi8 interface
		.spi_data_out( spi_data_out ),
		.spi_data_oe(  spi_data_oe  ),
		.spi_le_out( 	spi_le_out 	 ),
		.spi_data_in( 	spi_data_in  ),
		.spi_le_in( 	spi_le_in 	 ),
		.spi_clk( 		spi_clk 		 ),
		.spi_cs( 		spi_cs 		 ),
		.spi_rwds_out( spi_rwds_out ),
		.spi_rwds_oe( 	spi_rwds_oe  ),
		.spi_rwds_in( 	spi_rwds_in  ),
		// Status
		.psram_ready( psram_ready ),	// Indicates control is ready to accept requests
		// AXI4 R/W port
		// Write Data
		.wdata( 16'h0000 ),
		.wvalid( 1'b1 ), // always avail)
		.wready(      ),
		// Write Addr
		.awaddr( 25'h000_0000 ),
		.awlen( 8'h08 ),	// assumed 8
		.awvalid( 1'b0 ), // write valid
		.awready( ),
		// Write Response
		.bready( 1'b1 ),	// Assume 1, non blocking
		.bvalid(  ),
		.bresp(  ),
		// Read Addr
		.araddr( 25'h000_0000 ),
		.arlen( 8'h04 ),	// assumed 4
		.arvalid( 1'b0 ), // read valid	
		.arready(),
		// Read Data
		.rdata( rdata[17:0] ),
		.rvalid( rvalid ),
		.rready( 1'b1 ) // Assumed 1, non blocking
	);	

`endif // PSRAM_TEST

`ifdef BLASTER_TEST
parameter R = 10.0; // Load resistance
parameter CAP_VOLTAGE = 320.0;
parameter COIL_UH = 399.0;
parameter FREQ_MHZ = 48;
parameter PERIOD_NS = 20.8;
parameter CAP_UF = 1.0;  // typicall 100uF
parameter CURRENT = 2 	; // integer amps, 1 to 7

logic arm, fire;
logic pwm, dump;
logic done, charge;
logic n_sclk, n_cs;
logic [3:0] data, data_n;

    //////////////////////
    // Let there be Clock!
    //////////////////////
    
    logic clk;
    initial begin
        clk = 1'b1;
        forever begin
            #(10.4ns) clk = ~clk;
        end 
    end
	 
	 // Reset generation
	 
	 logic reset; // active high
	 initial begin
			reset = 1'b1;
			for( int ii = 0; ii < 3; ii++ ) begin
				@(posedge clk);
			end
			@(negedge clk);
			reset = 1'b0;
	 end
			
	 // Simulation stop.
	 
	 initial begin
        for( int ii = 0; ii < 80000; ii++ ) 
				@(posedge clk);
        $stop;
	 end
	 

blaster _uut (
	// Input Buttons
	.arm_button( arm ), // arm is the power button
	.fire_button( fire ),

	// Output LED/SPK
	.arm_led(),
	.cont_led(),
	.speaker(),
	
	// Charger
	.lt3420_done( done ),
	.lt3420_charge( charge ),

	// Voltage Controls
	.pwm( pwm ),
	.dump( dump ),

	// Continuity feedback
	.cont( 1'b1 ),
	
	// Current setting
	.iset( CURRENT  ),
	
	// External A/D Converters (2.5v)
	.ad_cs( n_cs ),
	.ad_sdata_a( data[1:0] ),
	.ad_sdata_b( data[3:2] ),

	// Input clock
	.clk( clk ),
	.reset( reset )
);


		
		// TODO, turn these back to reals (but after I get git setup)
		
		real ecap, ecap_n; 		
		real icap, icap_n; 
		real vcap, vcap_n; 
		real iout, iout_n; 
		real vout, vout_n; 
	 
	
		
	initial begin
		// hardware will wait for fire;
		fire = 1'b0;
		arm  = 1'b1; // arm assume to go high with power-on at this time.
		done = 1'b1; // for sim show charge complete
      // startup delay (for future reset)
      for( int ii = 0; ii < 10; ii++ ) @(posedge clk); // 10 cycles
		// Fire
		fire = 1'b1;
	end // fire		
		
	initial begin


	/////////////////////
	// System Model     
	/////////////////////	
	// Model of power module 
	// root states are capacitor energy and coil (output) current.
	// The model updates these dynamically from the pwm state
	// The cap voltage is derived from cap energy, and cap current from pwm and coil current.
	// The output voltage is a function of igniter resistance and coil (output) current.
	// 
	
		vcap = CAP_VOLTAGE;
		ecap = (0.5 * CAP_UF * CAP_VOLTAGE * CAP_VOLTAGE)/1000000.0; 
		icap = 0;
		vout = 0;
		iout = 0;

		// on a cycle loop, now responde to PWM 
		// TODO add dump modelling
		
		forever begin
			@(posedge clk ) ;
			if( pwm == 0 ) begin
				// output off, I falls at rate Vout/L*t
				iout_n = iout - ((vout / (COIL_UH * FREQ_MHZ))); 
				// The updated Vout is IR
				vout_n = (iout_n * R);
				// Icap = 0; Ecap, Vcap unchanged
				icap_n = 0;
				vcap_n = vcap;
				ecap_n = ecap;
			end else begin 
				// output is on, I rises by (Vin-Vout)/L
				iout_n = iout + (((vcap - vout) / (COIL_UH * FREQ_MHZ)));
				// The updated Vout is IR
				vout_n = (iout_n * R);
				// Icap = Iout
				icap_n = iout_n;
				// cap drops by energy used this cycle IVT
				ecap_n = ecap - ((((((iout+iout_n)/2)*vcap) / (FREQ_MHZ*1000000.0)))) ;
				// votlage is calc from energy

				vcap_n = ($sqrt( (2.0 * ecap_n) / CAP_UF ) * 1000.0);

			end
			
			iout = iout_n;
			vout = vout_n;
			ecap = ecap_n;
			vcap = vcap_n;
			icap = icap_n;		
		end // Analog model
	end // analog model

	
	/////////////////////
	// AD7352 Model     
	/////////////////////
	// Models amplification
	// adc sampling, conversion
	// and transmission
	
	// ADC sampling and transmission.
	logic [11:0] sh_vcap;	// 0 to 350 volts  350/4096
	logic [11:0] sh_icap;	// 0 to 12 amp     12/4096
	logic [11:0] sh_vout;	// 0 to 350 volts
	logic [11:0] sh_iout;	// 0 to 12 amp

	assign n_sclk = clk; // same clock, adc just uses negedge.
	
	initial begin
		data_n = 4'b0;
	   sh_vcap = 0.0;
		sh_icap = 0.0;
		sh_vout = 0.0;
		sh_iout = 0.0;
		forever begin
			@(negedge n_sclk ) 
			while( n_cs != 1 ) begin // it has to start high
				@(negedge n_sclk ); 
			end
			while( n_cs != 0 ) begin // wait for it to go low
				@(negedge n_sclk ); 
			end
			// first falling edge with n_cs active low, output 2nd zero
			data_n = 4'b0;
			// sample 
			sh_vcap[11:0] = int'(vcap * 8  );
			sh_icap[11:0] = int'(icap * 256);
			sh_vout[11:0] = int'(vout * 8  );
			sh_iout[11:0] = int'(iout * 256);

			@(negedge n_sclk ); 
			for( int bitpos = 11; bitpos >= 0; bitpos-- ) begin
				data_n[3:0] = { sh_vcap[bitpos], sh_icap[bitpos], sh_vout[bitpos], sh_iout[bitpos] };
				@(negedge n_sclk ); 
			end
			data_n[3:0] = 4'b0;
		end // adc
	end
	
	// adc oe.
	assign data[3:0] = ( n_cs == 1'b0 ) ? data_n[3:0] : 4'bxxxx;
`endif // BLASTER_TEST

endmodule
