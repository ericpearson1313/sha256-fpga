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
	logic hacc;
	
	logic [65:0] t;
	always_ff @(posedge clk)
		t <= { t[64:0], in_valid };
	
	assign out_valid = t[65];
	assign kt_shift = |t[63:0];
	assign wt_load = in_valid;
	assign wt_shift = |t[62:0];
	assign init_hash = t[0];
	assign hacc = t[64];
	
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
	logic	[31:0] wt;
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
	logic [0:7][31:0] acc_reg;
	
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
			{ da, db, dc, dd, de, df, dg, dh } = acc_reg;
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
		acc_reg <= { qa, qb, qc, qd, qe, qf, qg, qh };
	end
	
	always_ff @(posedge clk) begin
		if( init_hash ) begin
			hash_reg <= { da, db, dc, dd, de, df, dg, dh };
		end else if( hacc ) begin
			for( int ii = 0 ; ii < 8; ii++ ) begin
				hash_reg[ii] = hash_reg[ii] + acc_reg[ii];
			end
		end
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
