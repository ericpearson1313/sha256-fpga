module sha_core (
	input logic clk,
	input logic reset,
	// Input strobe and message
	input logic 		  in_valid,
	input logic			  redo, // indicates keep same input hash
	input logic [511:0] message,
	// Output 
	output logic         out_valid,
	output logic [0:7][31:0] hash
	);
	
	// Control logic
	logic kt_shift;	
	logic wt_shift;
	logic wt_load;	
	logic init_hash;	
	logic [1:0] hacc; // operation at init hash: 0 new block, 1 next block, 3 redo last block
	
	logic [65:0] t;
	logic [65:0] r;
	
	always_ff @(posedge clk) begin
		t <= { t[64:0], in_valid };
		r <= { r[64:0], in_valid & redo };
	end
	
	assign out_valid = t[64];
	assign kt_shift = |t[63:0];
	assign wt_load = in_valid;
	assign wt_shift = |t[62:0];
	assign init_hash = t[0];
	assign hacc[1:0] = { t[64] & r[64], t[64] };
	
	///////
	// Kt
	///////

	logic [0:63][31:0] kt_reg;
	logic	[0:63][31:0] kt;
	assign kt = kt_reg;
	
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
	logic	[0:63][31:0] wt;
	logic [0:63][31:0] s0, s1;
	logic [0:63][31:0] w2, w15, w7, w16;
	logic [0:63][31:0] wt_next;
	
	// build wt[64] array
	always_comb begin
		for( int ii = 0; ii < 64; ii++ ) begin
			if( ii < 16 ) begin
				wt[ii] = wt_reg[ii];
			end else begin 
				w2[ii] = wt[ii-2];
				w7[ii] = wt[ii-7];
				w15[ii]= wt[ii-15];
				w16[ii]= wt[ii-16];
				s1[ii] = {  w2[ii][16:0],  w2[ii][31:17] } ^ {  w2[ii][18:0],  w2[ii][31:19] } ^ { 10'b0,  w2[ii][31:10] }; // section 1.1.2, eqn 4.7
				s0[ii] = { w15[ii][ 6:0], w15[ii][31: 7] } ^ { w15[ii][17:0], w15[ii][31:18] } ^ {  3'b0, w15[ii][31: 3] }; // section 1.1.2, eqn 4.6
				wt[ii] = ( s1[ii] + w7[ii] ) + ( s0[ii] + w16[ii] ); // section 6.2.2 step 1		
			end
		end // for
	end //always
	
	int sh_wid = 1; // shift 1 to 48 rounds per cycle
	always_ff @(posedge clk) begin
		if( wt_load ) begin
			wt_reg <= message;						// Initally load with message 16 words
		end else if( wt_shift ) begin
			wt_reg[ 0] <= wt[ 0+sh_wid]; // Create remaining 48 words as we shift wt
			wt_reg[ 1] <= wt[ 1+sh_wid];
			wt_reg[ 2] <= wt[ 2+sh_wid];
			wt_reg[ 3] <= wt[ 3+sh_wid];
			wt_reg[ 4] <= wt[ 4+sh_wid];
			wt_reg[ 5] <= wt[ 5+sh_wid];
			wt_reg[ 6] <= wt[ 6+sh_wid];
			wt_reg[ 7] <= wt[ 7+sh_wid];
			wt_reg[ 8] <= wt[ 8+sh_wid];
			wt_reg[ 9] <= wt[ 9+sh_wid];
			wt_reg[10] <= wt[10+sh_wid];
			wt_reg[11] <= wt[11+sh_wid];
			wt_reg[12] <= wt[12+sh_wid];
			wt_reg[13] <= wt[13+sh_wid];
			wt_reg[14] <= wt[14+sh_wid];
			wt_reg[15] <= wt[15+sh_wid];		
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
	reg   [0:63][31:0]  ch_e_f_g, maj_a_b_c, sig1_e, sig0_a;
	reg   [0:63][31:0]  da, db, dc, dd, de, df, dg, dh;
	reg   [0:63][31:0]  qa, qb, qc, qd, qe, qf, qg, qh;

	always_comb	begin
//		ch_e_f_g = 0;
//		maj_a_b_c = 0;
//		sig1_e = 0;
//		sig0_a = 0;

		// starting hash
		if( init_hash && hacc == 0 ) begin // load standard start value
			da[0] = 32'h6a09e667;
			db[0] = 32'hbb67ae85;
			dc[0] = 32'h3c6ef372;
			dd[0] = 32'ha54ff53a;
			de[0] = 32'h510e527f;
			df[0] = 32'h9b05688c;
			dg[0] = 32'h1f83d9ab;
			dh[0] = 32'h5be0cd19;   // Step 2 for 6.1.2 and 6.2.2
		end else if ( init_hash && hacc == 1 ) begin // load next hash to continue
			da[0] = hash_reg[0] + acc_reg[0];
			db[0] = hash_reg[1] + acc_reg[1];
			dc[0] = hash_reg[2] + acc_reg[2];
			dd[0] = hash_reg[3] + acc_reg[3];
			de[0] = hash_reg[4] + acc_reg[4];
			df[0] = hash_reg[5] + acc_reg[5];
			dg[0] = hash_reg[6] + acc_reg[6];
			dh[0] = hash_reg[7] + acc_reg[7];	
	   end else if( init_hash && hacc == 3 ) begin // load old hash to redo block
			da[0] = hash_reg[0];
			db[0] = hash_reg[1];
			dc[0] = hash_reg[2];
			dd[0] = hash_reg[3];
			de[0] = hash_reg[4];
			df[0] = hash_reg[5];
			dg[0] = hash_reg[6];
			dh[0] = hash_reg[7];			
		end else begin // else normal case feed from acc reg
			{ da[0], db[0], dc[0], dd[0], de[0], df[0], dg[0], dh[0] } = acc_reg;
		end
		
		for( int ii = 0; ii < 64; ii++ ) begin : _sha_logic  // step 3 of 6.2.2
			ch_e_f_g[ii] = (de[ii] & df[ii]) ^ (~de[ii] & dg[ii]);
			maj_a_b_c[ii]= (da[ii] & db[ii]) ^ ( da[ii] & dc[ii]) ^ (db[ii] & dc[ii]);
			sig1_e[ii]   = { de[ii][5:0], de[ii][31:6] } ^ { de[ii][10:0], de[ii][31:11] } ^ { de[ii][24:0], de[ii][31:25] };
			sig0_a[ii]   = { da[ii][1:0], da[ii][31:2] } ^ { da[ii][12:0], da[ii][31:13] } ^ { da[ii][21:0], da[ii][31:22] };
			qa[ii] = ((dh[ii] + wt[ii]) + kt[ii]) + ( (ch_e_f_g[ii] + sig1_e[ii]) + (sig0_a[ii] + maj_a_b_c[ii]) );
			qb[ii] = da[ii];
			qc[ii] = db[ii];
			qd[ii] = dc[ii];
			qe[ii] = ((dh[ii] + wt[ii]) + (kt[ii] + dd[ii])) + (ch_e_f_g[ii] + sig1_e[ii]);
			qf[ii] = de[ii];
			qg[ii] = df[ii];
			qh[ii] = dg[ii];
			if( ii > 0 ) begin
				da[ii] = qa[ii-1];
				db[ii] = qb[ii-1];
				dc[ii] = qc[ii-1];
				dd[ii] = qd[ii-1];
				de[ii] = qe[ii-1];
				df[ii] = qf[ii-1];
				dg[ii] = qg[ii-1];
				dh[ii] = qh[ii-1];
			end
      end
	end	

	int idx = 0; // select output tap
	always_ff @(posedge clk) begin
		acc_reg <= { qa[idx], qb[idx], qc[idx], qd[idx], qe[idx], qf[idx], qg[idx], qh[idx] };
	end
	
	always_ff @(posedge clk) begin
		if( hacc == 1 ) begin // accumulate and hold this hash
			for( int ii = 0 ; ii < 8; ii++ ) begin
				hash_reg[ii] <= hash_reg[ii] + acc_reg[ii];
			end
		end else if( hacc == 3 ) begin // keep the old hash, we're reoding this block with updated message
				hash_reg <= hash_reg;
		end else if( init_hash ) begin // loads std Hashes into reg for for future acc
			hash_reg <= { da[0], db[0], dc[0], dd[0], de[0], df[0], dg[0], dh[0] };
		end else begin
			hash_reg <= hash_reg;
		end
	end
	
	always_comb begin
		for( int ii = 0 ; ii < 8; ii++ ) begin
			hash[ii] <= hash_reg[ii] + acc_reg[ii]; // output is always the sum
		end
	end
	
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
