

// 11 core, 6 hashes in 36 cycles.
// 
// Changed input to more axi-like, assumes 1 to 6 cycle burst input
module sha_11_6_core (
	input logic clk,
	input logic reset,
	// Input strobe and message
	input logic 		  i_valid,
	output logic 		  i_ready, 
	input logic [0:15][31:0] i_data,
	input logic	[1:0]	  i_mode, 
	// Output 
	output logic        o_valid,
	output logic [0:7][31:0] o_data
	);

	localparam MODE_INIT = 1;	// Will init with H* at start (both input and REG)
	localparam MODE_HASH = 0;	// starts with REG and will Update reg at END (Normal steady state(
	localparam MODE_REDO = 3;  // starts with Reg, but discards value at end (keeping REG unaltered for REDO
	
	
	// Control logic
	logic [0:5] kt_shift;	
	logic wt_shift;
	logic wt_load;	
	logic hshift; // to keep in phase
	logic init_hash;	
	logic [1:0] mode_start;
	logic [1:0] mode_done;

	// [col][row]
	logic [0:5][0:5] tmat; // t matrix
	logic [37:0] t;
	logic [37:0][1:0] md;
	
	always_ff @(posedge clk) begin
		if( reset ) begin
			t <= 0;
			md <= 0;
		end else begin
			t <=  {  t[36:0], i_valid };
			md <= { md[36:0], i_mode     };
		end
	end
	
	always_ff @(posedge clk) begin
		if( reset ) begin
			tmat <= 0;
		end else begin
			tmat <= { i_valid, tmat[5][0:4], tmat[0:4] };
		end
	end
	
	assign o_valid = t[36];
	assign wt_load = i_valid;
	
	always_comb begin // using coloumn or'ed leading edge t(matrix) to shift kt
		for( int ii = 0; ii < 6; ii++ ) begin
			if( ii == 0 ) 
				kt_shift[0] = |{{ i_valid, tmat[5][0:4] } & ~tmat[0] };
			else 
				kt_shift[ii] = |{ tmat[ii-1] & ~tmat[ii] };
		end
	end

	assign hshift = |t[35:0]; // shift H back to x6 start but need to shift out last hashes
	
	assign init_hash = t[0];
	assign mode_start = md[0];
	assign done = t[36];
	assign mode_done = md[36];
	
	///////
	// Kt
	///////
	
	logic	[0:63][31:0] kt_std; // Sec 4.2.2 6 sixty four constants of 32 bits
	assign kt_std  = { 32'h428a2f98, 32'h71374491, 32'hb5c0fbcf, 32'he9b5dba5,   
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

	logic [0:10][0:5][31:0] kt_reg;
	always_ff @(posedge clk) begin : _kt_reg_array
		if( reset ) begin
			for( int cc = 0; cc < 11; cc++ ) begin
				kt_reg[cc][5] <= kt_std[cc+44]; 
				kt_reg[cc][4] <= kt_std[cc+33]; 
				kt_reg[cc][3] <= kt_std[cc+22];
				kt_reg[cc][2] <= kt_std[cc+11]; 
				kt_reg[cc][1] <= kt_std[cc+ 0]; // Offset so first shift we start again
				kt_reg[cc][0] <= ( cc < 9 ) ? kt_std[cc+55] : 0;
			end
		end else begin
			kt_reg[0] <= ( kt_shift[0] ) ? { kt_reg[0][1:5], kt_reg[0][0] } : kt_reg[0] ;
			kt_reg[1] <= ( kt_shift[1] ) ? { kt_reg[1][1:5], kt_reg[1][0] } : kt_reg[1] ;
			kt_reg[2] <= ( kt_shift[1] ) ? { kt_reg[2][1:5], kt_reg[2][0] } : kt_reg[2] ;
			kt_reg[3] <= ( kt_shift[2] ) ? { kt_reg[3][1:5], kt_reg[3][0] } : kt_reg[3] ;
			kt_reg[4] <= ( kt_shift[2] ) ? { kt_reg[4][1:5], kt_reg[4][0] } : kt_reg[4] ;
			kt_reg[5] <= ( kt_shift[3] ) ? { kt_reg[5][1:5], kt_reg[5][0] } : kt_reg[5] ;
			kt_reg[6] <= ( kt_shift[3] ) ? { kt_reg[6][1:5], kt_reg[6][0] } : kt_reg[6] ;
			kt_reg[7] <= ( kt_shift[4] ) ? { kt_reg[7][1:5], kt_reg[7][0] } : kt_reg[7] ;
			kt_reg[8] <= ( kt_shift[4] ) ? { kt_reg[8][1:5], kt_reg[8][0] } : kt_reg[8] ;
			kt_reg[9] <= ( kt_shift[5] ) ? { kt_reg[9][1:5], kt_reg[9][0] } : kt_reg[9] ;
			kt_reg[10]<= ( kt_shift[5] ) ? { kt_reg[10][1:5],kt_reg[10][0]} : kt_reg[10];
		end
	end
				
	// get kt wires for the 11 stages
	logic [0:10][31:0] kts;
	always_comb
		for( int cc = 0; cc < 11; cc++ )
			kts[cc] = kt_reg[cc][0];
		

	///////
	// Wt
	///////
	
	logic [0:5][0:15][31:0] wt_reg;
	logic	[0:5][0:17][31:0] wt;
	logic [0:5][16:17][31:0] s0, s1;
	logic [0:5][16:17][31:0] w2, w15, w7, w16;
	
	// build wt[64] array function array (with up to 2 extra bits
	always_comb begin
		for( int qq = 0; qq < 6; qq++ ) begin	
			for( int ii = 0; ii < 18; ii++ ) begin
				if( ii < 16 ) begin
					wt[qq][ii] = wt_reg[qq][ii];
				end else begin 
					w2[qq][ii] = wt[qq][ii-2];
					w7[qq][ii] = wt[qq][ii-7];
					w15[qq][ii]= wt[qq][ii-15];
					w16[qq][ii]= wt[qq][ii-16];
					s1[qq][ii] = {  w2[qq][ii][16:0],  w2[qq][ii][31:17] } ^ {  w2[qq][ii][18:0],  w2[qq][ii][31:19] } ^ { 10'b0,  w2[qq][ii][31:10] }; // section 1.1.2, eqn 4.7
					s0[qq][ii] = { w15[qq][ii][ 6:0], w15[qq][ii][31: 7] } ^ { w15[qq][ii][17:0], w15[qq][ii][31:18] } ^ {  3'b0, w15[qq][ii][31: 3] }; // section 1.1.2, eqn 4.6
					wt[qq][ii] = ( s1[qq][ii] + w7[qq][ii] ) + ( s0[qq][ii] + w16[qq][ii] ); // section 6.2.2 step 1		
				end
			end // for
		end // for
	end //always

	always_ff @(posedge clk) begin
		if( reset ) begin
			wt_reg <= 0;
		end else begin
			wt_reg[0] <= ( wt_load ) ? i_data : wt[5][2:17]; // shift last by 2 or new input
			wt_reg[1] <= wt[0][1:16]; // first stage has only 1 round
			wt_reg[2] <= wt[1][2:17]; // stage has 2 rounds
			wt_reg[3] <= wt[2][2:17]; // stage has 2 rounds
			wt_reg[4] <= wt[3][2:17]; // stage has 2 rounds
			wt_reg[5] <= wt[4][2:17]; // stage has 2 rounds
		end
	end	

	// get wt wires for the 11 stages
	logic [0:10][31:0] wts;
	always_comb begin
		wts[0] = wt_reg[0][0];
		wts[1] = wt_reg[1][0];
		wts[2] = wt_reg[1][1];
		wts[3] = wt_reg[2][0];
		wts[4] = wt_reg[2][1];
		wts[5] = wt_reg[3][0];
		wts[6] = wt_reg[3][1];
		wts[7] = wt_reg[4][0];
		wts[8] = wt_reg[4][1];
		wts[9] = wt_reg[5][0];
		wts[10]= wt_reg[5][1];
	end	
	
	///////
	// SHA 
	///////

	// Comb logic for 11 Rounds
	reg   [0:10][31:0]  ch_e_f_g, maj_a_b_c, sig1_e, sig0_a;
	reg   [0:10][31:0]  da, db, dc, dd, de, df, dg, dh;
	reg   [0:10][31:0]  qa, qb, qc, qd, qe, qf, qg, qh;

	always_comb begin
		for( int ii = 0; ii < 11; ii++ ) begin : _sha_logic  // step 3 of 6.2.2
			ch_e_f_g[ii] = (de[ii] & df[ii]) ^ (~de[ii] & dg[ii]);
			maj_a_b_c[ii]= (da[ii] & db[ii]) ^ ( da[ii] & dc[ii]) ^ (db[ii] & dc[ii]);
			sig1_e[ii]   = { de[ii][5:0], de[ii][31:6] } ^ { de[ii][10:0], de[ii][31:11] } ^ { de[ii][24:0], de[ii][31:25] };
			sig0_a[ii]   = { da[ii][1:0], da[ii][31:2] } ^ { da[ii][12:0], da[ii][31:13] } ^ { da[ii][21:0], da[ii][31:22] };
			qa[ii] = ((dh[ii] + wts[ii]) + kts[ii]) + ( (ch_e_f_g[ii] + sig1_e[ii]) + (sig0_a[ii] + maj_a_b_c[ii]) );
			qb[ii] = da[ii];
			qc[ii] = db[ii];
			qd[ii] = dc[ii];
			qe[ii] = ((dh[ii] + wts[ii]) + (kts[ii] + dd[ii])) + (ch_e_f_g[ii] + sig1_e[ii]);
			qf[ii] = de[ii];
			qg[ii] = df[ii];
			qh[ii] = dg[ii];	
		end
	end

	// Wire up stages and stage registers
	reg [0:6][0:7][31:0] acc_reg;
	always_ff @(posedge clk) begin // Stage Registers
		if( reset ) begin
			acc_reg <= 0;
		end else begin
			acc_reg[0] <= { qa[0], qb[0], qc[0], qd[0], qe[0], qf[0], qg[0], qh[0] };
			acc_reg[1] <= { qa[2], qb[2], qc[2], qd[2], qe[2], qf[2], qg[2], qh[2] };
			acc_reg[2] <= { qa[4], qb[4], qc[4], qd[4], qe[4], qf[4], qg[4], qh[4] };
			acc_reg[3] <= { qa[6], qb[6], qc[6], qd[6], qe[6], qf[6], qg[6], qh[6] };
			acc_reg[4] <= { qa[8], qb[8], qc[8], qd[8], qe[8], qf[8], qg[8], qh[8] };
			acc_reg[5] <= { qa[10],qb[10],qc[10],qd[10],qe[10],qf[10],qg[10],qh[10]};
		end
	end
	always_comb begin // stage inputs (except stage 0)
		//{ da[0] , db[0] , dc[0] , dd[0] , de[0] , df[0] , dg[0] , dh[0] } = acc_reg[0];
		{ da[1] , db[1] , dc[1] , dd[1] , de[1] , df[1] , dg[1] , dh[1] } = acc_reg[0];
		{ da[2] , db[2] , dc[2] , dd[2] , de[2] , df[2] , dg[2] , dh[2] } = { qa[1], qb[1], qc[1], qd[1], qe[1], qf[1], qg[1], qh[1] };
		{ da[3] , db[3] , dc[3] , dd[3] , de[3] , df[3] , dg[3] , dh[3] } = acc_reg[1];                                    
		{ da[4] , db[4] , dc[4] , dd[4] , de[4] , df[4] , dg[4] , dh[4] } = { qa[3], qb[3], qc[3], qd[3], qe[3], qf[3], qg[3], qh[3] };
		{ da[5] , db[5] , dc[5] , dd[5] , de[5] , df[5] , dg[5] , dh[5] } = acc_reg[2];                                    
		{ da[6] , db[6] , dc[6] , dd[6] , de[6] , df[6] , dg[6] , dh[6] } = { qa[5], qb[5], qc[5], qd[5], qe[5], qf[5], qg[5], qh[5] };
		{ da[7] , db[7] , dc[7] , dd[7] , de[7] , df[7] , dg[7] , dh[7] } = acc_reg[3];                                    
		{ da[8] , db[8] , dc[8] , dd[8] , de[8] , df[8] , dg[8] , dh[8] } = { qa[7], qb[7], qc[7], qd[7], qe[7], qf[7], qg[7], qh[7] };
		{ da[9] , db[9] , dc[9] , dd[9] , de[9] , df[9] , dg[9] , dh[9] } = acc_reg[4];                                    
		{ da[10], db[10], dc[10], dd[10], de[10], df[10], dg[10], dh[10]} = { qa[9], qb[9], qc[9], qd[9], qe[9], qf[9], qg[9], qh[9] };
	end
	
	///////
	// HASH 
	///////
	
	logic [0:5][0:7][31:0] hash_reg;
	logic [0:7][31:0] sum_reg;
	always_ff @(posedge clk) begin
		if( reset ) begin
			hash_reg <= 0;
		end else begin
			// Pipeline of hash regs
			if( hshift ) begin
				hash_reg[1] <= hash_reg[0];
				hash_reg[2] <= hash_reg[1];
				hash_reg[3] <= hash_reg[2];
				hash_reg[4] <= hash_reg[3];
				hash_reg[5] <= hash_reg[4];

				// Sum reg aligns with hashreg 5
				for( int ii = 0; ii < 8; ii++ )
					sum_reg[ii] <= hash_reg[4][ii] + acc_reg[4][ii];

				// Input to hash reg pipe 
				if( init_hash && mode_start == MODE_INIT ) begin // load H* if first sha round
					hash_reg[0][0] <= 32'h6a09e667;
					hash_reg[0][1] <= 32'hbb67ae85;
					hash_reg[0][2] <= 32'h3c6ef372;
					hash_reg[0][3] <= 32'ha54ff53a;
					hash_reg[0][4] <= 32'h510e527f;
					hash_reg[0][5] <= 32'h9b05688c;
					hash_reg[0][6] <= 32'h1f83d9ab;
					hash_reg[0][7] <= 32'h5be0cd19;	
				end else if( init_hash && mode_start == !MODE_REDO ) begin // Load old
					hash_reg[0] <= sum_reg;
				end else begin
					hash_reg[0] <= hash_reg[0];
				end	
			end // hshift
		end // !reset
	end

	// First input round
		// starting hash
	always_comb begin
		if( init_hash ) begin
			if( mode_start == MODE_INIT ) begin // load standard start value
				da[0] = 32'h6a09e667;
				db[0] = 32'hbb67ae85;
				dc[0] = 32'h3c6ef372;
				dd[0] = 32'ha54ff53a;
				de[0] = 32'h510e527f;
				df[0] = 32'h9b05688c;
				dg[0] = 32'h1f83d9ab;
				dh[0] = 32'h5be0cd19;   // Step 2 for 6.1.2 and 6.2.2
			end else if ( mode_start == MODE_HASH ) begin // begin with sum hash
				{ da[0] , db[0] , dc[0] , dd[0] , de[0] , df[0] , dg[0] , dh[0] } = sum_reg;
			end else if( init_hash && mode_start == MODE_REDO) begin // reload old hash
				{ da[0] , db[0] , dc[0] , dd[0] , de[0] , df[0] , dg[0] , dh[0] } = hash_reg[5];
			end
		end else begin // else normal case feed from acc reg
				{ da[0] , db[0] , dc[0] , dd[0] , de[0] , df[0] , dg[0] , dh[0] } = acc_reg[5];
		end	

	end
	
	// Output is always the sum
	assign o_data = sum_reg;
	
endmodule // sha_11_6_core
	
	
	
	
	
	

// Single Core, 1 hash in 64 cycles, SHA-256 core
// Tested and working. 
module sha_core (
	input logic clk,
	input logic reset,
	// Input strobe and message
	input logic 		  in_valid,
	input logic	[1:0]		  mode, 
	input logic [511:0] message,
	// Output 
	output logic         out_valid,
	output logic [0:7][31:0] hash
	);
	
	localparam MODE_INIT = 1;	// Will init with H* at start (both input and REG)
	localparam MODE_HASH = 0;	// starts with REG and will Update reg at END (Normal steady state(
	localparam MODE_REDO = 3;  // starts with Reg, but discards value at end (keeping REG unaltered for REDO
	

	// Control logic
	logic kt_shift;	
	logic wt_shift;
	logic wt_load;	
	logic init_hash;	
	logic [1:0] mode_start;
	logic [1:0] mode_done;

	
	logic [65:0] t;
	logic [65:0][1:0] md;
	
	always_ff @(posedge clk) begin
		t <=  {  t[64:0], in_valid };
		md <= { md[64:0], mode     };
	end
	
	assign out_valid = t[64];
	assign kt_shift = |t[63:0];
	assign wt_load = in_valid;
	assign wt_shift = |t[62:0];
	assign init_hash = t[0];
	assign mode_start = md[0];
	assign done = t[64];
	assign mode_done = md[64];
	
	///////
	// Kt
	///////

	logic [0:63][31:0] kt_reg;
	logic	[0:63][31:0] kt;
	assign kt = kt_reg;
	
	int sh_wid = 1; // shift 1 to 48 rounds per cycle	

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
			for( int ii = 0; ii < 64; ii++ ) begin
					if( ii+sh_wid >= 64 )
						kt_reg[ii] <= kt_reg[ii+sh_wid-64];
					else
						kt_reg[ii] <= kt_reg[ii+sh_wid];
			end
			//kt_reg <= { kt_reg[1:63], kt_reg[0] }; // shift/Rotate through the values kt. ROM maybe?
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
	

	always_ff @(posedge clk) begin
		if( wt_load ) begin
			wt_reg <= message;						// Initally load with message 16 words
		end else if( wt_shift ) begin
			for( int ii = 0; ii < 16; ii++ ) begin
				wt_reg[ii] = wt[ii+sh_wid];
			end
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
		if( init_hash && mode_start == MODE_INIT ) begin // load standard start value
			da[0] = 32'h6a09e667;
			db[0] = 32'hbb67ae85;
			dc[0] = 32'h3c6ef372;
			dd[0] = 32'ha54ff53a;
			de[0] = 32'h510e527f;
			df[0] = 32'h9b05688c;
			dg[0] = 32'h1f83d9ab;
			dh[0] = 32'h5be0cd19;   // Step 2 for 6.1.2 and 6.2.2
		end else if ( init_hash && done & mode_done != MODE_REDO ) begin // overlap start/end non init, use sum to continue but not if REDO
			da[0] = hash_reg[0] + acc_reg[0];
			db[0] = hash_reg[1] + acc_reg[1];
			dc[0] = hash_reg[2] + acc_reg[2];
			dd[0] = hash_reg[3] + acc_reg[3];
			de[0] = hash_reg[4] + acc_reg[4];
			df[0] = hash_reg[5] + acc_reg[5];
			dg[0] = hash_reg[6] + acc_reg[6];
			dh[0] = hash_reg[7] + acc_reg[7];	
	   end else if( init_hash ) begin // load current hash
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

	always_ff @(posedge clk) begin
		acc_reg <= { qa[sh_wid-1], qb[sh_wid-1], qc[sh_wid-1], qd[sh_wid-1], qe[sh_wid-1], qf[sh_wid-1], qg[sh_wid-1], qh[sh_wid-1] };
	end
	
	always_ff @(posedge clk) begin
		if( init_hash && mode_start == MODE_INIT ) begin // load H* if first sha round
			hash_reg[0] = 32'h6a09e667;
			hash_reg[1] = 32'hbb67ae85;
			hash_reg[2] = 32'h3c6ef372;
			hash_reg[3] = 32'ha54ff53a;
			hash_reg[4] = 32'h510e527f;
			hash_reg[5] = 32'h9b05688c;
			hash_reg[6] = 32'h1f83d9ab;
			hash_reg[7] = 32'h5be0cd19;		
		end else if( done && mode_done != MODE_REDO  ) begin // accumulate and hold this hash, but discard on REDO
			for( int ii = 0 ; ii < 8; ii++ ) begin
				hash_reg[ii] <= hash_reg[ii] + acc_reg[ii];
			end
		end else begin
			hash_reg <= hash_reg;
		end
	end
	
	always_comb begin
		for( int ii = 0 ; ii < 8; ii++ ) begin
			hash[ii] = hash_reg[ii] + acc_reg[ii]; // output is always the sum
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
