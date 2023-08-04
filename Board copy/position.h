#include <iostream>
#include <cmath>
#include "bit_twiddling.h"
#include "eval.h"
#include "fen.h"
#include "hash.h"
#include "initialize_magic_bitboard.h"
#include "magic_masks.h"
#include "magics.h"
#include "preinitialized_arrays.h"
#include "squares.h"
//#include "move.h"

enum Piece_Types {P, p, N, n, B, b, R, r, Q, q, K, k, E};

enum colors {WHITE, BLACK, BOTH};

class Legal_Moves{
	public:
		//declare bitboard array
		uint64_t bitboards [13];
		//Castling rights
		bool Castle_White_Kingside;
		bool Castle_White_Queenside;
		bool Castle_Black_Kingside;
		bool Castle_Black_Queenside;
		std::vector <int> Castling_Rights;
		//en passant square
		std::vector <int> En_Passant_Sq;
		//side to move
		bool side_to_move;
		//Occupancies
		uint64_t occupancies[3];
		//get rook attacks
		inline uint64_t rook_attacks(int square, uint64_t occupancy);
		inline uint64_t bishop_attacks(int square, uint64_t occupancy);
		inline uint64_t queen_attacks(int square, uint64_t occupancy);
		//Rectangular lookup tables
		uint64_t Rect_Lookup [64] [64]; //[sq1] [sq2]
		//for initializing Rect_Lookup
		uint64_t inBetween(int sq1, int sq2);
		//init slider piece's attack tables
		void init_sliders_attacks(int bishop);
		//update occupancies
		void update_occupancies();
		//Direction relative to square one to square two
		int pin_direction [64] [64];
		//encode moves
		inline uint64_t encode_moves(int sourcesq, int targetsq, int piece, int promote, int capture, int move_flag);
		//xray rook attacks (for pins)
		inline uint64_t xray_rook_attacks(uint64_t occ, uint64_t blockers, int rookSq);
		//xray bishop attacks (for pins)
		inline uint64_t xray_bishop_attacks(uint64_t occ, uint64_t blockers, int bishopSq);
		//get absolute pins
		inline uint64_t absolute_pins(int side, int squareOfKing);
		//direction between two squares
		int direction(int sq1, int sq2);
		const uint64_t rook_positions [4] = {0x80, 0x8000000000000000, 0x1, 0x100000000000000};
		const uint64_t castle_constants [4] = {0x60, 0xc, 0x6000000000000000, 0xc00000000000000};
		const uint64_t castle_king_constants [2] = {0x10, 0x1000000000000000};
		const uint64_t en_passant_ranks [2] = {0xff00000000, 0xff000000};
		//promotion ranks
		const uint64_t promotion_ranks [2] = {0xff000000000000, 0xff00};
		//Castle masks for castle rights bits
		const int castle_modifiers [4] [2] = {
			{0xe, 0xf}, 
			{0xd, 0xf},
			{0xb, 0xf},
			{0x7, 0xf},
		};
		const uint64_t space_setbacks [2] = {0xe, 0xe00000000000000};
		const int remove_castle [2] = {0xa, 0x5};
		const int eval_multiplier [2] = {1, -1};
		//Pieces that attack a square
		inline uint64_t square_attackers(int square, int Side);
		//Attack map that doesn't treat king as a blocker so king has correct avoid mask
		inline uint64_t king_danger_squares(int side);
		//To castle, or not to castle
		inline bool castle_kingside(int side, uint64_t attacks);
		inline bool castle_queenside(int side, uint64_t attacks);
	
		inline bool legal_en_passant(int side, int source, int target);
		inline void legal_moves(std::vector <MOVE> &move_list);
		inline void legal_captures(std::vector <MOVE> &move_list);
		inline int count_moves();
		//Piece list for Redundancy
		int piece_list [64] = {
				E, E, E, E, E, E, E, E,
				E, E, E, E, E, E, E, E,
				E, E, E, E, E, E, E, E,
				E, E, E, E, E, E, E, E,
				E, E, E, E, E, E, E, E,
				E, E, E, E, E, E, E, E,
				E, E, E, E, E, E, E, E,
				E, E, E, E, E, E, E, E,
		};
		//print board
		void print();
		void print_bits(uint64_t bitboards);
		//Return move encoding
		inline int get_move(int source, int target, int piece, int capture, int promote, int flag){
			 return source | (target << 6) | (piece << 12) | (capture << 16) | (promote << 20) | (flag << 24);
		}
		//Make and unmake move
		inline void push_move(MOVE move);
		inline void pop_move(MOVE move);
		void Initialize_Everything(std::string input);
		void print_move_list(std::vector <MOVE> &move_list);
		void print_move_scores(std::vector <MOVE> &move_list);
		inline void remove_sq(int sq);
		inline void set_sq(int sq, int piece);
		void initialize_eval();
		int static_evaluation();
		//Zobrist hashing and stuff
		uint64_t rand64() {
	  	static uint64_t next = 1;
	  	next = next * 1103515245 + 12345;
	  	return next;
		}
		uint64_t hash = 0;
		uint64_t piece_keys [13] [64] = {};
		uint64_t castle_keys [16] = {};
		uint64_t en_passant_keys [65] = {};
		uint64_t turn_key;
		void init_hash(int board[64], std::vector <int> en_passant, std::vector <int> castling, bool side_to_move);
	
		//This assumes check condition has been met beforehand
		inline void push_null_move();
		inline void pop_null_move();
	
		inline bool in_check(){
			return square_attackers(get_ls1b(bitboards[K + side_to_move]), !side_to_move);
		}
		void print_info(){
			print();
			std::cout<<"Side to Move: "<<(side_to_move ? "BLACK" : "WHITE")<<"\n";
			std::cout<<"Castling: ";
			//Kingside White
			if (Castling_Rights.back() & 1) {
    		std::cout<<"K";
  		}
			//Queenside White
  		if (Castling_Rights.back() & 4) {
    		std::cout<<"Q";
  		}
			//Kingside Black
  		if (Castling_Rights.back() & 2) {
    		std::cout<<"k";
  		}
			//Queenside Black
  		if (Castling_Rights.back() & 8) {
    		std::cout<<"q";
  		}
			std::cout<<std::endl;
			std::cout<<"En Passant Square: "<<coordinates[En_Passant_Sq.back()]<<"\n";
			std::cout<<"0x"<<std::hex<<hash<<"\n";
		}

		void flip(){
			side_to_move ^= 1;
		}
	};
	
	inline uint64_t Legal_Moves::rook_attacks(int square, uint64_t occupancy){
		return lookup_table[rook_magics[square].offset + ((occupancy | rook_premask[square]) * rook_magics[square].magic >> 52)];
}

inline uint64_t Legal_Moves::bishop_attacks(int square, uint64_t occupancy){
	return lookup_table[bishop_magics[square].offset + ((occupancy | bishop_premask[square]) * bishop_magics[square].magic >> 55)];
}

inline uint64_t Legal_Moves::queen_attacks(int square, uint64_t occupancy){
	return rook_attacks(square, occupancy) | bishop_attacks(square, occupancy);
}

uint64_t Legal_Moves::inBetween(int sq1, int sq2) {
  const  uint64_t m1   = -1;
  const  uint64_t a2a7 = 0x0001010101010100;
  const  uint64_t b2g7 = 0x0040201008040200;
  const  uint64_t h1b7 = 0x0002040810204080;
  uint64_t btwn, line, rank, file;
  btwn  = (m1 << sq1) ^ (m1 << sq2);
  file  =   (sq2 & 7) - (sq1   & 7);
  rank  =  ((sq2 | 7) -  sq1) >> 3 ;
  line  =      (   (file  &  7) - 1) & a2a7;
  line += 2 * ((   (rank  &  7) - 1) >> 58);
  line += (((rank - file) & 15) - 1) & b2g7;
  line += (((rank + file) & 15) - 1) & h1b7;
  line *= btwn & -btwn;
  return line & btwn;
}

void Legal_Moves::init_sliders_attacks(int bishop){
  // loop over 64 board squares
  for (int square = 0; square < 64; square++){
    // init rook masks
    uint64_t attack_mask = ~rook_premask[square];
    // init relevant occupancy bit count
    int relevant_bits_count = pop_count(attack_mask);
    // init occupancy indicies
    int occupancy_indicies = (1 << relevant_bits_count);
    // loop over occupancy indicies
    for (int index = 0; index < occupancy_indicies; index++){
      // init current occupancy variation
      uint64_t occupancy = set_occupancy(index, relevant_bits_count, attack_mask);
      // init magic index
      int magic_index = ((occupancy | rook_premask[square]) * rook_magics[square].magic) >> 52;
      // init bishop attacks
			lookup_table[magic_index + rook_magics[square].offset] = rook_attacks_on_the_fly(square, occupancy);
		}
	}
	
	for (int square = 0; square < 64; square++){
    // init bishop masks
    uint64_t attack_mask = ~bishop_premask[square];
    // init relevant occupancy bit count
    int relevant_bits_count = pop_count(attack_mask);
    // init occupancy indicies
    int occupancy_indicies = (1 << relevant_bits_count);
    // loop over occupancy indicies
    for (int index = 0; index < occupancy_indicies; index++){
      // init current occupancy variation
      uint64_t occupancy = set_occupancy(index, relevant_bits_count, attack_mask);
      // init magic index
      int magic_index = ((occupancy | bishop_premask[square]) * bishop_magics[square].magic) >> 55;
    	// init bishop attacks
      lookup_table[magic_index + bishop_magics[square].offset] = bishop_attacks_on_the_fly(square, occupancy);
		}
	}
}

void Legal_Moves::update_occupancies(){
	occupancies[0] = bitboards[P] | bitboards [N] | bitboards[B] | bitboards[R] | bitboards[Q] | bitboards[K];
	occupancies[1] = bitboards[p] | bitboards [n] | bitboards[b] | bitboards[r] | bitboards[q] | bitboards[k];
	occupancies[2] = occupancies[0] | occupancies[1];
}

inline uint64_t Legal_Moves::encode_moves(int sourcesq, int targetsq, int piece, int promote, int capture, int move_flag){
	return sourcesq | (targetsq << 6) | (piece << 12) | (promote << 16) | (capture << 20) | (move_flag << 24);
}

inline uint64_t Legal_Moves::xray_rook_attacks(uint64_t occ, uint64_t blockers, int rookSq) {
	uint64_t attacks = rook_attacks(rookSq, occ);
  blockers &= attacks;
  return attacks ^ rook_attacks(rookSq, occ ^ blockers);
}

inline uint64_t Legal_Moves::xray_bishop_attacks(uint64_t occ, uint64_t blockers, int bishopSq) {
	uint64_t attacks = bishop_attacks(bishopSq, occ);
	blockers &= attacks;
  return attacks ^ bishop_attacks(bishopSq, occ ^ blockers);
}

inline uint64_t Legal_Moves::absolute_pins(int side, int squareOfKing){
	uint64_t pinned, pinner;
	pinned = 0ULL;
	side ^= 1;
	//Rook
	pinner = xray_rook_attacks(occupancies[BOTH], occupancies[side ^ 1], squareOfKing) & (bitboards[R + side] | bitboards[Q + side]);
	while (pinner) {
		//set square for rook
    int sq = get_ls1b(pinner);
    pinned |= Rect_Lookup[sq] [squareOfKing] & occupancies[side ^ 1];
		//delete least significant bit
    pinner &= pinner - 1;
	}
	//Bishop
	pinner = xray_bishop_attacks(occupancies[BOTH], occupancies[side ^ 1], squareOfKing) & (bitboards[B + side] | bitboards[Q + side]);
	while (pinner) {
		//set square for bishop
    int sq = get_ls1b(pinner);
    pinned |= Rect_Lookup[sq] [squareOfKing] & occupancies[side ^ 1];
		//delete least significant bit
    pinner &= pinner - 1;
	}
	return pinned;
}

int Legal_Moves::direction(int sq1, int sq2){
	if(sq1 == sq2) {
		return 8; //error, not applicable
	}
	for(int direction = 0; direction < 8; direction++){
		//finds ray to test on
		uint64_t test_mask = masks[direction][sq1];
		//find square 2 as bitboard
		uint64_t squares_bitboard = (1ULL << sq2);
		if(test_mask & squares_bitboard){
			return direction;
		}
	}
	return 8; //if there isn't a ray direction
}

inline uint64_t Legal_Moves::square_attackers(int square, int Side){
	uint64_t attackers = 0ULL;
	//Pawns
	uint64_t board = bitboards[Side];
	attackers |= Pawn_Attacks[!Side][square] & board;
	//Knights
	board = bitboards[N + Side];
	attackers |= Knight_Attacks[square] & board;
	//Bishops and Queens
	board = bitboards[B + Side] | bitboards[Q + Side];
	attackers |= bishop_attacks(square, occupancies[BOTH]) & board;
		
	//Rooks and Queens
	board = bitboards[R + Side] | bitboards[Q + Side];
	attackers |= rook_attacks(square, occupancies[BOTH]) & board;
	//Kings
	board = bitboards[K + Side];
	attackers |= King_Mask[square] & board;
	return attackers;
}

//work on this
inline uint64_t Legal_Moves::king_danger_squares(int side){
	uint64_t mapped_attacks{};
	//remove opponent king
	uint64_t blockers = occupancies[BOTH] ^ bitboards[K + (!side)];
	//pawns
	uint64_t board = bitboards[side];
	while(board){
		mapped_attacks |= Pawn_Attacks[side][return_ls1b(board)];
	}
	//knights
	board = bitboards[N + side];
	while(board){
		mapped_attacks |= Knight_Attacks[return_ls1b(board)];
	}
	//bishops and queens
	board = bitboards[B + side] | bitboards[Q + side];
	while(board){
		mapped_attacks |= bishop_attacks(return_ls1b(board), blockers);
	}
	//rooks and queens
	board = bitboards[R + side] | bitboards[Q + side];
	while(board){
		mapped_attacks |= rook_attacks(return_ls1b(board), blockers);
	}
	//king
	board = bitboards[K + side];
	while(board){
		mapped_attacks |= King_Mask[return_ls1b(board)];
	}
	return mapped_attacks;
}

inline bool Legal_Moves::castle_kingside(int side, uint64_t attacks){
	//Temporary setbacks for castling, not the permanent kind
	bool Temp = (attacks & (castle_king_constants[side] | castle_constants[side * 2])) || (castle_constants[side * 2] & occupancies[BOTH]);
		
	return !Temp && (Castling_Rights.back() & (1ULL << side));
}

inline bool Legal_Moves::castle_queenside(int side, uint64_t attacks){
	//Temporary setbacks for castling, not the permanent kind
	bool Temp = (attacks & (castle_king_constants[side] | castle_constants[1 + side * 2])) || (space_setbacks[side] & occupancies[BOTH]);
		
	return !Temp && (Castling_Rights.back() & (1ULL << (side + 2)));
}

inline bool Legal_Moves::legal_en_passant(int side, int source, int target){
	uint64_t attackers = 0ULL;
	int kingsq = get_ls1b(bitboards[K + side]);
	uint64_t revise_occ = (1ULL << ((source & 56) + (target & 7))) | (1ULL << source);
	//Pawns
	uint64_t board = bitboards[!side] ^ (1ULL << source);
	attackers |= Pawn_Attacks[side][kingsq] & board;
	//Knights
	board = bitboards[N + !side];
	attackers |= Knight_Attacks[kingsq] & board;
	//Bishops and Queens
	board = bitboards[B + !side] | bitboards[Q + !side];
	attackers |= bishop_attacks(kingsq, occupancies[BOTH] ^ revise_occ) & board;
		
	//Rooks and Queens
	board = bitboards[R + !side] | bitboards[Q + !side];
	attackers |= rook_attacks(kingsq, occupancies[BOTH] ^ revise_occ) & board;
	//Kings
	board = bitboards[K + !side];
	attackers |= King_Mask[kingsq] & board;
	return !attackers;
}

void Legal_Moves::legal_moves(std::vector <MOVE> &move_list){
	move_list = {};
	//get side to move
	bool side = side_to_move;
	//source and target squares
	int sourcesq, targetsq;
	//side constant
	//king square
	int kingsq = get_ls1b(bitboards[K + side]);
	//get pins
	uint64_t pinned = absolute_pins(side, kingsq);
	//get checkers
	uint64_t king_attacked_by = square_attackers(kingsq, side ^ 1);
	//pinmask
	uint64_t pinmask;
	//targets
	uint64_t targets;
	//Pawn blockers for double push
	//checkmask
	uint64_t checkmask = ~uint64_t(0);
	//Is pawn double pushed?
	bool double_pushed;
	//offset for castling
	const int castle_offset = side * 56;
	//opposite occupancies
	uint64_t opposite_occ = occupancies[side ^ 1];	
	//king danger squares
	uint64_t avoid = king_danger_squares(side ^ 1);
	uint64_t Board;
	//King moves (to apply before double check evasion because it will happen anyways)
	//get targets for attacks (not working?)
	targets = King_Mask[kingsq] & ~occupancies[side] & ~avoid;
	for(; targets; targets &= targets - 1){
		targetsq = get_ls1b(targets);
		move_list.push_back({get_move(kingsq, targetsq, K + side, piece_list[targetsq], 0, none), 0});
	}

	if(pop_count(king_attacked_by) > 1){
		return;
	}

	if(king_attacked_by){
		//checkmask
		checkmask ^= ~(Rect_Lookup[kingsq][get_ls1b(king_attacked_by)] | king_attacked_by);

		//Queen moves
		Board = bitboards[Q + side];
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get pinmask
			pinmask = masks[pin_direction[kingsq][get_bit(pinned, sourcesq) ? sourcesq : kingsq]][kingsq];
			//get targets for attacks
			targets = (queen_attacks(sourcesq, occupancies[BOTH]) & pinmask) & ~occupancies[side] & checkmask;
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				move_list.push_back({get_move(sourcesq, targetsq, Q + side, piece_list[targetsq], 0, none), 0});
			}
		}
	
		//Rook moves
		Board = bitboards[R + side];
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get pinmask
			pinmask = masks[pin_direction[kingsq][get_bit(pinned, sourcesq) ? sourcesq : kingsq]][kingsq];
			//get targets for attacks
			targets = (rook_attacks(sourcesq, occupancies[BOTH]) & pinmask) & ~occupancies[side] & checkmask;
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				move_list.push_back({get_move(sourcesq, targetsq, R + side, piece_list[targetsq], 0, none), 0});
			}
		}

		//Bishop moves
		Board = bitboards[B + side];
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get pinmask
			pinmask = masks[pin_direction[kingsq][get_bit(pinned, sourcesq) ? sourcesq : kingsq]][kingsq];
			//get targets for attacks
			targets = (bishop_attacks(sourcesq, occupancies[BOTH]) & pinmask) & ~occupancies[side] & checkmask;
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				move_list.push_back({get_move(sourcesq, targetsq, B + side, piece_list[targetsq], 0, none), 0});
			}
		}

		//Knight moves
		Board = bitboards[N + side] & ~pinned;
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get targets for attacks
			targets = Knight_Attacks[sourcesq] & ~occupancies[side] & checkmask;
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				move_list.push_back({get_move(sourcesq, targetsq, N + side, piece_list[targetsq], 0, none), 0});
			}
		}

		//Pawn moves (not promoting)
		Board = bitboards[P + side] & ~promotion_ranks[side];
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get pinmask
			pinmask = masks[pin_direction[kingsq][get_bit(pinned, sourcesq) ? sourcesq : kingsq]][kingsq];
			//get targets for attacks
			targets = ((Pawn_Attacks[side][sourcesq] & opposite_occ) | (Pawn_Push[side][sourcesq] & rook_attacks(sourcesq, occupancies[BOTH]) & ~occupancies[BOTH])) & checkmask & pinmask;
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				double_pushed = abs(targetsq - sourcesq) > 15;
				move_list.push_back({get_move(sourcesq, targetsq, side, piece_list[targetsq], 0, double_pushed << 2), 0});
			}
		}

		//Pawn moves (promoting)
		Board = bitboards[P + side] & promotion_ranks[side];
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get pinmask
			pinmask = masks[pin_direction[kingsq][get_bit(pinned, sourcesq) ? sourcesq : kingsq]][kingsq];
			//get targets for attacks
			targets = ((Pawn_Attacks[side][sourcesq] & opposite_occ) | (Pawn_Push[side][sourcesq] & ~occupancies[BOTH])) & checkmask & pinmask;
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				move_list.push_back({get_move(sourcesq, targetsq, side, piece_list[targetsq], Q, none), 0});
				move_list.push_back({get_move(sourcesq, targetsq, side, piece_list[targetsq], R, none), 0});
				move_list.push_back({get_move(sourcesq, targetsq, side, piece_list[targetsq], B, none), 0});
				move_list.push_back({get_move(sourcesq, targetsq, side, piece_list[targetsq], N, none), 0});
			}
		}

		//en passant
		int sq = En_Passant_Sq.back();
		//get potential target pawns (aka attacker pawns)
		targets = Pawn_Attacks[side ^ 1][sq] & bitboards[side];
		for(int iteration = pop_count(targets); iteration > 0; iteration--){
			sourcesq = get_ls1b(targets);
			if(legal_en_passant(side, sourcesq, sq)){
				move_list.push_back({get_move(sourcesq, sq, side, !side, 0, enpassant), 0});
			}
			//remove ls1b
			targets &= targets - 1;
		}
	} else {
		int castle = Castling_Rights.back();
		//castling moves
		if(castle_kingside(side, avoid)){
			move_list.push_back({get_move(e1 + castle_offset, g1 + castle_offset, K + side, E, 0, k_castling), 0});
		}
	
		if(castle_queenside(side, avoid)){
			move_list.push_back({get_move(e1 + castle_offset, c1 + castle_offset, K + side, E, 0, q_castling), 0});
		}

		//Queen moves
		Board = bitboards[Q + side];
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get pinmask
			pinmask = masks[pin_direction[kingsq][get_bit(pinned, sourcesq) ? sourcesq : kingsq]][kingsq];
			//get targets for attacks
			targets = (queen_attacks(sourcesq, occupancies[BOTH]) & pinmask) & ~occupancies[side];
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				move_list.push_back({get_move(sourcesq, targetsq, Q + side, piece_list[targetsq], 0, none), 0});
			}
		}
	
		//Rook moves
		Board = bitboards[R + side];
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get pinmask
			pinmask = masks[pin_direction[kingsq][get_bit(pinned, sourcesq) ? sourcesq : kingsq]][kingsq];
			//get targets for attacks
			targets = (rook_attacks(sourcesq, occupancies[BOTH]) & pinmask) & ~occupancies[side];
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				move_list.push_back({get_move(sourcesq, targetsq, R + side, piece_list[targetsq], 0, none), 0});
			}
		}

		//Bishop moves
		Board = bitboards[B + side];
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get pinmask
			pinmask = masks[pin_direction[kingsq][get_bit(pinned, sourcesq) ? sourcesq : kingsq]][kingsq];
			//get targets for attacks
			targets = (bishop_attacks(sourcesq, occupancies[BOTH]) & pinmask) & ~occupancies[side];
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				move_list.push_back({get_move(sourcesq, targetsq, B + side, piece_list[targetsq], 0, none), 0});
			}
		}

		//Knight moves
		Board = bitboards[N + side] & ~pinned;
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get targets for attacks
			targets = Knight_Attacks[sourcesq] & ~occupancies[side];
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				move_list.push_back({get_move(sourcesq, targetsq, N + side, piece_list[targetsq], 0, none), 0});
			}
		}

		//Pawn moves (not promoting)
		Board = bitboards[P + side] & ~promotion_ranks[side];
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get pinmask
			pinmask = masks[pin_direction[kingsq][get_bit(pinned, sourcesq) ? sourcesq : kingsq]][kingsq];
			//get targets for attacks
			targets = ((Pawn_Attacks[side][sourcesq] & opposite_occ) | (Pawn_Push[side][sourcesq] & rook_attacks(sourcesq, occupancies[BOTH]) & ~occupancies[BOTH])) & pinmask;
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				double_pushed = abs(targetsq - sourcesq) > 15;
				move_list.push_back({get_move(sourcesq, targetsq, side, piece_list[targetsq], 0, double_pushed << 2), 0});
			}
		}

		//Pawn moves (promoting)
		Board = bitboards[P + side] & promotion_ranks[side];
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get pinmask
			pinmask = masks[pin_direction[kingsq][get_bit(pinned, sourcesq) ? sourcesq : kingsq]][kingsq];
			//get targets for attacks
			targets = ((Pawn_Attacks[side][sourcesq] & opposite_occ) | (Pawn_Push[side][sourcesq] & ~occupancies[BOTH])) & pinmask;
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				move_list.push_back({get_move(sourcesq, targetsq, side, piece_list[targetsq], Q, none), 0});
				move_list.push_back({get_move(sourcesq, targetsq, side, piece_list[targetsq], R, none), 0});
				move_list.push_back({get_move(sourcesq, targetsq, side, piece_list[targetsq], B, none), 0});
				move_list.push_back({get_move(sourcesq, targetsq, side, piece_list[targetsq], N, none), 0});
			}
		}

		//en passant
		int sq = En_Passant_Sq.back();
		//get potential target pawns (aka attacker pawns)
		targets = Pawn_Attacks[side ^ 1][sq] & bitboards[side];
		for(int iteration = pop_count(targets); iteration > 0; iteration--){
			sourcesq = get_ls1b(targets);
			if(legal_en_passant(side, sourcesq, sq)){
				move_list.push_back({get_move(sourcesq, sq, side, !side, 0, enpassant), 0});
			}
			//remove ls1b
			targets &= targets - 1;
		}
	}
}

void Legal_Moves::legal_captures(std::vector <MOVE> &move_list){
	move_list = {};
	//get side to move
	bool side = side_to_move;
	//source and target squares
	int sourcesq, targetsq;
	//side constant
	//king square
	int kingsq = get_ls1b(bitboards[K + side]);
	//get pins
	uint64_t pinned = absolute_pins(side, kingsq);
	//get checkers
	uint64_t king_attacked_by = square_attackers(kingsq, !side);
	//pinmask
	uint64_t pinmask;
	//targets
	uint64_t targets;
	//checkmask
	uint64_t checkmask = ~uint64_t(0);
	//Is pawn double pushed?
	bool double_pushed;
	//offset for castling
	const int castle_offset = side * 56;
	//opposite occupancies
	uint64_t opposite_occ = occupancies[!side];	
	//king danger squares
	uint64_t avoid = king_danger_squares(!side ^ 1);
	uint64_t Board;
	//King moves (to apply before double check evasion because it will happen anyways)
	//get targets for attacks (not working?)
	targets = King_Mask[kingsq] & opposite_occ & ~avoid;
	for(; targets; targets &= targets - 1){
		targetsq = get_ls1b(targets);
		move_list.push_back({get_move(kingsq, targetsq, K + side, piece_list[targetsq], 0, none), 0});
	}

	if(pop_count(king_attacked_by) > 1){
		return;
	}

	if(king_attacked_by){
		//checkmask
		checkmask ^= ~(Rect_Lookup[kingsq][get_ls1b(king_attacked_by)] | king_attacked_by);

		//Queen moves
		Board = bitboards[Q + side];
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get pinmask
			pinmask = masks[pin_direction[kingsq][get_bit(pinned, sourcesq) ? sourcesq : kingsq]][kingsq];
			//get targets for attacks
			targets = (queen_attacks(sourcesq, occupancies[BOTH]) & pinmask) & opposite_occ & checkmask;
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				move_list.push_back({get_move(sourcesq, targetsq, Q + side, piece_list[targetsq], 0, none), 0});
			}
		}
	
		//Rook moves
		Board = bitboards[R + side];
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get pinmask
			pinmask = masks[pin_direction[kingsq][get_bit(pinned, sourcesq) ? sourcesq : kingsq]][kingsq];
			//get targets for attacks
			targets = (rook_attacks(sourcesq, occupancies[BOTH]) & pinmask) & opposite_occ & checkmask;
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				move_list.push_back({get_move(sourcesq, targetsq, R + side, piece_list[targetsq], 0, none), 0});
			}
		}

		//Bishop moves
		Board = bitboards[B + side];
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get pinmask
			pinmask = masks[pin_direction[kingsq][get_bit(pinned, sourcesq) ? sourcesq : kingsq]][kingsq];
			//get targets for attacks
			targets = (bishop_attacks(sourcesq, occupancies[BOTH]) & pinmask) & opposite_occ & checkmask;
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				move_list.push_back({get_move(sourcesq, targetsq, B + side, piece_list[targetsq], 0, none), 0});
			}
		}

		//Knight moves
		Board = bitboards[N + side] & ~pinned;
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get targets for attacks
			targets = Knight_Attacks[sourcesq] & opposite_occ & checkmask;
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				move_list.push_back({get_move(sourcesq, targetsq, N + side, piece_list[targetsq], 0, none), 0});
			}
		}

		//Pawn moves (not promoting)
		Board = bitboards[P + side] & ~promotion_ranks[side];
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get pinmask
			pinmask = masks[pin_direction[kingsq][get_bit(pinned, sourcesq) ? sourcesq : kingsq]][kingsq];
			//get targets for attacks
			targets = Pawn_Attacks[side][sourcesq] & opposite_occ & checkmask & pinmask;
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				double_pushed = abs(targetsq - sourcesq) > 15;
				move_list.push_back({get_move(sourcesq, targetsq, side, piece_list[targetsq], 0, double_pushed << 2), 0});
			}
		}

		//Pawn moves (promoting)
		Board = bitboards[P + side] & promotion_ranks[side];
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get pinmask
			pinmask = masks[pin_direction[kingsq][get_bit(pinned, sourcesq) ? sourcesq : kingsq]][kingsq];
			//get targets for attacks
			targets = Pawn_Attacks[side][sourcesq] & opposite_occ & checkmask & pinmask;
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				move_list.push_back({get_move(sourcesq, targetsq, side, piece_list[targetsq], Q, none), 0});
				move_list.push_back({get_move(sourcesq, targetsq, side, piece_list[targetsq], Q, none), 0});
				move_list.push_back({get_move(sourcesq, targetsq, side, piece_list[targetsq], Q, none), 0});
				move_list.push_back({get_move(sourcesq, targetsq, side, piece_list[targetsq], Q, none), 0});
			}
		}

		//en passant
		int sq = En_Passant_Sq.back();
		//get potential target pawns (aka attacker pawns)
		targets = Pawn_Attacks[side ^ 1][sq] & bitboards[side];
		for(int iteration = pop_count(targets); iteration > 0; iteration--){
			sourcesq = get_ls1b(targets);
			if(legal_en_passant(side, sourcesq, sq)){
				move_list.push_back({get_move(sourcesq, sq, side, !side, 0, enpassant), 0});
			}
			//remove ls1b
			targets &= targets - 1;
		}
		return;
	} else {
		//Queen moves
		Board = bitboards[Q + side];
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get pinmask
			pinmask = masks[pin_direction[kingsq][get_bit(pinned, sourcesq) ? sourcesq : kingsq]][kingsq];
			//get targets for attacks
			targets = (queen_attacks(sourcesq, occupancies[BOTH]) & pinmask) & opposite_occ;
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				move_list.push_back({get_move(sourcesq, targetsq, Q + side, piece_list[targetsq], 0, none), 0});
			}
		}
	
		//Rook moves
		Board = bitboards[R + side];
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get pinmask
			pinmask = masks[pin_direction[kingsq][get_bit(pinned, sourcesq) ? sourcesq : kingsq]][kingsq];
			//get targets for attacks
			targets = (rook_attacks(sourcesq, occupancies[BOTH]) & pinmask) & opposite_occ;
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				move_list.push_back({get_move(sourcesq, targetsq, R + side, piece_list[targetsq], 0, none), 0});
			}
		}

		//Bishop moves
		Board = bitboards[B + side];
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get pinmask
			pinmask = masks[pin_direction[kingsq][get_bit(pinned, sourcesq) ? sourcesq : kingsq]][kingsq];
			//get targets for attacks
			targets = (bishop_attacks(sourcesq, occupancies[BOTH]) & pinmask) & opposite_occ;
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				move_list.push_back({get_move(sourcesq, targetsq, B + side, piece_list[targetsq], 0, none), 0});
			}
		}

		//Knight moves
		Board = bitboards[N + side] & ~pinned;
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get targets for attacks
			targets = Knight_Attacks[sourcesq] & opposite_occ;
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				move_list.push_back({get_move(sourcesq, targetsq, N + side, piece_list[targetsq], 0, none), 0});
			}
		}

		//Pawn moves (not promoting)
		Board = bitboards[P + side] & ~promotion_ranks[side];
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get pinmask
			pinmask = masks[pin_direction[kingsq][get_bit(pinned, sourcesq) ? sourcesq : kingsq]][kingsq];
			//get targets for attacks
			targets = Pawn_Attacks[side][sourcesq] & opposite_occ & pinmask;
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				double_pushed = abs(targetsq - sourcesq) > 15;
				move_list.push_back({get_move(sourcesq, targetsq, side, piece_list[targetsq], 0, double_pushed << 2), 0});
			}
		}

		//Pawn moves (promoting)
		Board = bitboards[P + side] & promotion_ranks[side];
		for(; Board; Board &= Board - 1){
			//get source square
			sourcesq = get_ls1b(Board);
			//get pinmask
			pinmask = masks[pin_direction[kingsq][get_bit(pinned, sourcesq) ? sourcesq : kingsq]][kingsq];
			//get targets for attacks
			targets = Pawn_Attacks[side][sourcesq] & opposite_occ & pinmask;
			for(; targets; targets &= targets - 1){
				targetsq = get_ls1b(targets);
				move_list.push_back({get_move(sourcesq, targetsq, side, piece_list[targetsq], Q, none), 0});
				move_list.push_back({get_move(sourcesq, targetsq, side, piece_list[targetsq], R, none), 0});
				move_list.push_back({get_move(sourcesq, targetsq, side, piece_list[targetsq], B, none), 0});
				move_list.push_back({get_move(sourcesq, targetsq, side, piece_list[targetsq], N, none), 0});
			}
		}

		//en passant
		int sq = En_Passant_Sq.back();
		//get potential target pawns (aka attacker pawns)
		targets = Pawn_Attacks[side ^ 1][sq] & bitboards[side];
		for(int iteration = pop_count(targets); iteration > 0; iteration--){
			sourcesq = get_ls1b(targets);
			if(legal_en_passant(side, sourcesq, sq)){
				move_list.push_back({get_move(sourcesq, sq, side, !side, 0, enpassant), 0});
			}
			//remove ls1b
			targets &= targets - 1;
		}
		return;
	}
}

inline void Legal_Moves::remove_sq(int sq){
	int piece = piece_list [sq];
	pop_bit(bitboards[piece], sq);
	piece_list [sq] = E;
	mg_static_eval -= middlegame[piece][sq];
  eg_static_eval -= endgame[piece][sq];
	eval_phase -= gamephase[piece];
	//hash ^= piece_keys[piece][sq];
}

inline void Legal_Moves::set_sq(int sq, int piece){
	piece_list [sq] = piece;
	set_bit(bitboards[piece], sq);
	mg_static_eval += middlegame[piece][sq];
  eg_static_eval += endgame[piece][sq];
	eval_phase += gamephase[piece];
	//hash ^= piece_keys[piece][sq];
}
	
void Legal_Moves::push_move(MOVE move){
	//Add blank values for updation
	Castling_Rights.push_back(Castling_Rights.back());
	En_Passant_Sq.push_back(64);
	//Remove the hashes for updating later on
	//hash ^= castle_keys[Castling_Rights.back()];
  //hash ^= en_passant_keys[En_Passant_Sq.back()];
	int source = move.source();
	int target = move.target();
	int piece  =  move.piece();
	switch(move.flag()){
		case none: 
			//Remove moving piece
			remove_sq(source);
			//Remove potentially captured piece
			remove_sq(target);
			//Set piece (also accounts for promotion as well)
			set_sq(target, piece + move.promote());
			break;
		case k_castling:
			//Remove king from start square
			remove_sq(source);
			//Set moving king to square
			set_sq(target, piece);
			//Remove rook
			remove_sq(target + 1);
			//Set rook
			set_sq(source + 1, piece - 4);
			break;
		case q_castling: {
			//Remove king from square
			remove_sq(source);
			//Set moving king to square
			set_sq(target, piece);
			//Remove rook
			remove_sq(target - 2);
			//Set rook
			set_sq(target + 1, piece - 4);
			break;
		}
		case enpassant: {
			int en_passant_sq = (source & 56) + (target & 7);
			//attacking pawn
			remove_sq(source);
			set_sq(target, piece);
			//victim pawn
			remove_sq(en_passant_sq);
		  break;			
		}
		case doublepush: {
			//remove pawn from square
			remove_sq(source);
			//set pawn to square
			set_sq(target, piece);
			En_Passant_Sq.back() = target + ((source - target) / 2);
			break;
		}
	}

	switch(piece){
		case K: {
			if (piece == K && source == 4) {
      	Castling_Rights.back() &= ~0x5;
    	}
			break;
		}
		case k: {
			if (piece == k && source == 60) {
    		Castling_Rights.back() &= ~0xa;
    	}
		}
		default: {
			//Kingside White
			if ((Castling_Rights.back() & 1) && (source == 7 || target == 7)) {
    		Castling_Rights.back() &= ~1;
  		}
			//Kingside Black
  		if ((Castling_Rights.back() & 2) && (source == 63 || target == 63)) {
    		Castling_Rights.back() &= ~2;
  		}
			//Queenside White
  		if ((Castling_Rights.back() & 4) && (source == 0 || target == 0)) {
    		Castling_Rights.back() &= ~4;
  		}
			//Queenside Black
  		if ((Castling_Rights.back() & 8) && (source == 56 || target == 56)) {
    		Castling_Rights.back() &= ~8;
  		}
			break;
		}
	}

	//Get new hash keys
	//hash ^= castle_keys[Castling_Rights.back()];
  //hash ^= en_passant_keys[En_Passant_Sq.back()];

	//update occupancies
	update_occupancies();
	//change side to move
	side_to_move ^= 1;
	//hash ^= turn_key;
}

void Legal_Moves::pop_move(MOVE move){
	//castling and en passant
	//hash ^= castle_keys[Castling_Rights.back()];
  //hash ^= en_passant_keys[En_Passant_Sq.back()];
	Castling_Rights.pop_back();
	En_Passant_Sq.pop_back();
	int source = move.source();
	int target = move.target();
	int piece  =  move.piece();
	//get info

	switch(move.flag()){
		case none: 
			//remove moving piece to square
			remove_sq(target);
			//restore potential captured piece
			set_sq(target, move.capture());
			//restore moving piece from square
			set_sq(source, piece);
			break;
		case k_castling:
			//set king to source square
			set_sq(source, piece);
			//pop moving king to square
			remove_sq(target);
			//set rook
			set_sq(target + 1, piece - 4);
			//pop rook
			remove_sq(source + 1);
			break;
		case q_castling: {
			//set king to source square
			set_sq(source, piece);
			//pop moving king to square
			remove_sq(target);
			//remove rook
			set_sq(target - 2, piece - 4);
			//set rook
			remove_sq(source - 1);
			break;
		}
		case enpassant: {
			int en_passant_sq = (source & 56) + (target & 7);
			//attacking pawn
			set_sq(source, piece);
			remove_sq(target);
			//victim pawn
			set_sq(en_passant_sq, move.capture());
		  break;			
		}
		case doublepush: {
			//restore moving piece from square
			set_sq(source, piece);
			//remove moving piece to square
			remove_sq(target);
		}
	}

	//Update hash keys
	//hash ^= castle_keys[Castling_Rights.back()];
  //hash ^= en_passant_keys[En_Passant_Sq.back()];
	
	//update occupancies
	update_occupancies();
	//change side to move
	side_to_move ^= 1;
	//hash ^= turn_key;
}

inline void Legal_Moves::push_null_move(){
	//Skip turn
	side_to_move ^= 1;
	//Apply turn hash
	hash ^= turn_key;
	//Deal with en passant (set potential en passant square to null)
	hash ^= en_passant_keys[En_Passant_Sq.back()];
	En_Passant_Sq.push_back(64);
	//Change hash to null square
	hash ^= en_passant_keys[64];
}

inline void Legal_Moves::pop_null_move(){
	//Revert turn
	side_to_move ^= 1;
	//Switch key back again
	hash ^= turn_key;
	//Deal with en passant to set back to original
	hash ^= en_passant_keys[64];
	
	En_Passant_Sq.pop_back();
	//Change hash by null square
	hash ^= en_passant_keys[En_Passant_Sq.back()];
}

int Legal_Moves::static_evaluation(){
	int phase = std::min(eval_phase, 24);
	return (eval_multiplier[side_to_move] * (mg_static_eval * phase + eg_static_eval * (24 - phase))) / 24 + eval_multiplier[side_to_move] * 20;
}

void Legal_Moves::Initialize_Everything(std::string input){
	Castle_White_Kingside = false;
	Castle_White_Queenside = false;
	Castle_Black_Kingside = false;
	Castle_Black_Queenside = false;
	//initialize bitboards
	Parse_FEN(input, bitboards, occupancies, Castle_White_Kingside, Castle_White_Queenside, Castle_Black_Kingside, Castle_Black_Queenside, En_Passant_Sq, side_to_move, piece_list);
	
	update_occupancies();

	Castling_Rights.push_back(Castle_White_Kingside | (Castle_Black_Kingside << 1) | (Castle_White_Queenside << 2) | (Castle_Black_Queenside << 3));

	init_hash(piece_list, En_Passant_Sq, Castling_Rights, side_to_move);
	
	//Initialize sliding piece attacks
	init_sliders_attacks(1);
	init_sliders_attacks(0);
	//initialize pawn and knight attacks
	for (int square = 0; square < 65; square++){
    //Pawns
    Pawn_Attacks[WHITE][square] = mask_Pawn_Attacks(WHITE, square);
    Pawn_Attacks[BLACK][square] = mask_Pawn_Attacks(BLACK, square);
	}
	//pin directions
	for(int sq1 = 0; sq1 < 64; sq1++){
		for(int sq2 = 0; sq2 < 64; sq2++){
			pin_direction[sq1][sq2] = direction(sq1, sq2);
		}
	}
	//rectangular lookup table for pins
	for(int sq1 = 0; sq1 < 64; sq1++){
		for(int sq2 = 0; sq2 < 64; sq2++){
			Rect_Lookup [sq1] [sq2] = inBetween(sq1, sq2);
		}
	}
}

void Legal_Moves::print_move_list(std::vector <MOVE> &move_list){
   // loop over moves within a move list
	std::cout << "\nMove:	Capture:\n";
  for (int move_count = 0; move_count < move_list.size(); move_count++){
    // init move
    MOVE move = move_list.at(move_count);
        
    std::cout << coordinates[move.source()];
		std::cout << coordinates[move.target()];
		std::cout << promoted_pieces[move.promote()]; 
		std::cout << "	 " << "\x1B[32m";
		bool captured = move.capture() != E ? 1 : 0;
		std::cout << captured << "\n";
    std::cout << "\033[0m";
  }
	// print total number of moves
  std::cout << "\nTotal number of moves: ";
	std::cout << move_list.size() << "\n";
}

void Legal_Moves::print_move_scores(std::vector <MOVE> &move_list){
   // loop over moves within a move list
	std::cout << "\nMove:	Score:\n";
  for (int move_count = 0; move_count < move_list.size(); move_count++){
    // init move
    MOVE move = move_list.at(move_count);
        
    std::cout << coordinates[move.source()];
		std::cout << coordinates[move.target()];
		std::cout << promoted_pieces[move.promote()]; 
		std::cout << "	 " << "\x1B[32m";
		std::cout << move.score << "\n";
    std::cout << "\033[0m";
  }
}

void Legal_Moves::print(){
	const std::string pieces [13] = {"♟", "♙", "♞", "♘", "♝", "♗", "♜", "♖", "♛", "♕", "♚", "♔", "."};
	int square;
	for(int i = 0; i < 64; i++){
		square = 63 - (floor(i/8) * 8 + (7 - (i%8)));
		if(i%8 == 0 && i != 64){
			std::cout<<"\n"<<floor(square/8) + 1<<" ";
		} else if (i % 8 == 0) {
			std::cout<<"\n"<<floor(square/8) + 1;
		} 
		if(i/8 == 8){
			std::cout<<" ";
		}
		
		std::cout << pieces[piece_list[square]] << " ";
	}
	std::cout<<"\n  a b c d e f g h\n";
}

void Legal_Moves::init_hash(int board[64], std::vector <int> en_passant, std::vector <int> castling, bool side_to_move){
	//Initialize keys
	//Pieces
	rand64();
	for(int i = 0; i < 12; i++){
    for(int j = 0; j < 64; j++){
      piece_keys[i][j] = rand64();
    }
  }
		
	//Null squares
  for (int i = 0; i < 64; i++) {
    piece_keys[12][i] = 0;
  }
		
	//Castle keys
	for(int i = 0; i < 16; i++){
		castle_keys[i] = rand64();
	}

	//En passant keys
	for(int i = 0; i < 65; i++){
		en_passant_keys[i] = rand64();
	}

	//Side to move key
	turn_key = rand64();

	//Initialize for pieces
	for(int sq = 0; sq < 64; sq++){
		hash ^= piece_keys[board[sq]][sq];
	}
		
	//Castling
	hash ^= castle_keys[castling.back()];
		
	//En passant
	hash ^= en_passant_keys[en_passant.back()];
		
	//Side to move
	if(side_to_move){
		hash ^= turn_key;
	}
}

/*void Legal_Moves::print_bits(uint64_t bitboard){
	int square;
	for(int i = 0; i < 64; i++){
		square = 63 - (floor(i/8) * 8 + (7 - (i%8)));
		if(i%8 == 0 && i != 64){
			std::cout<<"\n"<<floor(square/8) + 1<<" ";
		} else if (i % 8 == 0) {
			std::cout<<"\n"<<floor(square/8) + 1;
		} 
		if(i/8 == 8){
			std::cout<<" ";
		}
		
		std::cout << ((bitboard & (1 << square)) ? "1" : ".") << " ";
	}
	std::cout<<"\n  a b c d e f g h\n";
}*/