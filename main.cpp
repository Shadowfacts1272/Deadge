#include <iostream>
#include <bitset>
#include <fstream>
#include <stdio.h>
#include <utility>
#include "Board/uci.h"
#include "Board/timer.h"
#include "Board/hash.h"
#include "Board/history.h"
#include "Board/see.h"

//g++ -pthread -std=c++17 -Ofast -DNDEBUG -m64 -mpopcnt -flto -static -o C:\Users\kyley\OneDrive\Documents\chess\engines\snek-dev.exe C:\Users\kyley\OneDrive\Documents\chess\snek\*.cpp

const int MAX_PLY = 64;

//Perft function
inline uint64_t Perft(Legal_Moves &board, int depth){
	if (depth == 1) {
    std::vector<MOVE> movelist;
    board.legal_moves(movelist);
    return movelist.size();
  } else {
    uint64_t total{};
    std::vector<MOVE> movelist;
    board.legal_moves(movelist);
    for (MOVE move : movelist) {
      board.push_move(move);
      total += Perft(board, depth - 1);
      board.pop_move(move);
    }
    return total;
  }
}

void Perft_Split(Legal_Moves &board, int depth){
	std::vector <MOVE> move_list;
	uint64_t nodes = 0;
	uint64_t t_nodes = 0;

	board.legal_moves(move_list);

	std::cout << "\nGo PERFT: " << depth << "\n";
	for(MOVE i : move_list){
		nodes = 0;
		board.push_move(i);
		std::cout << coordinates[i.source()];
		std::cout << coordinates[i.target()];
		std::cout << promoted_pieces[i.promote()];
		if(depth == 1){
			nodes ++;
		} else {
			nodes += Perft(board, depth - 1);
		}
		t_nodes += nodes;
		board.pop_move(i);
		std::cout << ":		" << nodes << "\n";
	}
	std::cout << "Total nodes:	" <<t_nodes << "\n";
}

typedef struct killer {
	MOVE table_1 [MAX_PLY];
	MOVE table_2 [MAX_PLY];

	void append (MOVE beta_cutoff, int ply){
		table_2[ply] = table_1[ply]; 
		table_1[ply] = beta_cutoff; 
	}

	void clear() {
    for (int i = 0; i < MAX_PLY; i++) {
      table_1 [i] = {0, 0};
			table_2 [i] = {0, 0};
    }
  }
} KILLER;

//A principal variation table and associated stuff
int pv_length [MAX_PLY];
MOVE pv_table [MAX_PLY] [MAX_PLY];

//Search class
class Search {
  //Using piece square table to help order moves
	int order_psqt [12] [64];

	//In which first field is victim and second field is the attacker
	int mvv_lva [12] [12];

	//Determine move similarity using move properties
	bool compare_move (MOVE a_move, MOVE b_move){
		return (a_move.source() == b_move.source()) && (a_move.target() == b_move.target()) && (a_move.piece() == b_move.piece());
	}

	//Static Exchange Evaluation piece values
	const int see_val [13] = {100, 100, 300, 300, 300, 300, 500, 500, 900, 900, 9999, 9999, 0};

	//Attain the least valuable piece
	uint64_t least_valuable_piece(uint64_t attacks, int by_side, int &piece, Legal_Moves &board)
	{
	  for (piece = by_side; piece <= K + by_side; piece += 2) {
	    uint64_t subset = attacks & board.bitboards[piece];
	    if(subset){
	      return subset & -subset; // single bit
			}
	  }
	  return 0; // empty set
	}
	public:

	//The main SEE routine
	int see(MOVE move, Legal_Moves &board){
	  int gain[32], d = 0;
		//Move properties
		int source = move.source(), target_sq = move.target(), tar_piece = board.piece_list[target_sq], piece = move.piece();
	
		//Pieces that can be used for potential xray attacks
		bool stm = board.side_to_move;
		uint64_t hv_xray = (board.bitboards[R] | board.bitboards[r] | board.bitboards[Q] | board.bitboards[q]) & board.rook_attacks(target_sq, 0);
		uint64_t da_xray = (board.bitboards[B] | board.bitboards[b] | board.bitboards[Q] | board.bitboards[q]) & board.bishop_attacks(target_sq, 0);
	
		//Source square bitboard
	  uint64_t from_set = 1ULL << source;
	  uint64_t occ = board.occupancies[BOTH];
	  uint64_t attacks = board.square_attackers(target_sq, 0) | board.square_attackers(target_sq, 1);
		//Get initial target value
	  gain[d] = see_val[tar_piece];
	  do {
			//next depth and side
	    d++; 
			stm ^= 1;
			//speculative store, if defended
	    gain[d]  = see_val[piece] - gain[d-1]; 
			//pruning does not influence the result
	    if(std::max(-gain[d-1], gain[d]) < 0) break; 
			//reset bit in set to traverse
	    attacks ^= from_set;
			//reset bit in temporary occupancy for xrays
	    occ &= ~from_set; 
	    if(from_set & (hv_xray | board.bitboards[P] | board.bitboards[p])){
				//If rook related pins
				attacks |= board.xray_rook_attacks(occ, occ, target_sq) & hv_xray;
			} 
			if (from_set & (da_xray | board.bitboards[P] | board.bitboards[p])){
				//If bishop related pins
				attacks |= board.xray_bishop_attacks(occ, occ, target_sq) & da_xray;
			}
	    from_set = least_valuable_piece(attacks, stm, piece, board);
	  } while(from_set);
		while (--d){
		  gain[d-1]= -std::max(-gain[d-1], gain[d]);
		}
		return gain[0];
	}

	inline void get_best_move(std::vector <MOVE> &move_list, int start_index){
		for(int i = start_index + 1; i < move_list.size(); i++){
			if(move_list[i].score > move_list[start_index].score){
				std::swap(move_list[i], move_list[start_index]);
			}
		}
	}

	//Initialize some things, quite explanatory
	void move_order_psqt_init(){
	  for(int pc = 0; pc < 6; ++pc){
	    for(int sq = 0; sq < 64; ++sq){
	      order_psqt[pc * 2][sq] = mg_table[pc][sq ^ 56];
	    }
	    for(int sq = 0; sq < 64; ++sq){
	      order_psqt[pc * 2 + 1][sq] = mg_table[pc][sq];
	    }
	  }
	}

	void init_mvv_lva(){
		for(int i = 0; i < 12; i++){
			for(int j = 0; j < 12; j++){
				mvv_lva [i] [j] = see_val[i] - see_val[j];
			}
		}
	}

	//Some useful move ordering constant
	const int remove_side = ~1;

	void score_moves(std::vector <MOVE> &move_list, int ply, KILLER &killer, HISTORY history, Legal_Moves &board){
		//Create some variables for usage in loop
		int source;
		int target;
		int piece;
		int capture;
		int score;

		//Let the loop begin
		for(int n; n < move_list.size(); n++){
			source = move_list[n].source();
			target = move_list[n].target();
			piece = move_list[n].piece();
			capture = move_list[n].capture();
			score = 0;

			if(capture == E){
				//If it isn't a capture
				score = history.table[piece] [target];
				if(move_list[n].move == killer.table_1[ply].move){
					score += 9000;
					break;
				} else if (move_list[n].move == killer.table_2[ply].move){
					score += 8000;
					break;
				}
			} else {
				if((target & remove_side) < (piece & remove_side)){
					//Do MVV-LVA
					score = mvv_lva [capture] [piece];
				} else {
					//Otherwise, do SEE
					score = see(move_list[n], board);
				}
			}
			move_list[n].set_score(score + order_psqt[piece] [target] - order_psqt[piece] [source]);
		}
	}

	//Seperate score moves function for q_search
	void score_moves_q(std::vector <MOVE> &move_list, Legal_Moves &board){
		//Create some variables for usage in loop
		int source;
		int target;
		int piece;
		int capture;
		int score;

		//Let the loop begin
		for(int n; n < move_list.size(); n++){
			source = move_list[n].source();
			target = move_list[n].target();
			piece = move_list[n].piece();
			capture = move_list[n].capture();
			score = 0;
			
			if((target & remove_side) < (piece & remove_side)){
				//Do MVV-LVA
				score = mvv_lva [capture] [piece];
			} else {
				//Otherwise, do SEE
				score = see(move_list[n], board);
			}		
			move_list[n].set_score(score + order_psqt[piece] [target] - order_psqt[piece] [source]);
		}
	}

	//Quiescence Search
	int quiescence(int alpha, int beta, Legal_Moves &board){
		//Score variable
		int score;
		//Move variable
		MOVE move;
		//Stand pat
		int stand_pat = board.static_evaluation();
		if(stand_pat >= beta){
			return beta;
		}
		if(alpha < stand_pat){
			alpha = stand_pat;
		}

		//Generate moves
		std::vector <MOVE> move_list = {};
		board.legal_captures(move_list);
		score_moves_q(move_list, board);
		for(int n = 0; n < move_list.size(); n++){
			get_best_move(move_list, n);
			move = move_list[n];
			board.push_move(move);
			score = -quiescence(-beta, -alpha, board);
			board.pop_move(move);
			if(score >= beta){
				return beta;
			}
			if(score > alpha){
				alpha = score;
			}
		}
		return alpha;
	}

	//Search routine
	int alphabeta(int alpha, int beta, int depth, int ply, KILLER &killer, HISTORY &history, Legal_Moves &board){
		//Score variable
		int score;

		//Move variable
		MOVE move;

		//Init PV length
		pv_length[ply] = ply;

		//For quiescence search
		if(depth == 0){
			return quiescence(alpha, beta, board);
		}

		//Generate moves
		std::vector <MOVE> move_list = {};
		board.legal_moves(move_list);
		score_moves(move_list, ply, killer, history, board);
		for(int n = 0; n < move_list.size(); n++){
			get_best_move(move_list, n);
			move = move_list[n];
			board.push_move(move);
			score = -alphabeta(-beta, -alpha, depth - 1, ply + 1, killer, history, board);
			board.pop_move(move);
			
			if(score > alpha){
				if(score >= beta){
					if(move.capture() == E){
						//Update killer heuristics
						killer.append(move, ply);
					}
					return beta;
				}
				alpha = score;
				if(move.capture() == E){
					//Update history heuristics
					history.edit(move.piece(), move.target(), 1 << depth);
				}
				pv_table[ply][ply] = move;
				for (int next_ply = ply + 1; next_ply < pv_length[ply + 1]; next_ply++){
          pv_table[ply][next_ply] = pv_table[ply + 1][next_ply];
        }
        pv_length[ply] = pv_length[ply + 1];  
			}
		}
		return alpha;
	}
};

void see_testing(Search &search, Legal_Moves &board){
	int counter = 0;
	int score;
	int correct = 0;
  while(counter < 74){ 
		//Do SEE stuff
    board.Initialize_Everything(test_pos[counter].pos);
		score = search.see(get_move_from_input(test_pos[counter].move, board), board);
		if(score == test_pos[counter].score){
			correct++;
		}
		counter++;
  }
	std::cout<<correct<<" out of 74 positions are correct";
}

int double_pushed [64] [64];

int main() {
	//Initialize everything
	Legal_Moves board;
	Timer time;
	Search search;
	parse_position(board);
	tables_init();
	init_eval(board.piece_list);
	search.init_mvv_lva();
	search.move_order_psqt_init();
	HISTORY history = {};
	KILLER killer = {};

	//Begin!
	board.print();
	see_testing(search, board);

	//std::cout<<search.see(get_move_from_input("e1g1", board), board);

	//Use this: r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 
	/*int score = -search.alphabeta(-18000, 18000, 3, 0, killer, history, board);
	std::cout<<score;
	//print_move(pv_table[0][0]);
	for (int count = 0; count < pv_length[0]; count++){
  	std::cout<<print_move(pv_table[0][count]);
  }*/
}