#ifndef SIDEWINDER_HASH
#define SIDEWINDER_HASH

#include <iostream>
#include <cmath>
#include "move.h"

//32 megabytes or smth
#define table_size 0x1000000

enum hash_types {
		hash_exact =  0,
		hash_alpha =  1,
		hash_beta  =  2,
		no_hash_entry = 16000 //Supposed to fail hard
};

/*struct hash_info {
    uint64_t hash_key;  
    int depth;  
    int flags;    
    int score;   
		MOVE best_move;
};    */

typedef struct hash_table{
	uint64_t hash_key [table_size] = {};
	int depth_val [table_size] = {};
	int flags [table_size] = {};
	int score[table_size] = {};
	MOVE best_move [table_size] = {};

	int query(uint64_t zobrist_key, int depth, int alpha, int beta, MOVE *best_move){
		uint64_t hash_index = zobrist_key & (table_size - 1);
 		if (hash_key[hash_index] == zobrist_key) {
	 	  if (depth_val[hash_index] >= depth) {
	 	    if (flags[hash_index] == hash_exact){
	 	      return score[hash_index];
				}
	 	    if ((flags[hash_index] == hash_alpha) && (score[hash_index] <= alpha)){
					return alpha;
				}
	 	    if ((flags[hash_index] == hash_beta) && (score[hash_index] >= beta)){
					return beta;
				}
	 		}
			*best_move = best_move[hash_index];
	  }
	  return no_hash_entry;
	}

	void insert(uint64_t zobrist_key, int result, int hash_flag, int depth, MOVE move){
		uint64_t hash_index = zobrist_key & (table_size - 1);
		hash_key[hash_index] = zobrist_key;
		depth_val[hash_index] = depth;
		score[hash_index] = result;
		flags[hash_index] = hash_flag;
		best_move[hash_index] = move;
	}

	void allocate_memory(int megabytes){
		
	}
} HASH;

//insert(board.hash, alpha, hash_flag, depth, hash_move)

#endif