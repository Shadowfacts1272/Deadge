#include <iostream>
#include <bitset>
#include <cstdint>
#include <cassert>

//general bit manipulations
inline int pop_count (uint64_t bitboard){
	return __builtin_popcountll(bitboard);
}

inline int get_ls1b (uint64_t bitboard){
   return __builtin_ctzll(bitboard);
}

inline void pop_ls1b (uint64_t &bitboard){
	bitboard &= bitboard - 1;
}

inline void set_bit(uint64_t &bitboard, int square){
	bitboard |= 1ULL << square;
}

inline void pop_bit(uint64_t &bitboard, int square){
	bitboard &= ~(1ULL << square);
}

inline uint64_t get_bit(uint64_t bitboard, int square){
	return bitboard & (1ULL << square);
}

inline int return_ls1b(uint64_t &bitboard){
	int ls1b = get_ls1b(bitboard);
	bitboard &= bitboard - 1;
	return ls1b;
}