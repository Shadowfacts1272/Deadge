#include <iostream>
#include <cassert>

// generate bishop attacks on the fly
uint64_t bishop_attacks_on_the_fly(int square, uint64_t block){
  // result attacks bitboard
  uint64_t attacks = 0ULL;
  // init ranks & files
  int r, f;
  // init target rank & files
  int tr = square / 8;
  int tf = square % 8;
  // generate bishop atacks
  for (r = tr + 1, f = tf + 1; r <= 7 && f <= 7; r++, f++){
    attacks |= (1ULL << (r * 8 + f));
    if ((1ULL << (r * 8 + f)) & block) break;
  }
  for (r = tr - 1, f = tf + 1; r >= 0 && f <= 7; r--, f++){
    attacks |= (1ULL << (r * 8 + f));
    if ((1ULL << (r * 8 + f)) & block) break;
  }
  for (r = tr + 1, f = tf - 1; r <= 7 && f >= 0; r++, f--){
    attacks |= (1ULL << (r * 8 + f));
    if ((1ULL << (r * 8 + f)) & block) break;
  }
  for (r = tr - 1, f = tf - 1; r >= 0 && f >= 0; r--, f--){
    attacks |= (1ULL << (r * 8 + f));
    if ((1ULL << (r * 8 + f)) & block) break;
  }
  // return attack map
  return attacks;
}

// generate rook attacks on the fly
uint64_t rook_attacks_on_the_fly(int square, uint64_t block){
  // result attacks bitboard
  uint64_t attacks = 0ULL;
  // init ranks & files
  int r, f;
  // init target rank & files
  int tr = square / 8;
  int tf = square % 8;
  // generate rook attacks
  for (r = tr + 1; r <= 7; r++){
    attacks |= (1ULL << (r * 8 + tf));
    if ((1ULL << (r * 8 + tf)) & block) break;
  }
  for (r = tr - 1; r >= 0; r--){
    attacks |= (1ULL << (r * 8 + tf));
    if ((1ULL << (r * 8 + tf)) & block) break;
  }
  for (f = tf + 1; f <= 7; f++){
    attacks |= (1ULL << (tr * 8 + f));
    if ((1ULL << (tr * 8 + f)) & block) break;
  }
  for (f = tf - 1; f >= 0; f--){
    attacks |= (1ULL << (tr * 8 + f));
    if ((1ULL << (tr * 8 + f)) & block) break;
  }
  // return attack map
  return attacks;
}

uint64_t set_occupancy(int index, int bits_in_mask, uint64_t attack_mask){
  // occupancy map
  uint64_t occupancy = 0ULL;
  // loop over the range of bits within attack mask
  for (int count = 0; count < bits_in_mask; count++){
    // get LS1B index of attacks mask
		assert(attack_mask);
    int square = __builtin_ctzll(attack_mask);
    // pop LS1B in attack map
    pop_bit(attack_mask, square);
    // make sure occupancy is on board
    if (index & (1 << count)){
			// populate occupancy map
      occupancy |= (1ULL << square);
		}
  }// return occupancy map
	return occupancy;
}