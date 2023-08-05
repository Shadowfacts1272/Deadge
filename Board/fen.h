#include <iostream>
#include <string.h>
#include <string>
#include <cstdlib>
#include <algorithm>
#include <vector>

uint64_t Mirror (uint64_t x) {
   const uint64_t k1 = 0x5555555555555555;
   const uint64_t k2 = 0x3333333333333333;
   const uint64_t k4 = 0x0f0f0f0f0f0f0f0f;
   x = ((x >> 1) & k1) | ((x & k1) << 1);
   x = ((x >> 2) & k2) | ((x & k2) << 2);
   x = ((x >> 4) & k4) | ((x & k4) << 4);
   return x;
}

//Poorly done Parse FEN string function (still does it's job)
void Parse_FEN(std::string FEN, uint64_t Bitboards[13], uint64_t occupancies[3], bool &Castle_White_Kingside, bool &Castle_White_Queenside, bool &Castle_Black_Kingside, bool &Castle_Black_Queenside, std::vector<int> &En_Passant_Sq, bool &Side_To_Move, int piece_list[64]){
	memset(Bitboards, 0ULL, 104);
	memset(occupancies, 0ULL, 24);
	Castle_White_Kingside = 0;
	Castle_White_Queenside = 0;
	Castle_Black_Kingside = 0;
	Castle_Black_Queenside = 0;
	En_Passant_Sq.clear();
	
	//Declaring character array
  char reader[FEN.length() + 1];
 
	strcpy(reader, FEN.c_str());

	int j = 1, i = 0;
	int aRank, aFile;
	const int inc = -1;
	
	//For a8 (because it doesn't work like the other squares for some reason. And I'm too lazy)
	switch(reader[0]){
		case 'p' : Bitboards[1] |= 1ULL << 56; break;
		case 'r' : Bitboards[7] |= 1ULL << 56; break;
		case 'n' : Bitboards[3] |= 1ULL << 56; break;
		case 'b' : Bitboards[5] |= 1ULL << 56; break;
		case 'q' : Bitboards[9] |= 1ULL << 56; break;
		case 'k' : Bitboards[11] |= 1ULL << 56; break;
		case 'P' : Bitboards[0] |= 1ULL << 56; break;
		case 'R' : Bitboards[6] |= 1ULL << 56; break;
		case 'N' : Bitboards[2] |= 1ULL << 56; break;
		case 'B' : Bitboards[4] |= 1ULL << 56; break;
		case 'Q' : Bitboards[8] |= 1ULL << 56; break;
		case 'K' : Bitboards[10] |= 1ULL << 56; break;
		case '/' : break;
		case '1' : break;
		case '2' : j++; break;
		case '3' : j+=2; break;
		case '4' : j+=3; break;
		case '5' : j+=4; break;
		case '6' : j+=5; break;
		case '7' : j+=6; break;
		case '8' : j+=7; break;
	}
	
	int sq;
	while(reader[i] != ' '){
		i++;
		aFile = 7-(1+((j-1) % 8));
		aRank = 8-((j-1) / 8);
		sq = ((aRank-1)*8) + (aFile - 1);

		
		switch(reader[i]){
			case 'p' : Bitboards[1] |= Mirror(1ULL << (sq - inc)); break;
			case 'r' : Bitboards[7] |= Mirror(1ULL << (sq - inc)); break;
			case 'n' : Bitboards[3] |= Mirror(1ULL << (sq - inc)); break;
			case 'b' : Bitboards[5] |= Mirror(1ULL << (sq - inc)); break;
			case 'q' : Bitboards[9] |= Mirror(1ULL << (sq - inc)); break;
			case 'k' : Bitboards[11] |= Mirror(1ULL << (sq - inc)); break;
			case 'P' : Bitboards[0] |= Mirror(1ULL << (sq - inc)); break;
			case 'R' : Bitboards[6] |= Mirror(1ULL << (sq - inc)); break;
			case 'N' : Bitboards[2] |= Mirror(1ULL << (sq - inc)); break;
			case 'B' : Bitboards[4] |= Mirror(1ULL << (sq - inc)); break;
			case 'Q' : Bitboards[8] |= Mirror(1ULL << (sq - inc)); break;
			case 'K' : Bitboards[10] |= Mirror(1ULL << (sq - inc)); break;
			case '/' : j--; break;
			case '1' : break;
			case '2' : j++; break;
			case '3' : j+=2; break;
			case '4' : j+=3; break;
			case '5' : j+=4; break;
			case '6' : j+=5; break;
			case '7' : j+=6; break;
			case '8' : j+=7; break;
		}
		j++;
	}
	int iterator;
	uint64_t board;
	for(int sq = 0; sq < 64; sq++){
		for(iterator = 0; iterator < 12; iterator++){
			board = Bitboards[iterator];
			if((board >> sq) & 1){
				piece_list [sq] = iterator;
			}
		}
	}
	
	i+=1;
	//side to move
	if(reader[i] == 'w'){
		Side_To_Move = 0;
	} else {
		Side_To_Move = 1;
	}
	i+=2;
	while(reader[i] != ' '){
		switch(reader[i]){
			case 'K': Castle_White_Kingside = 1; break;
			case 'k': Castle_Black_Kingside = 1; break;
			case 'Q': Castle_White_Queenside = 1; break;
			case 'q': Castle_Black_Queenside = 1; break;
			case '-': break;
		}
		i++;
	}
	//en passant
	i++;
	int En_Passant = 0;
	if(reader[i] == '-'){
		En_Passant = 64;
	} else {
		switch(reader[i]){
			case 'a': break;
			case 'b': En_Passant += 1; break;
			case 'c': En_Passant += 2; break;
			case 'd': En_Passant += 3; break;
			case 'e': En_Passant += 4; break;
			case 'f': En_Passant += 5; break;
			case 'g': En_Passant += 6; break;
			case 'h': En_Passant += 7; break;
		}
		switch(reader[i + 1]){
			case '3': En_Passant += 16; break;
			case '6': En_Passant += 40; break;
			default: break;
		}
	}

	
	En_Passant_Sq.push_back(En_Passant);
	memset(Bitboards, 0ULL, 104);
	for(int count = 0; count < 64; count++){
		Bitboards[piece_list[count]] |= 1ULL << count;
	}
}