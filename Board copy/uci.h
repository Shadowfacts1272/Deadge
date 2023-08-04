#include <iostream>
#include "position.h"
#include <vector>

//Bunch of FEN strings
const std::string Start_Pos = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

const std::string Test_Pos_1 = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - ";

const std::string Test_Pos_2 = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -";

const std::string Test_Pos_3 = "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1";

const std::string Test_Pos_4 = "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8";

const std::string Test_Pos_5 = "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10";

const std::string Killer_Heuristic  = "6k1/3q1pp1/pp5p/1r5n/8/1P3PP1/PQ4BP/2R3K1 w - - 0 1";

//UCI Stuff
MOVE get_move_from_input(std::string move, Legal_Moves& board){
	char str_move[move.length() + 1];
	strcpy(str_move, move.c_str());
	int source = 0;
	int target = 0;
	int promote = 0;
	switch(str_move[0]){
		case 'a': break;
		case 'b': source ++; break;
		case 'c': source += 2; break;
		case 'd': source += 3; break;
		case 'e': source += 4; break;
		case 'f': source += 5; break;
		case 'g': source += 6; break;
		case 'h': source += 7; break;
	}
	
	switch(str_move[1]){
		case '1': break;
		case '2': source += 8; break;
		case '3': source += 16; break;
		case '4': source += 24; break;
		case '5': source += 32; break;
		case '6': source += 40; break;
		case '7': source += 48; break;
		case '8': source += 56; break;
	}

	switch(str_move[2]){
		case 'a': break;
		case 'b': target ++; break;
		case 'c': target += 2; break;
		case 'd': target += 3; break;
		case 'e': target += 4; break;
		case 'f': target += 5; break;
		case 'g': target += 6; break;
		case 'h': target += 7; break;
	}
	
	switch(str_move[3]){
		case '1': break;
		case '2': target += 8; break;
		case '3': target += 16; break;
		case '4': target += 24; break;
		case '5': target += 32; break;
		case '6': target += 40; break;
		case '7': target += 48; break;
		case '8': target += 56; break;
	}

	if(move.length() == 5){
		switch(str_move[4]){
			case 'n': promote += N; break;
			case 'b': promote += B; break;
			case 'r': promote += R; break;
			case 'q': promote += Q; break;
		}
	}

	std::vector <MOVE> move_list = {};
	board.legal_moves(move_list);
	for(MOVE test : move_list){
		if((source == test.source()) && (target == test.target()) && (promote == test.promote())){
			return test;
		}
	}
	return move_list.front();
}

void parse_position(Legal_Moves& board){
	std::string pos;
	std::getline (std::cin, pos);
	//Take substring to check for potential startpos
	std::string string_z("position");
	size_t is_it_position = pos.find(string_z);
	
	if(is_it_position < 100000000){
		std::string string_t("startpos");
		size_t is_it_startpos = pos.find(string_t);
		//Large comparison number, correct number is probably 2^23-1
		if(is_it_startpos < 100000000){
			//Default start position
			std::string target_string("moves");
			size_t any_moves = pos.find(target_string);
			board.Initialize_Everything(Start_Pos);
			if(any_moves < 100000000) {
				std::string additional_move_string = pos.substr(any_moves + 6, pos.length() - 1);
				int move_sequence_len = additional_move_string.length() -  1;
				//parse additional move string
				int count = 0;
				while(count < move_sequence_len){
					//check for promotion commands
					if(((count + 5) < move_sequence_len)
	&& additional_move_string.at(count + 5) == ' '){
						//Promotion case
						board.push_move(get_move_from_input(additional_move_string.substr(count, count + 5), board));
						count += 6;
					} else {
						board.push_move(get_move_from_input(additional_move_string.substr(count, count + 4), board));
						count += 5;
					}
				}
			}
		} else {
			//If we have the fen segment which indicates a need to use custom FEN string
			std::string target_string("moves");
			size_t any_moves = pos.find(target_string);
			//Same thing as last time
			if(any_moves > 100000000){
				//If it doesn't have the additional move stuff
				board.Initialize_Everything(pos.substr(13, pos.length() - 1));
			} else {
				std::string additional_move_string = pos.substr(any_moves + 6, pos.length() - 1);
				int move_sequence_len = additional_move_string.length() -  1;
				board.Initialize_Everything(pos.substr(13, pos.length() - move_sequence_len - 7));
				//parse additional move string
				int count = 0;
				while(count < move_sequence_len){
					//check for promotion commands
					if(((count + 5) < move_sequence_len)
	&& additional_move_string.at(count + 5) == ' '){
						//Promotion case
						board.push_move(get_move_from_input(additional_move_string.substr(count, count + 5), board));
						count += 6;
					} else {
						board.push_move(get_move_from_input(additional_move_string.substr(count, count + 4), board));
						count += 5;
					}
				}
			}
		}
	} else {
		//If the keyword "position" isn't found, recursion!
		parse_position(board);
	}
}

void parse_go(){
	//hi there
}

//A utility concerning UCI
std::string print_move(MOVE move){
	std::string output = coordinates[move.source()] + coordinates[move.target()] + promoted_pieces[move.promote()];
	return output;
}