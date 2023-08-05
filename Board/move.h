#include <iostream>

enum move_flags {
							none, 
							k_castling, 
							q_castling, 
							enpassant, 
							doublepush,
};

//FFFPPPPCCCCPPPPTTTTTTSSSSSS
typedef struct move {
	int move;
	int score;

	inline void set_score(int val){
		score = val;
	}

	inline int source()   {return move & 0x3f;}
	inline int target()   {return (move >> 6) & 0x3f;}
	inline int piece()    {return (move >> 12) & 0xf;}
	inline int capture () {return (move >> 16) & 0xf;}
	inline int promote () {return (move >> 20) & 0xf;}
	inline int flag ()		{return (move >> 24) & 0x7;}
} MOVE;

