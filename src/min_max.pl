:- module(min_max, [minimax/3]).
:- use_module(chess, [move/2, value/2]).

minimax(Pos, Move, Depth) :- minimax(Depth, Pos, 1, _, Move).

minimax(0, Position, Player, Value, _) :-
      value(Position, V),
      Value is V*Player.

minimax(D, Position, Player, Value, Move) :-
      D > 0,
      D1 is D - 1,
      findall(M, move(Position, M), Moves), % There must be at least one move!
      minimax(Moves, Position, D1, Player, -100000, nil, Value, Move).

minimax(0, Position, Player, Value, _) :-
      value(Position, V),
      Value is V*Player.

minimax(D, Position, Player, Value, Move) :-
      D > 0,
      D1 is D - 1,
      findall(M, move(Position, M), Positions), % There must be at least one move!
      minimax(Positions, Position, D1, Player, -10000, nil, Value, Move).

minimax([], _, _, _, Value, Best, Value, Best).

minimax([NextPosition|Positions],Position,D,Player, Value0, _, BestValue, BestMove):-
      Opponent is -Player,
      minimax(D, NextPosition, Opponent, OppValue, _OppMove),
      Value is -OppValue,
      Value >= Value0,
      minimax(Positions,Position,D,Player, Value ,NextPosition ,BestValue,BestMove), !.

minimax([NextPosition|Positions],Position,D,Player, Value0, Move0, BestValue, BestMove):-
      Opponent is -Player,
      minimax(D, NextPosition, Opponent, OppValue, _OppMove),
      Value is -OppValue,
      Value < Value0,
      minimax(Positions,Position,D,Player, Value0,Move0,BestValue,BestMove), !.
