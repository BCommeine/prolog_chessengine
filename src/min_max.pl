:- module(min_max, [minimax/3]).
:- use_module(chess, [move/2, value/2]).

minimax(Pos, Move, Depth) :- minimax(Depth, Pos, _, Move).

minimax(0, Position, Value, _) :-
      value(Position, Value).

minimax(D, Position, Value, Move) :-
      D > 0,
      D1 is D - 1,
      findall(M, move(Position, M), Positions), % There must be at least one move!
      minimax(Positions, Position, D1, -10000, nil, Value, Move).

minimax([], _, _, Value, Best, Value, Best).

minimax([NextPosition|Positions], Position, D, Value0, _, BestValue, BestMove):-
      minimax(D, NextPosition, OppValue, _OppMove),
      Value is -OppValue,
      Value >= Value0,
      minimax(Positions,Position,D, Value ,NextPosition ,BestValue,BestMove), !.

minimax([NextPosition|Positions],Position,D, Value0, Move0, BestValue, BestMove):-
      minimax(D, NextPosition, OppValue, _OppMove),
      Value is -OppValue,
      Value < Value0,
      minimax(Positions,Position,D, Value0,Move0,BestValue,BestMove), !.
