:- use_module(chess, [move/2, utility/2, wining_to_play/1, losing_to_move/1]).

minimax(Pos, Pos, Val, 0) :-
    utility(Pos, Val), !.
minimax(Pos, BestNextPos, Val, Depth) :-
    D is Depth - 1,
    bagof(NextPos, move(Pos, NextPos), NextPosList),
    best(NextPosList, BestNextPos, Val, D), !.
minimax(Pos, _, Val, _) :-
    utility(Pos, Val).

best([Pos], Pos, Val, _) :-
    minimax(Pos, _, Val, _), !.
best([Pos1 | PosList], BestPos, BestVal, Depth) :-
    minimax(Pos1, _, Val1, Depth),
    best(PosList, Pos2, Val2, Depth),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).

betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-
    wining_to_play(Pos0),
    Val0 > Val1, !. 
betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-
    losing_to_move(Pos0),
    Val0 < Val1, !.
betterOf(_, _, Pos1, Val1, Pos1, Val1).
