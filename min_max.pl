:- module(min_max, [minimax/3]).
:- use_module(chess, [move/2, utility/2, wining_to_play/1, losing_to_move/1]).


minimax(Pos, BestNextPos, Val) :- % Pos has successors
    bagof(NextPos, move(Pos, NextPos), NextPosList),
    best(NextPosList, BestNextPos, Val), !.
minimax(Pos, _, Val) :- % Pos has no successors
    utility(Pos, Val).
best([Pos], Pos, Val) :-
    utility(Pos, Val), !.
best([Pos1 | PosList], BestPos, BestVal) :-
    utility(Pos1, Val1),
    best(PosList, Pos2, Val2),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).

betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-
    wining_to_play(Pos0),
    Val0 > Val1, !. 
betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-
    losing_to_move(Pos0),
    Val0 < Val1, !.
betterOf(_, _, Pos1, Val1, Pos1, Val1).
