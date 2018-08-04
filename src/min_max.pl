:- module(min_max, [minimax/3]).
:- use_module(chess, [move/2, utility/2, wining_to_play/1, losing_to_move/1]).

% Deze minimax gaat op een niveau zoeken naar de beste zet. Voor elke mogelijke zet zal hij de waarde berekenen en de hoogste waarde uitkiezen.
% Aanmaken van zetten
minimax(Pos, BestNextPos, Val) :- % Pos has successors
    bagof(NextPos, move(Pos, NextPos), NextPosList),
    best(NextPosList, BestNextPos, Val), !.
minimax(Pos, _, Val) :- % Pos has no successors
    utility(Pos, Val).

% Inplaats van de kinderen te checken op waarde berekenen we direct de waarde van de positie
best([Pos], Pos, Val) :-
    utility(Pos, Val), !.
best([Pos1 | PosList], BestPos, BestVal) :-
    utility(Pos1, Val1),
    best(PosList, Pos2, Val2),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).

% Deze functie beslist of we zoeken naar een zo goed mogelijke of zo slecht mogelijke waarde, maar is in deze minmaxboom overbodig
betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-
    wining_to_play(Pos0),
    Val0 > Val1, !. 
betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-
    losing_to_move(Pos0),
    Val0 < Val1, !.
betterOf(_, _, Pos1, Val1, Pos1, Val1).
