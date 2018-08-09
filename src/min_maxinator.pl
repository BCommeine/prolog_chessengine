:- module(min_maxinator, [minimax/5, minimax/4]).
:- use_module(chess, [get_turn/2, move/2, utility/2, wining_to_play/2, losing_to_move/2]).

% Bij deze minmaxboom kunnen we een diepte meegeven en dus controleren hoeveel zetten ver we kijken
minimax(Pos, Best, Val, N) :- get_turn(Pos, Turn), minimax(Pos, Best, Val, N, Turn).

% Basisgeval minmax
minimax(Pos, Pos, Val, 0, _) :-
    utility(Pos, Val).
% We verminderen de diepte met één, en gaan op zoek naar de beste moves
minimax(Pos, BestNextPos, Val, Depth, Col) :-
    D is Depth - 1,
    bagof(NextPos, move(Pos, NextPos), NextPosList),
    best(NextPosList, BestNextPos, Val, D, Col), !.

minimax(Pos, _, Val, _, _) :-
    utility(Pos, Val).

% Om de beste zet te onderscheiden zullen we opnieuw de minimax gebruiken, om te zien wat de scores zijn van de volgende diepte
best([Pos], Pos, Val, _, Col) :-
    minimax(Pos, _, Val, _, Col), !.
best([Pos1 | PosList], BestPos, BestVal, Depth, Col) :-
    minimax(Pos1, _, Val1, Depth, Col),
    best(PosList, Pos2, Val2, Depth, Col),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal, Col).

% Hier wordt er beslist of we een zo hoog mogelijke of zo laag mogelijke score willen voor het huidig niveau
betterOf(Pos0, Val0, _, Val1, Pos0, Val0, Col) :-
    wining_to_play(Pos0, Col),
    Val0 < Val1, !.
betterOf(Pos0, Val0, _, Val1, Pos0, Val0, Col) :-
    losing_to_move(Pos0, Col),
    Val0 > Val1, !.
betterOf(_, _, Pos1, Val1, Pos1, Val1, _).
