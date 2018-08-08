:- module(min_maxinator, [minimax/4]).
:- use_module(chess, [move/2, utility/2, wining_to_play/1, losing_to_move/1]).

% Bij deze minmaxboom kunnen we een diepte meegeven en dus controleren hoeveel zetten ver we kijken

% Basisgeval minmax
minimax(Pos, Pos, Val, 0) :-
    utility(Pos, Val).
% We verminderen de diepte met één, en gaan op zoek naar de beste moves
minimax(Pos, BestNextPos, Val, Depth) :-
    D is Depth - 1,
    bagof(NextPos, move(Pos, NextPos), NextPosList),
    best(NextPosList, BestNextPos, Val, D), !.

minimax(Pos, _, Val, _) :-
    utility(Pos, Val).

% Om de beste zet te onderscheiden zullen we opnieuw de minimax gebruiken, om te zien wat de scores zijn van de volgende diepte
best([Pos], Pos, Val, _) :-
    minimax(Pos, _, Val, _), !.
best([Pos1 | PosList], BestPos, BestVal, Depth) :-
    minimax(Pos1, _, Val1, Depth),
    best(PosList, Pos2, Val2, Depth),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).

% Hier wordt er beslist of we een zo hoog mogelijke of zo laag mogelijke score willen voor het huidig niveau
betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-
    wining_to_play(Pos0),
    Val0 < Val1, !.
betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-
    losing_to_move(Pos0),
    Val0 > Val1, !.
betterOf(_, _, Pos1, Val1, Pos1, Val1).
