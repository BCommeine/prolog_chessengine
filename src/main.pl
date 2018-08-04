#!/usr/bin/env swipl

:- use_module(parser, [parse/2]).
:- use_module(chess, [move/2, utility/2, wining_to_play/1, losing_to_move/1]).
:- use_module(min_max, [minimax/3]).

:- initialization(main, main).

%Omdat de parser begonnen is van het idee een string om te zetten naar een spelvoorstelling zetten we de argumenten eerst om naar een string

main([Board, Turn, Castling, Passant, Half, Full]) :- 
    atomic_list_concat([Board, Turn, Castling, Passant, Half, Full],' ',Fen),
    parse(Fen, X),
    minimax(X, B, _),
    parse(Response, B), 
    write(Response),
    nl,
    halt(0).

main([Board, Turn, Castling, Passant, Half, Full, Test]) :- 
    Test  = 'TEST',
    atomic_list_concat([Board, Turn, Castling, Passant, Half, Full],' ',Fen),
    parse(Fen, B),
    findall(X, move(B, X), L),
    parse_list(L),
    halt(0).

parse_list([H]) :- parse(Fen, H), write(Fen), nl.
parse_list([H|T]) :- parse(Fen, H), write(Fen), nl, parse_list(T).