:-module(chess, [move/2, utility/2, min_to_move/1, max_to_move/1]).
:- use_module(parser, [parse/2, dirk/1]).

move(_, _).
utility(_, _).
min_to_move(_).
max_to_move(_).

% Positie van één stuk uit de bordvoorstelling halen
positie_stuk(Stuk, Spel, Rij, Kolom) :- positie_rij(Stuk, Spel, Rij), nth0(Rij, Spel, SpelRij), positie_kolom_string(Stuk, SpelRij, Kolom).

positie_rij(Stuk, [H|_], 0) :- sub_string(H, _, _, _, Stuk).
positie_rij(Stuk, [_|T], N) :- positie_rij(Stuk, T, N1), !, N is N1+1.

positie_kolom_string(Stuk, String, N) :- string_chars(String, Lijst), positie_kolom(Stuk, Lijst, N).

positie_kolom(Stuk, [Stuk|_], 0).
positie_kolom(Stuk, [H|T], N) :- positie_kolom(Stuk, T, N1), char_type(H, digit), atom_number(H, Number), N is N1 + Number.
positie_kolom(Stuk, [H|T], N) :- positie_kolom(Stuk, T, N1), char_type(H, alpha), N is N1 + 1.

combine(X, Y, (X, Y)).
all_positions_piece(Stuk, Spel, Results) :- combine(X, Y, Result), findall(Result, positie_stuk(Stuk, Spel, X, Y), Results).