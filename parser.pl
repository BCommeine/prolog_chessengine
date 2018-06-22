:- module(parser, [parse/2, dirk/1]).

parse(Fen, Voorstelling) :- ground(Fen), string_chars(Fen, Chars), position(Voorstelling, Chars, []), !.
parse(Fen, Voorstelling) :- position(Voorstelling, Chars, []), string_chars(Fen, Chars), !.

initial_fen_string("rnbqkbnr/ppppppp/8/8/8/8/3R4/RNBQKBN1 w KQkq - 0 1").
dirk(X) :- initial_fen_string(F), parse(F, X).
        
position([Board, Turn, Castling, Passant, Half, Full, 1]) -->
    board(Board),
    space,
    turn(Turn),
    space, 
    castling_rights(Castling),
    space,
    passant_square(Passant),
    space,
    digits(Half),
    space,
    digits(Full).

position([Board, Turn, Castling, Passant, Half, Full, 0]) -->
    board(Board),
    space,
    turn(Turn),
    space, 
    castling_rights(Castling),
    space,
    passant_square(Passant),
    space,
    digits(Half),
    space,
    digits(Full).

board(rows(R1, R2, R3, R4, R5, R6, R7, R8)) -->
    row(R1), [/],
    row(R2), [/],
    row(R3), [/],
    row(R4), [/],
    row(R5), [/],
    row(R6), [/],
    row(R7), [/],
    row(R8).


row([]) --> [].
row([empty, empty, empty, empty, empty, empty, empty, empty| T]) --> ['8'], row(T).
row([empty, empty, empty, empty, empty, empty, empty| T]) --> ['7'], row(T).
row([empty, empty, empty, empty, empty, empty| T]) --> ['6'], row(T).
row([empty, empty, empty, empty, empty| T]) --> ['5'], row(T).
row([empty, empty, empty, empty| T]) --> ['4'], row(T).
row([empty, empty, empty| T]) --> ['3'], row(T).
row([empty, empty| T]) --> ['2'], row(T).
row([empty| T]) --> ['1'], row(T).
row([B|T]) --> piece(B), !, row(T).

piece(piece(king, black)) --> [k].
piece(piece(queen, black)) --> [q].
piece(piece(rook, black)) --> [r].
piece(piece(bishop, black)) --> [b].
piece(piece(knight, black)) --> [n].
piece(piece(pawn, black)) --> [p].

piece(piece(king, white)) --> ['K'].
piece(piece(queen, white)) --> ['Q'].
piece(piece(rook, white)) --> ['R'].
piece(piece(bishop, white)) --> ['B'].
piece(piece(knight, white)) --> ['N'].
piece(piece(pawn, white)) --> ['P'].

turn(white) --> [w].
turn(black) --> [b].

castling_rights(-) --> [-], !.
castling_rights([]) --> [].
castling_rights([H|T]) --> !, castling_rights_char(H), castling_rights(T).

castling_rights_char('K') --> ['K'].
castling_rights_char('Q') --> ['Q'].
castling_rights_char(k) --> [k].
castling_rights_char(q) --> [q].

passant_square([-]) --> [-].
passant_square([C, D]) --> [C, D], {sub_string("abcdefgh", _, 1, _, C), is_digit(D)}.

space --> [' '], !.

digits([D|T]) -->
    digit(D), !,
    digits(T).
digits([]) -->
    [].

digit(D) -->
    [D],
    { code_type(D, digit)
    }.