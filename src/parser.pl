:- module(parser, [parse/2, dirk/1]).
% Deze parse wordt enkel gebruikt om een FEN-string naar een voorstelling om te zetten
parse(Fen, Voorstelling) :- ground(Fen), string_chars(Fen, Chars), position(Voorstelling, Chars, []), !.
% Deze dan weer enkel om een voorstelling naar een Fen-string om te zetten
parse(Fen, Voorstelling) :- position(Voorstelling, Chars, []), string_chars(F, Chars), atom_string(F, Fen),!.

initial_fen_string("rnbqkbn1/ppppppp1/5r2/8/8/8/8/R3K2R w KQq - 1 11").
dirk(X) :- initial_fen_string(F), parse(F, X).



% De interne voorstelling is een lijst met Board, Turn, ..., adhv een dcg parsen we de string.
% Let op de twee voorkomens van deze functie, eindigend op 1 en 0. Deze boolean is om te kijken of we een zo hoog mogelijke of zo laag mogelijke score willen,
% en dus enkel handig bij de minmaxboom. De beide functie zijn nodig omdat we beide voorkomens willen kunnen parsen.
position([Board, Turn, Castling, Passant, Half, Full]) -->
  board(Board),
  space,
  turn(Turn),
  space,
  castling_rights(Castling),
  space,
  passant_square(Passant),
  space,
  integer(Half),
  space,
  integer(Full).

% Dit is een dcg om een bord te parsen. Een bord is een relatie rows tussen 8 row-objecten
board(rows(R1, R2, R3, R4, R5, R6, R7, R8)) -->
  row(R1), [/],
  row(R2), [/],
  row(R3), [/],
  row(R4), [/],
  row(R5), [/],
  row(R6), [/],
  row(R7), [/],
  row(R8).

% Hier wordt een row op een vrij eenvoudige, maar snelle, manier geparst.
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

% Dit is om de zwarte stukken te kunnen parsen
piece(piece(king, black)) --> [k].
piece(piece(queen, black)) --> [q].
piece(piece(rook, black)) --> [r].
piece(piece(bishop, black)) --> [b].
piece(piece(knight, black)) --> [n].
piece(piece(pawn, black)) --> [p].

% En dit voor de witte
piece(piece(king, white)) --> ['K'].
piece(piece(queen, white)) --> ['Q'].
piece(piece(rook, white)) --> ['R'].
piece(piece(bishop, white)) --> ['B'].
piece(piece(knight, white)) --> ['N'].
piece(piece(pawn, white)) --> ['P'].

% Hier wordt de turn geparst
turn(white) --> [w].
turn(black) --> [b].

% De rokade mogelijkheden houden we karakter per karakter bij, om aanpassingen gemakkelijker te maken
castling_rights(-) --> [-], !.
castling_rights([]) --> [].
castling_rights([H|T]) --> !, castling_rights_char(H), castling_rights(T).

castling_rights_char('K') --> ['K'].
castling_rights_char('Q') --> ['Q'].
castling_rights_char(k) --> [k].
castling_rights_char(q) --> [q].

% Hier wordt het vakje voor de enpassantregel bijgehouden
passant_square([-]) --> [-].
passant_square([C, D2]) --> [Char, D], {sub_string('xabcdefgh', C, 1, _, Ch), atom_string(Char, Ch), atom_number(D, D2)} .

%Hulpmiddel voor het parsen van de fen
space --> [' '], !.

% Cijfers parsen met een dcg ging niet met een voorziene functie dus heb ik een andere gevonden
integer(N) -->
  {nonvar(N), !,
  atom_chars(N, Codes)},
  Codes.


integer(N, Before, After) :-
  var(N),
  digits(Codes, Before, After),
number_codes(N, Codes).

digits([D|T]) -->
  digit(D), !,
  digits(T).
digits([]) -->
  [].

digit(D) -->
  [D],
  {
    code_type(D, digit)
  }.
