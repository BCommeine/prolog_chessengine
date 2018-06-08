:-module(chess, [move/2, utility/2, min_to_move/1, max_to_move/1]).
:- use_module(parser, [parse/2, dirk/1]).

move(_, _).
utility(_, _).
min_to_move(_).
max_to_move(_).

% Utility functions to find data quickly
get_board([Board|_], Board).
get_Turn([_, Turn| _], Turn).
get_Castling([_, _, Castling|_], Castling).
get_Passant([_, _, _, Passant], Passant).
get_Half([_, _, _, _, Half], Half).
get_Full([_, _, _, _, _, Full], Full).

get_row(Board, Number, Row) :- arg(Number, Board, Row).
get_piece(Board, Row, Column, Piece) :- get_row(Board, Row, R), nth0(Column, R, Piece).

% Vind de positie van een bepaald stuk
piece_row([Piece |_], 0, Piece).
piece_row([_ | Tail], N, Piece) :- piece_row(Tail, N1, Piece), N is N1 + 1.
find_piece(rows( Column, _, _, _, _, _, _, _), 0, Row, Piece) :- piece_row(Column, Row, Piece).
find_piece(rows(_ , Column, _, _, _, _, _, _), 1, Row, Piece) :- piece_row(Column, Row, Piece).
find_piece(rows(_ , _, Column, _, _, _, _, _), 2, Row, Piece) :- piece_row(Column, Row, Piece).
find_piece(rows(_ , _, _, Column, _, _, _, _), 3, Row, Piece) :- piece_row(Column, Row, Piece).
find_piece(rows(_ , _, _, _, Column, _, _, _), 4, Row, Piece) :- piece_row(Column, Row, Piece).
find_piece(rows(_ , _, _, _, _, Column, _, _), 5, Row, Piece) :- piece_row(Column, Row, Piece).
find_piece(rows(_ , _, _, _, _, _, Column, _), 6, Row, Piece) :- piece_row(Column, Row, Piece).
find_piece(rows(_ , _, _, _, _, _, _, Column), 7, Row, Piece) :- piece_row(Column, Row, Piece).
