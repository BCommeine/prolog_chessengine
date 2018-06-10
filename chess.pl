:-module(chess, [move/2, utility/2, min_to_move/1, max_to_move/1]).
:- use_module(parser, [parse/2, dirk/1]).

% Interface of minimax-module
move(_, _).
utility(_, _).
min_to_move(_).
max_to_move(_).

% Utility functions to find data quickly
get_board([Board|_], Board).
get_turn([_, Turn| _], Turn).
get_castling([_, _, Castling|_], Castling).
get_passant([_, _, _, Passant], Passant).
get_half([_, _, _, _, Half], Half).
get_full([_, _, _, _, _, Full], Full).

% Find piece on a certain position
get_row(Board, Number, Row) :- arg(Number, Board, Row).
get_piece(Board, Row, Column, Piece) :- get_row(Board, Row, R), nth0(C, R, Piece), C is Column - 1.
is_empty(Board, Column, Row) :- get_piece(Board, Row, Column, empty).

% Find position of a certain piece
valid(X) :- X > 0, X < 9.
other_color(white, black).
other_color(black, white). 
piece_row([Piece |_], 1, Piece).
piece_row([_ | Tail], N, Piece) :- piece_row(Tail, N1, Piece), N is N1 + 1.
find_piece(rows( Row, _, _, _, _, _, _, _), Column, 1, Piece) :- piece_row(Row, Column, Piece).
find_piece(rows(_ , Row, _, _, _, _, _, _), Column, 2, Piece) :- piece_row(Row, Column, Piece).
find_piece(rows(_ , _, Row, _, _, _, _, _), Column, 3, Piece) :- piece_row(Row, Column, Piece).
find_piece(rows(_ , _, _, Row, _, _, _, _), Column, 4, Piece) :- piece_row(Row, Column, Piece).
find_piece(rows(_ , _, _, _, Row, _, _, _), Column, 5, Piece) :- piece_row(Row, Column, Piece).
find_piece(rows(_ , _, _, _, _, Row, _, _), Column, 6, Piece) :- piece_row(Row, Column, Piece).
find_piece(rows(_ , _, _, _, _, _, Row, _), Column, 7, Piece) :- piece_row(Row, Column, Piece).
find_piece(rows(_ , _, _, _, _, _, _, Row), Column, 8, Piece) :- piece_row(Row, Column, Piece).

% Move piece on new board, R1 C1
% set_move(Board, NewBoard, Piece, Column)
set_move([_|T], [Piece|T], Piece, 1).
set_move([H|T1], [H|T2], Piece, N) :- set_move(T1, T2, Piece, N1), N is N1 +1.

put_piece(Board, Nextboard, Col, Row, Piece) :-
    functor(Board, rows, N),
    functor(Nextboard, rows, N),
    put_piece(N, Board, Nextboard, Col, Row, Piece). 

put_piece(Row, Board, Nextboard, Col, Row, Piece):-
    !,
    arg(Row, Board, CurrentRow),
    set_move(CurrentRow, Next, Piece, Col),
    arg(Row, Nextboard, Next),
    N1 is Row-1,
    put_piece(N1,Board,Nextboard,Col, Row,Piece).

put_piece(N, Board, Nextboard, Col, Row, Piece):-
    N > 0,
    arg(N,Board,Arg),
    arg(N,Nextboard,Arg),
    N1 is N-1,
    put_piece(N1,Board,Nextboard,Col, Row,Piece).

put_piece(0, _, _, _, _, _).

% Move knight
patrick(Y) :- dirk(X), move_knight(X, Y).
dif(A, B, D) :- B is A + D,valid(B).
dif(A, B, D) :- B is A - D,valid(B).
knight_jump(Column, Row, C, R) :- dif(Column, C, 2), dif(Row, R, 1), valid(C), valid(R).
knight_jump(Column, Row, C, R) :- dif(Column, C, 1), dif(Row, R, 2), valid(C), valid(R).
move_knight(Position, Nextboard) :- get_board(Position, Board),
                                    get_turn(Position, Turn),
                                    find_piece(Board, Column, Row, piece(knight, Turn)),
                                    knight_jump(Column, Row, C, R),
                                    is_empty(Board, C, R),
                                    put_piece(Board, Next, Column, Row, empty),
                                    put_piece(Next, Nextboard, C, R, piece(knight, Turn)).

% Move Pawn
xavier(Y) :- dirk(X), move_pawn(X, Y).

pawn_jump(Row, R, white) :- R is Row - 1, valid(R).
pawn_jump(Row, R, black) :- R is Row + 1, valid(R).
pawn_jump(7, 5, white).
pawn_jump(2, 4, black).
move_pawn(Position, Nextboard) :-   get_board(Position, Board),
                                    get_turn(Position, Turn),
                                    find_piece(Board, Column, Row, piece(pawn, Turn)),
                                    pawn_jump(Row, R, Turn),
                                    is_empty(Board, Column, R),
                                    put_piece(Board, Next, Column, Row, empty),
                                    put_piece(Next, Nextboard, Column, R, piece(pawn, Turn)).
  
move_pawn(Position, Nextboard) :-   get_board(Position, Board),
                                    get_turn(Position, Turn),
                                    other_color(Turn, Other),
                                    find_piece(Board, Column, Row, piece(pawn, Turn)),
                                    pawn_jump(Row, R, Turn),
                                    dif(Column, C, 1),
                                    find_piece(Board, C, R, piece(_, Other)),
                                    put_piece(Board, Next, Column, Row, empty),
                                    put_piece(Next, Nextboard, C, R, piece(pawn, Turn)).

% Move Rook
