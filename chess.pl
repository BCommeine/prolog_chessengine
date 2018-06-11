:-module(chess, [move/2, utility/2, min_to_move/1, max_to_move/1]).
:- use_module(parser, [parse/2, dirk/1]).

% Interface of minimax-module:
% Move
move(Position, NextPosition) :- all_moves(Position, Next), valid_move(Next), swap(Next, NextPosition).

all_moves(Position, NextPosition) :- move_bishop(Position, NextPosition).
all_moves(Position, NextPosition) :- move_king(Position, NextPosition).
all_moves(Position, NextPosition) :- move_knight(Position, NextPosition).
all_moves(Position, NextPosition) :- move_pawn(Position, NextPosition).
all_moves(Position, NextPosition) :- move_queen(Position, NextPosition).
all_moves(Position, NextPosition) :- move_rook(Position, NextPosition).
jeffrey(Y) :- dirk(X), all_moves(X, Y).

/* valid_move(Position) :- 
 */
utility(Position, Value) :- get_board(Position, Board), get_turn(Position, Turn), board_value(Board, Turn, Value).
min_to_move([_, _, _, _, _, _, 0]).
max_to_move([_, _, _, _, _, _, 1]).



% Board Value
board_value(Board, Turn, Value) :-  get_row(Board, 1, R1), row_value(R1, V1, Turn),
                                    get_row(Board, 2, R2), row_value(R2, V2, Turn),
                                    get_row(Board, 3, R3), row_value(R3, V3, Turn),
                                    get_row(Board, 4, R4), row_value(R4, V4, Turn),
                                    get_row(Board, 5, R5), row_value(R5, V5, Turn),
                                    get_row(Board, 6, R6), row_value(R6, V6, Turn),
                                    get_row(Board, 7, R7), row_value(R7, V7, Turn),
                                    get_row(Board, 8, R8), row_value(R8, V8, Turn),
                                    Value is V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8, !.

% Row Value
row_value([], _, _).
row_value([E], V, Turn) :- piece_value(E, Turn, V), !.
row_value([H|T], V, Turn) :- piece_value(H, Turn, V1), row_value(T, V2, Turn), V is V2 + V1.

% Piece Value
piece_value(empty, _, 0).
piece_value(piece(pawn, Turn), Turn, 10).
piece_value(piece(pawn, Other), Turn, -10) :- other_color(Turn, Other).
piece_value(piece(rook, Turn), Turn, 50).
piece_value(piece(rook, Other), Turn, -50) :- other_color(Turn, Other).
piece_value(piece(knight, Turn), Turn, 30).
piece_value(piece(knight, Other), Turn, -30) :- other_color(Turn, Other).
piece_value(piece(bishop, Turn), Turn, 30).
piece_value(piece(bishop, Other), Turn, -30) :- other_color(Turn, Other).
piece_value(piece(queen, Turn), Turn, 90).
piece_value(piece(queen, Other), Turn, -90) :- other_color(Turn, Other).
piece_value(piece(king, Turn), Turn, 9001).
piece_value(piece(king, Other), Turn, -9001) :- other_color(Turn, Other).
                                         
% Utility functions to find data quickly
get_board([Board|_], Board).
get_turn([_, Turn| _], Turn).
get_castling([_, _, Castling|_], Castling).
get_passant([_, _, _, Passant], Passant).
get_half([_, _, _, _, Half], Half).
get_full([_, _, _, _, _, Full], Full).

% Utility functions to alter gamestate
set_board([_|T], B2,  [B2|T]).
swap_player([Board, Turn, Castling, Passant, Half, Full, 1], [Board, Other, Castling, Passant, Half, Full, 1]) :- other_color(Turn, Other).
swap_minmax([Board, Turn, Castling, Passant, Half, Full, X], [Board, Turn, Castling, Passant, Half, Full, Y]) :- Y is 1 - X.
swap(X, Y) :- swap_player(X, Z), swap_minmax(Z, Y).
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
move_knight(Position, NextPosition) :-  get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        find_piece(Board, Column, Row, piece(knight, Turn)),
                                        knight_jump(Column, Row, C, R),
                                        is_empty(Board, C, R),
                                        put_piece(Board, Next, Column, Row, empty),
                                        put_piece(Next, Nextboard, C, R, piece(knight, Turn)),
                                        set_board(Position, Nextboard, NextPosition).

% Move Pawn
xavier(Y) :- dirk(X), move_pawn(X, Y).

pawn_jump(Row, R, white) :- R is Row - 1, valid(R).
pawn_jump(Row, R, black) :- R is Row + 1, valid(R).
pawn_jump(7, 5, white).
pawn_jump(2, 4, black).
move_pawn(Position, NextPosition) :-    get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        find_piece(Board, Column, Row, piece(pawn, Turn)),
                                        pawn_jump(Row, R, Turn),
                                        is_empty(Board, Column, R),
                                        put_piece(Board, Next, Column, Row, empty),
                                        put_piece(Next, Nextboard, Column, R, piece(pawn, Turn)),
                                        set_board(Position, Nextboard, NextPosition).

move_pawn(Position, NextPosition) :-    get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        other_color(Turn, Other),
                                        find_piece(Board, Column, Row, piece(pawn, Turn)),
                                        pawn_jump(Row, R, Turn),
                                        dif(Column, C, 1),
                                        find_piece(Board, C, R, piece(_, Other)),
                                        put_piece(Board, Next, Column, Row, empty),
                                        put_piece(Next, Nextboard, C, R, piece(pawn, Turn)),
                                        set_board(Position, Nextboard, NextPosition).
% Move Rook
kevin(Y) :- dirk(X), move_rook(X, Y).
valid_rook_move(Board, Row, Column, R2, Column, Turn) :- R is Row + 1, valid(R), valid_rook_move_h(Board, R, Column, R2, Column, r, Turn).
valid_rook_move(Board, Row, Column, R2, Column, Turn) :- R is Row - 1, valid(R), valid_rook_move_h(Board, R, Column, R2, Column, l, Turn).
valid_rook_move(Board, Row, Column, Row, C2,Turn) :- C is Column + 1, valid(C), valid_rook_move_v(Board, Row, C, Row, C2, u, Turn).
valid_rook_move(Board, Row, Column, Row, C2,Turn) :- C is Column - 1, valid(C), valid_rook_move_v(Board, Row, C, Row, C2, d, Turn).

valid_rook_move_h(Board, Row, Column, Row, Column, _, _) :- is_empty(Board, Column, Row).
valid_rook_move_h(Board, Row, Column, Row, Column, _, Turn) :- get_piece(Board, Row, Column, piece(_, Other)), other_color(Turn, Other).
valid_rook_move_h(Board, Row, Column, R1, Column, r, Turn) :- is_empty(Board, Column, Row), R is Row + 1, valid(R), valid_rook_move_h(Board, R, Column, R1, Column, r, Turn).
valid_rook_move_h(Board, Row, Column, R1, Column, l, Turn) :- is_empty(Board, Column, Row), R is Row - 1, valid(R), valid_rook_move_h(Board, R, Column, R1, Column, l, Turn).

valid_rook_move_v(Board, Row, Column, Row, Column, _, _) :- is_empty(Board, Column, Row).
valid_rook_move_v(Board, Row, Column, Row, Column, _, Turn) :- get_piece(Board, Row, Column, piece(_, Other)), other_color(Turn, Other).
valid_rook_move_v(Board, Row, Column, Row, C1, u, Turn) :- is_empty(Board, Column, Row), C is Column + 1, valid(C), valid_rook_move_v(Board, Row, C, Row, C1, u, Turn).
valid_rook_move_v(Board, Row, Column, Row, C1, d, Turn) :- is_empty(Board, Column, Row), C is Column - 1, valid(C), valid_rook_move_v(Board, Row, C, Row, C1, d, Turn).
 
move_rook(Position, NextPosition) :-    get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        find_piece(Board, Column, Row, piece(rook, Turn)),
                                        valid_rook_move(Board, Row, Column, R, C, Turn),
                                        put_piece(Board, Next, Column, Row, empty),
                                        put_piece(Next, Nextboard, C, R, piece(rook, Turn)),
                                        set_board(Position, Nextboard, NextPosition).

% Move Bishop
kenny(Y) :- dirk(X), move_bishop(X, Y).
valid_bishop_move(Board, Row, Column, R, C, Turn) :- R1 is Row + 1, C1 is Column + 1, valid(R1), valid(C1), valid_bishop_move_u(Board, R1, C1, R, C, r, Turn).
valid_bishop_move(Board, Row, Column, R, C, Turn) :- R1 is Row + 1, C1 is Column - 1, valid(R1), valid(C1), valid_bishop_move_u(Board, R1, C1, R, C, l, Turn).
valid_bishop_move(Board, Row, Column, R, C, Turn) :- R1 is Row - 1, C1 is Column + 1, valid(C1), valid(R1), valid_bishop_move_d(Board, R1, C1, R, C, r, Turn).
valid_bishop_move(Board, Row, Column, R, C, Turn) :- R1 is Row - 1, C1 is Column - 1, valid(C1), valid(R1), valid_bishop_move_d(Board, R1, C1, R, C, l, Turn).

valid_bishop_move_u(Board, Row, Column, Row, Column, _, _) :- is_empty(Board, Column, Row).
valid_bishop_move_u(Board, Row, Column, Row, Column, _, Turn) :- get_piece(Board, Row, Column, piece(_, Other)), other_color(Turn, Other).
valid_bishop_move_u(Board, Row, Column, R, C, r, Turn) :- is_empty(Board, Column, Row), R1 is Row + 1, C1 is Column + 1, valid(R1), valid(C1), valid_bishop_move_u(Board, R1, C1, R, C, r, Turn).
valid_bishop_move_u(Board, Row, Column, R, C, l, Turn) :- is_empty(Board, Column, Row), R1 is Row + 1, C1 is Column - 1, valid(R1), valid(C1), valid_bishop_move_u(Board, R1, C1, R, C, l, Turn).

valid_bishop_move_d(Board, Row, Column, Row, Column, _, _) :- is_empty(Board, Column, Row).
valid_bishop_move_d(Board, Row, Column, Row, Column, _, Turn) :- get_piece(Board, Row, Column, piece(_, Other)), other_color(Turn, Other).
valid_bishop_move_d(Board, Row, Column, R, C, r, Turn) :- is_empty(Board, Column, Row), R1 is Row - 1, C1 is Column + 1, valid(R1), valid(C1), valid_bishop_move_d(Board, R1, C1, R, C, r, Turn).
valid_bishop_move_d(Board, Row, Column, R, C, l, Turn) :- is_empty(Board, Column, Row), R1 is Row - 1, C1 is Column - 1, valid(R1), valid(C1), valid_bishop_move_d(Board, R1, C1, R, C, l, Turn).
                
move_bishop(Position, NextPosition) :-  get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        find_piece(Board, Column, Row, piece(bishop, Turn)),
                                        valid_bishop_move(Board, Row, Column, R, C, Turn),
                                        put_piece(Board, Next, Column, Row, empty),
                                        put_piece(Next, Nextboard, C, R, piece(bishop, Turn)),
                                        set_board(Position, Nextboard, NextPosition).
% Move Queen
chantal(Y) :- dirk(X), move_queen(X, Y).
move_queen(Position, NextPosition) :-   get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        find_piece(Board, Column, Row, piece(queen, Turn)),
                                        valid_rook_move(Board, Row, Column, R, C, Turn),
                                        put_piece(Board, Next, Column, Row, empty),
                                        put_piece(Next, Nextboard, C, R, piece(queen, Turn)),
                                        set_board(Position, Nextboard, NextPosition).

move_queen(Position, NextPosition) :-   get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        find_piece(Board, Column, Row, piece(queen, Turn)),
                                        valid_bishop_move(Board, Row, Column, R, C, Turn),
                                        put_piece(Board, Next, Column, Row, empty),
                                        put_piece(Next, Nextboard, C, R, piece(queen, Turn)),
                                        set_board(Position, Nextboard, NextPosition).

% Move King
filip(Y) :- dirk(X), move_king(X, Y).
move_king(Position, NextPosition) :-    get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        find_piece(Board, Column, Row, piece(king, Turn)),
                                        valid_king_move(Board, Row, Column, R, C, Turn),
                                        put_piece(Board, Next, Column, Row, empty),
                                        put_piece(Next, Nextboard, C, R, piece(king, Turn)),
                                        set_board(Position, Nextboard, NextPosition).

valid_king_move(Board, Row, Column, R, C, _) :- king_move(Row, Column, R, C), is_empty(Board, C, R).
valid_king_move(Board, Row, Column, R, C, Turn) :- king_move(Row, Column, R, C), get_piece(Board, R, C, piece(_, Other)), other_color(Turn, Other).

king_move(Row, Column, R, Column) :- dif(Row, R, 1).
king_move(Row, Column, Row, C) :- dif(Column, C, 1).
king_move(Row, Column, R, C) :- dif(Column, C, 1), dif(Row, R, 1).