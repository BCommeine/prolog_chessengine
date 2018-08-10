:- module(chess, [get_turn/2, all_moves/2, move/2, value/2, wining_to_play/2, losing_to_move/2]).
:- use_module(parser, [parse/2,  dirk/1]).

% Interface of minimax-module:
% Move

% Deze move voldoet aan het interface, voorzien in de minmax boom
move(Position, NextPosition) :- all_moves(Position, NextPosition), \+(invalid_position(NextPosition)).

% all_moves gaat alle mogelijke zetten, schaak of niet, genereren
all_moves(Position, NextPosition) :- move_bishop(Position, NextPosition).
all_moves(Position, NextPosition) :- move_king(Position, NextPosition).
all_moves(Position, NextPosition) :- move_knight(Position, NextPosition).
all_moves(Position, NextPosition) :- move_pawn(Position, NextPosition).
all_moves(Position, NextPosition) :- move_queen(Position, NextPosition).
all_moves(Position, NextPosition) :- move_rook(Position, NextPosition).
jeffrey(Y) :- dirk(X), all_moves(X, Y).
% Wanneer iemand een zet doet, waarop zijn tegenstander zijn koning kan slaan (schaak) is deze zet invalid
invalid_position(Position) :-   get_board(Position, Board),
                                get_turn(Position, Turn),
                                other_color(Turn, Other),
                                find_piece(Board, Column, Row, piece(king, Other)),
                                all_moves(Position, NextPosition),
                                get_board(NextPosition, NextBoard),
                                get_piece(NextBoard, Row, Column, piece(_, Turn)).

% Deze utility functie behoort tot het interface van de minmaxboom
value(Position, Value) :- get_board(Position, Board), get_turn(Position, Turn), board_value(Board, Turn, Value).
% De volgende twee functies ook
wining_to_play(Board, Turn) :- get_turn(Board, Turn).
losing_to_move(Board, Turn) :- get_turn(Board, Other), other_color(Turn, Other).



% Board Value is een functie die een waarde zal geven aan een bord naargelang de speler aan beurt nog stukken heeft, en zijn tegenstander geen meer
board_value(Board, Turn, Value) :-  get_row(Board, 1, R1), row_value(R1, V1, Turn),
                                    get_row(Board, 2, R2), row_value(R2, V2, Turn),
                                    get_row(Board, 3, R3), row_value(R3, V3, Turn),
                                    get_row(Board, 4, R4), row_value(R4, V4, Turn),
                                    get_row(Board, 5, R5), row_value(R5, V5, Turn),
                                    get_row(Board, 6, R6), row_value(R6, V6, Turn),
                                    get_row(Board, 7, R7), row_value(R7, V7, Turn),
                                    get_row(Board, 8, R8), row_value(R8, V8, Turn),
                                    Value is V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8, !.

% Row Value (hulpfunctie)
row_value([], 0, _).
row_value([E], V, Turn) :- piece_value(E, Turn, V), !.
row_value([H|T], V, Turn) :- piece_value(H, Turn, V1), row_value(T, V2, Turn), V is V2 + V1.

% Piece Value (hulpfunctie)
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
get_passant([_, _, _, Passant|_], Passant).
get_half([_, _, _, _, Half|_], Half).
get_full([_, _, _, _, _, Full|_], Full).

% Utility functions to alter gamestate
set_board([_|T], B2,  [B2|T]).
set_castling([Board, Turn, _|T], [Board, Turn, C|T], C).
set_enpassant([Board, Turn, Castling, _|T], [Board, Turn, Castling, Passant|T], Passant).
set_halfmove([Board, Turn, Castling, Passant, _|T], [Board, Turn, Castling, Passant, Halfmove|T], Halfmove).

% Swap players en min-max na een beurt
swap([Board, white, Castling, Passant, Half, Full], [Board, black, Castling, Passant, Half, Full]).
swap([Board, black, Castling, Passant, Half, Full], [Board, white, Castling, Passant, Half, Fuller]):- Fuller is Full + 1.
% Find piece on a certain position
get_row(Board, Number, Row) :- arg(Number, Board, Row).
get_piece(Board, Row, Column, Piece) :- get_row(Board, Row, R), nth0(C, R, Piece), C is Column - 1.
is_empty(Board, Column, Row) :- get_piece(Board, Row, Column, empty).

increment_halfmove(Old, New) :- get_half(Old, OldHalve), NewHalve is OldHalve + 1, set_halfmove(Old, New, NewHalve).

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

% Zet een stuk op een bepaalde plaats op het bord (adhv row/column)
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

take_piece(Board, NewBoard, Castling, Cas, Column, Row, C, R, Piece, Color) :-
  startposition_rook(Board, Other, C, R, T), !,
  delete_castling(T, Castling, Cas),
  other_color(Color, Other),
  put_piece(Board, Next, Column, Row, empty),
  put_piece(Next, NewBoard, C, R, piece(Piece, Color)).

take_piece(Board, NewBoard, Castling, Castling, Column, Row, C, R, Piece, Color) :-
  find_piece(Board, C, R, piece(_, Other)),
  other_color(Color, Other),
  put_piece(Board, Next, Column, Row, empty),
  put_piece(Next, NewBoard, C, R, piece(Piece, Color)).


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
                                        set_enpassant(Position, Positioner, ['-']),
                                        increment_halfmove(Positioner, Positionest),
                                        set_board(Positionest, Nextboard, NewPosition),
                                        swap(NewPosition, NextPosition).

move_knight(Position, NextPosition) :-  get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        get_castling(Position, Castling),
                                        find_piece(Board, Column, Row, piece(knight, Turn)),
                                        set_halfmove(Position, Positioner, 0),
                                        set_enpassant(Positioner, Positionest, ['-']),
                                        knight_jump(Column, Row, C, R),
                                        take_piece(Board, NewBoard, Castling, Cas, Column, Row, C, R, knight, Turn),
                                        set_board(Positionest, NewBoard, NewPosition),
                                        set_castling(NewPosition, NewerPosition, Cas),
                                        swap(NewerPosition, NextPosition).

% Move Pawn
xavier(Y) :- dirk(X), move_pawn(X, Y).

pawn_jump(Row, R, white) :- R is Row - 1, valid(R).
pawn_jump(Row, R, black) :- R is Row + 1, valid(R).
pawn_double_jump(7, 5, white).
pawn_double_jump(2, 4, black).
pawn_jump_promo(2, 1, white).
pawn_jump_promo(7, 8, black).

% Promotie
move_pawn(Position, NextPosition) :-    get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        find_piece(Board, Column, Row, piece(pawn, Turn)),
                                        pawn_jump_promo(Row, R, Turn),
                                        is_empty(Board, Column, R), !,
                                        put_piece(Board, Next, Column, Row, empty),
                                        put_piece(Next, Nextboard, Column, R, piece(Piece, Turn)), is_piece(Piece),
                                        set_enpassant(Position, Positioner, ['-']),
                                        set_halfmove(Positioner, Positionest, 0),
                                        set_board(Positionest, Nextboard, NewPosition),
                                        swap(NewPosition, NextPosition).

move_pawn(Position, NextPosition) :-    get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        get_castling(Position, Castling),
                                        find_piece(Board, Column, Row, piece(pawn, Turn)),
                                        pawn_jump_promo(Row, R, Turn),
                                        dif(Column, C, 1),
                                        take_piece(Board, NewBoard, Castling, Cas, Column, Row, C, R, Piece, Turn), !,
                                        is_piece(Piece),
                                        set_enpassant(Position, Positioner, ['-']),
                                        set_halfmove(Positioner, Positionest, 0),
                                        set_board(Positionest, NewBoard, NewPosition),
                                        set_castling(NewPosition, NewerPosition, Cas),
                                        swap(NewerPosition, NextPosition).

% Dubbele sprong zet
move_pawn(Position, NextPosition) :-    get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        find_piece(Board, Column, Row, piece(pawn, Turn)),
                                        pawn_double_jump(Row, R, Turn),
                                        is_empty(Board, Column, R),
                                        put_piece(Board, Next, Column, Row, empty),
                                        put_piece(Next, Nextboard, Column, R, piece(pawn, Turn)),
                                        set_enpassant(Position, Positioner, [Column, R2]),
                                        R2 is 9 - ((Row + R) / 2),
                                        set_halfmove(Positioner, Positionest, 0),
                                        set_board(Positionest, Nextboard, NewPosition),
                                        swap(NewPosition, NextPosition).

% Gewone zet
move_pawn(Position, NextPosition) :-    get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        find_piece(Board, Column, Row, piece(pawn, Turn)),
                                        pawn_jump(Row, R, Turn),
                                        is_empty(Board, Column, R),
                                        put_piece(Board, Next, Column, Row, empty),
                                        put_piece(Next, Nextboard, Column, R, piece(pawn, Turn)),
                                        set_enpassant(Position, Positioner, ['-']),
                                        set_halfmove(Positioner, Positionest, 0),
                                        set_board(Positionest, Nextboard, NewPosition),
                                        swap(NewPosition, NextPosition).

move_pawn(Position, NextPosition) :-    get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        get_castling(Position, Castling),
                                        find_piece(Board, Column, Row, piece(pawn, Turn)),
                                        pawn_jump(Row, R, Turn),
                                        dif(Column, C, 1),
                                        take_piece(Board, NewBoard, Castling, Cas, Column, Row, C, R, pawn, Turn),
                                        set_enpassant(Position, Positioner, ['-']),
                                        set_halfmove(Positioner, Positionest, 0),
                                        set_board(Positionest, NewBoard, NewPosition),
                                        set_castling(NewPosition, NewerPosition, Cas),
                                        swap(NewerPosition, NextPosition).

% Dit zijn de stukken die gebruikt mogen worden bij het promoveren
is_piece(queen).
is_piece(rook).
is_piece(knight).
is_piece(bishop).

% Move Rook
kevin(Y) :- dirk(X), move_rook(X, Y).

valid_rook_move(Board, Row, Column, R2, Column, Turn, Halfmove) :- R is Row + 1, valid(R), valid_rook_move_h(Board, R, Column, R2, Column, r, Turn, Halfmove).
valid_rook_move(Board, Row, Column, R2, Column, Turn, Halfmove) :- R is Row - 1, valid(R), valid_rook_move_h(Board, R, Column, R2, Column, l, Turn, Halfmove).
valid_rook_move(Board, Row, Column, Row, C2,Turn, Halfmove) :- C is Column + 1, valid(C), valid_rook_move_v(Board, Row, C, Row, C2, u, Turn, Halfmove).
valid_rook_move(Board, Row, Column, Row, C2,Turn, Halfmove) :- C is Column - 1, valid(C), valid_rook_move_v(Board, Row, C, Row, C2, d, Turn, Halfmove).

% Wanneer de eerste move horizontaal was blijven we adhv deze relatie horizontale vakjes vinden. Er is een variabele voor links/rechts
valid_rook_move_h(Board, Row, Column, Row, Column, _, _, inc) :- is_empty(Board, Column, Row).
valid_rook_move_h(Board, Row, Column, Row, Column, _, Turn, reset) :- get_piece(Board, Row, Column, piece(_, Other)), other_color(Turn, Other).
valid_rook_move_h(Board, Row, Column, R1, Column, r, Turn, Halfmove) :- is_empty(Board, Column, Row), R is Row + 1, valid(R), valid_rook_move_h(Board, R, Column, R1, Column, r, Turn, Halfmove).
valid_rook_move_h(Board, Row, Column, R1, Column, l, Turn, Halfmove) :- is_empty(Board, Column, Row), R is Row - 1, valid(R), valid_rook_move_h(Board, R, Column, R1, Column, l, Turn, Halfmove).

% Wanneer de eerste move verticaal was blijven we adhv deze relatie verticale vakjes vinden, er is een variabele voor up/down
valid_rook_move_v(Board, Row, Column, Row, Column, _, _, inc) :- is_empty(Board, Column, Row).
valid_rook_move_v(Board, Row, Column, Row, Column, _, Turn, reset) :- get_piece(Board, Row, Column, piece(_, Other)), other_color(Turn, Other).
valid_rook_move_v(Board, Row, Column, Row, C1, u, Turn, Halfmove) :- is_empty(Board, Column, Row), C is Column + 1, valid(C), valid_rook_move_v(Board, Row, C, Row, C1, u, Turn, Halfmove).
valid_rook_move_v(Board, Row, Column, Row, C1, d, Turn, Halfmove) :- is_empty(Board, Column, Row), C is Column - 1, valid(C), valid_rook_move_v(Board, Row, C, Row, C1, d, Turn, Halfmove).

startposition_rook(Board, Turn, 1, 1, 'q') :- find_piece(Board, 1, 1, piece(rook, Turn)).
startposition_rook(Board, Turn, 8, 1, 'k') :- find_piece(Board, 8, 1, piece(rook, Turn)).
startposition_rook(Board, Turn, 8, 8, 'K') :- find_piece(Board, 8, 8, piece(rook, Turn)).
startposition_rook(Board, Turn, 1, 8, 'Q') :- find_piece(Board, 1, 8, piece(rook, Turn)).

% Hier gebeurt er niets bijzonders (stuk vinden, zet genereren en uitvoeren), alleen zal de rokade-aanduider ook aangepast worden
move_rook(Position, NextPosition) :-    get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        startposition_rook(Board, Turn, Column, Row, Del),
                                        get_castling(Position, Castling),
                                        delete_castling(Del, Castling, Cas),
                                        set_castling(Position, NewerPosition, Cas),
                                        valid_rook_move(Board, Row, Column, R, C, Turn, inc),
                                        put_piece(Board, Next, Column, Row, empty),
                                        put_piece(Next, Nextboard, C, R, piece(rook, Turn)),
                                        increment_halfmove(NewerPosition, NewererPosition),
                                        set_enpassant(NewererPosition, NewerPositioner, ['-']),
                                        set_board(NewerPositioner, Nextboard, NewPosition),
                                        swap(NewPosition, NextPosition).

move_rook(Position, NextPosition) :-    get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        startposition_rook(Board, Turn, Column, Row, Del),
                                        get_castling(Position, Castling),
                                        delete_castling(Del, Castling, Cas),
                                        set_castling(Position, NewerPosition, Cas),
                                        valid_rook_move(Board, Row, Column, R, C, Turn, reset),
                                        take_piece(Board, NewBoard, Cas, Cas2, Column, Row, C, R, rook, Turn),
                                        set_halfmove(NewerPosition, NewererPosition, 0),
                                        set_enpassant(NewererPosition, NewerPositioner, ['-']),
                                        set_board(NewerPositioner, NewBoard, NewPosition),
                                        set_castling(NewPosition, Chewbacca, Cas2),
                                        swap(Chewbacca, NextPosition).


move_rook(Position, NextPosition) :-    get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        find_piece(Board, Column, Row, piece(rook, Turn)),
                                        \+(startposition_rook(Board, Turn, Column, Row, _)),
                                        valid_rook_move(Board, Row, Column, R, C, Turn, inc),
                                        put_piece(Board, Next, Column, Row, empty),
                                        put_piece(Next, Nextboard, C, R, piece(rook, Turn)),
                                        increment_halfmove(Position, Poposition),
                                        set_enpassant(Poposition, Positioner, ['-']),
                                        set_board(Positioner, Nextboard, NewPosition),
                                        swap(NewPosition, NextPosition).

move_rook(Position, NextPosition) :-    get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        get_castling(Position, Castling),
                                        find_piece(Board, Column, Row, piece(rook, Turn)),
                                        \+(startposition_rook(Board, Turn, Column, Row, _)),
                                        valid_rook_move(Board, Row, Column, R, C, Turn, reset),
                                        take_piece(Board, NewBoard, Castling, Cas, Column, Row, C, R, rook, Turn),
                                        set_halfmove(Position, Poposition, 0),
                                        set_enpassant(Poposition, Positioner, ['-']),
                                        set_board(Positioner, NewBoard, NewPosition),
                                        set_castling(NewPosition, Chewbacca, Cas),
                                        swap(Chewbacca, NextPosition).

% Move Bishop
kenny(Y) :- dirk(X), move_bishop(X, Y).

% De bishop is op dezelfde manier gedefinieerd als de toren, minus het rokeren
valid_bishop_move(Board, Row, Column, R, C, Turn, Halfmove) :- R1 is Row + 1, C1 is Column + 1, valid(R1), valid(C1), valid_bishop_move_u(Board, R1, C1, R, C, r, Turn, Halfmove).
valid_bishop_move(Board, Row, Column, R, C, Turn, Halfmove) :- R1 is Row + 1, C1 is Column - 1, valid(R1), valid(C1), valid_bishop_move_u(Board, R1, C1, R, C, l, Turn, Halfmove).
valid_bishop_move(Board, Row, Column, R, C, Turn, Halfmove) :- R1 is Row - 1, C1 is Column + 1, valid(C1), valid(R1), valid_bishop_move_d(Board, R1, C1, R, C, r, Turn, Halfmove).
valid_bishop_move(Board, Row, Column, R, C, Turn, Halfmove) :- R1 is Row - 1, C1 is Column - 1, valid(C1), valid(R1), valid_bishop_move_d(Board, R1, C1, R, C, l, Turn, Halfmove).

valid_bishop_move_u(Board, Row, Column, Row, Column, _, _, inc) :- is_empty(Board, Column, Row).
valid_bishop_move_u(Board, Row, Column, Row, Column, _, Turn, reset) :- get_piece(Board, Row, Column, piece(_, Other)), other_color(Turn, Other).
valid_bishop_move_u(Board, Row, Column, R, C, r, Turn, Halfmove) :- is_empty(Board, Column, Row), R1 is Row + 1, C1 is Column + 1, valid(R1), valid(C1), valid_bishop_move_u(Board, R1, C1, R, C, r, Turn, Halfmove).
valid_bishop_move_u(Board, Row, Column, R, C, l, Turn, Halfmove) :- is_empty(Board, Column, Row), R1 is Row + 1, C1 is Column - 1, valid(R1), valid(C1), valid_bishop_move_u(Board, R1, C1, R, C, l, Turn, Halfmove).

valid_bishop_move_d(Board, Row, Column, Row, Column, _, _, inc) :- is_empty(Board, Column, Row).
valid_bishop_move_d(Board, Row, Column, Row, Column, _, Turn, reset) :- get_piece(Board, Row, Column, piece(_, Other)), other_color(Turn, Other).
valid_bishop_move_d(Board, Row, Column, R, C, r, Turn, Halfmove) :- is_empty(Board, Column, Row), R1 is Row - 1, C1 is Column + 1, valid(R1), valid(C1), valid_bishop_move_d(Board, R1, C1, R, C, r, Turn, Halfmove).
valid_bishop_move_d(Board, Row, Column, R, C, l, Turn, Halfmove) :- is_empty(Board, Column, Row), R1 is Row - 1, C1 is Column - 1, valid(R1), valid(C1), valid_bishop_move_d(Board, R1, C1, R, C, l, Turn, Halfmove).

move_bishop(Position, NextPosition) :-  get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        find_piece(Board, Column, Row, piece(bishop, Turn)),
                                        valid_bishop_move(Board, Row, Column, R, C, Turn, inc),
                                        put_piece(Board, Next, Column, Row, empty),
                                        put_piece(Next, Nextboard, C, R, piece(bishop, Turn)),
                                        increment_halfmove(Position, Poposition),
                                        set_enpassant(Poposition, Positioner, ['-']),
                                        set_board(Positioner, Nextboard, NewPosition),
                                        swap(NewPosition, NextPosition).

move_bishop(Position, NextPosition) :-  get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        get_castling(Position, Castling),
                                        find_piece(Board, Column, Row, piece(bishop, Turn)),
                                        valid_bishop_move(Board, Row, Column, R, C, Turn, reset),
                                        take_piece(Board, NewBoard, Castling, Cas, Column, Row, C, R, bishop, Turn),
                                        set_halfmove(Position, Poposition, 0),
                                        set_enpassant(Poposition, Positioner, ['-']),
                                        set_board(Positioner, NewBoard, NewPosition),
                                        set_castling(NewPosition, Chewbacca, Cas),
                                        swap(Chewbacca, NextPosition).
% Move Queen
chantal(Y) :- dirk(X), move_queen(X, Y).

% Om een zet te genereren voor de koningin genereren we alle zetten die zowel torens als lopers zouden kunnen maken
move_queen(Position, NextPosition) :-   get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        find_piece(Board, Column, Row, piece(queen, Turn)),
                                        valid_rook_move(Board, Row, Column, R, C, Turn, inc),
                                        put_piece(Board, Next, Column, Row, empty),
                                        put_piece(Next, Nextboard, C, R, piece(queen, Turn)),
                                        increment_halfmove(Position, Poposition),
                                        set_enpassant(Poposition, Positioner, ['-']),
                                        set_board(Positioner, Nextboard, NewPosition),
                                        swap(NewPosition, NextPosition).

move_queen(Position, NextPosition) :-   get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        get_castling(Position, Castling),
                                        find_piece(Board, Column, Row, piece(queen, Turn)),
                                        valid_rook_move(Board, Row, Column, R, C, Turn, reset),
                                        take_piece(Board, NewBoard, Castling, Cas, Column, Row, C, R, queen, Turn),
                                        set_halfmove(Position, Poposition, 0),
                                        set_enpassant(Poposition, Positioner, ['-']),
                                        set_board(Positioner, NewBoard, NewPosition),
                                        set_castling(NewPosition, Chewbacca, Cas),
                                        swap(Chewbacca, NextPosition).

move_queen(Position, NextPosition) :-   get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        find_piece(Board, Column, Row, piece(queen, Turn)),
                                        valid_bishop_move(Board, Row, Column, R, C, Turn, inc),
                                        put_piece(Board, Next, Column, Row, empty),
                                        put_piece(Next, Nextboard, C, R, piece(queen, Turn)),
                                        increment_halfmove(Position, Poposition),
                                        set_enpassant(Poposition, Positioner, ['-']),
                                        set_board(Positioner, Nextboard, NewPosition),
                                        swap(NewPosition, NextPosition).

move_queen(Position, NextPosition) :-   get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        get_castling(Position, Castling),
                                        find_piece(Board, Column, Row, piece(queen, Turn)),
                                        valid_bishop_move(Board, Row, Column, R, C, Turn, reset),
                                        take_piece(Board, NewBoard, Castling, Cas, Column, Row, C, R, queen, Turn),
                                        set_halfmove(Position, Poposition, 0),
                                        set_enpassant(Poposition, Positioner, ['-']),
                                        set_board(Positioner, NewBoard, NewPosition),
                                        set_castling(NewPosition, Chewbacca, Cas),
                                        swap(Chewbacca, NextPosition).

% Move King
filip(Y) :- dirk(X), move_king(X, Y).

% Voor de koning genereren we alle zetten op één plaats afstand. Ook worden de rokade variabelen aangepast
move_king(Position, NextPosition) :-    get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        find_piece(Board, Column, Row, piece(king, Turn)),
                                        valid_king_move(Board, Row, Column, R, C, Turn, inc),
                                        put_piece(Board, Next, Column, Row, empty),
                                        put_piece(Next, Nextboard, C, R, piece(king, Turn)),
                                        increment_halfmove(Position, Poposition),
                                        set_enpassant(Poposition, Positioner, ['-']),
                                        set_board(Positioner, Nextboard, NewPosition),
                                        disable_castling(NewPosition, NewerPosition),
                                        swap(NewerPosition, NextPosition).

move_king(Position, NextPosition) :-    get_board(Position, Board),
                                        get_turn(Position, Turn),
                                        get_castling(Position, Castling),
                                        find_piece(Board, Column, Row, piece(king, Turn)),
                                        valid_king_move(Board, Row, Column, R, C, Turn, reset),
                                        take_piece(Board, NewBoard, Castling, Cas, Column, Row, C, R, king, Turn),
                                        set_halfmove(Position, Poposition, 0),
                                        set_enpassant(Poposition, Positioner, ['-']),
                                        set_board(Positioner, NewBoard, NewPosition),
                                        set_castling(NewPosition, NewerPosition , Cas),
                                        disable_castling(NewerPosition, Chewbacca),
                                        swap(Chewbacca, NextPosition).

valid_king_move(Board, Row, Column, R, C, _, inc) :- king_move(Row, Column, R, C), is_empty(Board, C, R).
valid_king_move(Board, Row, Column, R, C, Turn, reset) :- king_move(Row, Column, R, C), get_piece(Board, R, C, piece(_, Other)), other_color(Turn, Other).

king_move(Row, Column, R, Column) :- dif(Row, R, 1).
king_move(Row, Column, Row, C) :- dif(Column, C, 1).
king_move(Row, Column, R, C) :- dif(Column, C, 1), dif(Row, R, 1).

disable_castling([Board, white, Castling | Tail], [Board, white, C | Tail]) :- delete_castling('K', Castling, C1), delete_castling('Q', C1, C).
disable_castling([Board, black, Castling | Tail], [Board, black, C | Tail]) :- delete_castling('k', Castling, C1), delete_castling('q', C1, C).

delete_castling(_, '-', '-') :- !.
delete_castling(_, [], []) :- !.
delete_castling(K, [K], '-') :- !.
delete_castling(Term, [Term|Tail], Tail) :- !.
delete_castling(Term, [Head|Tail], [Head|Result]) :- delete_castling(Term, Tail, Result).
