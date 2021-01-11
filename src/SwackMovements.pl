%In this file, we have an algoritm that handles the flow of the game. It keeps the game running, alternating the turns, until the end of the game

:- use_module(library(random)).

%This means the 2 players have passed in sequence, so we end the game.
turn(Board, Player, 2) :- otherPlayer(Player, LastPlayerToPlay), game_over(Board-LastPlayerToPlay, Winner/Points), end(Winner, Points).
turnBot(Board, Player, 2) :- otherPlayer(Player, LastPlayerToPlay), game_over(Board-LastPlayerToPlay, Winner/Points), end(Winner, Points).

%This predicate is for a human turn
turn(Board, Player, NumPasses) :- display_game(Board, Player), write('Do you want to make a move or pass your turn? (move/pass)'),
									read(PassOrMove), nl, nl, checkPassTurn(PassOrMove, Board, Player, NumPasses).


%This predicate is for a bot turn
turnBot(Board, white, NumPasses) :- display_game(Board, white), whiteBotIntelligence(Intelligence), botChooseMove(Board, white, NumPasses, Intelligence).
turnBot(Board, black, NumPasses) :- display_game(Board, black), blackBotIntelligence(Intelligence), botChooseMove(Board, black, NumPasses, Intelligence).

%Bot chooses the moves and then make a play
botChooseMove(Board, Player, NumPasses, Level) :- choose_move(Board, Player, Level, Move), botMakeMove(Board, Player, NumPasses, Move).


%Random bot chooses a random move of the list
choose_move(Board, Player, 1, Move) :- valid_moves(Board, Player, ListOfMoves), random_permutation(ListOfMoves, [Move | _]).
%Smart bot always chooses the first move of the list, because it is the best move in the short term
choose_move(Board, Player, 2, Move) :- valid_moves(Board, Player, [Move | _ ]).

%Bot pass his turn
botMakeMove(Board, Player, NumPasses, _-999/999-pass) :- checkPassTurn('pass', Board, Player, NumPasses).

%Bot actually makes a move
botMakeMove(Board, Player, _, _-InitialRow/InitialColumn-DestRow/DestColumn) :- 
																	move(Board-Player, InitialRow/InitialColumn-DestRow/DestColumn, NewBoard),
																	printCapture(Player, InitialRow, InitialColumn, DestRow, DestColumn), gameMode(Mode),
																	endTurn(NewBoard, Player, 0, Mode).

move(Board-Player, InitialRow/InitialColumn-DestRow/DestColumn, NewBoard) :- 
																	findStack(Board, InitialRow, InitialColumn, _, Height),
																	capture(Board, Player, InitialRow, InitialColumn, Height, DestRow, DestColumn, NewBoard).

%Returns the list of the valid moves
valid_moves(Board, Player, ListOfMoves) :- findall(Value-InitialRow/InitialColumn-DestRow/DestColumn, (
										  validateChosenPiece(Board, Player, InitialRow, InitialColumn, Height),
										  validateStackToCapture(Board, Player, InitialRow, InitialColumn, Height, DestRow, DestColumn),
										  capture(Board, Player, InitialRow, InitialColumn, Height, DestRow, DestColumn, NewBoard),
										  value(NewBoard, Player, Value)  %Counts the points of the new scenario, after a possible move
										  ), L),
										  value(Board, Player, Value),  %Counts the points of the current board and calculates the value (in case of pass move)
										  append([Value-999/999-pass], L, L1),  %The structure DiffPoints-999/999/pass is only for sorting purposes. If 'pass' has higher or equal value of the highest value valid move(s), the smart bot will choose to pass (This makes the game end more quickly)
										  sort(L1, InvListOfMoves), reverse(InvListOfMoves, ListOfMoves). %Sorted from the best to the worst move

%Counts the points of each player in this board, and calculates the difference of points between the player and his oponent
value(Board, Player, Value) :- countPoints(Board, ListWhitePoints, ListBlackPoints), diffPoints(Player, ListWhitePoints, ListBlackPoints, Value).

%Calculates the diffence between the points of the current player and his oponent
diffPoints(white, [WhitePoints | _ ], [BlackPoints | _ ], DiffPoints) :- DiffPoints is WhitePoints - BlackPoints.
diffPoints(black, [WhitePoints | _ ], [BlackPoints | _ ], DiffPoints) :- DiffPoints is BlackPoints - WhitePoints.


%In case of someone passes the turn
checkPassTurn('pass', Board, Player, NumPasses) :- write('Looks like '), write(Player), write(' has passed his turn'), nl, nl,
											NewNumPasses is NumPasses + 1, gameMode(Mode), endTurn(Board, Player, NewNumPasses, Mode).
checkPassTurn('move', Board, Player, NumPasses) :- nl, write('Choose the piece which you want to move.'), nl, chooseInitialRow(Board, Player, NumPasses).
checkPassTurn( _, Board, Player, NumPasses) :- write('Invalid answer, try again'), nl, nl, turn(Board, Player, NumPasses).


%Asks the human player to choose the row of the piece he wants to move
chooseInitialRow(Board, Player, NumPasses) :- write('Write the row of the piece you want to move'), read(InitialRow),
											nl, chooseInitialColumn(Board, Player, NumPasses, InitialRow).

%Asks the human player to choose the column of the piece he wants to move											
chooseInitialColumn(Board, Player, NumPasses, InitialRow) :- write('Write the column of the piece you want to move'), read(InitialColumn),
											nl, validateChosenPiece(Board, Player, InitialRow, InitialColumn, Height),
											chooseDestRow(Board, Player, NumPasses, InitialRow, InitialColumn, Height).

%This is for the case of a invalid choice for the piece to move											
chooseInitialColumn(Board, Player, NumPasses, _) :- write('Invalid choice, lets reset your turn'), nl, nl,
															turn(Board, Player, NumPasses).



%Asks the human player to choose the row of the piece he wants to capture
chooseDestRow(Board, Player, NumPasses, InitialRow, InitialColumn, Height) :- write('Write the row of the stack you want to capture'), read(DestRow),
											nl, chooseDestColumn(Board, Player, NumPasses, InitialRow, InitialColumn, Height, DestRow).

%Asks the human player to choose the column of the piece he wants to capture											
chooseDestColumn(Board, Player, NumPasses, InitialRow, InitialColumn, Height, DestRow) :- write('Write the column of the stack you want to capture'), read(DestColumn),
											nl, validateStackToCapture(Board, Player, InitialRow, InitialColumn, Height, DestRow, DestColumn),
											move(Board-Player, InitialRow/InitialColumn-DestRow/DestColumn, NewBoard),
											printCapture(Player, InitialRow, InitialColumn, DestRow, DestColumn), gameMode(Mode),
											endTurn(NewBoard, Player, 0, Mode).

%This is for the case of a invalid choice for the piece to capture	
chooseDestColumn(Board, Player, NumPasses, _, _, _, _) :- write('Invalid move, lets reset your turn'), nl, nl, turn(Board, Player, NumPasses).



%End turn for Human vs Human, gameMode 1
endTurn(Board, Player, NumPasses, 1) :- otherPlayer(Player, OtherPlayer), turn(Board, OtherPlayer, NumPasses).

%End turn for Human vs BlackBot, gameMode 2
endTurn(Board, black, NumPasses, 2) :- turn(Board, white, NumPasses).
endTurn(Board, white, NumPasses, 2) :- turnBot(Board, black, NumPasses).

%End turn for WhiteBot vs Human, gameMode 3
endTurn(Board, white, NumPasses, 3) :- turn(Board, black, NumPasses).
endTurn(Board, black, NumPasses, 3) :- turnBot(Board, white, NumPasses).

%End turn for WhiteBot vs BlackBot, gameMode 4
endTurn(Board, Player, NumPasses, 4) :- otherPlayer(Player, OtherPlayer), turnBot(Board, OtherPlayer, NumPasses).

%Simple predicate that returns the other player
otherPlayer(white, black).
otherPlayer(black, white).