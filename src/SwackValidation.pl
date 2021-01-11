/**
We're using an auxiliar function, to search for a stack in our board and return its colour and height:
				findStack(+Board, +Row, +Column, -Owner, -Height).
Arguments:  Board: The current board of the game
			Row: The row of the stack we're searching for.
			Column: The column of the stack we're searching for.
			Owner: The owner of the stack we've found.
			Height: The height of the stack we've found.

*/

%Checking the validation of the piece the player wants to move
validateChosenPiece(Board, PlayerColour, InitialRow, InitialColumn, Height) :- findStack(Board, InitialRow, InitialColumn, PlayerColour, Height).
								

								
%For selecting the stack to capture, we have 2 scenarios:
																	
%Case 1: A player is trying to capture an enemy stack in the same row.															
validateStackToCapture(Board, Player, Row, InitialColumn, Height, Row, DestColumn) :- ((DestColumn is InitialColumn + 1);(DestColumn is InitialColumn - 1)),
																	otherPlayer(Player, OtherPlayer),
																	findStack(Board, Row, DestColumn, OtherPlayer, Height).
																	
																	
%Case 2: A player is trying to capture an enemy stack in the same column.															
validateStackToCapture(Board, Player, InitialRow, Column, Height, DestRow, Column) :- ((DestRow is InitialRow + 1);(DestRow is InitialRow - 1)),
																	otherPlayer(Player, OtherPlayer),
																	findStack(Board, DestRow, Column, OtherPlayer, Height).



/**
Now, we'll also use searchRow and searchColumn functions
				searchRow(+Board, +Row, +Column, -Owner, -Height, +CurrentRow).
				searchColumn(+Line, +Column, -Owner, -Height, +CurrentColumn).
Arguments:  Board: The current board of the game
			Line: The line we are current in.
			Row: The row of the stack we're searching for.
			Column: The column of the stack we're searching for.
			Owner: The owner of the stack we've found.
			Height: The height of the stack we've found.
			CurrentRow: The current row of the iteration.
			CurrentColumn: The current column of the iteration.
*/

findStack(Board, Row, Column, Owner, Height) :- searchRow(Board, Row, Column, Owner, Height, 1).

searchRow([Line | _ ], Row, Column, Owner, Height, Row) :- searchColumn(Line, Column, Owner, Height, 1).
searchRow([ _ | RemainingLines], Row, Column, Owner, Height, CurrentRow) :- NewCurrentRow is CurrentRow + 1,
																				searchRow(RemainingLines, Row, Column, Owner, Height, NewCurrentRow).

searchColumn([Cell | _ ], Column, Owner, Height, Column) :- seeCellContent(Cell, Owner, Height).
searchColumn([ _ | RemainingCells], Column, Owner, Height, CurrentColumn) :- NewCurrentColumn is CurrentColumn + 1,
																					searchColumn(RemainingCells, Column, Owner, Height, NewCurrentColumn).

seeCellContent([Height, Owner | _ ], Owner, Height).