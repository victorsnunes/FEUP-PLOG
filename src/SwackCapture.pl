/**
Here we'll use a function to change the contents of a cell
					changeBoard(+Board, +Row, +Column, +NewHeight, +NewColour, -NewBoard)
Arguments:	Board: The board before the changes.
			Row: The row of the cell we want to change
			Column: The column of the cell we want to change
			NewHeight: The new height we want to write on the cell
			NewColour: The new colour we want to write on the cell
			NewBoard: The board after the changes;
*/

%Makes a capture move and returns the new board after the capture
capture(Board, Player, InitialRow, InitialColumn, Height, DestRow, DestColumn, NewNewBoard) :- NewHeight is Height + 1, otherPlayer(Player, OtherPlayer),
																			changeBoard(Board, DestRow, DestColumn, NewHeight, Player, NewBoard),
																			changeBoard(NewBoard, InitialRow, InitialColumn, Height, OtherPlayer, NewNewBoard), !.
		
%Prints informations about the capture move just made
printCapture(Player, InitialRow, InitialColumn, DestRow, DestColumn) :- otherPlayer(Player, OtherPlayer),
																		write(Player), write(' stack in '), write(InitialRow), write('x'), write(InitialColumn),
																		write(' captured a '), write(OtherPlayer), write(' stack in '),
																		write(DestRow), write('x'), write(DestColumn),
																		nl, nl, nl, nl.


/**
Now, we'll also use captureSearchRow and captureSearchColumn functions
				captureSearchRow(+Board, +Row, +Column, +NewHeight, +NewColour, -NewBoard, +BoardUnderConstruction, +CurrentRow).
				captureSearchColumn(+Line, +Column, +NewHeight, +NewColour, -NewLine, +LineUnderConstruction, +CurrentColumn).
Arguments:  Board: The current board of the game
			Line: The line we are current in.
			Row: The row of the stack we're searching for.
			Column: The column of the stack we're searching for.
			NewHeight: The height that we'll write in the cell we want.
			NewColour: The colour (new owner) that we'll write in the cell we want.
			CurrentRow: The current row of the iteration.
			CurrentColumn: The current column of the iteration.
			NewLine: The new line we've just built, with our changed cell.
			LineUnderConstruction: The line we're still building, its used in the iteration process.
			NewBoard: The new board we've just built, with our changed cell.
			BoardUnderConstruction: The board we're still building, its used in the iteration process.
*/


changeBoard(Board, Row, Column, NewHeight, NewColour, NewBoard) :- captureSearchRow(Board, Row, Column, NewHeight, NewColour, NewBoard, [], 1).


%Case when all the lines of the board have been added
captureSearchRow([], _, _, _, _, NewBoard, NewBoard, _).

%The cell that we want to change is in this row, so we're going to look into that line, change the cell, and then add the line to our board under construction
captureSearchRow([Line | RemainingLines], Row, Column, NewHeight, NewColour, NewBoard, BoardUnderConstruction, Row) :-
																	captureSearchColumn(Line, Column, NewHeight, NewColour, NewLine, [], 1),
																	append(BoardUnderConstruction, [NewLine], NewBoardUnderConstruction),
																	NewCurrentRow is Row + 1,
																	captureSearchRow(RemainingLines, Row, Column, NewHeight, NewColour, NewBoard, NewBoardUnderConstruction, NewCurrentRow).

%This isn't the line of the cell we want to change, so we simply add it to our board under construction as it is.
captureSearchRow([Line | RemainingLines], Row, Column, NewHeight, NewColour, NewBoard, BoardUnderConstruction, CurrentRow) :-
																	append(BoardUnderConstruction, [Line], NewBoardUnderConstruction),
																	NewCurrentRow is CurrentRow + 1,
																	captureSearchRow(RemainingLines, Row, Column, NewHeight, NewColour, NewBoard, NewBoardUnderConstruction, NewCurrentRow).

																				
%Case when all the cells of the line have been added to our line under construction
captureSearchColumn([], _, _, _, NewLine, NewLine, _).

%This is the cell that we want to change, so we're going to look create a cell as we wanted and add it to our line under construction, instead of the old cell
captureSearchColumn([ _ | RemainingCells], Column, NewHeight, NewColour, NewLine, LineUnderConstruction, Column) :-
																	append(LineUnderConstruction, [[NewHeight, NewColour]], NewLineUnderConstruction),
																	NewCurrentColumn is Column + 1,
																	captureSearchColumn(RemainingCells, Column, NewHeight, NewColour, NewLine, NewLineUnderConstruction, NewCurrentColumn).
																	
%This isn't the the cell we want to change, so we simply add it to our line under construction as it is.																	
captureSearchColumn([Cell | RemainingCells], Column, NewHeight, NewColour, NewLine, LineUnderConstruction, CurrentColumn) :-
																	append(LineUnderConstruction, [Cell], NewLineUnderConstruction),
																	NewCurrentColumn is CurrentColumn + 1,
																	captureSearchColumn(RemainingCells, Column, NewHeight, NewColour, NewLine, NewLineUnderConstruction, NewCurrentColumn).
																		