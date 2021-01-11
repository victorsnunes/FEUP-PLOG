%In this file, we have a function that prints a given board in the screen

%This function will print a given board: display_game(+GameState, +Player).
display_game(Board, Player) :- length(Board, N),
							   write('      '),
							   firstLine(1, N),		%Writing the first line, which are the indexes
							   display_game(Board, Player, 1). %Prints the board


firstLine(N, N) :- write(N), nl.  %Final vertical index

firstLine(Index, N) :- Index < 10, write(' '), write(Index), write('   '),	%Extra space for index < 10
				       NewIndex is Index + 1, firstLine(NewIndex, N).  %Writing the vertical indexes < 10
					   
firstLine(Index, N) :- Index >= 10, write(Index), write('   '),
				       NewIndex is Index + 1, firstLine(NewIndex, N).  %Writing the vertical indexes >= 10
					   

%display_game(+GameState, +Player, +RowIndex).
display_game([Line|RemainingLines], Player, Index):-
	Index < 10, write(' '), 	%Extra space for index < 10
	write(Index), write('  |'),
	printLine(Line), %Prints the current line
	nl, NewIndex is Index + 1,
	display_game(RemainingLines, Player, NewIndex). %Prints the remaining lines
	
display_game([Line|RemainingLines], Player, Index):-
	Index >= 10, write(Index), write('  |'),
	printLine(Line), %Prints the current line
	nl, NewIndex is Index + 1,
	display_game(RemainingLines, Player, NewIndex). %Prints the remaining lines
	
display_game([], Player, _ ) :- nl,
				write('It is '), write(Player), write('s turn.'), nl, nl. %Prints at the end whose turn it is.


%This function will print a given line: printLine(Line).
printLine([Cell|RemainingCells]):-
	printCell(Cell), %Prints the current cell
	printLine(RemainingCells). %Prints the remaining cells
	
printLine([]). %The final instance, when the line is printed, returns true.
	
%This function will print a given cell: printLine(Cell).	
printCell([Ammount, Colour | _ ]):-
	Ammount < 10,
	character(Colour,Char), %Gives us the char of the colour
	write(' '),		%Extra space for ammount < 10
	write(Ammount),
	write(Char),
	write(' |').
	
printCell([Ammount, Colour | _ ]):-
	Ammount >= 10,
	character(Colour,Char), %Gives us the char of the colour
	write(Ammount),
	write(Char),
	write(' |').
 
%Simple facts that establish the relationship between the atoms of the colour and their respective char symbol
character(white, 'W').
character(black, 'B').
