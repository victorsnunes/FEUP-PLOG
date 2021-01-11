%This is the main file, that includes all the other files and simply starts the game.
%Once started, the game will go on until the end.

:- consult('SwackBoardGenerator.pl'). %File that generates the initial board
:- consult('SwackMovements.pl'). %This file is the core of the game
:- consult('SwackPrintBoard.pl'). %File that contains functions for printing the board
:- consult('SwackValidation.pl'). %File that contains functions for checking the validation of a move or choice
:- consult('SwackCapture.pl'). 	  %File that contains functions for capturing a stack
:- consult('SwackPointsCounting.pl'). %File that contains functions for counting the points


dynamic(gameMode/1).	%Stores the mode of the game (1: HxH, 2: HxC, 3: CxH, 4: CxC)
dynamic(blackBotIntelligence/1).   %Stores the intelligence of the black bot (1: random bot, 2: smart bot)
dynamic(whiteBotIntelligence/1).   %Stores the intelligence of the white bot (1: random bot, 2: smart bot)


%Starts the game
play :- write('Welcome to the Swack game! Lets have some fun'), nl , nl, chooseMode.

%Chooses the mode of the game
chooseMode :-
	    write('Select the mode which you want to play:'), nl,
		write('Write 1 for Human vs Human'), nl,
		write('Write 2 for Human vs Computer'), nl,
		write('Write 3 for Computer vs Human'), nl,
		write('Write 4 for Computer vs Computer'), nl,
		read(Mode), nl, nl, startMode(Mode).
	
	
startMode(1) :- !, initial(Board), nl, write('Human vs Human'), asserta(gameMode(1)), nl, write('And game begins!'), nl, nl, nl, turn(Board, white, 0).
startMode(2) :- !, initial(Board), nl, nl, 
				write('Human vs BlackBot'), nl, nl, write('Select the intelligence of the bot:'), nl,
				write('Write 1 for a random bot'), nl,
				write('Write 2 for a smart bot'), nl,
				read(Intelligence), startBotGameMode(2, Intelligence, Board).
startMode(3) :- !, initial(Board), nl, nl, 
				write('WhiteBot vs Human'), nl, nl, write('Select the intelligence of the bot:'), nl,
				write('Write 1 for a random bot'), nl,
				write('Write 2 for a smart bot'), nl,
				read(Intelligence), startBotGameMode(3, Intelligence, Board).
startMode(4) :- !, initial(Board), nl, nl, 
				write('WhiteBot vs BlackBot'), nl, nl,
				
				write('Select the intelligence of the WhiteBot:'), nl,
				write('Write 1 for a random bot'), nl,
				write('Write 2 for a smart bot'), nl,
				read(WhiteBotIntelligence), nl, 
				
				write('Select the intelligence of the BlackBot:'), nl,
				write('Write 1 for a random bot'), nl,
				write('Write 2 for a smart bot'), nl,
				read(BlackBotIntelligence), nl,
				
				startBotGameMode(4, WhiteBotIntelligence, BlackBotIntelligence, Board).

%Catches invalid answers for the game mode				
startMode(_) :- write('Invalid answer! Try again'), nl, nl, chooseMode.


startBotGameMode(2, Intelligence, Board) :- Intelligence >= 1, Intelligence =< 2, asserta(gameMode(2)), asserta(blackBotIntelligence(Intelligence)),
											nl, write('And game begins!'), nl, nl, nl, turn(Board, white, 0).										
startBotGameMode(3, Intelligence, Board) :- Intelligence >= 1, Intelligence =< 2, asserta(gameMode(3)), asserta(whiteBotIntelligence(Intelligence)),
											nl, write('And game begins!'), nl, nl, nl, turnBot(Board, white, 0).
startBotGameMode(4, WhiteBotIntelligence, BlackBotIntelligence, Board) :- WhiteBotIntelligence >= 1, WhiteBotIntelligence =< 2,
																		  BlackBotIntelligence >= 1, BlackBotIntelligence =< 2,
																		  asserta(gameMode(4)), asserta(blackBotIntelligence(BlackBotIntelligence)),
																		  asserta(whiteBotIntelligence(WhiteBotIntelligence)),
																		  nl, write('And game begins!'), nl, nl, nl,
																		  turnBot(Board, white, 0).

%Catches invalid answers for the intelligence of the bot															   
startBotGameMode(Mode, _, _) :- write('Invalid answer! Try again'), nl, nl, startMode(Mode).
startBotGameMode(Mode, _, _, _) :- write('Invalid answer! Try again'), nl, nl, startMode(Mode).

%Ends the game
end(Winner, Points) :- write('We are pleased to announce that '), write(Winner), write(' is the winner! Having won with '), write(Points), write(' points!').