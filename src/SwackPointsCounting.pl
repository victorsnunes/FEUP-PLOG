:- use_module(library(lists)).

game_over(Board-LastPlayerToPlay, Winner/Points) :- countPoints(Board, ListWhitePoints, ListBlackPoints),
								winner(ListWhitePoints, ListBlackPoints, LastPlayerToPlay, Winner, Points). %Remember ListWhitePoints and ListBlackPoints are already sorted

/**
Predicate winner details:
				winner(+ListWhitePoints, +ListBlackPoints, +LastPlayerToPlay, -Winner, -Points).
Arguments:  ListWhitePoints: Sorted list of the points stack groups of white.
			ListBlackPoints: Sorted list of the points stack groups of black.
			LastPlayerToPlay: This is used in case of a complete draw, so the last player who has played wins.
			Winner: The winner of the game.
			Points: The points of the winner.
*/
												
%Checking which player has the largest group
winner([WhitePoints | RemainingWhitePoints], [BlackPoints | RemainingBlackPoints], LastPlayerToPlay, white, WhitePoints) :- WhitePoints > BlackPoints.
winner([WhitePoints | RemainingWhitePoints], [BlackPoints | RemainingBlackPoints], LastPlayerToPlay, black, BlackPoints) :- WhitePoints < BlackPoints.

%If the points of the current largest group of both players are the same, check the next largest one, adding its points to it.
winner([Points | RemainingWhitePoints], [Points | RemainingBlackPoints], LastPlayerToPlay, Winner, TotalPoints) :- 
									winner(RemainingWhitePoints, RemainingBlackPoints, LastPlayerToPlay, Winner, Points1),
									TotalPoints is Points + Points1.
											
winner([], [], LastPlayerToPlay, LastPlayerToPlay, 0). %When its a complete draw, the last player to play wins the game.


%This predicate is very important. It receives a board and returns descending order lists of the white points and the black points.
countPoints(Board, ListWhitePoints, ListBlackPoints) :- counting(Board, [], UnsortedListWhitePoints, UnsortedListBlackPoints),
														insert_sort(UnsortedListWhitePoints, RevListWhitePoints), insert_sort(UnsortedListBlackPoints, RevListBlackPoints),
														reverse(RevListWhitePoints, ListWhitePoints), reverse(RevListBlackPoints, ListBlackPoints), !.


%Counting the points from the black groups
counting(Board, AlreadyVisited, ListWhitePoints, [BlackPoints | RemainingBlackPoints]) :-
																	 findNotYetCountedCell(Board, AlreadyVisited, Row-Column, black),
																	 countGroup(Board, Row-Column, AlreadyVisited, NewAlreadyVisited1),
																	 remove_duplicates(NewAlreadyVisited1, NewAlreadyVisited),
																	 length(NewAlreadyVisited, A), length(AlreadyVisited, B),
																	 BlackPoints is A - B,
																	 counting(Board, NewAlreadyVisited, ListWhitePoints, RemainingBlackPoints).

%Counting the points from the white groups																	 
counting(Board, AlreadyVisited, [WhitePoints | RemainingWhitePoints], ListBlackPoints) :-
																	 findNotYetCountedCell(Board, AlreadyVisited, Row-Column, white),
																	 countGroup(Board, Row-Column, AlreadyVisited, NewAlreadyVisited1),
																	 remove_duplicates(NewAlreadyVisited1, NewAlreadyVisited),
																	 length(NewAlreadyVisited, A), length(AlreadyVisited, B),
																	 WhitePoints is A - B,
																	 counting(Board, NewAlreadyVisited, RemainingWhitePoints, ListBlackPoints).

%Base case																	 
counting(_, _, [], []).

%Insertion sort algorithm
insert_sort(List,Sorted) :- i_sort(List,[],Sorted).
i_sort([],Acc,Acc).
i_sort([H|T],Acc,Sorted) :- insert(H,Acc,NAcc),i_sort(T,NAcc,Sorted).
   
insert(X,[Y|T],[Y|NT]) :- X>Y,insert(X,T,NT).
insert(X,[Y|T],[X,Y|T]) :- X=<Y.
insert(X,[],[X]).

%Algorithm to remove duplicates in a list
remove_duplicates([H | T], [H | T1]) :- nonmember(H, T), remove_duplicates(T, T1).
remove_duplicates([_| T], T1) :- remove_duplicates(T, T1).
remove_duplicates([], []).

%Searches for a not yet counted cell 
findNotYetCountedCell(Board, AlreadyVisited, Row-Column, Colour) :- findStack(Board, Row, Column, Colour, _ ), nonmember(Row-Column, AlreadyVisited).

%Simple predicate to check if an adjacnet cell exists.
adjacent(Board, Row-Column, NewRow-Column) :- NewRow is Row + 1, length(Board, BoardSize), NewRow =< BoardSize.
adjacent(Board, Row-Column, NewRow-Column) :- NewRow is Row - 1, length(Board, BoardSize), NewRow =< BoardSize.
adjacent(Board, Row-Column, Row-NewColumn) :- NewColumn is Column + 1, length(Board, BoardSize), NewColumn =< BoardSize.
adjacent(Board, Row-Column, Row-NewColumn) :- NewColumn is Column - 1, length(Board, BoardSize), NewColumn =< BoardSize.

%Checks if the adjacent cell is valid, for us to count it in the group
validAdjacent(Board, Row-Column, AlreadyVisited, NewRow-NewColumn) :- adjacent(Board, Row-Column, NewRow-NewColumn), nonmember(NewRow-NewColumn, AlreadyVisited),
																	  findStack(Board, Row, Column, Colour, _ ), findStack(Board, NewRow, NewColumn, Colour, _ ).
															
%This predicate returns a list of all the valid adjacent cells of our current cell															
allAdjacentCells(Board, Row-Column, AlreadyVisited, ListAdjacent) :- findall(NewRow-NewColumn , validAdjacent(Board, Row-Column, AlreadyVisited, NewRow-NewColumn), ListAdjacent).


%This predicate will count the whole group that belongs the cell it received, and add all the visited cells to the list of already visited cells, resulting in the NewAlreadyVisited
%It is very likely that this NewAlreadyVisited may count more than one time the same cell, but thats not a problem because we're going to remove the duplicates later on
countGroup(Board, Row-Column, AlreadyVisited, NewAlreadyVisited) :- allAdjacentCells(Board, Row-Column, AlreadyVisited, ListAdjacent),
									countAdjacentCells(Board, [Row-Column | AlreadyVisited], ListAdjacent, NewAlreadyVisited).

%This is a recursive predicate that will count the current cell and all its valid adjacents cells, and the valid adjacents cells of those cells and so on
countAdjacentCells(Board, AlreadyVisited, [AdjacentCell | RemainingAdjacentCells], NewAlreadyVisited) :- 
				allAdjacentCells(Board, AdjacentCell, AlreadyVisited, ListAdjacent),
				countAdjacentCells(Board, [AdjacentCell | AlreadyVisited], ListAdjacent, NewAlreadyVisited1),
				countAdjacentCells(Board, AlreadyVisited, RemainingAdjacentCells, NewAlreadyVisited2),
				append(NewAlreadyVisited1, NewAlreadyVisited2, NewAlreadyVisited).  %We're appending together the path of the visited cells of the valid adjacents and the previous adjacents of the previous cell, that's why there may be repeated counted cells, but there's no problem with that, because in the end matters here is all cells of the current group being counted 

%When there's no valid adjacent cell, the list of visited cells of that path is returned
countAdjacentCells(Board, AlreadyVisited, [], AlreadyVisited).
							   