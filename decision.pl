/**
 * Définition de l'IA du joueur artificiel de Rasende Roboter
 * @author Kevin Maingené & Alexandre Guyon
 */

:- module( decision, [
	init/1,
	move/2
] ).

/** 
 * Target( numero de la cible, x ,y)
 */
target(0,7,5).
target(1,6,1).
target(2,9,10).
target(3,13,5).
target(4,6,13).
target(5,11,2).
target(6,5,4).
target(7,1,10).
target(8,14,13).
target(9,4,9).
target(10,9,1).
target(11,9,14).
target(12,1,3).
target(13,12,9).
target(14,2,14).
target(15,2,5).
target(16,10,7).

/**
 * Wall(X1,Y1,X2,Y2)
 * Means there a wall between thoses two cases
 */
wall(0,3,0,4).
wall(0,6,0,7).
wall(1,2,1,3).
wall(1,3,2,3).
wall(1,9,1,10).
wall(2,5,2,6).
wall(2,5,3,5).
wall(2,14,2,15).
wall(2,14,3,14).
wall(3,0,4,0).
wall(3,9,4,9).
wall(3,15,4,15).
wall(4,9,4,10).
wall(4,4,5,4).
wall(5,3,5,4).
wall(5,1,6,1).
wall(5,13,6,13).
wall(6,1,6,2).
wall(6,12,6,13).
wall(6,7,7,7).
wall(6,8,7,8).
wall(7,5,7,6).
wall(7,5,8,5).
wall(7,6,7,7).
wall(7,8,7,9).
wall(8,6,8,7).
wall(8,7,9,7).
wall(8,8,9,8).
wall(8,8,8,9).
wall(8,10,9,10).
wall(8,14,9,14).
wall(9,0,9,1).
wall(9,1,10,2).
wall(9,7,10,7).
wall(9,10,9,11).
wall(9,13,9,14).
wall(10,0,11,0).
wall(10,6,10,7).
wall(10,15,11,15).
wall(11,2,11,3).
wall(11,2,12,2).
wall(12,5,13,5).
wall(12,8,12,9).
wall(12,9,13,9).
wall(13,5,13,6).
wall(14,13,14,14).
wall(14,13,15,13).
wall(15,3,15,4).
wall(15,9,15,10).

not(A):- A,!,fail.
not(_).

init(_).

/**
 * deplacement(+X1,+Y1,+N,-X2,-Y2)
 * X1,Y1 : old robot's place
 * X2,Y2 : new robot's place
 * Possible N values :
 *   1 -> move right
 *   2 -> move top
 *   3 -> move left
 *   4 -> move Down
 */
 
deplacement(X1,Y1,1,X1,Y1):- X1 = 15,!.
deplacement(X1,Y1,1,X1,Y1):- X3 is X1+1, X3 < 16, wall(X1,Y1,X3,Y1),!.
deplacement(X1,Y1,1,X2,Y2):- X3 is X1+1, X3 < 16, deplacement(X3,Y1,1,X2,Y2).

deplacement(X1,Y1,2,X1,Y1):- Y1 = 0,!.
deplacement(X1,Y1,2,X1,Y1):- Y3 is Y1-1, Y3 >= 0, wall(X1,Y3,X1,Y1),!.
deplacement(X1,Y1,2,X2,Y2):- Y3 is Y1-1, Y3 >= 0, deplacement(X1,Y3,2,X2,Y2).

deplacement(X1,Y1,3,X1,Y1):- X1 = 0,!.
deplacement(X1,Y1,3,X1,Y1):- X3 is X1-1, X3 >= 0, wall(X3,Y1,X1,Y1),!.
deplacement(X1,Y1,3,X2,Y2):- X3 is X1-1, X3 >= 0, deplacement(X3,Y1,3,X2,Y2).

deplacement(X1,Y1,4,X1,Y1):- Y1 = 15,!.
deplacement(X1,Y1,4,X1,Y1):- Y3 is Y1+1, Y3 < 16, wall(X1,Y1,X1,Y3),!.
deplacement(X1,Y1,4,X2,Y2):- Y3 is Y1+1, Y3 < 16, deplacement(X1,Y3,4,X2,Y2).

/**
 * move( +L, -ActionId )
 */

%move( [0,0,0,0, T| R], L):- .

% Examples
move( [0,0,0,0,  1, 4,0 | _], [0,4,0,1,0,4,0,1,0,2,0,3,0,2,0,3] ) :- !.
move( [0,0,0,0,  2, 6,1 | _], [0,1,0,4] ) :- !.
move( [0,0,0,0, 14, _,_, _,_, _,_, 5,15], [3,3,3,2,3,3,3,4] ) :- !.

move( _, [] ) :- !.
%        ^
%        |
%        Action: next configuration
