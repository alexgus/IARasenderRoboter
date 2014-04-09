/**
 * Définition de l'IA du joueur artificiel de Rasende Roboter
 * @author Kevin Maingené & Alexandre Guyon
 */

:- module( decision, [
	init/1,
	move/2
] ).


/** 
 * Target( numero de la cible, x ,y, couleur)
 * Couleur 0 -> toute couleurs confonduent
 */
target(0,7,5,0).
target(1,6,1,1).
target(2,9,10,1).
target(3,13,5,1).
target(4,6,13,1).
target(5,11,2,2).
target(6,5,4,2).
target(7,1,10,2).
target(8,14,13,2).
target(9,4,9,3).
target(10,9,1,3).
target(11,9,14,3).
target(12,1,3,3).
target(13,12,9,4).
target(14,2,14,4).
target(15,2,5,4).
target(16,10,7,4).

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

%not(A):- A,!,fail.
%not(_).

init(_).

/**
 * deplacement(+X1,+Y1,+N,-X2,-Y2)
 * X1,Y1 : old robot's place
 * X2,Y2 : new robot's place
 * N : the direction (top,right,bottom,left)
 */
 
deplacement(X1,Y1,right,X1,Y1):- X1 = 15,!.
deplacement(X1,Y1,right,X1,Y1):- X3 is X1+1, X3 < 16, robot(X3,Y1),!.
deplacement(X1,Y1,right,X1,Y1):- X3 is X1+1, X3 < 16, wall(X1,Y1,X3,Y1),!.
deplacement(X1,Y1,right,X2,Y2):- X3 is X1+1, X3 < 16, deplacement(X3,Y1,1,X2,Y2).

deplacement(X1,Y1,top,X1,Y1):- Y1 = 0,!.
deplacement(X1,Y1,top,X1,Y1):- Y3 is Y1-1, Y3 >= 0, robot(X1,Y3),!.
deplacement(X1,Y1,top,X1,Y1):- Y3 is Y1-1, Y3 >= 0, wall(X1,Y3,X1,Y1),!.
deplacement(X1,Y1,top,X2,Y2):- Y3 is Y1-1, Y3 >= 0, deplacement(X1,Y3,2,X2,Y2).

deplacement(X1,Y1,left,X1,Y1):- X1 = 0,!.
deplacement(X1,Y1,left,X1,Y1):- X3 is X1-1, X3 >= 0, robot(X3,Y1),!.
deplacement(X1,Y1,left,X1,Y1):- X3 is X1-1, X3 >= 0, wall(X3,Y1,X1,Y1),!.
deplacement(X1,Y1,left,X2,Y2):- X3 is X1-1, X3 >= 0, deplacement(X3,Y1,3,X2,Y2).

deplacement(X1,Y1,bottom,X1,Y1):- Y1 = 15,!.
deplacement(X1,Y1,bottom,X1,Y1):- Y3 is Y1+1, Y3 < 16, robot(X1,Y3),!.
deplacement(X1,Y1,bottom,X1,Y1):- Y3 is Y1+1, Y3 < 16, wall(X1,Y1,X1,Y3),!.
deplacement(X1,Y1,bottom,X2,Y2):- Y3 is Y1+1, Y3 < 16, deplacement(X1,Y3,4,X2,Y2).


/**
 * movableRobot(+IdTarget,-idRobot)
 *  0 : Blue robot
 *  1 : Green robot
 *  2 : Yellow robot
 *  3 : Red robot
 * -1 : Any
 */
movableRobot(T,R):- target(T,_,_,I), R is I-1.

/**
 * assertRobot(N,T,L)
 * assert all the robot in the database without
 * the movable one(s).
 * N : Nb of robots to enter
 * T : id of the target
 * L : List of robot's coordinates in reverse order
 *     (e.g : [X3,Y3,X2,Y2,X1,Y1,X0,Y0])
 */
 
assertRobot(N,_,_) :- N < 0,!.
assertRobot(_,T,_) :- movableRobot(T,R), R = -1,!.
assertRobot(N,T,[X,Y|Q]) :- movableRobot(T,R), R \= N, 
									assert(robot(X,Y)), N1 is N-1,assertRobot(N1,T,Q).
assertRobot(N,T,[_,_|Q]) :- movableRobot(T,R), R = N, 
									N1 is N-1, assertRobot(N1,T,Q).

/**
* recherche( +T, +X, +Y, -L )
* T is the target
* X and Y are the position of the robot
* L is the move to go to the target
*
* ne marche pas, faut trouver autre chose
**/

chercher(T,X,Y,L):- target(T,X,Y,_).
chercher(T,X,Y,[N1,D|Q]):- target(T,_,_,N), N1 is N -1, deplacement(X,Y,D,X2,Y2), chercher(T,X2,Y2,Q).
									
/**
 * move( +L, -ActionId )
 * L is the game configuration
 * _ is the list of action
 *exemple :assertRobot(3,1,[5,15,0,2,1,4,4,0]).
 */
move([0,0,0,0, T, XB,YB, XG,YG, XY,YY, XR,YR], _):- 
	assertRobot(3,T,[XR,YR, XY,YY, XG,YG, XB,YB]), listing(robot).

% Examples
move( [0,0,0,0,  1, 4,0 | _], [0,4,0,1,0,4,0,1,0,2,0,3,0,2,0,3] ) :- !.
move( [0,0,0,0,  2, 6,1 | _], [0,1,0,4] ) :- !.
move( [0,0,0,0, 14, _,_, _,_, _,_, 15,5], [3,3,3,2,3,3,3,4] ) :- !.

move( _, [] ) :- !.
%        ^
%        |
%        Action: next configuration
