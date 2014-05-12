/**
 * Définition de l'IA du joueur artificiel de Rasende Roboter
 * @author Kevin Maingené & Alexandre Guyon
 */

:- module( decision, [
	init/1,
	move/2
] ).

/**
 * A robot
 * 
 * robot(N,X,Y)
 * with :
 * 	N   : Id of the robot
 *    X,Y : Position of the robot
 * 
 *
 * A graph in inserted in the database like this :
 *
 * sommet(N,X,Y,T).
 * With :
 * 	N   : id of sommet
 *		X,Y : the position
 * 	T   : the target if it exists
 *
 *
 * arc(N1,N2).
 * With :
 *
 * 	N1,N2 : id of the sommet
 */
:- dynamic
	robot/3,
	sommet/4,
	arc/2.

/**
 * Init à la main
 * robot(N,X,Y)
 * N : numéro robot
 * X,Y : positon robot
 */
robot(1,1,1).
robot(2,4,15).
robot(3,13,6).
robot(4,13,13).

/***************************************************************
********** Définition de la base de connaissance ***************
***************************************************************/

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
wall(9,1,10,1).
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


/***************************************************************
***************************** Util *****************************
***************************************************************/

%not(A):- A,!,fail.
%not(_).

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
 * Defines the robot allowed to move, depends on target
 * movableRobot(+IdTarget,-idRobot)
 *  0 : Blue robot
 *  1 : Green robot
 *  2 : Yellow robot
 *  3 : Red robot
 * -1 : Any
 */
movableRobot(T,R):- target(T,_,_,I), R is I-1.

/**
 * where(+T,+X,+Y,-DX,-DY)
 * T : the target
 * X,Y : Position of the robot
 * DX : direction (top,bottom,line)
 * DY : direction (right,left,column)
 */
where(T,X,Y,left,top) :- target(T,TX,TY,_),TX < X,TY < Y.
where(T,X,Y,left,bottom) :- target(T,TX,TY,_),TX < X,TY > Y.
where(T,X,Y,left,column) :- target(T,TX,TY,_),TX < X,TY = Y.
where(T,X,Y,right,top) :- target(T,TX,TY,_),TX > X,TY < Y.
where(T,X,Y,right,bottom) :- target(T,TX,TY,_),TX > X,TY > Y.
where(T,X,Y,right,column) :- target(T,TX,TY,_),TX > X,TY = Y.
where(T,X,Y,line,top) :- target(T,TX,TY,_),TX = X,TY < Y.
where(T,X,Y,line,bottom) :- target(T,TX,TY,_),TX = X,TY > Y.

/**
 * Test if there a wall around this case
 * wallAround(+X1,+Y1,?X2,?Y2)
 * X1,Y1 : the position of the case
 * X2,Y2 : the position of the second case
 * 			means that you can't go through this
 */
wallAround(X1,Y1,X2,Y2) :- X2 is X1 + 1, 
					(wall(X1,Y1,X2,Y2);wall(X2,Y2,X1,Y1)).
wallAround(X1,Y1,X2,Y2) :- X2 is X1 - 1, 
					(wall(X1,Y1,X2,Y2);wall(X2,Y2,X1,Y1)).
wallAround(X1,Y1,X2,Y2) :- Y2 is Y1 + 1, 
					(wall(X1,Y1,X2,Y2);wall(X2,Y2,X1,Y1)).
wallAround(X1,Y1,X2,Y2) :- Y2 is Y1 - 1, 
					(wall(X1,Y1,X2,Y2);wall(X2,Y2,X1,Y1)).
/* TODO Beurk ! Fix wall for checking reverse too */
/* TODO Add limits of the map */
/* TODO Add wallAround(X,Y,D) with D app {top,right,bottom,left} */

/**
 * Test if the target is reachable
 * possibleTarget(T)
 * T : the target to test
 */
possibleTarget(T) :- target(T,X,Y,_), wallAround(X,Y,_,_).
/* TODO Add robots */

/***************************************************************
************************* Prédicat *****************************
***************************************************************/

/**
 * deplacement(+X1,+Y1,+N,-X2,-Y2)
 * X1,Y1 : old robot's place
 * X2,Y2 : new robot's place
 * N : the direction (top,right,bottom,left)
 */
deplacement(X1,Y1,right,X1,Y1):- X1 = 15,!.
deplacement(X1,Y1,right,X1,Y1):- X3 is X1+1, X3 < 16, robot(_,X3,Y1),!.
deplacement(X1,Y1,right,X1,Y1):- X3 is X1+1, X3 < 16, wall(X1,Y1,X3,Y1),!.
deplacement(X1,Y1,right,X2,Y2):- X3 is X1+1, X3 < 16, deplacement(X3,Y1,right,X2,Y2).

deplacement(X1,Y1,top,X1,Y1):- Y1 = 0,!.
deplacement(X1,Y1,top,X1,Y1):- Y3 is Y1-1, Y3 >= 0, robot(_,X1,Y3),!.
deplacement(X1,Y1,top,X1,Y1):- Y3 is Y1-1, Y3 >= 0, wall(X1,Y3,X1,Y1),!.
deplacement(X1,Y1,top,X2,Y2):- Y3 is Y1-1, Y3 >= 0, deplacement(X1,Y3,top,X2,Y2).

deplacement(X1,Y1,left,X1,Y1):- X1 = 0,!.
deplacement(X1,Y1,left,X1,Y1):- X3 is X1-1, X3 >= 0, robot(_,X3,Y1),!.
deplacement(X1,Y1,left,X1,Y1):- X3 is X1-1, X3 >= 0, wall(X3,Y1,X1,Y1),!.
deplacement(X1,Y1,left,X2,Y2):- X3 is X1-1, X3 >= 0, deplacement(X3,Y1,left,X2,Y2).

deplacement(X1,Y1,bottom,X1,Y1):- Y1 = 15,!.
deplacement(X1,Y1,bottom,X1,Y1):- Y3 is Y1+1, Y3 < 16, robot(_,X1,Y3),!.
deplacement(X1,Y1,bottom,X1,Y1):- Y3 is Y1+1, Y3 < 16, wall(X1,Y1,X1,Y3),!.
deplacement(X1,Y1,bottom,X2,Y2):- Y3 is Y1+1, Y3 < 16, deplacement(X1,Y3,bottom,X2,Y2).


/**
* recherche( +T, +X, +Y, -L )
* T is the target
* X and Y are the position of the robot
* L is the move to go to the target
*
* ne marche pas, faut trouver autre chose
**/
/*
chercher(T,X,Y,L):- target(T,X,Y,_).
chercher(T,X,Y,[N1,D|Q]):- target(T,_,_,N), N1 is N -1, deplacement(X,Y,D,X2,Y2), chercher(T,X2,Y2,Q).*/

/**
 * Check is there's obstacle on the line
 * until be blocked by other obstacle on
 * these line.
 * checkLine(X,Y,D)
 * X,Y : the position at the begin of the line
 */
checkLine(X0,Y0,top) :- wallAround(X0,Y0,X1,Y0),X0 \= X1,!.
checkLine(X0,Y0,top) :- X0 >= 0, X1 is X0 - 1,
									not((wall(X0,Y0,X1,Y0);wall(X1,Y0,X0,Y0))),
									checkLine(X1,Y0,top).
checkLine(X0,Y0,right) :- wallAround(X0,Y0,X0,Y1),Y0 \= Y1,!.
checkLine(X0,Y0,right) :- X0 =< 16, X1 is X0 + 1,
									not((wall(X0,Y0,X1,Y0);wall(X1,Y0,X0,Y0))),
									checkLine(X1,Y0,right).
checkLine(X0,Y0,bottom) :- wallAround(X0,Y0,X1,Y0),X0 \= X1,!.
checkLine(X0,Y0,bottom) :- Y0 =< 16, X1 is X0 + 1,
									not((wall(X0,Y0,X1,Y0);wall(X1,Y0,X0,Y0))),
									checkLine(X1,Y0,bottom).
checkLine(X0,Y0,left) :- wallAround(X0,Y0,X0,Y1),Y0 \= Y1,!.
checkLine(X0,Y0,left) :- X0 >= 0, X1 is X0 - 1,
									not((wall(X0,Y0,X1,Y0);wall(X1,Y0,X0,Y0))),
									checkLine(X1,Y0,left).

/**
 * Test if the target is reachable
 * possibleTarget(T)
 * T : the target to test
 */
reachableTarget(T) :- target(T,X1,Y1,_),wallAround(X1,Y1,X2,Y1),
								X1 < X2,!,checkLine(X1,Y1,left),!.
reachableTarget(T) :- target(T,X1,Y1,_),wallAround(X1,Y1,X2,Y1),
								X1 > X2,!,checkLine(X1,Y1,right),!.
reachableTarget(T) :- target(T,X1,Y1,_),wallAround(X1,Y1,X1,Y2),
								Y1 < Y2,!,checkLine(X1,Y1,top),!.
reachableTarget(T) :- target(T,X1,Y1,_),wallAround(X1,Y1,X1,Y2),
								Y1 > Y2,!,checkLine(X1,Y1,bottom),!.

/**
 * Check is there's an obstacle on the line
 * until be blocked by other obstacle on
 * these line.
 * way(X,Y,D,L)
 * X,Y : the position at the begin of the line
 * L : List of tuple of possible way
 */
way(_,-1,top,[]).
way(X0,Y0,top,[(X0,Y0)|Q]) :- wallAround(X0,Y0,X1,Y0),X0 \= X1,!,
									YN is Y0 - 1, way(X0,YN,top,Q).
way(X0,Y0,top,L) :- X0 >= 0, X1 is X0 - 1,
									not((wall(X0,Y0,X1,Y0);wall(X1,Y0,X0,Y0))),
									way(X1,Y0,top,L).
way(17,_,right,[]).
way(X0,Y0,right,[(X0,Y0)|Q]) :- wallAround(X0,Y0,X0,Y1),Y0 \= Y1,!,
									XN is X0 + 1, way(XN,Y0,right,Q).
way(X0,Y0,right,L) :- X0 =< 16, X1 is X0 + 1,
									not((wall(X0,Y0,X1,Y0);wall(X1,Y0,X0,Y0))),
									way(X1,Y0,right,L).
way(_,17,bottom,[]).
way(X0,Y0,bottom,[(X0,Y0)|Q]) :- wallAround(X0,Y0,X1,Y0),X0 \= X1,!,
									YN is Y0 + 1, way(X0,YN,bottom,Q).
way(X0,Y0,bottom,L) :- Y0 =< 16, X1 is X0 + 1,
									not((wall(X0,Y0,X1,Y0);wall(X1,Y0,X0,Y0))),
									way(X1,Y0,bottom,L).
way(-1,_,left,[]).
way(X0,Y0,left,[(X0,Y0)|Q]) :- wallAround(X0,Y0,X0,Y1),Y0 \= Y1,!,
									XN is X0 - 1, way(XN,Y0,left,Q).
way(X0,Y0,left,L) :- X0 >= 0, X1 is X0 - 1,!,
									not((wall(X0,Y0,X1,Y0);wall(X1,Y0,X0,Y0))),
									way(X1,Y0,left,L).

/**
 * Get the best way, based on heuristique
 * heuristique(+T,+L,-B)
 * T : the target
 * L : List of possible way
 * B : Best way based on heuristique (couple X,Y)
 */
heuristique(_,_,_,[],_).
heuristique(T,[(XW,YW)|Q],(XB,YB)) :- target(T,XT,YT,_), 
	(XT - XB) > (XT - XW), (YT - YB) > (YT - YW),!,
	heuristique(T,Q,(XW,YW)).
heuristique(T,[(XW,YW)|Q],(XB,YB)) :- target(T,XT,YT,_),
	(XT - XB) < (XT - XW), (YT - YB) < (YT - YW),!,
	heuristique(T,Q,(XB,YB)).


/***************************************************************
********************** Make graphe  ****************************
***************************************************************/

/**
 * Count the number of wall N on a position X,Y and a direction D
 * wallDir(+X,+Y,+D,?N)
 * X,Y : Coordinates of the case to test
 * N : The number of wall
 * D : top,right,bottom,left
 */
wallDir(X,Y,top,N) :- Y1 is Y-1,
								(wall(X,Y1,X,Y);wall(X,Y,X,Y1)),!, N is 1.
wallDir(_,_,top,N) :- N is 0.
wallDir(X,Y,right,N) :- X1 is X+1, 
								(wall(X1,Y,X,Y);wall(X,Y,X1,Y)),!, N is 1.
wallDir(_,_,right,N) :- N is 0.
wallDir(X,Y,bottom,N) :- Y1 is Y+1,
								(wall(X,Y,X,Y1);wall(X,Y1,X,Y)),!, N is 1.
wallDir(_,_,bottom,N) :- N is 0.
wallDir(X,Y,left,N) :- X1 is X-1, 
								(wall(X1,Y,X,Y);wall(X,Y,X1,Y)),!, N is 1.
wallDir(_,_,left,N) :- N is 0.

/**
 * Count the number of wall N on a position X,Y
 * countWall(+X,+Y,?N).
 * X,Y : Coordinates of the case to test
 * N : The number of wall
 */
countWall(X,Y,N) :- wallDir(X,Y,top,N1), wallDir(X,Y,right,N2), 
							wallDir(X,Y,bottom,N3), wallDir(X,Y,left,N4),
							N is (N1+N2+N3+N4).

/**
 * Give the wall around
 * lWallDir2(+X,+Y,+T,?S)
 * 	X,Y : Position to check
 * 	T	 : Tested direction
 * 	S	 : List of wall direction (top,right,bottom,left)
 */
lWallDir2(X,Y,T,[top|Q]) :- wallDir(X,Y,top,N), N = 1, 
								not(member(top, T)),!,
								lWallDir2(X,Y,[top|T],Q).
lWallDir2(X,Y,T,Q) :- not(member(top, T)),!,lWallDir2(X,Y,[top|T],Q).
lWallDir2(X,Y,T,[right|Q]) :- wallDir(X,Y,right,N), N = 1, 
								not(member(right, T)),!,
								lWallDir2(X,Y,[right|T],Q).
lWallDir2(X,Y,T,Q) :- not(member(right, T)),!,lWallDir2(X,Y,[right|T],Q).
lWallDir2(X,Y,T,[bottom|Q]) :- wallDir(X,Y,bottom,N), N = 1, 
								not(member(bottom, T)),!,
								lWallDir2(X,Y,[bottom|T],Q).
lWallDir2(X,Y,T,Q) :- not(member(bottom, T)),!,lWallDir2(X,Y,[bottom|T],Q).
lWallDir2(X,Y,T,[left|Q]) :- wallDir(X,Y,left,N), N = 1, 
								not(member(left, T)),!,
								lWallDir2(X,Y,[left|T],Q).
lWallDir2(X,Y,T,Q) :- not(member(left, T)),!,lWallDir2(X,Y,[left|T],Q).
lWallDir2(_,_,_,[]).

/**
 * Give the wall around
 * lWallDir(+X,+Y,?S)
 * 	X,Y : Position to check
 * 	S	 : List of wall direction (top,right,bottom,left)
 */
lWallDir(X,Y,S) :- lWallDir2(X,Y,[],S).

/**
 * Give the other direction not listed
 * dirBar(?L1,?L2)
 * 	L1,L2 : two lists of directions
 */
dirBar([],[left,bottom,right,top]) :- !.
dirBar([top|Q1],L2) :- !, dirBar(Q1,LT,right), delete(LT,top,L2).
dirBar(L1,L2) :- dirBar(L1,L2,right).
dirBar([right|Q1],L2,right) :- !, dirBar(Q1,LT,bottom), delete(LT,right,L2).
dirBar(L1,L2,right) :- dirBar(L1,L2,bottom).
dirBar([bottom|Q1],L2,bottom) :- !, dirBar(Q1,LT,left), delete(LT,bottom,L2).
dirBar(L1,L2,bottom) :- dirBar(L1,L2,left).
dirBar([left|Q1],L2,left) :- !, dirBar(Q1,LT), delete(LT,left,L2).
dirBar(L1,L2,left) :- dirBar(L1,L2).

/**
 * Give the next possible directions
 * lDir(X,Y,L)
 * 	X,Y : Position to check
 * 	L	 : Possible directions to go
 */
lDir(X,Y,L) :- lWallDir(X,Y,T),dirBar(T,L).

/**
 * nextSommetID(-N)
 * Give the next sommet's number unused
 */
nextSomID(I,F) :- sommet(I1,_,_,_), I1 > I, !, nextSomID(I1,F).
nextSomID(I,F) :- F is I.
nextSommetID(N) :- nextSomID(0,M), N is M +1.

/**
 * Insert the a sommet with X,Y coordinates
 * insertSommet(+X,+Y)
 */
insertSommet(X,Y) :- not(sommet(_,X,Y,_)), nextSommetID(NS), 
						target(NT,X,Y,_), assert(sommet(NS,X,Y,NT)).
insertSommet(X,Y) :- not(sommet(_,X,Y,_)), nextSommetID(NS), 
						assert(sommet(NS,X,Y,-1)).

/**
 * Insert the an arc with S1,S2(,S3) coordinates
 * insertSommet(+S1,+S2[,+S3])
 */
insertArc(S1,S2) :- S1 \= S2, not(arc(S1,S2)), assert(arc(S1,S2)).

/**
 * Get the arc of a sommet
 * getArc2(+S,+E,-L)
 * 	S : The sommet to check
 * 	E : List of arc (entry)
 * 	L : List of arc
 */
getArc2(S,E,[arc(S,S1)|Q]) :- (arc(S,S1);arc(S1,S)), 
											not(member(arc(S,S1),E)),!,
											getArc2(S,[arc(S,S1)|E],Q).
getArc2(_,_,[]).

/**
 * Get the arc of a sommet
 * getArc(+S,-L)
 * 	S : The sommet to check
 * 	L : List of arc(S1,S2,S3)
 */
getArc(S,L) :- getArc2(S,[],L).

/**
 * Make the graphe
 * mkG(+X,+Y,+L)
 * 	X,Y : current position to handle
 * 	L 	 : List of next directions to handle
 */
mkG(X,Y,[]) :- not(sommet(_,X,Y,_)),!,
				insertSommet(X,Y),
				lDir(X,Y,LDIR),
				mkG(X,Y,LDIR).
mkG(_,_,[]).
mkG(X,Y,[T|Q]) :- ((T \= top,Y >= 0) ; (T \= bottom,Y =< 15) ;
							(T \= right, X >= 0) ; (T \= left, X =< 15)),
						deplacement(X,Y,T,XN,YN),mkG(XN,YN,[]),mkG(X,Y,Q),!,
						sommet(S1,X,Y,_),sommet(S2,XN,YN,_),insertArc(S1,S2).

makeGraphe :- target(1,X,Y,_), mkG(X,Y,[]).

/***************************************************************
******************* Graphe  Manipulation  **********************
***************************************************************/

/**
 * Translate way between two sommet in direction
 * tSom(+S1,+S2,?D)
 * 	S1 : Begining sommet
 * 	S2 : Ending sommet
 * 	D  : Corresponding direction
 */
tSom(S1,S2,top) :- sommet(S1,X1,Y1,_), sommet(S2,X1,Y2,_), Y2 < Y1.
tSom(S1,S2,right) :- sommet(S1,X1,Y1,_), sommet(S2,X2,Y1,_), X1 < X2.
tSom(S1,S2,bottom) :- sommet(S1,X1,Y1,_), sommet(S2,X1,Y2,_), Y2 > Y1.
tSom(S1,S2,left) :- sommet(S1,X1,Y1,_), sommet(S2,X2,Y1,_), X1 > X2.


/**
 * Translate List of sommet by list of direction
 * tListSom(+LS,+LD)
 * 	LS : List of sommet
 * 	LD : List of directions
 */
tListSom([],[]).
tListSom([T1a,T1b|Q1],[T2|Q2]) :- tSom(T1a,T1b,T2), tListSom(Q1,Q2).


/**
 * Shortest way between two sommet
 * shWay(+S1,+S2,?L)
 * 	S1 : Begin point
 * 	S2 : End point
 * 	L 	: List of sommet begining by S1 and ending by S2
 */
shWay(_,_,_) :- !.

/* TODO function with cost notion */
sWay(S1,S2,LI,[S1,S2],1) :- arc(S1,S2), 
									not(member(S2,LI)).
/*sWay(S1,S2,LI,[S2,S1],1) :- arc(S2,S1),
									not(member(S2,LI)).*/
sWay(S1,S2,LI,[LT1,LT2],C) :-
								sWay(S1,ST,LI,LT1,C1), 
								sWay(ST,S2,[S1|LI],LT2,C2), 
								C is (C1 + C2).
sWay(S,S,_,_,_) :- false.

/***************************************************************
********************** Appel externe ***************************
***************************************************************/

/**
 * Initialize the begin of the game
 */
init(_) :- makeGraphe.						
									
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
