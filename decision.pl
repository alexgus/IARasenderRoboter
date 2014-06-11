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
%robot(2,4,15).
%robot(3,13,6).
%robot(4,13,13).

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
wall(1,10,2,10).
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

member(_,[]):- fail.
member(X,[X|_]). 
member(X,[_|Q]):- member(X,Q).


/**
 * assertRobot(N,L)
 * assert all the robot in the database without
 * the movable one(s).
 * N : Nb of robots to enter
 * L : List of robot's coordinates in reverse order
 *     (e.g : [X3,Y3,X2,Y2,X1,Y1,X0,Y0])
 */ 
assertRobot(_,[]).
assertRobot(N,[X,Y|Q]):- assert(robot(N,X,Y)),N1 is N-1, assertRobot(N1,Q).


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
								(wall(X,Y1,X,Y);wall(X,Y,X,Y1);Y1<0),!, N is 1.
wallDir(_,_,top,N) :- N is 0.
wallDir(X,Y,right,N) :- X1 is X+1,
								(wall(X1,Y,X,Y);wall(X,Y,X1,Y);X1>15),!, N is 1.
wallDir(_,_,right,N) :- N is 0.
wallDir(X,Y,bottom,N) :- Y1 is Y+1,
								(wall(X,Y,X,Y1);wall(X,Y1,X,Y);Y1>15),!, N is 1.
wallDir(_,_,bottom,N) :- N is 0.
wallDir(X,Y,left,N) :- X1 is X-1,
								(wall(X1,Y,X,Y);wall(X,Y,X1,Y);X1<0),!, N is 1.
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

/*
* Insert the an arc with S1,S2(,S3) coordinates
* insertSommet(+S1,+S2[,+S3])
*/
insertArc(S,S).
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
mkG(X,Y,[]) :- sommet(_,X,Y,_).				
mkG(X,Y,[T|Q]) :- deplacement(X,Y,T,XN,YN),mkG(XN,YN,[]),mkG(X,Y,Q),!,
						sommet(S1,X,Y,_),sommet(S2,XN,YN,_),insertArc(S1,S2).

						
makeGraphe :- target(1,X,Y,_), mkG(X,Y,[]).
makeGraphe(R) :- robot(R,X,Y), mkG(X,Y,[]).


/***************************************************************
******************* Graphe  Manipulation  **********************
***************************************************************/

/*
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
shWay(S1,S2,L) :- sWay2(S1,S2,[],T),trWay(T,D),flatten(D,L1), racc(L1,L), length(L,C),writeln('cout'+C),writeln(L).

sWay(S1,S2,LI,[S1,S2],1) :- arc(S1,S2),
									not(member(S2,LI)).
sWay(S1,S2,LI,[LT1,LT2],C) :-
								sWay(S1,ST,LI,LT1,C1),
								sWay(ST,S2,[S1|LI],LT2,C2), 
								C is (C1 + C2).
sWay(S,S,_,_,_) :- false.


sWay2(S1,S2,LI,[S1,S2]) :- arc(S1,S2),
									not(member(S2,LI)).
sWay2(S1,S2,LI,[ST|LT]) :-
								heuristique(S1,ST,S2,LI),
								sWay2(ST,S2,[ST|LI],LT).
sWay2(S,S,_,_,_) :- false.


heuristique(S1,S2,S3,L) :- arc(S1,S2), sommet(S1,_,_,_), 
								sommet(S2,X2,Y2,_), sommet(S3,X3,Y3,_), 
								not(member(S2,L)),(X2 = X3 ; Y2 = Y3).
heuristique(S1,S2,S3,L) :- arc(S1,S2), sommet(S1,X1,Y1,_), 
								sommet(S2,X2,Y2,_), sommet(S3,X3,Y3,_), 
								not(member(S2,L)), ((X1 - X3) > (X2 - X3) ; 
														(Y1 - Y3) > (Y2 - Y3)).
heuristique(S1,S2,_,L) :- arc(S1,S2),not(member(S2,L)).

/*
* dirArc(+A,?D)
* A : une liste de 2 sommet forment un arc
* D : la direction de l'arc pour du sommet 1 atteindre le sommet 2
*/
dirArc([S1,S2],2):- arc(S1,S2),sommet(S1,X,Y1,_), sommet(S2,X,Y2,_), Y1 > Y2.
dirArc([S1,S2],4):- arc(S1,S2),sommet(S1,X,Y1,_), sommet(S2,X,Y2,_), Y1 < Y2.
dirArc([S1,S2],3):- arc(S1,S2),sommet(S1,X1,Y,_), sommet(S2,X2,Y,_), X1 > X2.
dirArc([S1,S2],1):- arc(S1,S2),sommet(S1,X1,Y,_), sommet(S2,X2,Y,_), X1 < X2.

/*
* trWay(+L,?D)
* L : liste imbriquer d'arc
* D : liste de direction traduisant L
*/
trWay([],[]).
trWay([S1,S2|Q],[T|D]):- dirArc([S1,S2],T),trWay(Q,D).
trWay([L|Q],[T|D]):- trWay(L,T),trWay(Q,D).

/*
* racc(+L1,?L2)
* L1 : liste des directions
* L2 : liste simplifié des directions
*/
racc([],[]).
racc([2,4|Q],[4|L]):- racc(Q,L).
racc([4,2|Q],[2|L]):- racc(Q,L).
racc([1,3|Q],[3|L]):- racc(Q,L).
racc([3,1|Q],[1|L]):- racc(Q,L).
racc([T|Q],[T|L]):- racc(Q,L).

cmpS :- cmpS(1).
cmpS(C):- not(sommet(C,_,_,_)),!,C1 is C-1,assert(size(C1)).
cmpS(C):- sommet(C,_,_,_), C1 is C +1, cmpS(C1).

supA :- size(N), N1 is N+1, supA(N1).
supA(N) :- retract(arc(N,_)), supA(N).
supA(N) :- retract(arc(_,N)), supA(N).
supA(N) :- N1 is N+1, sommet(N1,_,_,_), supA(N1).
supA(_).

supS :- size(N), N1 is N+1, supS(N1).
supS(N) :- retract(sommet(N,_,_,_)), N1 is N+1, supS(N1).
supS(_).

resteGraphe:- supA,supS.

moveRobot([],_,[]).
moveRobot([T|Q],R,[R,T|Q1]):- moveRobot(Q,R,Q1). 
/***************************************************************
********************** Appel externe ***************************
***************************************************************/

/**
 * Initialize the begin of the game
 */
init(_) :- makeGraphe, cmpS, writeln('initialisation OK').						
									
/**
 * move( +L, -ActionId )
 * L is the game configuration
 * _ is the list of action
 *exemple :assertRobot(3,1,[5,15,0,2,1,4,4,0]).
 */
move([0,0,0,0, T, XB,YB, XG,YG, XY,YY, XR,YR], L):- writeln('recherche cible'+T),
	assertRobot(3,[XR,YR, XY,YY, XG,YG, XB,YB]), target(T,_,_,R1), R is R1-1, makeGraphe(R), robot(R,X1,Y1),sommet(SR,X1,Y1,_), 
	sommet(ST,_,_,T),writeln('start rechercher'), shWay(SR,ST,L1),!, writeln(L1), moveRobot(L1,R,L), writeln(L), resteGraphe.

% Examples
%move( [0,0,0,0,  1, 4,0 | _], [0,4,0,1,0,4,0,1,0,2,0,3,0,2,0,3] ) :- !.
%move( [0,0,0,0,  2, 6,1 | _], [0,1,0,4] ) :- !.

%move( [0,0,0,0, 14, _,_, _,_, _,_, 15,5], [3,3,3,2,3,3,3,4] ) :- !.
%move( _, [] ) :- !.
%        ^
%        |
%        Action: next configuration
