
:- dynamic( ensVers/2 ).
:- dynamic( compose/2 ).
:- dynamic( parole/1 ).

% Charles Baudelaire (1821-1867)

ensVers( a, [ vers( [ syllabe( "t", "a", "" ) ] ),
 vers( [ syllabe( "s", "a", "" ),
 syllabe( "r", "a", "") ] ) ] ).
ensVers( b, [ vers( [ syllabe( "t", "e", "" ) ] ),
 vers( [ syllabe( "s", "e", "" ) ] ) ] ).

compose( q, binaire( 2, ens( a ), 2, ens( b ) ) ).
compose( t, binaire( 1, ens( c ), 2, ens( d ) ) ).
compose( u, binaire( 1, ens( c ), 2, ens( e ) ) ).
compose( m, strophique( 2, seq( q ) ) ).
compose( n, prosique( [ seq( t ), seq( u ) ] ) ).
compose( p, prosique( [ seq( m ), seq( n ) ] ) ).

parole( seq( p ) ).

% style( heureux, 57, 16, 0.25 ).



%%%%%%------------------------------------------------%

pr(prosique([]),[]).

pr(prosique([H|T]),R) :- 
    H\=ens(_) , H\=seq(_) , pr(prosique(T),R2),flatten(R2,R).

pr(prosique([seq(A)|T]),[G|R]) :-
seq(A,G),
pr(prosique(T),R2),flatten(R2,R).

pr(prosique([ens(A)|T]),[ens(A)|R]) :-
pr(prosique(T),R2),flatten(R2,R).

%-----------------------------------------------%

repeatNtimes(0,_,[]).
repeatNtimes(N,E,[E|R]):-
    NewN is N-1,
    repeatNtimes(NewN,E,R),!.


st(strophique(N,E),R) :- E=seq(H),seq(H,F),repeatNtimes(N,F,R2),flatten(R2,R).
st(strophique(N,E),R) :- E=ens(_),repeatNtimes(N,E,R2),flatten(R2,R). 


%-----------------------------------------------%

bn(binaire(N1,E1,N2,E2),R) :- 
E1=ens(_) , E2=ens(_) , repeatNtimes(N1,E1,R1),
repeatNtimes(N2,E2,R2) , append(R1,R2,R),!.

bn(binaire(N1,E1,N2,E2),R) :- 
E1=ens(_) , E2=seq(SQ),seq(SQ,VSQ) , repeatNtimes(N1,E1,R1),
repeatNtimes(N2,VSQ,R2) , append(R1,R2,R),!.

bn(binaire(N1,E1,N2,E2),R) :- 
E1=seq(SQ) , E2=ens(_),seq(SQ,VSQ) , repeatNtimes(N1,VSQ,R1),
repeatNtimes(N2,E2,R2) , append(R1,R2,R),!.

bn(binaire(N1,E1,N2,E2),R) :- 
E1=seq(SQ1) , E2=seq(SQ2),seq(SQ1,VSQ1) , seq(SQ2,VSQ2),
repeatNtimes(N1,VSQ1,R1),repeatNtimes(N2,VSQ2,R2),
append(R1,R2,R),!.



%-----------------------------------------------%

tr(ternaire(N,E1,E2),R) :- 
E1=ens(_) , E2=ens(_) , repeatNtimes(N,E1,R1),
append(R1,[E2],R2),flatten(R2,R),!.

tr(ternaire(N,E1,E2),R) :- 
E1=seq(SQ1) , seq(SQ1,VSQ1), E2=ens(_) , repeatNtimes(N,VSQ1,R1),
append(R1,[E2],R2),flatten(R2,R),!.

tr(ternaire(N,E1,E2),R) :- 
E1=seq(SQ1) , seq(SQ1,VSQ1), E2=seq(SQ2) , seq(SQ2,VSQ2) , repeatNtimes(N,VSQ1,R1),
append(R1,VSQ2,R2),flatten(R2,R),!.

tr(ternaire(N,E1,E2),R) :- 
E1=ens(_) , E2=seq(SQ2) , seq(SQ2,VSQ2) , repeatNtimes(N,E1,R1),
append(R1,VSQ2,R2),flatten(R2,R),!.



%-----------------------------------------------%

placeBeforeEach(_,[],[]) .      
placeBeforeEach( V , [X|Xs] , [V,X|Ys] ) :- placeBeforeEach(V,Xs,Ys).


rond(rondo(E1,E2),R) :- E1=seq(SQ1) , E2=seq(SQ2) , seq(SQ1,VSQ1) , seq(SQ2,VSQ2),
placeBeforeEach(VSQ1,VSQ2,R2),flatten(R2,R).

rond(rondo(E1,E2),R) :- E1=ens(_) , E2=seq(SQ2) , seq(SQ2,VSQ2),
placeBeforeEach(E1,VSQ2,R2),flatten(R2,R).

rond(rondo(E1,E2),R) :- E1=seq(SQ1) , E2=ens(_) , seq(SQ1,VSQ1),
placeBeforeEach(VSQ1,[E2],R2),flatten(R2,R).

rond(rondo(E1,E2),R) :- E1=ens(_) , E2=ens(_),
placeBeforeEach(E1,[E2],R2),flatten(R2,R).


%-----------------------------------------------%



seq(X,R) :-
compose(X,prosique(G)),
pr(prosique(G),R2),flatten(R2,R),!.

seq(X,R) :-
compose(X,strophique(N,G)),
st(strophique(N,G),R2),flatten(R2,R),!.

seq(X,R) :-
compose(X,binaire(N1,E1,N2,E2)),
bn(binaire(N1,E1,N2,E2),R2),flatten(R2,R),!.
    
seq(X,R) :-
compose(X,ternaire(N,E1,E2)),
tr(ternaire(N,E1,E2),R2)
,flatten(R2,R),!.

seq(X,R) :-
compose(X,rondo(E1,E2)),
rond(rondo(E1,E2),R2),
flatten(R2,R),!.


%-----------------------------------------------%




% (stl(syllabe to list)) prend une liste de syllabes et les mets tous dans une liste.
stl([],[]).
stl([syllabe(A,B,C)|T],[A,B,C|R]) :-
    stl(T,R).


% prend([ de vers ] et retourne [ de syllabes ]).

fromVerstoList([],[]).
fromVerstoList([vers([syllabe(A,B,C)|[]])|T],[[A,B,C]|R]) :-
fromVerstoList(T,R).


fromVerstoList([vers([syllabe(A,B,C)|Sx])|T],[[A,B,C|SS]|R]) :-
stl(Sx,SS),
fromVerstoList(T,R).


ensVersResultat(A,R) :- ensVers(A,T) , fromVerstoList(T,R) ,  !.

    
%-----------------------------------------------%

getN(Count, [_|Tail], R) :- 
BB is Count - 1 , getN(BB, Tail, R).
getN(0, [X | _], R) :- R = X. 


% [a, 0], [b, 0], [c, 0]...

% getOrder(a, [], R) ==> R == 0
% getOrder(a, [Pair(a, 1)], R) ===> R == 1

getOrder(_, [], R) :- R = 0.
getOrder(V, [[V, Count|_]], Count).
getOrder(V, [_|Tail], R) :- getOrder(V, Tail, R).

% incrementArr(a, [], New) ==> [[a, 1]]
% incrementArr(a, [[b, 9]], New) => [[b, 9], [a, 1]]
% incrementArr(a, [[b, 9], [a, 1]], New) => [[b, 9], [a, 1]]
% incrementArr(a, [[a, 1], [b, 9]], New) => [[b, 9], [a, 1]]

incrementArr(V, [], New) :- New = [[V, 1]].
incrementArr(V, [[V, Count]|Tail], New) :- BB is Count + 1 , append( [[V, BB]], Tail, New).
incrementArr(V, [X|Tail], New) :- New = [X|BB], incrementArr(V, Tail, BB).



% create([ ens(a), ens(a), ens(b) ], R)

create([], _, Result) :- Result = [].

create([ens(X)|Tail], Counters, Result) :- 
ensVersResultat(X, RX),
getOrder(X, Counters, RC), 
incrementArr(X, Counters, RI), 
length(RX, Len),
RC2 is RC mod Len,
getN(RC2, RX, S), 
append([S],Next,Result),
create(Tail, RI, Next).



%-----------------------------------------------%





%-----------------------------------------------%




