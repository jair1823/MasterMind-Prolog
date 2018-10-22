/*
codigo repetido
*/

/*Aumenta Rondas*/
aumentarRondasCVP:-
    rondasCVP(R),
    retract(rondasCVP(R)),
    R1 is R + 1,
    assert(rondasCVP(R1)).

eliminarRondasCVP:-
  (retract(rondasCVP(_)), eliminarRondasCVP;write('Memoria Limpia: rondasCVP')).


/*Aumenta lista que contiene la cantidad de ValorYPos igual*/
aumentarBuenosCVP:-
    buenosCVP(B),
    retract(buenosCVP(B)),
    B1 is B + 1,
    assert(buenosCVP(B1)).

/*Aumenta lista que contiene la cantidad de solo valor igual*/
aumentarRegularesCVP:-
  regularesCVP(R),
  retract(regularesCVP(R)),
  R1 is R + 1,
  assert(regularesCVP(R1)).

genBuenosCVP:-
    assert(buenosCVP(0)).
eliBuenosCVP:-
  (retract(buenosCVP(_)) , eliBuenosCVP;!).

genRegularesCVP:-
  assert(regularesCVP(0)).
eliRegularesCVP:-
  (retract(regularesCVP(_)) , eliRegularesCVP;!).


buenosRegularesCVP:-
  eliBuenosCVP,genBuenosCVP,
  eliRegularesCVP,genRegularesCVP.

checkV(E, [X], Restante):-
    (E == X, Restante = [], aumentarRegularesCVP,!
    ;
    Restante = [X]).

checkV(E,[X|Resto],Restante):-
    (E == X, Restante = Resto, aumentarRegularesCVP,!
    ;
    checkV(E,Resto,Restante2), append([X],Restante2,Restante)).

checkV([A],Lista):-
    checkV(A,Lista,_),!.
checkV([A|Resto],Lista):-
    checkV(A,Lista,Resultado),checkV(Resto,Resultado).

check([A,B,C,D],[E,F,G,H],Buenos,Regulares):-
    buenosRegularesCVP,
    Pro = [],
    Cor = [],
    (A == E, aumentarBuenosCVP,Pro1 = Pro, Cor1 = Cor; append([A],Pro,Pro1), append([E],Cor,Cor1)),
    (B == F, aumentarBuenosCVP,Pro2 = Pro1, Cor2 = Cor1; append([B],Pro1,Pro2), append([F],Cor1,Cor2)),
    (C == G, aumentarBuenosCVP,Pro3 = Pro2, Cor3 = Cor2; append([C],Pro2,Pro3), append([G],Cor2,Cor3)),
    (D == H, aumentarBuenosCVP,Pro4 = Pro3, Cor4 = Cor3; append([D],Pro3,Pro4), append([H],Cor3,Cor4)),
    checkV(Pro4,Cor4),
    buenosCVP(Buenos),
    regularesCVP(Regulares).



/***********************/

f(0).
f(1).
f(2).
f(3).
f(4).
f(5).
f(6).
f(7).
f(8).
f(9).

secuencia(X,Y,Z,J):-
	f(X),f(Y),f(Z),f(J).

eliminarListaSecuencias:-
	(retract(secuenciasAll(_)),eliminarListaSecuencias;nl,write('Memoria Limpia: secuenciasAll'),nl).

listaSecuencias:-
    eliminarListaSecuencias,
    findall([X,Y,Z,J],secuencia(X,Y,Z,J),A),
		assert(secuenciasAll(A)).




leerVP(N1):-
	nl,
	write('Cuantos estan bien en valor y pos?'),nl,
	get_char(A),
	read_string(user_input, "\n", "\r", _, _),
	(member(A,['1','2','3','0']),
	    N1 = A,!;
	    write('Solo puede ser [0,1,2,3]'),nl,
	    leerVP(A1),N1 = A1).


leerV(N1):-
	nl,
	write('Cuantos estan bien solo en valor?'),nl,
	get_char(A),
	read_string(user_input, "\n", "\r", _, _),
	(member(A,['1','2','3','0','4']),
	    N1 = A,!;
	    write('Solo puede ser [0,1,2,3,4]'),nl,
	    leerVP(A1),N1 = A1).

leerSN(P):-
  rondasCVP(Ronda),nl,
  write('Ronda '), write(Ronda),
	nl,
	write('Su numero es: '),write(P),write('?(Y/N)'),nl,
	get_char(R), read_string(user_input, "\n", "\r", _, _),
	((member(R,['Y','y']), write('Termino :)'); member(R,['N','n']),aumentarRondasCVP, seguir(P)); write('Tiene que ser Y/N'), nl, leerSN(P)).

reducirLista(P,VP,V,[],[]):-!.
reducirLista(P,VP,V,[X|Resto],Resultado):-
		check(P,X,PV1,V1),
		(VP == PV1, V == V1, reducirLista(P,VP,V,Resto,Rest), append([X],Rest,Resultado);
		reducirLista(P,VP,V,Resto,Rest), Resultado = Rest).


reducirLista(P,VP,V):-
	secuenciasAll(A),
	reducirLista(P,VP,V,A,NewSecu),
	retract(secuenciasAll(A)),
	assert(secuenciasAll(NewSecu)).

seguir(P):-
		leerVP(VPchar),
		leerV(Vchar),
		atom_number(VPchar, VP),
		atom_number(Vchar, V),
		reducirLista(P,VP,V),
		primeraSecuencia(Secu),
		(Secu == [0],write('Fin del juego') ;leerSN(Secu)).


pop([X|Resto],0,X,Resto):-!.
pop([X|Resto],Num,B,[X|Rest]):-
		N is Num - 1,
		pop(Resto,N,B,Rest).

primeraSecuencia(OneSecu):-
	secuenciasAll(A),
	length(A,X),
	(X == 0,nl,write('Mentiroso :3'),eliminarRondasCVP,assert(rondasCVP(0)),nl,OneSecu = [0];
	random(0,X,Random),
	pop(A,Random,OneSecu,NewSecu),
	retract(secuenciasAll(A)),
	assert(secuenciasAll(NewSecu))).

playCVP:-
    eliminarRondasCVP,assert(rondasCVP(1)),
		listaSecuencias,
		primeraSecuencia(Secu),nl,
		leerSN(Secu),
		eliminarListaSecuencias,!.
