/*Parte en la que el jugador adivina*/





/***********************************************************************/

working_dir('/home/corderoluis/Documents/Lenguajes/Prolog/progra/').

path_file(Name,Path):-
    working_dir(Dir),
    atom_concat(Dir,Name,Path).

foo:-
    path_file('temp.pl',Path),
    tell(Path),
    get_char(N1),get_char(N2),get_char(N3),get_char(N4),
    write('jugadorTxt(['),
        write(N1),
        write(','),
        write(N2),
        write(','),
        write(N3),
        write(','),
        write(N4),
    write(']).'),
    /*read_string(user_input, "\n", "\r", _, X),
    write('jugadorTxt('),write("'"),write(X),write("'"),write(').'),*/
    
    told.


/************************************************************************/




eliminarCorrecto:-
    retract(correcto(_)).

generarCorrecto:-
    random(0,10,N1),
    random(0,10,N2),
    random(0,10,N3),
    random(0,10,N4),
    assert(correcto([N1,N2,N3,N4])).

eliminarVitoria:-
    retract(victoria(_)).
generarVictoria:-
    assert(victoria(0)).

iniciarDinamicas:-
    assert(rondas([])),
    generarCorrecto,
    correcto(A),
    write(A),
    generarVictoria.

eliminarDinamicas:-
    retract(rondas(_)),
    eliminarCorrecto,
    eliminarVitoria.

iniciarBuenosYRegulares:-
    assert(buenos([])),
    assert(regulares([])).
eliminarBuenosYRegulares:-
    retract(buenos(_)),
    retract(regulares(_)).

aumentarBuenos:-
    buenos(B),
    retract(buenos(B)),
    append([1],B,NB),
    assert(buenos(NB)).

aumentarRegulares:-
    regulares(R),
    retract(regulares(R)),
    append([1],R,NR),
    assert(regulares(NR)).    

comparar([A,B,C,D],[A,B,C,D],_,_):-
    nl,nl,
    write('EL NUMERO ES CORRECTO'),nl,nl,
    eliminarCorrecto,
    eliminarVitoria,
    assert(victoria(1)),menu.


comparar([A,B,C,D],[E,F,G,H],Buenos,Regulares):-
    iniciarBuenosYRegulares,
    (A == E, aumentarBuenos; member(A,[F,G,H]),aumentarRegulares;write('')),
    (B == F, aumentarBuenos; member(B,[E,G,H]),aumentarRegulares;write('')),
    (C == G, aumentarBuenos; member(C,[F,E,H]),aumentarRegulares;write('')),
    (D == H, aumentarBuenos; member(D,[F,G,E]),aumentarRegulares;write('')),nl,
    buenos(Bue),
    regulares(Reg),
    sumar(Bue,Buenos),
    sumar(Reg,Regulares),
    write('Iguales en valor y posicion: '),write(Buenos),nl,
    write('Iguales en valor: '),write(Regulares),nl,nl,nl,
    eliminarBuenosYRegulares,play.


fee:-
    generarCorrecto,
    correcto(A),
    write(A),
    generarVictoria.

play:-
    write('Ingrese el posible numero'),nl,
    path_file('compu.pl',Path),
    get_char(N1),get_char(N2),get_char(N3),get_char(N4),
    read_string(user_input, "\n", "\r", _, String),
    tell(Path),
    write('propuesto(['),
        write(N1),
        write(','),
        write(N2),
        write(','),
        write(N3),
        write(','),
        write(N4),
    write(']).'),
    told,
    consult(Path),
    /*para este punto tengo el correcto y el brindado por el usuario toca comparar*/
    propuesto(Propuesto),
    correcto(Correcto),
    comparar(Propuesto,Correcto,I,J).


sumar([],0).
sumar([X|Resto],Resultado):-
    sumar(Resto,Resultado1),
    Resultado is X + Resultado1.


recorrerArreglo([X]):-
    write(X).
recorrerArreglo([X|Resto]):-
    write(X),
    recorrerArreglo(Resto).

/************************************************************************/


pedirNombre:-
    path_file('temp.pl',Path),
    write('Digite su nombre'),nl,
    tell(Path),
    read_string(user_input, "\n", "\r", _, X),
    write('jugador('),write('"'),write(X),write('"'),write(').'),
    told,
    consult(Path).




jugar:-
    /*Pedir Nombre*/
    iniciarDinamicas,

    /**/
    adivino,
    /**/

    eliminarDinamicas.

cleanPuntaje:-
    path_file('puntaje.pl',Path),
    tell(Path),
    write( 'puntaje(' ) ,
    write('[]'),
    write( ').' ),
    told.

terminarJuego:-
    path_file('puntaje.pl',Path),
    catch(consult(Path), _, cleanPuntaje),
    consult(Path), /*Con esto obtenjo los puntajes*/
    puntaje(P),
    jugador(J),
    random(20,30,R),/*ronda(R)*/
    append([R],[J],JR),
    my_append(JR,P,NewP),
    tell(Path),
    write( 'puntaje(' ) ,
    writeq(NewP),
    write( ').' ),
    told.

/*
catch(consult('caca.pl'), E, tell('caca.pl')).
*/

escribirPuntaje([]):-
    nl.
escribirPuntaje([X|Resto]):-
    write(X),put(9),put(9),
    escribirPuntaje(Resto).

mostrarPuntaje([X]):-
    escribirPuntaje(X),!.

mostrarPuntaje([X|Resto]):-
    escribirPuntaje(X),
    mostrarPuntaje(Resto).

mostrarPuntaje([]):- 
    write('No hay puntajes').

verPuntaje:-
    path_file('puntaje.pl',Path),
    catch(consult(Path), _, cleanPuntaje),
    write('Lista de puntajes'),nl,nl,
    write('Rondas'),put(9),put(9),write('Nombre'),nl,nl,
    consult(Path), /*Con esto obtenjo los puntajes*/
    puntaje(P),
    mostrarPuntaje(P).

my_append([],[],[]).
my_append([],Lista,Lista).
my_append(Lista1,Lista2,[Lista1|Lista2]).



opcion("1"):- pedirNombre,jugar,terminarJuego,nl,run.
opcion("2"):- write('-----------------------------------'),nl,verPuntaje,nl,nl,write('-----------------------------------'),nl,nl,run.
opcion(_):- write("Me salgo del programa"),consult('/home/corderoluis/Documents/Lenguajes/Prolog/progra/juego.pl').

menu:-
    write('1. Jugar.'),nl,
    write('2. Ver puntajes.'),nl,
    write('Cualquier otro para salir'),nl,nl,
    write('Digite el numero'),nl,read_string(user_input, "\n", "\r", _, String),shell(clear),nl,
    opcion(String).


/* shell(clear). para consola unix*/
/*write('\033[2J')*/

run:- menu,nl.

/*
run:- menu,nl,nl,nl,
write("Desea continuar(s/n)"),read(Ch), =(Ch,'n').
*/

/*
run:-
    nl,nl,nl,run.

*/


















