/*
    Luis Jair Cordero Barona
    2017107227
*/

/*Se encarga de generar la ruta donde se van a crear todos los archivos
    usando la ruta en la que este la ejecucion de swipl*/
path_file(Name,Path):-
    working_directory(Dir,Dir),
    atom_concat(Dir,Name,Path).

/**/
borrarPantalla:-
     write('\033[2J'),shell(clear).

/************************************************************************/
initMasterMind:-
  assert(correcto([])),
  assert(rondasPVC(0)),
  assert(rondasCVP([])),
  assert(buenosPVC(0)),assert(regularesPVC(0)),
  assert(secuenciasAll([])),
  assert(buenosCVP(0)),assert(regularesCVP(0)),!.

/*Estas dos ultimas se encargan de generar y eliminar un numero aleatorio para cada juego*/
eliminarCorrecto:-
    (retract(correcto(_)), eliminarCorrecto;!).

generarCorrecto:-
    random(0,10,N1),
    random(0,10,N2),
    random(0,10,N3),
    random(0,10,N4),
    assert(correcto([N1,N2,N3,N4])).

eliminarRondasPVC:-
  (retract(rondasPVC(_)), eliminarRondasPVC;!).

/*Conjunto de funciones que se encargan de crear y borrar todas los hechos dinamos que necesitamos*/
iniciarDinamicas:-
    eliminarRondasPVC,assert(rondasPVC(1)),
    eliminarCorrecto,generarCorrecto,
    correcto(A),
    nl,nl,
    write(A),
    nl,nl,
    write('Oculto: '),write('[?,?,?,?]').

genBuenosPVC:-
    assert(buenosPVC(0)).
eliBuenosPVC:-
  (retract(buenosPVC(_)) , eliBuenosPVC;!).

genRegularesPVC:-
  assert(regularesPVC(0)).
eliRegularesPVC:-
  (retract(regularesPVC(_)) , eliRegularesPVC;!).


buenosRegularesPVC:-
  eliBuenosPVC,genBuenosPVC,
  eliRegularesPVC,genRegularesPVC.

/*Aumenta lista que contiene la cantidad de ValorYPos igual*/
aumentarBuenosPVC:-
    buenosPVC(B),
    retract(buenosPVC(B)),
    B1 is B + 1,
    assert(buenosPVC(B1)).

/*Aumenta lista que contiene la cantidad de solo valor igual*/
aumentarRegularesPVC:-
    regularesPVC(R),
    retract(regularesPVC(R)),
    R1 is R + 1,
    assert(regularesPVC(R1)).

/*Aumenta Rondas*/
aumentarRondasPVC:-
    rondasPVC(R),
    retract(rondasPVC(R)),
    R1 is R + 1,
    write('Ronda: '),
    write(R1),
    assert(rondasPVC(R1)).



/*Esta funcion se encarga de revisar si el usuario dio el numero exacto que tenemos generado
    Tambien muestra la victoria del usuario
    Esta tiene que saltar a ganaUsuario(Cuando user gana)*/


chequearPVCvalor(E, [X], Restante):-
    (E == X, Restante = [], aumentarRegularesPVC,!
    ;
    Restante = [X]).

chequearPVCvalor(E,[X|Resto],Restante):-
    (E == X, Restante = Resto, aumentarRegularesPVC,!
    ;
    chequearPVCvalor(E,Resto,Restante2), append([X],Restante2,Restante)).

chequearPVCvalor([A],Lista):-
    chequearPVCvalor(A,Lista,_),!.
chequearPVCvalor([A|Resto],Lista):-
    chequearPVCvalor(A,Lista,Resultado),chequearPVCvalor(Resto,Resultado).

chequearPVC([A,B,C,D],[A,B,C,D],_,_):-
    nl,nl,
    write('Ese numero es correcto.'),nl,nl,rondasPVC(R),write('Total de rondas en adivinarlo: '),write(R),nl.

chequearPVC([A,B,C,D],[E,F,G,H],Buenos,Regulares):-
    buenosRegularesPVC,
    Pro = [],
    Cor = [],
    (A == E, aumentarBuenosPVC,Pro1 = Pro, Cor1 = Cor; append([A],Pro,Pro1), append([E],Cor,Cor1)),
    (B == F, aumentarBuenosPVC,Pro2 = Pro1, Cor2 = Cor1; append([B],Pro1,Pro2), append([F],Cor1,Cor2)),
    (C == G, aumentarBuenosPVC,Pro3 = Pro2, Cor3 = Cor2; append([C],Pro2,Pro3), append([G],Cor2,Cor3)),
    (D == H, aumentarBuenosPVC,Pro4 = Pro3, Cor4 = Cor3; append([D],Pro3,Pro4), append([H],Cor3,Cor4)),
    chequearPVCvalor(Pro4,Cor4),
    buenosPVC(Buenos),
    regularesPVC(Regulares),
    write('Iguales en valor y posicion: '),write(Buenos),nl,
    write('Iguales en valor: '),write(Regulares),nl,nl,nl,
    aumentarRondasPVC,playPVC. /*POR AHORA VOY A AUMENTAR LAS RONDAS AQUI*/

/*Funcion que ayuda a probrar el juego cuando el usuario adivina
    Utilizada solo en tiempo de desarrollo*/
fee:-
    iniciarDinamicas.

numeros0_9(['1','2','3','4','5','6','7','8','9','0']).

leerNumero(N1,N2,N3,N4):-
    nl,
    write('Digite una Secuencia de 4 numeros'),nl,nl,
    get_char(A),get_char(B),get_char(C),get_char(D),
    read_string(user_input, "\n", "\r", _, _),
    numeros0_9(Numeros),
    (member(A,Numeros),
        member(B,Numeros),
        member(C,Numeros),
        member(D,Numeros),N1 = A, N2 = B,N3 = C, N4 = D,!;
        write('Debe ser un Secuencia de 4 numeros'),nl,
        leerNumero(A1,B1,C1,D1),N1 = A1, N2 = B1, N3 = C1, N4 = D1).


playPVC:-
    nl,
    rondasPVC(Rondas),
    write('Ronda '),write(Rondas),nl,
    write('Ingrese el posible numero'),
    path_file('tempPVC.pl',Path),
    leerNumero(N1,N2,N3,N4),
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
    propuesto(Propuesto),nl,
    write('Propuesto: '),write(Propuesto),nl,
    correcto(Correcto),
    chequearPVC(Propuesto,Correcto,_,_).


/************************************************************************/
onlySpace([X]):-
  X == "",!.
onlySpace([X|Resto]):-
  X == "",
  onlySpace(Resto).

/*Esta funcion se encarga de solicitar el nombre por consola
    Tambien lo guarda en un archivo temporal
    y lo consulta*/
pedirNombre:-
    path_file('temp.pl',Path),
    write('Ingrese su nickname'),nl,
    tell(Path),

    read_string(user_input, "\n", "\r", _, X),
    split_string(X, " ", "", L),
    (onlySpace(L), told, pedirNombre;
    write('jugador('),write('"'),write(X),write('"'),write(').'),
    told,
    consult(Path)).

definirGanador(R,R):-
  nl,
  write('Empataron... Ninguno gana, intenta otra vez :)'),
  nl.
definirGanador(RS,RC):-
    RS < RC,
    nl,
    marge,
    nl,
    printGanaste,
    nl,
    nl,
    ganaUsuario(RS).
definirGanador(RS,RC):-
    RC < RS,
    nl,
    write('La computadora adivino el numero en menos rondas'),nl,
    printPerdiste,
    nl.
/*Esta solo es una funcion de ayuda para saltar a playPVC con todo iniciado correctamente*/
jugar:-
    pedirNombre,
    fee,
    playPVC,
    playCVP,
    rondasPVC(RS),
    rondasCVP(RC),
    nl,
    write('Rondas que tardo el usuario para adivinar: '),write(RS),
    nl,
    nl,
    write('Rondas que tardo la computadora en adivinar: '),write(RC),
    nl,

    definirGanador(RS,RC).


/*funcion para borrar todos los puntajes
    solo utilizada en tiempos desarrollo del juego*/
cleanPuntaje:-
    writeln('Limpiando puntaje'),
    path_file('puntaje.pl',Path),
    tell(Path),
    write( 'puntaje(' ) ,
    write('[]'),
    write( ').' ),
    told.

/*Esta funcion es la encargada de terminar el juego cuando el usuario gana
    Es decir que tambien guarda el nombre y rondas que le tomo al user ganar
*/

/*una opcion es hacer otra con parametros distintos para cuando el user no gana*/
ganaUsuario(R):-
    path_file('puntaje.pl',Path),
    catch(consult(Path), _, cleanPuntaje),
    consult(Path), /*Con esto obtenjo los puntajes*/
    puntaje(P),
    jugador(J),
    %write('Rondas : '),write(R),nl,/*ronda(R)*/
    append([R],[J],JR),
    my_append(JR,P,NewP),
    tell(Path),
    write( 'puntaje(' ) ,
    writeq(NewP),
    write( ').' ),
    told.

/*escribirPuntaje y mostrarPuntaje son solo funciones de ayuda para imprimir la lista de puntajes en pantalla*/
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

/*Esta funcion se encarga de abrir el archivo que tiene el puntaje y mandarlo a imprimir en pantalla*/
verPuntaje:-
    path_file('puntaje.pl',Path),
    catch(consult(Path), _, cleanPuntaje),
    catch(puntaje(P), _, cleanPuntaje),
    consult(Path),
    borrarPantalla,
    listaDePuntajes,nl,nl,
    write('Rondas'),put(9),put(9),write('Nombre'),nl,nl,
     /*Con esto obtenjo los puntajes*/
    mostrarPuntaje(P).

/*Funciones de append para caso especial*/
my_append([],[],[]).
my_append([],Lista,Lista).
my_append(Lista1,Lista2,[Lista1|Lista2]).

/*Opcion para jugar*/
opcion("1"):- jugar,nl,run.

/*opcion de mostrar el puntaje*/
opcion("2"):- write('-----------------------------------'),nl,verPuntaje,nl,nl,write('-----------------------------------'),nl,nl,run.

/*opcion para salir del programa*/
opcion(_):- write("Se cerro el programa :D"),nl,nl,informacacion.

/*Funcion que muestra las opciones del programa
    Solicita una opcion y con respecto a esta:
        Comienza un juego
        Muestra los puntajes
        Sale del juego

*/
menu:-
    printMasterMind,
    nl,nl,
    write('                 Jugar (1)'),nl,
    write('                 Ver puntajes (2)'),nl,
    write('                 Cualquier otro para salir'),nl,nl,
    write('                 Digite el numero'),nl,read_string(user_input, "\n", "\r", _, String),borrarPantalla,nl,
    opcion(String).

/*Funcion para el loop del menu*/
run:- menu,nl.

informacacion:-
  borrarPantalla,nl,write("Utilizar la sentencia 'menu.' para comenzar el juego. :)"),nl.

:- ['adivinar.pl','prints.pl'],initMasterMind,informacacion.
