/*
    Luis Jair Cordero Barona
    2017107227
*/

/*Se encarga de generar la ruta donde se van a crear todos los archivos
    usando la ruta en la que este la ejecucion de swipl*/
path_file(Name,Path):-
    working_directory(Dir,Dir),
    atom_concat(Dir,Name,Path).

/*Con esto limpio la pantalla
  con fin de que funcione en windows y linux
*/
borrarPantalla:-
     write('\033[2J'),shell(clear).

/*Aqui inicializo los predicados dinamos que voy a usar*/
initMasterMind:-
  assert(correcto([])),
  assert(rondasPVC(0)),
  assert(rondasCVP([])),
  assert(buenosPVC(0)),assert(regularesPVC(0)),
  assert(secuenciasAll([])),
  assert(buenosCVP(0)),assert(regularesCVP(0)),!.

/*Con esto elimino el numero que se selecciona correcto en cada juego*/
eliminarCorrecto:-
    (retract(correcto(_)), eliminarCorrecto;!).
/*Aqui genero el numero correcto que el usuario tendra que adivinar*/
generarCorrecto:-
    random(0,10,N1),
    random(0,10,N2),
    random(0,10,N3),
    random(0,10,N4),
    assert(correcto([N1,N2,N3,N4])).
/*Aqui elimino el predicado que lleva el conteo de las rodas de cada juego
  'rondas que tarda el usuario en adivinar'*/
eliminarRondasPVC:-
  (retract(rondasPVC(_)), eliminarRondasPVC;!).

/*Con esto se inicia los predicados dinamos que se usaran por todo el juego*/
iniciarDinamicas:-
    eliminarRondasPVC,assert(rondasPVC(1)),
    eliminarCorrecto,generarCorrecto,
    correcto(A),
    nl,nl,
    %write(A),%quitar comentario si se desea saber la secuenca oculta
    nl,nl,
    write('Oculto: '),write('[?,?,?,?]').

/*Los siguientes se encargan de crear todo lo que se necesita para comparar un numero con otro
  y decir los correctos en valor y los correctos en valor y posicion*/
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

/*Aumenta la cantidad de numeros de igual valor y posicion*/
aumentarBuenosPVC:-
    buenosPVC(B),
    retract(buenosPVC(B)),
    B1 is B + 1,
    assert(buenosPVC(B1)).

/*Aumenta la cantidad de numeros de igual valor*/
aumentarRegularesPVC:-
    regularesPVC(R),
    retract(regularesPVC(R)),
    R1 is R + 1,
    assert(regularesPVC(R1)).

/*Aumenta Rondas que tarda el usuario en adivinar*/
aumentarRondasPVC:-
    rondasPVC(R),
    retract(rondasPVC(R)),
    R1 is R + 1,
    write(R1),
    assert(rondasPVC(R1)).



/*La siguientes se encargan de comparar dos secuencias de numeros
  y decir cuantos son similares en valor y cuantos en valor y posicion*/
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

/*De todas la anteriores esta es la que se encarga de llamarlas*/
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

/*Lista con los numeros permitidos para la secuencia de 4 numeros*/
numeros0_9(['1','2','3','4','5','6','7','8','9','0']).

/*Aqui se lee la secuencia de 4 numeros y tambien se revisa que sea una secuencia de 4 numeros*/
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

/*Con esto se Comienza la parte del juego donde el usuario tiene que adivinar*/
playPVC:-
    nl,
    rondasPVC(Rondas),
    write('Ronda '),write(Rondas),nl,
    write('Ingrese el posible numero'),
    path_file('tempPVC.pl',Path),
    leerNumero(N1,N2,N3,N4),
    tell(Path),
    write('propuesto('),write([N1,N2,N3,N4]),write(').'),
    told,
    consult(Path),
    propuesto(Propuesto),nl,
    write('Propuesto: '),write(Propuesto),nl,
    correcto(Correcto),
    chequearPVC(Propuesto,Correcto,_,_).


/************************************************************************/

/*Con estas se revisa que el nombre no este vacio o solo sean espacios*/
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

/*Con estas se define quien gana dependiendo de las rondas que tardo*/
definirGanador(R,R):-
  nl, write('Empataron... Ninguno gana, intenta otra vez :)'),nl.
definirGanador(RS,RC):-
    RS < RC,
    nl, marge, nl, printGanaste, nl, nl, ganaUsuario(RS).
definirGanador(RS,RC):-
    RC < RS,
    nl, write('La computadora adivino el numero en menos rondas'), nl, printPerdiste, nl.

/*Esta se encarga de llevar al usuario por todo el juego*/
jugar:-
    pedirNombre, iniciarDinamicas, nl,write('Tienes que adivinar la secuencia oculta'),nl,playPVC,
    nl,nl,write('Es el turno de la computadora de adivinar el numero que tienes Oculto'),nl,playCVP,
    rondasPVC(RS), rondasCVP(RC),
    nl, write('Rondas que tardo el usuario para adivinar: '), write(RS), nl,
    nl, write('Rondas que tardo la computadora en adivinar: '), write(RC), nl,
    definirGanador(RS,RC).

/*funcion para borrar todos los puntajes
    solo utilizada en tiempos desarrollo del juego*/
cleanPuntaje:-
    writeln('Limpiando puntaje'), path_file('puntaje.pl',Path),
    tell(Path),
    write( 'puntaje(' ), write('[]'), write( ').' ),
    told.

/* Aqui se registra al usuario en el puntaje en caso de que gane el juego*/
ganaUsuario(R):-
    path_file('puntaje.pl',Path), catch(consult(Path), _, cleanPuntaje), consult(Path),
    puntaje(P), jugador(J), append([R],[J],JR), my_append(JR,P,NewP),
    tell(Path),
    write( 'puntaje(' ) ,writeq(NewP),write( ').' ),
    told.

/*Funciones para mostrar el puntaje, pueden ser mejores*/
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

/*Esto se encarga de mostrar los mensajes para el puntaje y llamar a las funciones necesarias*/
verPuntaje:-
    path_file('puntaje.pl',Path),
    catch(consult(Path), _, cleanPuntaje),catch(puntaje(P), _, cleanPuntaje),
    consult(Path),
    borrarPantalla,listaDePuntajes,
    nl,nl,write('Rondas'),put(9),put(9),write('Nombre'),nl,nl,
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


/*Menu que le indica al usuario que hacer*/
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
