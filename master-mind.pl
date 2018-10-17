/*
    Luis Jair Cordero Barona
    2017107227

*/

/*

    IMPORTANTE
    EN WORKING_DIR EN NECESARIO COLOCAR LA RUTA EXACTA DONDE ESTA MASTER-MIND.PL
    SIN INCLUIR EL NOMBRE DEL ARCHIVO


working_dir('/home/corderoluis/git_workspace/MasterMind-Prolog/').
TODO ESTO QUEDA EN PAUSA MIENTRAS working_directory siga funcionando... probar en windows!
*/

/*Se encarga de generar la ruta donde se van a crear todos los archivos
    usando la ruta en la que este la ejecucion de swipl*/
path_file(Name,Path):-
    /*working_dir(Dir),*/
    working_directory(Dir,Dir),
    atom_concat(Dir,Name,Path).

borrarPantalla:-
     write('\033[2J'),shell(clear).


marge:-
    write(" ("),nl,
    write("  \\  _,--._,-.__,         )"),nl,
    write("  / (,  ,       ,`-._    /"),nl,
    write(" (  ,^--^-. ;--^--/ (    \\"),nl,
    write("  :'      `/       \\ )   /"),nl,
    write("  (  o    (   o    |(  \\'"),nl,
    write("   \\  ,----\\       /(,-.)"),nl,
    write("  ,'`-\\___  `.___,'  ,. )"),nl,
    write(",'                   __/"),nl,
    write("`-.______________   |,---,"),nl,
    write("      `-^;-^--^-'\\  |   '----,"),nl,
    write("        ( '------'  .',-.___/"),nl,
    write("         ;._____,--' / \\"),nl,
    write(" -hrr-  (           /   \\"),nl,
    write("        (`-        /     \\"),nl,
    write("         \\       ,'       \\"),nl,
    write("        / )  _,-'          \\").


/************************************************************************/

/*Estas dos ultimas se encargan de generar y eliminar un numero aleatorio para cada juego*/
eliminarCorrecto:-
    retract(correcto(_)).

generarCorrecto:-
    random(0,10,N1),
    random(0,10,N2),
    random(0,10,N3),
    random(0,10,N4),
    assert(correcto([N1,N2,N3,N4])).


/*Conjunto de funciones que se encargan de crear y borrar todas los hechos dinamos que necesitamos*/
eliminarVitoria:-
    retract(victoria(_)).
generarVictoria:-
    assert(victoria(0)).
iniciarDinamicas:-
    assert(rondas([1])),
    generarCorrecto,
    correcto(A),
    nl,nl,
    write('Oculto: '),write(A),
    nl,nl,
    generarVictoria.
eliminarDinamicas:-
    (retract(rondas(_));write('')),
    (eliminarCorrecto;write('')),
    (eliminarVitoria;write('')).
beginB_R:-
    (retract(buenos(_)) , assert(buenos([])); assert(buenos([])) ),
    (retract(regulares(_)) , assert(regulares([])) ; assert(regulares([]))).
deleteB_R:-
    retract(buenos(_)),
    retract(regulares(_)).


/*Aumenta lista que contiene la cantidad de ValorYPos igual*/
succB:-
    buenos(B),
    retract(buenos(B)),
    append([1],B,NB),
    assert(buenos(NB)).

/*Aumenta lista que contiene la cantidad de solo valor igual*/
succR:-
    regulares(R),
    retract(regulares(R)),
    append([1],R,NR),
    assert(regulares(NR)).

/*Aumenta Rondas*/
succR:-
    rondas(R),
    retract(rondas(R)),
    append([1],R,NR),
    write('Rondas: '),
    write(NR),
    assert(rondas(NR)).


/*Esta funcion se encarga de revisar si el usuario dio el numero exacto que tenemos generado
    Tambien muestra la victoria del usuario
    Esta tiene que saltar a terminarJuego(Cuando user gana)*/
check([A,B,C,D],[A,B,C,D],_,_):-
    nl,nl,
    write('EL NUMERO ES CORRECTO'),nl,nl,
    rondas(R),
    succ(R,Rondas),
    terminarJuego(Rondas),
    eliminarDinamicas,
    assert(victoria(1)).

checkV(E, [X], Restante):-
    (E == X, Restante = [], succR,!
    ;
    Restante = [X]).

checkV(E,[X|Resto],Restante):-
    (E == X, Restante = Resto, succR,!
    ;
    checkV(E,Resto,Restante2), append([X],Restante2,Restante)).

checkV([A],Lista):-
    checkV(A,Lista,_),!.
checkV([A|Resto],Lista):-
    checkV(A,Lista,Resultado),checkV(Resto,Resultado).

check([A,B,C,D],[E,F,G,H],Buenos,Regulares):-
    beginB_R,
    Pro = [],
    Cor = [],
    (A == E, succB,Pro1 = Pro, Cor1 = Cor; append([A],Pro,Pro1), append([E],Cor,Cor1)),
    (B == F, succB,Pro2 = Pro1, Cor2 = Cor1; append([B],Pro1,Pro2), append([F],Cor1,Cor2)),
    (C == G, succB,Pro3 = Pro2, Cor3 = Cor2; append([C],Pro2,Pro3), append([G],Cor2,Cor3)),
    (D == H, succB,Pro4 = Pro3, Cor4 = Cor3; append([D],Pro3,Pro4), append([H],Cor3,Cor4)),
    checkV(Pro4,Cor4),
    buenos(Bue),
    regulares(Reg),
    succ(Bue,Buenos),
    succ(Reg,Regulares),
    write('Iguales en valor y posicion: '),write(Buenos),nl,
    write('Iguales en valor: '),write(Regulares),nl,nl,nl,
    deleteB_R,succR,play. /*POR AHORA VOY A AUMENTAR LAS RONDAS AQUI*/

/*Funcion que ayuda a probrar el juego cuando el usuario adivina
    Utilizada solo en tiempo de desarrollo*/
fee:-
    eliminarDinamicas,
    iniciarDinamicas.
/*******************************************************************************/

generarSecuencia(S):-
  random(0,10,S1),
  random(0,10,S2),
  random(0,10,S3),
  random(0,10,S4),
  S = [S1,S2,S3,S4].

leerSN(X,Respuesta):-
  nl,
  write('Su numero es: '),write(X),write('?(Y/N)'),nl,
  get_char(R),
  read_string(user_input, "\n", "\r", _, _).



inicio:-
    genearSecuencia(X).
/*******************************************************************************/

leerNumero(N1,N2,N3,N4):-
    nl,
    write('Digite una Secuencia de 4 numeros'),nl,nl,
    get_char(A),get_char(B),get_char(C),get_char(D),
    read_string(user_input, "\n", "\r", _, _),
    (member(A,['1','2','3','4','5','6','7','8','9','0']),
        member(B,['1','2','3','4','5','6','7','8','9','0']),
        member(C,['1','2','3','4','5','6','7','8','9','0']),
        member(D,['1','2','3','4','5','6','7','8','9','0']),N1 = A, N2 = B,N3 = C, N4 = D,!;
        write('Debe ser un Secuencia de 4 numeros'),nl,
        leerNumero(A1,B1,C1,D1),N1 = A1, N2 = B1, N3 = C1, N4 = D1).
/*
leerNumero(String,Num):-
    write(String),nl,
    get_char(X),
    read_string(user_input, "\n", "\r", _, _),
    nl,
    writeq(X),
    (member(X,['1','2','3','4','5','6','7','8','9','0']), Num = X,!; leerNumero(String,Num1),Num = Num1).
*/

/*Esta funcion va a cargar con todo el ciclo del juego
    Es decir que cada ejecucion de esta funcion se toma como una ronda
        Primero el usuario trata de adivinar el numero generado
            Correcto
                Se detiene el proceso y se pasa a guardar el usuario con la cantidad de rondas
                Se borra todo lo que se deba borrar(Rondas, Victoria, Correcto)
            Incorrecto
                La maquina trata de adivinar
                    para este voy a generar 4 numeros random, meterlos en una lista
                        y preguntar si esa lista no esta en la lista de listas, para no repetir estados,
                            eso lo hace exhaustivo...
                    Correcto
                        Se detiene el proceso y se le dice al usuario que perdio por ende no se almacena
                        Se borra todo lo que se deba borrar(Rondas, Victoria, Correcto)
                    Incorrecto
                        Se procede a aumentar la roda y ejecutar de nuevo*/
play:-
    nl,
    rondas(R),
    succ(R,Rondas),
    write('Ronda '),write(Rondas),nl,
    write('Ingrese el posible numero'),
    path_file('compu.pl',Path),
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
    /*para este punto tengo el correcto y el brindado por el usuario toca check*/
    propuesto(Propuesto),nl,
    write('Propuesto: '),write(Propuesto),nl,
    correcto(Correcto),
    check(Propuesto,Correcto,I,J).


/*Esta funcion recibe una lista con numeros y retorna su suma
    [1,1,1,1] = 4
    sirve para contar los numeros que estan en valorYpos bien o los de solo valor
    y tambien para el numero de rondas*/
succ([],0).
succ([X|Resto],Resultado):-
    succ(Resto,Resultado1),
    Resultado is X + Resultado1.



/*funcion que reccorre un arreglo*/
/*BORRAR PARA LA ENTREGA YA QUE NO ES NECESARIA*/
recorrerArreglo([X]):-
    write(X).
recorrerArreglo([X|Resto]):-
    write(X),
    recorrerArreglo(Resto).

/************************************************************************/

/*Esta funcion se encarga de solicitar el nombre por consola
    Tambien lo guarda en un archivo temporal
    y lo consulta*/
pedirNombre:-
    path_file('temp.pl',Path),
    write('Digite su nombre'),nl,
    tell(Path),
    read_string(user_input, "\n", "\r", _, X),
    write('jugador('),write('"'),write(X),write('"'),write(').'),
    told,
    consult(Path).


/*Esta solo es una funcion de ayuda para saltar a play con todo iniciado correctamente*/
jugar:-
    pedirNombre,
    fee,
    play.


/*funcion para borrar todos los puntajes
    solo utilizada en tiempos desarrollo del juego*/
cleanPuntaje:-
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
terminarJuego(R):-
    path_file('puntaje.pl',Path),
    catch(consult(Path), _, cleanPuntaje),
    consult(Path), /*Con esto obtenjo los puntajes*/
    puntaje(P),
    jugador(J),
    write('Rondas : '),write(R),nl,/*ronda(R)*/
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
    write('Lista de puntajes'),nl,nl,
    write('Rondas'),put(9),put(9),write('Nombre'),nl,nl,
    consult(Path), /*Con esto obtenjo los puntajes*/
    puntaje(P),
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
opcion(_):- write("Se cerro el programa :D"),nl,nl,halt.


/*Funcion que muestra las opciones del programa
    Solicita una opcion y con respecto a esta:
        Comienza un juego
        Muestra los puntajes
        Sale del juego
*/
menu:-
    write('1. Jugar.'),nl,
    write('2. Ver puntajes.'),nl,
    write('Cualquier otro para salir'),nl,nl,
    write('Digite el numero'),nl,read_string(user_input, "\n", "\r", _, String),borrarPantalla,nl,
    opcion(String).


/* shell(clear). para consola unix*/
/*write('\033[2J')*/


/*Funcion para el loop del menu*/
run:- menu,nl.

/*
run:- menu,nl,nl,nl,
write("Desea continuar(s/n)"),read(Ch), =(Ch,'n').
*/

/*
run:-
    nl,nl,nl,run.

*/

:- borrarPantalla,write("Utilizar la sentencia 'menu.' para comenzar el juego.").
