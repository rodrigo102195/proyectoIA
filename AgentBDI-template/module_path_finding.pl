:- encoding('iso_latin_1').

:- module(path_finding,
	  [
	    buscar_plan_desplazamiento/3
	  ]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% buscar_plan_desplazamiento(+Metas, -Plan, -Destino)
% Realiza el plan óptimo para un dado conjunto de metas
% Metas: Lista de id de los nodos meta
% Plan: Lista de id de los nodos que deberíamos tomar
% Destino: Es el id del nodo destino al cual nos dirigimos
buscar_plan_desplazamiento(Metas, Plan, Destino):-
	at([agent,me], MyPos),
	Metas \= [],
	write('Mi posicion es: '), write(MyPos), nl,
	write('Las metas son: '), write(Metas), nl,
	a_estrella_cascara([path(0,[MyPos])], Metas, PlanReverso, Destino, []), !,
	agregarMove(PlanReverso,PlanReversoConMove),
	reverse(PlanReversoConMove, [_MiPosicion|Plan]).

% Estructura: [path(Costo,[Header|RestoCamino])|Caminos]

a_estrella_cascara([],_Metas,[],_Destino,_Visitados):- !.

a_estrella_cascara(Caminos, Metas, Plan, Destino, Visitados):-
	elegir_mejor_camino(Caminos, Metas, path(Costo,[Header|RestoCamino])),
	a_estrella(Caminos, Metas, Plan, Destino, path(Costo,[Header|RestoCamino]), [Header|Visitados]).

% a_estrella(+Caminos, +Metas, -Plan, -Destino, +MejorCamino)
% Caso base: Si el nodo que visito contiene a la meta entonces devuelvo el plan
a_estrella(_Caminos, Metas, [Header|RestoCamino], Header, path(_Costo,[Header|RestoCamino]), _Visitados):-
	member(Header, Metas).

% Caso Recursivo:
%	Elijo el mejor Camino, lo elimino de la lista de Caminos
% Agrego los nuevos caminos producto de mirar los adyacentes del mejor camino
% Llamo al a_estrella con los caminos sin el mejor camino mas los nuevos caminos
a_estrella(Caminos, Metas, Plan, Destino, MejorCamino, Visitados):-
	delete(Caminos, MejorCamino, CaminosSinElMejor),
	generar_nuevos_caminos(Visitados, MejorCamino, NuevosCaminos),
	eliminar_peores_caminos(NuevosCaminos, CaminosSinElMejor, CaminosActualizados),
	a_estrella_cascara(CaminosActualizados, Metas, Plan, Destino, Visitados).

% eliminar_peores_caminos(+NuevosCaminos, +CaminosSinElMejor, -CaminosActualizados)
% Toma dos listas de caminos, una con los nuevos caminos generados a partir del ultimo mejor y otra con todos los demas caminos de la frontera
% Elimina de la lista de los caminos de la frontera los que tengan mismo header que un nuevo camino pero con peor costo
% Sino descarta el camino nuevo con mismo header pero mayor costo que un camino de la frontera
% Si el nuevo camino no comparte header con ningun camino de la frontera, entonces se lo agrega directamente
eliminar_peores_caminos([], CaminosViejos, CaminosViejos).

eliminar_peores_caminos([path(CostoNuevo,[Header|RestoNuevo])|RestoCaminosNuevos], CaminosViejos, [path(CostoNuevo,[Header|RestoNuevo])|RestoCaminosFiltrados]):-
	member(path(CostoViejo,[Header|RestoViejo]), CaminosViejos),
	CostoNuevo < CostoViejo,
	delete(path(CostoViejo,[Header|RestoViejo]), CaminosViejos, CaminosViejosSinPeor),
	eliminar_peores_caminos(RestoCaminosNuevos, CaminosViejosSinPeor, RestoCaminosFiltrados).

eliminar_peores_caminos([path(_CostoNuevo,[Header|_RestoNuevo])|RestoCaminosNuevos], CaminosViejos, CaminosFiltrados):-
	member(path(_CostoViejo,[Header|_RestoViejo]), CaminosViejos),
	eliminar_peores_caminos(RestoCaminosNuevos, CaminosViejos, CaminosFiltrados).

eliminar_peores_caminos([CaminoNuevo|RestoCaminosNuevos], CaminosViejos, [CaminoNuevo|RestoCaminosFiltrados]):-
	eliminar_peores_caminos(RestoCaminosNuevos, CaminosViejos, RestoCaminosFiltrados).

% cambiar_costos(+Caminos, -CaminosActualizados)
% Actualiza los costos de cada camino nuevo, sumandole al costo actual, el costo del nuevo header del camino
cambiar_costos([], [], _).
cambiar_costos([path(CostoActual,[Header1|RestoActual])|RestoActual1], [path(NuevoCosto,[Header1|RestoActual])|RestoActual2], Ady):-
	member([Header1,CostoAdy], Ady),
	NuevoCosto is CostoActual + CostoAdy,
	cambiar_costos(RestoActual1, RestoActual2, Ady).

% generar_nuevos_caminos(+Camino, -NuevosCaminos)
% Genera todos los nuevos caminos posibles a partir de los adyacentes del último elemento del camino
generar_nuevos_caminos(Visitados, path(CostoActual,[HeaderActual|RestoActual]), NuevosCaminos):-
	node(HeaderActual, _Vec, Ady),
	findall(path(CostoActual,[NuevoHeader,HeaderActual|RestoActual]),
	(member([NuevoHeader,_CostoAdy], Ady),
	node(NuevoHeader,_VecAdy,_AdyAdy),%Es para no agreagar los adyacentes desconocidos
	not(member(NuevoHeader, Visitados))), % Control de visitados
	ListaNueva),
	cambiar_costos(ListaNueva, NuevosCaminos, Ady).

generar_nuevos_caminos(_Visitados, _MejorCamino, []).

% elegir_mejor_camino(+Caminos, +Metas, -MejorCamino)
% Devuelve el mejor de los caminos para un dado conjunto de Metas
% El mejor camino será aquel que esté mas cercano a una de las metas
elegir_mejor_camino([UnicoCamino], _Metas, UnicoCamino).

elegir_mejor_camino([path(Costo1,[Header1|Resto1]),path(Costo2,[Header2|_Resto2])|RestoListas], Metas, MejorCamino):-
	node(Header1, Pos1, _Ady1),
	node(Header2, Pos2, _Ady2),
	menorDistancia(Pos1, Metas, MenorDist1),
	menorDistancia(Pos2, Metas, MenorDist2),
	MenorDist1 + Costo1 =< MenorDist2 + Costo2,
	elegir_mejor_camino([path(Costo1,[Header1|Resto1])|RestoListas], Metas, MejorCamino).

elegir_mejor_camino([path(_Costo1,[_Header1|_Resto1]),path(Costo2,[Header2|Resto2])|RestoListas], Metas, MejorCamino):-
	elegir_mejor_camino([path(Costo2,[Header2|Resto2])|RestoListas], Metas, MejorCamino).

% menorDistancia(+VectorMiNodo, +Metas, -MenorDistancia)
% Elije la menor distancia entre nuestro nodo y la lista de metas
menorDistancia(VectorMiNodo, [IdObservado], MenorDistancia):-
	node(IdObservado, VectorObservado, _AdyObs),
	distance(VectorMiNodo, VectorObservado, MenorDistancia).

menorDistancia(VectorMiNodo, [IdObservado1,IdObservado2|RestoIds], MenorDistancia):-
	node(IdObservado1, VectorObservado1, _AdyObs1),
	node(IdObservado2, VectorObservado2, _AdyObs2),
	distance(VectorMiNodo, VectorObservado1, Distancia1),
	distance(VectorMiNodo, VectorObservado2, Distancia2),
	Distancia1 =< Distancia2,
	menorDistancia(VectorMiNodo, [IdObservado1|RestoIds], MenorDistancia).

menorDistancia(VectorMiNodo, [IdObservado1,IdObservado2|RestoIds], MenorDistancia):-
	node(IdObservado1, VectorObservado1, _AdyObs1),
	node(IdObservado2, VectorObservado2, _AdyObs2),
	distance(VectorMiNodo, VectorObservado1, Distancia1),
	distance(VectorMiNodo, VectorObservado2, Distancia2),
	Distancia1 > Distancia2,
	menorDistancia(VectorMiNodo, [IdObservado2|RestoIds], MenorDistancia).

agregarMove([],[]).
agregarMove([X|Xs],[move(X)|XsConMove]):-agregarMove(Xs,XsConMove).
