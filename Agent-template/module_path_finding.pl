:- module(path_finding,
	  [
	    buscar_plan_desplazamiento/3
	  ]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% buscar_plan_desplazamiento(+Metas, -Plan, -Destino)
%Realiza el plan óptimo para un dado conjunto de metas
%Metas: Lista de id de los nodos meta
%Plan: Lista de id de los nodos que deberíamos tomar
%Destino: Es el id del nodo destino al cual nos dirigimos
buscar_plan_desplazamiento(Metas, Plan, Destino):-
	at([agent,me],MyPos),write('mi posicion es '),writeln(MyPos),write('las metas son: '),writeln(Metas),not(Metas=[]) ,write('y'),a_estrella([[0,MyPos]],Metas,PlanReverso,Destino),!,write('j'), reverse(PlanReverso,[_MiPosicion|Plan]),write('h').

%a_estrella(+Caminos,+Metas,-Plan,-Destino)
%Caso base: Si existe un camino tal que contenga a una de las metas entonces ya lo encontré.
a_estrella(Caminos,Metas,[Header|RestoCamino],Header):-
	member([C,Header|RestoCamino],Caminos),
	member(Header,Metas), elegir_mejor_camino(Caminos,Metas,[C1|_]),
	C1==C.

%Caso Recursivo:
%	Elijo el mejor Camino, lo elimino de la lista de Caminos
% Agrego los nuevos caminos producto de mirar los adyacentes del mejor camino
%Llamo al a_estrella con los caminos sin el mejor camino mas los nuevos caminos
a_estrella(Caminos,Metas,Plan,Destino):-
	elegir_mejor_camino(Caminos,Metas,MejorCamino),
	delete(Caminos,MejorCamino,CaminosSinElMejor),
	generar_nuevos_caminos(MejorCamino,NuevosCaminos),
	write('El costo del mejor camino es mejor camino es: '),MejorCamino=[H|_T],write(H),nl,
	limpiar_caminos(Caminos, NuevosCaminos,NuevosCaminosSinRepetidos), %Para que los nuevos caminos que pasen por un mismo nodo que los caminos viejos y sean mas costosos no aparezcan
	write('los nuevos caminos sin repetir son: '),write(NuevosCaminosSinRepetidos),nl,
	eliminar_caminos_viejos(CaminosSinElMejor,NuevosCaminosSinRepetidos,CaminosSinElMejorSinViejos),%Eliminar de los caminos viejos todos los caminos que contengan algun nodo de los nuevos nodos agregados en los caminos nuevos
	append(CaminosSinElMejorSinViejos,NuevosCaminosSinRepetidos,CaminosActualizados),
	a_estrella(CaminosActualizados,Metas,Plan,Destino).

eliminar_caminos_viejos([],_CaminosNuevos,[]):-nl,!.
%Si nosotros tenemos un camino viejo que tiene la misma cabeza que algun nuevo camino entonces el camino viejo debe dejar de existir
eliminar_caminos_viejos([[CostoCaminoViejo,HeaderCaminoViejo|RestoCaminoViejo]|RestoCaminosViejos],CaminosNuevos,Z):-
	member([CostoCaminoNuevo,HeaderCaminoNuevo|RestoCaminoNuevo],CaminosNuevos),HeaderCaminoNuevo=HeaderCaminoViejo,
	eliminar_caminos_viejos(RestoCaminosViejos,CaminosNuevos,Z).

%En este caso el header del camino viejo no se corresponde con ningun header de los caminos nuevos, por lo que no debemos ignorarlo
eliminar_caminos_viejos([HeaderCaminosViejos|RestoCaminosViejos],CaminosNuevos,[HeaderCaminosViejos|Z]):-
	eliminar_caminos_viejos(RestoCaminosViejos,CaminosNuevos,Z).

%Terminamos de recorrer los caminos nuevos
limpiar_caminos(_Caminos,[],[]):-!. %Termine de limpiar los caminos
%Si tenemos un camino viejo con el mismo header que el camino nuevo, y el camino viejo es peor entonces es necesario agregar el nuevo camino
limpiar_caminos(Caminos,[[CostoCaminoNuevo,HeaderCaminoNuevo|RestoCaminoNuevo]|ColaCaminosNuevos],[[CostoCaminoNuevo,HeaderCaminoNuevo|RestoCaminoNuevo]|Z]):-
	forall(member([CostoCamino,HeaderCaminoNuevo|_RestoCamino],Caminos),CostoCaminoNuevo<CostoCamino),
	limpiar_caminos(Caminos,ColaCaminosNuevos,Z). %para cada uno de los caminos me fijo si el nuevo nodo del camino nuevo existe en un camino viejo y además este nuevo caminos es mejor

%Si tenemos un camino nuevo con un header que no es miembro de ninguno de los caminos viejos entonces es necesario agregarlo
limpiar_caminos(Caminos,[[CostoCaminoNuevo,HeaderCaminoNuevo|RestoCaminoNuevo]|ColaCaminosNuevos],[[CostoCaminoNuevo,HeaderCaminoNuevo|RestoCaminoNuevo]|Z]):-
	forall(member([_CostoCamino|RestoCamino],Caminos),not(member(HeaderCaminoNuevo,RestoCamino))),
	limpiar_caminos(Caminos,ColaCaminosNuevos,Z).

%Sino quiere decir que el camino viejo es mejor o bien el nuevo camino agregado pasa por un nodo que es parte de un camino viejo
limpiar_caminos(Caminos,[_PrimerCaminoNuevo|ColaCaminosNuevos],Z):-
		limpiar_caminos(Caminos,ColaCaminosNuevos,Z).
%generar_nuevos_caminos(+Camino,-NuevosCaminos)
%Te genera todos los nuevos caminos posibles a partir de los adyacentes del último elemento del camino
generar_nuevos_caminos([CostoActual,HeaderActual|RestoActual],NuevosCaminos):-
	node(HeaderActual,_Vec,Ady),
	findall([CostoActual,NuevoHeader,HeaderActual|RestoActual],
	(member([NuevoHeader,_Costo],Ady), %Fijarse si se podría haber agregado el nuevo costo acá
	not(member(NuevoHeader,RestoActual))), %Control de ciclos
	ListaNueva),
	cambiar_costos(ListaNueva,NuevosCaminos).

%cambiar_costos(+Caminos,-CaminosActualizados)
%Actualiza los costos de cada camino por el nuevo nodo agregado
cambiar_costos([],[]):-!.
cambiar_costos([[CostoActual,Header1,Header2|RestoActual]|RestoActual1],[[NuevoCosto,Header1,Header2|RestoActual]|RestoActual2]):-
	node(Header1,Vec1,_Ady),
	node(Header2,Vec2,_Ady2),
	distance(Vec1,Vec2,Distancia),
	NuevoCosto is CostoActual+Distancia,
	cambiar_costos(RestoActual1,RestoActual2).


%elegir_mejor_camino(+Caminos,+Metas,-MejorCamino)
%Devuelve el mejor de los caminos para un dado conjunto de Metas
%El mejor camino será aquel que esté mas cercano a una de las metas
elegir_mejor_camino([UnicoCamino],_Metas,UnicoCamino):-!. %Habría que probarlo sin la negación por falla?
elegir_mejor_camino([[Costo1,Header1|Resto1],[Costo2,Header2|_Resto2]|RestoListas],Metas,MejorCamino):-
	node(Header1,Pos1,_Ady), node(Header2,Pos2,_Ady2),
	menorDistancia(Pos1,Metas,MenorDist1),
	menorDistancia(Pos2,Metas,MenorDist2),
	MenorDist1 + Costo1 =< MenorDist2 + Costo2,
	elegir_mejor_camino([[Costo1,Header1|Resto1]|RestoListas],Metas,MejorCamino).

elegir_mejor_camino([[Costo1,Header1|_Resto1],[Costo2,Header2|Resto2]|RestoListas],Metas,MejorCamino):-
	node(Header1,Pos1,_Ady), node(Header2,Pos2,_Ady2),
	menorDistancia(Pos1,Metas,MenorDist1),
	menorDistancia(Pos2,Metas,MenorDist2),
	MenorDist1 + Costo1 > MenorDist2 + Costo2,
	elegir_mejor_camino([[Costo2,Header2|Resto2]|RestoListas],Metas,MejorCamino).


% menorDistancia(+VectorMiNodo,+Metas,-MenorDistancia)
% Elije la menor distancia entre nuestro nodo y la lista de metas
menorDistancia(VectorMiNodo,[IdObservado],MenorDistancia):-
	node(IdObservado,VectorObservado,_AdyObs),
	distance(VectorMiNodo,VectorObservado,MenorDistancia).

menorDistancia(VectorMiNodo,[IdObservado1,IdObservado2|RestoIds],MenorDistancia):-
	node(IdObservado1,VectorObservado1,_AdyObs1),
	node(IdObservado2,VectorObservado2,_AdyObs2),
	distance(VectorMiNodo,VectorObservado1,Distancia1),
	distance(VectorMiNodo,VectorObservado2,Distancia2),
	Distancia1=<Distancia2,
	menorDistancia(VectorMiNodo,[IdObservado1|RestoIds],MenorDistancia).

	menorDistancia(VectorMiNodo,[IdObservado1,IdObservado2|RestoIds],MenorDistancia):-
		node(IdObservado1,VectorObservado1,_AdyObs1),
		node(IdObservado2,VectorObservado2,_AdyObs2),
		distance(VectorMiNodo,VectorObservado1,Distancia1),
		distance(VectorMiNodo,VectorObservado2,Distancia2),
		Distancia1>Distancia2,
		menorDistancia(VectorMiNodo,[IdObservado2|RestoIds],MenorDistancia).
