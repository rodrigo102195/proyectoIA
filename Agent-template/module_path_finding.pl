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
	at([agent,me],MyPos),write('mi posicion es '),writeln(MyPos),write('las metas son: '),writeln(Metas),not(Metas=[]) ,write('y'),a_estrella_cascara([[0,MyPos]],Metas,PlanReverso,Destino,[]),write('j'), reverse(PlanReverso,[_MiPosicion|Plan]),write('h').



a_estrella_cascara(Caminos,Metas,Plan,Destino,Visitados):-
	%write('Ultimo camino en cascara: '), write(UltimoMejorCamino), nl,
	elegir_mejor_camino(Caminos,Metas,[Costo,Header|RestoCamino]), %Se podría mejorar
	%write('Mejor Camino en cascara: '), write(MejorCamino), nl,
	%UltimoMejorCamino \= MejorCamino, %Debería agregar la lista de visitados y con eso chequeo ciclos y visitados.. con eso ya estaría
	a_estrella(Caminos,Metas,Plan,Destino,[Costo,Header|RestoCamino],[Header|Visitados]).

	%a_estrella(+Caminos,+Metas,-Plan,-Destino,+MejorCamino)
	%Caso base: Si el nodo que visito contiene a la meta entonces devuelvo el plan
	a_estrella(_Caminos,Metas,[Header|RestoCamino],Header,[_Costo,Header|RestoCamino],_Visitados):-
	member(Header,Metas).

%Caso Recursivo:
%	Elijo el mejor Camino, lo elimino de la lista de Caminos
% Agrego los nuevos caminos producto de mirar los adyacentes del mejor camino
%Llamo al a_estrella con los caminos sin el mejor camino mas los nuevos caminos
a_estrella(Caminos,Metas,Plan,Destino,MejorCamino,Visitados):-
	%write('Mejor Camino 1: '), write(MejorCamino), nl,
	delete(Caminos,MejorCamino,CaminosSinElMejor),
	%write('Mejor Camino 2: '), write(MejorCamino), nl,
	generar_nuevos_caminos(CaminosSinElMejor,Visitados,MejorCamino,NuevosCaminos),
	filtrar_caminos(NuevosCaminos, CaminosSinElMejor, CaminosActualizados),
	%append(NuevosCaminos,CaminosSinElMejor,CaminosActualizados),
	%write('Mejor Camino en salida: '), write(MejorCamino), nl,
	a_estrella_cascara(CaminosActualizados,Metas,Plan,Destino,Visitados).

filtrar_caminos([], CaminosViejos, CaminosViejos).

filtrar_caminos([[CostoNuevo,Header|RestoNuevo]|RestoCaminosNuevos], CaminosViejos, [[CostoNuevo,Header|RestoNuevo]|RestoCaminosFiltrados]):-
	member([CostoViejo,Header|RestoViejo], CaminosViejos),
	CostoNuevo < CostoViejo,
	delete([CostoViejo,Header|RestoViejo], CaminosViejos, CaminosViejosSinPeor),
	filtrar_caminos(RestoCaminosNuevos, CaminosViejosSinPeor, RestoCaminosFiltrados).

	filtrar_caminos([[_CostoNuevo,Header|_RestoNuevo]|RestoCaminosNuevos], CaminosViejos, CaminosFiltrados):-
		member([_CostoViejo,Header|_RestoViejo], CaminosViejos),
	  filtrar_caminos(RestoCaminosNuevos, CaminosViejos, CaminosFiltrados).

 filtrar_caminos([CaminoNuevo|RestoCaminosNuevos], CaminosViejos, [CaminoNuevo|RestoCaminosFiltrados]):-
		filtrar_caminos(RestoCaminosNuevos, CaminosViejos, RestoCaminosFiltrados).
%sacar_caminos_con_peor_costo([[CostoNuevo,Header|RestoNuevo]|RestoCaminosNuevos], CaminosViejos, [[CostoNuevo,Header|RestoNuevo]|RestoCaminosFiltrados]):-
	%member([CostoViejo,Header|RestoViejo], CaminosViejos),
	%delete([CostoViejo,Header|RestoViejo], CaminosViejos, CaminosViejosSinPeor),
	%sacar_caminos_con_peor_costo(RestoCaminosNuevos, CaminosViejosSinPeor, RestoCaminosFiltrados).

%sacar_caminos_con_peor_costo([CaminoNuevo|RestoCaminosNuevos], CaminosViejos, [CaminoNuevo|RestoCaminosFiltrados]):-
	%sacar_caminos_con_peor_costo(RestoCaminosNuevos, CaminosViejos, RestoCaminosFiltrados).

%filtrar_caminos([[_CostoNuevo,Header|_RestoNuevo]|RestoCaminosNuevos], CaminosViejos, CaminosFiltrados):-
	%member([_CostoViejo,Header|_RestoViejo], CaminosViejos),
	%filtrar_caminos(RestoCaminosNuevos, CaminosViejos, CaminosFiltrados).

%filtrar_caminos([[_CostoNuevo,Header|_RestoNuevo]|RestoCaminosNuevos], CaminosViejos, CaminosFiltrados):-
	%member(UnCaminoViejo, CaminosViejos),
	%member(Header, UnCaminoViejo),
	%filtrar_caminos(RestoCaminosNuevos, CaminosViejos, CaminosFiltrados).



eliminar_caminos_viejos([],_CaminosNuevos,[]).
%Si nosotros tenemos un camino viejo que tiene la misma cabeza que algun nuevo camino entonces el camino viejo debe dejar de existir
%eliminar_caminos_viejos([[CostoCaminoViejo,HeaderCaminoViejo|RestoCaminoViejo]|RestoCaminosViejos],CaminosNuevos,Z):-
	%member([CostoCaminoNuevo,HeaderCaminoNuevo|RestoCaminoNuevo],CaminosNuevos),HeaderCaminoNuevo=HeaderCaminoViejo,
	%eliminar_caminos_viejos(RestoCaminosViejos,CaminosNuevos,Z).

%En este caso el header del camino viejo no se corresponde con ningun header de los caminos nuevos, por lo que no debemos ignorarlo
eliminar_caminos_viejos([HeaderCaminosViejos|RestoCaminosViejos],CaminosNuevos,[HeaderCaminosViejos|Z]):-
	eliminar_caminos_viejos(RestoCaminosViejos,CaminosNuevos,Z).

%Terminamos de recorrer los caminos nuevos
limpiar_caminos(_Caminos,[],[]). %Termine de limpiar los caminos
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

%cambiar_costos(+Caminos,-CaminosActualizados)
%Actualiza los costos de cada camino por el nuevo nodo agregado
cambiar_costos([],[],_).
cambiar_costos([[CostoActual,Header1|RestoActual]|RestoActual1],[[NuevoCosto,Header1|RestoActual]|RestoActual2],Ady):-
	member([Header1,CostoAdy],Ady),
	NuevoCosto is CostoActual+CostoAdy,
	cambiar_costos(RestoActual1,RestoActual2,Ady).


%generar_nuevos_caminos(+Camino,-NuevosCaminos)
%Te genera todos los nuevos caminos posibles a partir de los adyacentes del último elemento del camino
generar_nuevos_caminos(Caminos,Visitados,[CostoActual,HeaderActual|RestoActual],NuevosCaminos):-
	node(HeaderActual,_Vec,Ady),%write('Visitados es: '),write(Visitados),nl,
	findall([CostoActual,NuevoHeader,HeaderActual|RestoActual],
	(member([NuevoHeader,CostoAdy],Ady), %Fijarse si se podría haber agregado el nuevo costo acá
	not(member(NuevoHeader,Visitados))), %Control de visitados
	%not(camino_con_peor_costo(NuevoHeader,CostoAdy,CostoActual,Caminos))),%Que no haya otro nodo en la frontera que tenga un menor costo con el mismo header
	ListaNueva),
	cambiar_costos(ListaNueva,NuevosCaminos,Ady).

generar_nuevos_caminos(_Caminos,_Visitados,_MejorCamino,[]).

camino_con_peor_costo(NuevoHeader,CostoAdy,CostoActual,Caminos):-
	member([[CostoViejo,NuevoHeader|RestoViejo]|RestoCaminoViejo],Caminos),
	NuevoCosto is CostoAdy+CostoActual,
	NuevoCosto>CostoViejo,write('Camino peor'),nl.
%Si no hay ningun
%camino_con_peor_costo(_NuevoHeader,_CostoAdy,_CostoRestante,[]).

%camino_con_peor_costo(NuevoHeader,CostoAdy,CostoRestante,[[_CostoActual,HeaderActual|_RestoActual]|RestoCaminos]):-
	%write('Estoy en 1'),nl,
	%not(NuevoHeader=HeaderActual),
	%camino_con_peor_costo(NuevoHeader,CostoAdy,CostoRestante,RestoCaminos).

%camino_con_peor_costo(NuevoHeader,CostoAdy,CostoRestante,[[CostoActual,_HeaderActual|_RestoActual]|RestoCaminos]):-
	%write('Estoy en 2'),nl,
	%NuevoCosto is CostoRestante+CostoAdy,
	%NuevoCosto<CostoActual,
	%camino_con_peor_costo(NuevoHeader,CostoAdy,CostoRestante,RestoCaminos).





%elegir_mejor_camino(+Caminos,+Metas,-MejorCamino)
%Devuelve el mejor de los caminos para un dado conjunto de Metas
%El mejor camino será aquel que esté mas cercano a una de las metas
elegir_mejor_camino([UnicoCamino],_Metas,UnicoCamino).

elegir_mejor_camino([[Costo1,Header1|Resto1],[Costo2,Header2|_Resto2]|RestoListas],Metas,MejorCamino):-
	node(Header1,Pos1,_Ady), node(Header2,Pos2,_Ady2),
	menorDistancia(Pos1,Metas,MenorDist1),
	menorDistancia(Pos2,Metas,MenorDist2),
	MenorDist1 + Costo1 =< MenorDist2 + Costo2,
	elegir_mejor_camino([[Costo1,Header1|Resto1]|RestoListas],Metas,MejorCamino).

elegir_mejor_camino([[Costo1,Header1|_Resto1],[Costo2,Header2|Resto2]|RestoListas],Metas,MejorCamino):-
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
