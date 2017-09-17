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
	at([agent,me],MyPos),not(Metas=[]) ,a_estrella([[0,MyPos]],Metas,Plan,DestinoReverso), reverse(DestinoReverso,Destino).

%a_estrella(+Caminos,+Metas,-Plan,-Destino)
%Caso base: Si existe un camino tal que contenga a una de las metas entonces ya lo encontré.
a_estrella(Caminos,Metas,[Header|RestoCamino],Header):-
	member([_C,Header|RestoCamino],Caminos),
	member(Header,Metas).

%Caso Recursivo:
%	Elijo el mejor Camino, lo elimino de la lista de Caminos
% Agrego los nuevos caminos producto de mirar los adyacentes del mejor camino
%Llamo al a_estrella con los caminos sin el mejor camino mas los nuevos caminos
a_estrella(Caminos,Metas,Plan,Destino):-
	elegir_mejor_camino(Caminos,Metas,MejorCamino),
	delete(Caminos,MejorCamino,CaminosSinElMejor),
	generar_nuevos_caminos(MejorCamino,NuevosCaminos),
	append(CaminosSinElMejor,NuevosCaminos,CaminosActualizados),
	a_estrella(CaminosActualizados,Metas,Plan,Destino).

generar_nuevos_caminos([CostoActual,HeaderActual|RestoActual],NuevosCaminos):-
	node(HeaderActual,_Vector,)

%elegir_mejor_camino(+Caminos,+Metas,-MejorCamino)
%Devuelve el mejor de los caminos para un dado conjunto de Metas
%El mejor camino será aquel que esté mas cercano a una de las metas
elegir_mejor_camino([UnicoCamino],Metas,UnicoCamino):-!. %Habría que probarlo sin la negación por falla?
elegir_mejor_camino([[Costo1,Header1|Resto1],[Costo2,Header2|_Resto2]|RestoListas],Metas,MejorCamino):-
	node(Header1,Pos1,_Ady), node(Header2,Pos2,_Ady2),
	menorDistancia(Pos1,Metas,MenorDist1),
	menorDistancia(Pos2,Metas,MenorDist2),
	MenorDist1 + Costo1 <= MenorDist2 + Costo2,
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
	distance(VectorMiNodo,IdObservado1,Distancia1),
	distance(VectorMiNodo,IdObservado2,Distancia2),
	Distancia1<=Distancia2,
	menorDistancia(VectorMiNodo,[IdObservado1|RestoIds],MenorDistancia).

	menorDistancia(VectorMiNodo,[IdObservado1,IdObservado2|RestoIds],MenorDistancia):-
		node(IdObservado1,VectorObservado1,_AdyObs1),
		node(IdObservado2,VectorObservado2,_AdyObs2),
		distance(VectorMiNodo,IdObservado1,Distancia1),
		distance(VectorMiNodo,IdObservado2,Distancia2),
		Distancia1>Distancia2,
		menorDistancia(VectorMiNodo,[IdObservado2|RestoIds],MenorDistancia).
