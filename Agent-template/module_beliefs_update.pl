:- module(beliefs_update,
	  [
	    update_beliefs/1,
	    time/1,
	    node/3,
	    at/2,
	    atPos/2,
	    has/2,
	    entity_descr/2
	  ]).

:- dynamic time/1, node/3, at/2, atPos/2, has/2, entity_descr/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% update_beliefs(+Perc)
%
% IMPORTANTE: Debe exportarse todo predicado dinámico (creencia)
% manipulado por la actualización de creencias, para que puedan ser
% consultadon por el resto del código del agente.
%

update_beliefs(Perc):-

	% El agente olvida todo lo que recordaba
	%retractall(time(_)),
	%retractall(at(_,_)),
	%retractall(atPos(_,_)),
	%retractall(has(_,_)),
	%retractall(entity_descr(_,_)),
	%retractall(node(_,_,_)),

	% y recuerda lo que percibió
	%forall(member(Rel, Perc), assert(Rel)).
	
	% Agregar los nodos vistos en el rango de visión
	% Primero eliminamos lo que no necesitamos
	retractall(time(_)),
	retractall(atPos(_,_)),
	member(time(T), Perc), assert(time(T)),
	%write('El tiempo es de: '),write(T),nl,
	forall(member(at(Ent,Id), Perc), updateAt(at(Ent,Id))),
	%write('Ahora van los at:'),forall(at(Ent,Id),write(at(Ent,Id))),nl,
	forall(member(atPos(Id,Vec), Perc), assert(atPos(Id,Vec))),
	%write('Las posiciones son:'),forall(atPos(Ent,Pos),write(atPos(Ent,Pos))),nl,
	forall(member(has(Id,Item), Perc), updateHas(has(Id,Item))),
	%write('Ahora van los has:'),forall(has(Id,Item),write(has(Id,Item))),nl,
	forall(member(entity_descr(Id,Desc), Perc), assert(entity_descr(Id,Desc))),
	%write('Ahora van las desc:'),forall(entity_descr(Ent,Id),write(entity_descr(Ent,Id))),nl,
	
	% Agregamos los nodos que no tenemos
	forall(member(node(Id,Vec,Ady), Perc), addNode(node(Id,Vec,Ady))).
	%write('Ahora van los nodos:'),forall(node(Id,Vec,Ady),write(node(Id,Vec,Ady))),nl.

addNode(Fact):- not(Fact), assert(Fact).
addNode(_).

% En el caso de que un item haya estado en el mapa, y ahora se encuentra dentro de una entidad
% En este caso se borra del mapa y se agrega en el has de esa entidad
updateHas(has([OwnerType,OwnerId],[ItemType,ItemId])):-
	at([_,ItemId],_),retract(at([_,ItemId],_)),assert(has([OwnerType,OwnerId],[ItemType,ItemId])).

% En el caso de que un item haya estado en una entidad, y ahora se encuentra dentro de otra entidad
% En este caso se borra de la vieja entidad y se agrega en el has de la nueva entidad	
updateHas(has([OwnerType,OwnerId],[ItemType,ItemId])):-
	has(_,[_,ItemId]),retract(has(_,[_,ItemId])),assert(has([OwnerType,OwnerId],[ItemType,ItemId])).

% En caso de que sea un nuevo item, se agrega directamente
updateHas(has([OwnerType,OwnerId],[ItemType,ItemId])):-
	assert(has([OwnerType,OwnerId],[ItemType,ItemId])).

% Si alguien lo tenía, pero ahora está en el mapa, entonces actualizamos para que solo este en el mapa.	
updateAt(at([Type,Id],NodeId)):-
	has(_,[_,Id]),retract(has(_,[_,Id])),assert(at([Type,Id],NodeId)).

% Si estaba en una parte del mapa y ahora esta en otro, se actualiza su posicion en el mapa	
updateAt(at([Type,Id],NodeId)):-
	at([_,Id],_),retract(at([_,Id],_)),assert(at([Type,Id],NodeId)).

% Si es un item nuevo, se lo agrega su posicion
updateAt(at([Type,Id],NodeId)):-
	assert(at([Type,Id],NodeId)).