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
% IMPORTANTE: Debe exportarse todo predicado din�mico (creencia)
% manipulado por la actualizaci�n de creencias, para que puedan ser
% consultadon por el resto del c�digo del agente.
%

update_beliefs(Perc):-

	% El agente olvida todo lo que recordaba
	%retractall(time(_)),
	%retractall(at(_,_)),
	%retractall(atPos(_,_)),
	%retractall(has(_,_)),
	%retractall(entity_descr(_,_)),
	%retractall(node(_,_,_)),

	% y recuerda lo que percibi�
	%forall(member(Rel, Perc), assert(Rel)).

	% Agregar los nodos vistos en el rango de visi�n
	% Primero eliminamos lo que no necesitamos
	retractall(time(_)),
	retractall(entity_descr(_,_)),
	%retractall(atPos(_,_)),
	member(time(T), Perc), assert(time(T)),
	%write('El tiempo es de: '),write(T),nl,
	forall(member(entity_descr(Id,Desc), Perc), assert(entity_descr(Id,Desc))),
	%write('Ahora van las desc:'),forall(entity_descr(Ent,Id),write(entity_descr(Ent,Id))),nl,
	forall(member(at([Type,Id],NodeId), Perc), updateAt(at([Type,Id],NodeId), Perc)),
	%write('Ahora van los at:'),forall(at(Ent,Id),write(at(Ent,Id))),nl,
	%forall(member(atPos(Id,Vec), Perc), assert(atPos(Id,Vec))),
	%write('Las posiciones son:'),forall(atPos(Ent,Pos),write(atPos(Ent,Pos))),nl,
	forall(member(has(Id,Item), Perc), updateHas(has(Id,Item))),
	%write('Ahora van los has:'),forall(has(Id,Item),write(has(Id,Item))),nl,

	% Agregamos los nodos que no tenemos
	forall(member(node(Id,Vec,Ady), Perc), addNode(node(Id,Vec,Ady))).
	%write('Ahora van los nodos:'),forall(node(Id,Vec,Ady),write(node(Id,Vec,Ady))),nl.

% KB = Knowledge Base (Base de conocimiento)

% Si el nodo no esta en la KB, entonces lo agrega a la misma
addNode(Fact):- not(Fact), assert(Fact).
addNode(_).

% Si alguien lo ten�a, pero ahora est� en el mapa, entonces actualizamos para que solo este en el mapa.
updateAt(at([Type,Id],NodeId), Perc):-
	has(_,[_,Id]),
	retract(has(_,[_,Id])),
	assert(at([Type,Id],NodeId)),
	member(atPos([_,Id],Vec), Perc),
	assert(atPos([Type,Id],Vec)).

% Si estaba en una parte del mapa y ahora esta en otro, se actualiza su posicion en el mapa
updateAt(at([Type,Id],NodeId), Perc):-
	at([_,Id],_),
	retract(at([_,Id],_)),
	retract(atPos([_,Id],_)),
	assert(at([Type,Id],NodeId)),
	member(atPos([_,Id],Vec), Perc),
	assert(atPos([Type,Id],Vec)).

% Si es un item nuevo, se lo agrega su posicion
updateAt(at([Type,Id],NodeId), Perc):-
	assert(at([Type,Id],NodeId)),
	member(atPos([_,Id],Vec), Perc),
	assert(atPos([Type,Id],Vec)).

% En el caso de que un item haya estado en el mapa, y ahora se encuentra dentro de una entidad
% En este caso se borra del mapa y se agrega en el has de esa entidad
updateHas(has([OwnerType,OwnerId],[ItemType,ItemId])):-
	at([_,ItemId],_),
	retract(at([_,ItemId],_)),
	retract(atPos([_,ItemId],_)),
	assert(has([OwnerType,OwnerId],[ItemType,ItemId])).

% En el caso de que un item haya estado en una entidad, y ahora se encuentra dentro de otra entidad
% En este caso se borra de la vieja entidad y se agrega en el has de la nueva entidad
updateHas(has([OwnerType,OwnerId],[ItemType,ItemId])):-
	has(_,[_,ItemId]),
	retract(has(_,[_,ItemId])),
	assert(has([OwnerType,OwnerId],[ItemType,ItemId])).

% En caso de que sea un nuevo item, se agrega directamente
updateHas(has([OwnerType,OwnerId],[ItemType,ItemId])):-
	assert(has([OwnerType,OwnerId],[ItemType,ItemId])).
