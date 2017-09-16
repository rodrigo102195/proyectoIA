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

% lastSeen(Id,Time)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% update_beliefs(+Perc)
%
% IMPORTANTE: Debe exportarse todo predicado dinamico (creencia)
% manipulado por la actualizacion de creencias, para que puedan ser
% consultadon por el resto del codigo del agente.
%

% KB = Knowledge Base (Base de conocimiento)

update_beliefs(Perc):-

	% Se elimina de la KB la informacion que no es necesaria recordar
	retractall(time(_)),
	retractall(entity_descr([agent,me],_)),

	% Se agrega a la KB la informacion nueva, actualizando la antigua si es necesario
	member(time(T), Perc), assert(time(T)),
	forall(member(entity_descr(Id,Desc), Perc), updateDesc(entity_descr(Id,Desc))),
	forall(member(at(Ent,NodeId), Perc), updateAt(at(Ent,NodeId), Perc)),
	forall(member(has(Owner,Item), Perc), updateHas(has(Owner,Item))),

	% Se agregan los nodos en el rango de vision que no estan en la KB
	forall(member(node(Id,Vec,Ady), Perc), addNode(node(Id,Vec,Ady))).

% Si el nodo no esta en la KB, entonces lo agrega a la misma
addNode(Fact):- not(Fact), assert(Fact).
addNode(_).

% Actualiza las descripciones de las entidades
updateDesc(Fact):- not(Fact), assert(Fact).
updateDesc(_).

% Si el objeto se encontraba en posesion de una entidad y ahora esta en el mapa, entonces se actualiza esa informacion en la KB
updateAt(at([Type,Id],NodeId), Perc):-
	has(_,[_,Id]),
	retract(has(_,[_,Id])),
	assert(at([Type,Id],NodeId)),
	member(atPos([_,Id],Vec), Perc),
	assert(atPos([Type,Id],Vec)).

% Si el objeto se encontraba en una parte del mapa y ahora esta en otra, entonces se actualiza esa informacion en la KB
updateAt(at([Type,Id],NodeId), Perc):-
	at([_,Id],_),
	retract(at([_,Id],_)),
	retract(atPos([_,Id],_)),
	assert(at([Type,Id],NodeId)),
	member(atPos([_,Id],Vec), Perc),
	assert(atPos([Type,Id],Vec)).

% Si es un item desconocido, entonces se lo agrega a la KB
updateAt(at([Type,Id],NodeId), Perc):-
	assert(at([Type,Id],NodeId)),
	member(atPos([_,Id],Vec), Perc),
	assert(atPos([Type,Id],Vec)).

% Si el objeto se encontraba en el mapa y ahora esta en posesion de una entidad, entonces se actualiza esa informacion en la KB
updateHas(has([OwnerType,OwnerId],[ItemType,ItemId])):-
	at([_,ItemId],_),
	retract(at([_,ItemId],_)),
	retract(atPos([_,ItemId],_)),
	assert(has([OwnerType,OwnerId],[ItemType,ItemId])).

% Si el objeto se encontraba en posesion de una entidad y ahora esta en posesion de otra, entonces se actualiza esa informacion en la KB
updateHas(has([OwnerType,OwnerId],[ItemType,ItemId])):-
	has(_,[_,ItemId]),
	retract(has(_,[_,ItemId])),
	assert(has([OwnerType,OwnerId],[ItemType,ItemId])).

% Si es un item desconocido, entonces se lo agrega a la KB
updateHas(has([OwnerType,OwnerId],[ItemType,ItemId])):-
	assert(has([OwnerType,OwnerId],[ItemType,ItemId])).
