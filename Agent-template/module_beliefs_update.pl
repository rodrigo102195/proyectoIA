:- module(beliefs_update,
	  [
	    update_beliefs/1,
	    time/1,
	    node/3,
	    at/2,
	    atPos/2,
	    has/2,
	    entity_descr/2,
			lastSeen/2
	  ]).

:- dynamic time/1, node/3, at/2, atPos/2, has/2, entity_descr/2, lastSeen/2.

% lastSeen(Id,Time)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% update_beliefs(+Perc)
%
% IMPORTANTE: Debe exportarse todo predicado dinámico (creencia)
% manipulado por la actualización de creencias, para que puedan ser
% consultadon por el resto del código del agente.
%

% KB = Knowledge Base (Base de conocimiento)

update_beliefs(Perc):-

	% Se elimina de la KB la información que no es necesaria recordar
	retractall(time(_)),
	retractall(entity_descr([agent,me],_)),

	% Se agrega a la KB la información nueva, actualizando la antigua si es necesario
	member(time(T), Perc), assert(time(T)),
	forall(member(entity_descr(Id,Desc), Perc), updateDesc(entity_descr(Id,Desc))),
	forall(member(at(Ent,NodeId), Perc), updateAt(at(Ent,NodeId), Perc, T)),
	forall(member(has(Owner,Item), Perc), updateHas(has(Owner,Item), T)),
	cleanMissingAt(Perc),
	cleanMissingHas(Perc),
	cleanUnreachableAt(),

	% Se agregan los nodos en el rango de visión que no estan en la KB
	forall(member(node(Id,Vec,Ady), Perc), addNode(node(Id,Vec,Ady))).

% Si el nodo no está en la KB, entonces lo agrega a la misma
addNode(Fact):- not(Fact), assert(Fact).
addNode(_).

% Actualiza las descripciones de las entidades
updateDesc(entity_descr(Id,Desc)):- entity_descr(Id,_), retract(entity_descr(Id,_)), assert(entity_descr(Id,Desc)).
updateDesc(entity_descr(Id,Desc)):- assert(entity_descr(Id,Desc)).

% Se eliminan de la KB los objetos que se perdió se vista
cleanMissingAt(Perc):- forall(member(node(Id,_,_), Perc), cleanMissingAtAux(Id, Perc)).

cleanMissingAtAux(NodeId, Perc):-
	at([_,EntId],NodeId),
	not(member(at([_,EntId],NodeId), Perc)),
	retract(at([_,EntId],_)),
	retract(atPos([_,EntId],_)),
	retract(lastSeen(EntId,_)).

cleanMissingAtAux(_,_).

% Se eliminan de la KB los objetos inalcanzables por el agente
cleanUnreachableAt():- forall(unreachable(NodeId), cleanUnreachableAtAux(NodeId)).

cleanUnreachableAtAux(NodeId):-
	at([_,EntId],NodeId),
	retract(at([_,EntId],NodeId)),
	retract(lastSeen(EntId,_)),
	forall(has([_,EntId],[_,ItemId]), retract(lastSeen(ItemId,_))),
	retractall(has([_,EntId],_)),
	retract(unreachable(NodeId)).

cleanUnreachableAtAux(_).

% Se eliminan de la KB los objetos que ya no son poseidos
cleanMissingHas(Perc):- forall(member(node(Id,_,_), Perc), cleanMissingHasAux(Id, Perc)).

cleanMissingHasAux(NodeId, Perc):-
	member(at([_,OwnerId],NodeId), Perc),
	forall(has([_,OwnerId],Item), checkHasExistence(has([_,OwnerId],Item), Perc)).

cleanMissingHasAux(_,_).

checkHasExistence(has([_,OwnerId],[_,ItemId]), Perc):-
	not(member(has([_,OwnerId],[_,ItemId]), Perc)),
	retract(has([_,OwnerId],[_,ItemId])),
	retract(lastSeen(ItemId,_)).

checkHasExistence(_,_).

% Si el objeto se encontraba en posesión de una entidad y ahora esta en el mapa, entonces se actualiza esa información en la KB
updateAt(at([Type,Id],NodeId), Perc, Time):-
	has(_,[_,Id]),
	retract(has(_,[_,Id])),
	retract(lastSeen(Id,_)),
	assert(at([Type,Id],NodeId)),
	member(atPos([_,Id],Vec), Perc),
	assert(atPos([Type,Id],Vec)),
	assert(lastSeen(Id,Time)).

% Si el objeto se encontraba en una parte del mapa y ahora esta en otra, entonces se actualiza esa información en la KB
updateAt(at([Type,Id],NodeId), Perc, Time):-
	at([_,Id],_),
	retract(at([_,Id],_)),
	retract(atPos([_,Id],_)),
	retract(lastSeen(Id,_)),
	assert(at([Type,Id],NodeId)),
	member(atPos([_,Id],Vec), Perc),
	assert(atPos([Type,Id],Vec)),
	assert(lastSeen(Id,Time)).

% Si es un item desconocido, entonces se lo agrega a la KB
updateAt(at([Type,Id],NodeId), Perc, Time):-
	assert(at([Type,Id],NodeId)),
	member(atPos([_,Id],Vec), Perc),
	assert(atPos([Type,Id],Vec)),
	assert(lastSeen(Id,Time)).

% Si el objeto se encontraba en el mapa y ahora esta en posesión de una entidad, entonces se actualiza esa información en la KB
updateHas(has([OwnerType,OwnerId],[ItemType,ItemId]), Time):-
	at([_,ItemId],_),
	retract(at([_,ItemId],_)),
	retract(atPos([_,ItemId],_)),
	retract(lastSeen(ItemId,_)),
	assert(has([OwnerType,OwnerId],[ItemType,ItemId])),
	assert(lastSeen(ItemId,Time)).

% Si el objeto se encontraba en posesión de una entidad y ahora esta en posesión de otra, entonces se actualiza esa información en la KB
updateHas(has([OwnerType,OwnerId],[ItemType,ItemId]), Time):-
	has(_,[_,ItemId]),
	retract(has(_,[_,ItemId])),
	retract(lastSeen(ItemId,_)),
	assert(has([OwnerType,OwnerId],[ItemType,ItemId])),
	assert(lastSeen(ItemId,Time)).

% Si es un item desconocido, entonces se lo agrega a la KB
updateHas(has([OwnerType,OwnerId],[ItemType,ItemId]), Time):-
	assert(has([OwnerType,OwnerId],[ItemType,ItemId])),
	assert(lastSeen(ItemId,Time)).
