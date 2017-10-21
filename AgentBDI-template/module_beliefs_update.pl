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
	retractall(time(_)),
	retractall(at(_,_)),
	retractall(atPos(_,_)),
	retractall(has(_,_)),
	retractall(entity_descr(_,_)),
	retractall(node(_,_,_)),

	% y recuerda lo que percibi�
	forall(member(Rel, Perc), assert(Rel)).

































