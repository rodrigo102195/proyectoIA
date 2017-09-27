%% Player-Agent Template
%

:- [disable_writes].

:- [ag_primitives, module_beliefs_update, module_path_finding, extras_for_agents].

:- consult(extras_meta_preds).

:- dynamic intention/1, plan/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%           AGENT	         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%       EXECUTION CYCLE	   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


run:-
      get_percept(Percept),

      %writeln(Percept),

      update_beliefs(Percept),

      display_ag, nl,

      decide_action(Action),

      do_action(Action),

      write('Action: '), writeln(Action), nl, nl,

      run.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%        DELIBERATION      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decide_action(Action):-
  at([agent, me], MyNode),
  at([inn, _IdEnt], MyNode),
  property([agent, me], life, MyLife),
  MyLife < 450,
  Action = noop.

decide_action(Action):-
	at([agent, me], MyNode),
	at([gold, GName], MyNode),
	write('Encontró un tesoro: '), write(GName), write('!!!'),nl,
  write('voy a intentar tomarlo...'),nl,
  Action = pickup([gold, GName]).

decide_action(Action):-
	atPos([agent, me], MyPos),
	atPos([agent, Target], TPos),
	Target \= me,
	property([agent, Target], life, TLife),
	TLife > 0,
	pos_in_attack_range(MyPos, TPos),
	Action = attack([agent, Target]).

decide_action(Action):-
  plan([SiguienteNodo|[]]),
  write('Primer caso de A*'), nl,
  write('El plan es: '), write([SiguienteNodo|[]]), nl,
  retractall(plan(_)),
  retractall(intention(_)),
  Action = move(SiguienteNodo),
  write('La accion a realizar es: '), write(Action), nl.

decide_action(Action):-
  plan([SiguienteNodo|Resto]),
  write('Segundo caso de A*'), nl,
  write('El plan es: '), write([SiguienteNodo|Resto]), nl,
  retractall(plan(_)),
  assert(plan(Resto)),
  Action = move(SiguienteNodo),
  write('La accion a realizar es: '), write(Action), nl.

decide_action(Action):-
  property([agent, me], life, MyLife),
  MyLife =< 80,
  findall(IdNodo, at([inn,_IdEnt],IdNodo), Metas),
  buscar_plan_desplazamiento(Metas, Plan, Destino),
  write('Tercer caso de A* (ir a una posada)'), nl,
  write('Metas: '), writeln(Metas), nl,
  assert(plan(Plan)),
  write('El nuevo plan es: '), write(Plan), nl,
  assert(intention(Destino)),
  decide_action(Action),
  write('La accion a realizar es: '), write(Action), nl.

decide_action(Action):-
  findall(IdNodo, at([gold,_IdEnt],IdNodo), Metas),
  buscar_plan_desplazamiento(Metas, Plan, Destino),
  write('Tercer caso de A* (ir a un tesoro)'), nl,
  write('Metas: '), writeln(Metas), nl,
  assert(plan(Plan)),
  write('El nuevo plan es: '), write(Plan), nl,
  assert(intention(Destino)),
  decide_action(Action),
  write('La accion a realizar es: '), write(Action), nl.

decide_action(Action):-
  write('Movimiento aleatorio'), nl,
	at([agent, me], MyNode),
	findall(Node, ady(MyNode, Node), PossibleDestNodes),
	random_member(DestNode, PossibleDestNodes), % Selecciona aleatoriamente una posición destino.
	Action = move(DestNode).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%        AGENT SETUP       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%      & REGISTRATION      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- dynamic ag_name/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% start_ag
%
% Solicita la registración al juego, y recuerda su nombre.


start_ag:- AgName = template,
           agent_init(AgName),
           assert(ag_name(AgName)),
	   	   agent_reset,
           connect,
           run,
           disconnect.

s:- start_ag.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% start_ag_instance(+InstanceID)
%
% Solicita la registración al juego de una instancia, y recuerda su
% nombre, que será el de la versión original seguido del InstanceID
% entre paréntesis.


start_ag_instance(InstanceID):-
                    AgClassName = template,
                    AgInstanceName =.. [AgClassName, InstanceID],
		    agent_init(AgInstanceName),
		    assert(ag_name(AgInstanceName)),
		    agent_reset,
		    connect,
		    run,
		    disconnect.

si(InstanceID):- start_ag_instance(InstanceID).
