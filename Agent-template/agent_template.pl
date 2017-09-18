%% Player-Agent Template
%

:- [disable_writes].

:- [ag_primitives, module_beliefs_update, module_path_finding, extras_for_agents].

:- consult(extras_meta_preds).

:- dynamic intention/1, plan/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%           AGENT	       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
	at([gold, GName], MyNode),
	write('Encontr� un tesoro: '), write(GName), write('!!!'),nl,
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
  write('siiiii'),plan(H),
  nl,write('el plan es111: '),write(H),write('jijijiji'),nl,nl,
    plan([SiguienteNodo|[]]),
    write('El plan existe 1'),nl,
    retractall(plan(_)),write('k'),nl,
    retractall(intention(_)),write('kk'),nl,
    Action = move(SiguienteNodo),write('La accion a realizar es: '),write(Action).

decide_action(Action):-
 write('claaaaroo'),plan(H),
  nl,write('el plan es222: '),write(H),write('jijijiji'),nl,nl,
  plan([SiguienteNodo|Resto]),
  write('El plan existe 2'),nl,
  retractall(plan(_)),write('k'),nl,
  assert(plan(Resto)),write('k'),nl,
  Action = move(SiguienteNodo),write('La accion a realizar es: '),write(Action).

decide_action(Action):-
  write('a'),nl,

  findall(IdNodo,at([gold,_IdEnt],IdNodo),Metas),
  write('b'),nl,writeln(Metas),nl,
  buscar_plan_desplazamiento(Metas,Plan,Destino),
  retractall(plan(_)),
  retractall(intention(_)),
  assert(plan(Plan)),
  write('d'),nl,
  assert(intention(Destino)),
  write('e'),nl,plan(H),
  write('el plan a realizar es: '),write(plan(H)),nl,
  decide_action(Action), write('f'),nl.

decide_action(Action):-
	at([agent, me], MyNode),
	findall(Node, ady(MyNode, Node), PossibleDestNodes),
	random_member(DestNode, PossibleDestNodes), % Selecciona aleatoriamente una posici�n destino.
	Action = move(DestNode),write('La accion a realizar essss '), write(Action).



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
% Solicita la registraci�n al juego, y recuerda su nombre.


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
% Solicita la registraci�n al juego de una instancia, y recuerda su
% nombre, que ser� el de la versi�n original seguido del InstanceID
% entre par�ntesis.


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
