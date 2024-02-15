% ist1106324 Cristiano Pantea
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- ['dados.pl'], ['keywords.pl']. % ficheiros a importar.

% NOTA: para facilitar a leitura dos comentarios, usei a
% expressao: (ee) como alternativa ao (e) com acento e utilizei
% tambem a expressao: (aa) como alternativa ao (a) com acento.

/*
eventosSemSalas(Eventos) ee verdade se Eventos ee uma lista, 
ordenada e sem elementos repetidos, de IDs de eventos sem sala.
*/
eventosSemSalas(Eventos):- setof(ID,A^B^C^ evento(ID,A,B,C,semSala), Eventos).


/*
eventosSemSalasDiaSemana(DiaDaSemana, Eventos) ee verdade se
Eventos ee uma lista, ordenada e sem elementos repetidos, de
IDs de eventos sem sala que decorrem em DiaDaSemana.
*/
eventosSemSalasDiaSemana(DiaDaSemana, Eventos):-
    findall(ID, (evento(ID,_,_,_,semSala), horario(ID,DiaDaSemana,_,_,_,_)) , Aux),
    sort(Aux, Eventos).


/*
relaciona_p_s(Lista_Periodo, Lista_p_s) ee verdade se Lista_Periodo
ee uma lista com um ou mais periodos e Lista_p_s ee uma lista com os
periodos da lista Lista_Periodo e os semestres correspondentes aos
mesmos. A lista Lista_p_s ee uma lista ordenada e sem repeticoes.
*/
relaciona_p_s(p1, p1_2).
relaciona_p_s(p2, p1_2).
relaciona_p_s(p3, p3_4).
relaciona_p_s(p4, p3_4).
relaciona_p_s(Lista_Periodo, Lista_p_s):-
    maplist(junta_p_s, Lista_Periodo, Aux),
    append(Aux, Aux2), sort(Aux2, Lista_p_s).
junta_p_s(El, [El, R]):-
    relaciona_p_s(El, R).


/*
eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala) ee verdade se
ListaPeriodos ee uma lista de periodos (p1, p2, p3, p4) e EventosSemSala
ee uma lista, ordenada e sem elementos repetidos, de IDs de eventos sem
sala nos periodos de ListaPeriodos.
*/
eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala):-
    relaciona_p_s(ListaPeriodos, Lista_p_s),
    maplist(id_semSala, Lista_p_s, SemSalaTodos),
    append(SemSalaTodos, Junta), sort(Junta, EventosSemSala).

id_semSala(P_ou_S, SemSala):-
    findall(ID, (horario(ID,_,_,_,_,P_ou_S), evento(ID,_,_,_,semSala)), SemSala).


% Factos definidos para auxiliar na funcao recursiva.
periodo_semestre(p1, p1_2). 
periodo_semestre(p2, p1_2). 
periodo_semestre(p3, p3_4).
periodo_semestre(p4, p3_4).

/*
organizaEventos(ListaIDS, Periodo, EventosNoPeriodo) ee verdade se
EventosNoPeriodo ee a lista, ordenada e sem elementos repetidos, de IDs
dos eventos de ListaIDS que ocorrem no periodo Periodo (p1, p2, p3, p4). 
*/

organizaEventos([],_,[]):-!.

% Auxiliar ee criada para ajudar na ordenacao dos IDs.
organizaEventos(ListaIDs, Periodo , EventosNoPeriodo):- 
    organizaEventos(ListaIDs, Periodo, [], EventosNoPeriodo).

organizaEventos([ID|RestoIDs], Periodo, Aux, EventosNoPeriodo):- 
    periodo_semestre(Periodo, Semestre),
    (horario(ID,_,_,_,_,Periodo); horario(ID,_,_,_,_,Semestre)), !,
    organizaEventos(RestoIDs, Periodo, [ID|Aux], EventosNoPeriodo).

organizaEventos([], _ , Aux, EventosNoPeriodo):-
    sort(Aux, EventosNoPeriodo).

organizaEventos([_|RestoIDs], Periodo, Aux, EventosNoPeriodo):-
    organizaEventos(RestoIDs, Periodo, Aux, EventosNoPeriodo).


/*
eventosMenoresQue(Duracao, ListaEventosMenoresQue) ee verdade se
ListaEventosMenoresQue ee a lista ordenada e sem elementos repetidos dos
identificadores dos eventos que possuem duracao menor ou igual a Duracao.
*/
eventosMenoresQue(Duracao, ListaEventosMenoresQue):-
    findall(ID, (horario(ID,_,_,_,Tempo_Evento,_), Tempo_Evento =< Duracao), Aux),
    sort(Aux, ListaEventosMenoresQue).


/*
eventosMenoresQueBool(ID, Duracao) ee verdade se o evento identificado por
ID tiver duracao igual ou menor a Duracao.
*/
eventosMenoresQueBool(ID, Duracao):-
    horario(ID,_,_,_,Tempo_Evento,_), Tempo_Evento =< Duracao.


/*
procuraDisciplinas(Curso, ListaDisciplinas) ee verdade se ListaDisciplinas
ee a lista ordenada alfabeticamente do nome das disciplinas do curso Curso.
*/
procuraDisciplinas(Curso, ListaDisciplinas):-
    findall(NomeDisciplina, (turno(ID, Curso,_,_), evento(ID, NomeDisciplina,_,_,_)), Aux),
    sort(Aux, ListaDisciplinas).


/*
organizaDisciplinas(ListaDisciplinas, Curso, Semestres) ee verdade se Semestres
ee uma lista com duas listas. A lista na primeira posicao possui as disciplinas 
de ListaDisciplinas do curso Curso que ocorrem no primeiro semestre; idem para 
a lista na segunda posicao, que possui as que ocorrem no segundo semestre. 
Ambas as listas estao ordenadas alfabeticamente e nao possuem elementos repetidos. 
O predicado falha se nao existir no curso Curso uma disciplina de ListaDisciplinas.
*/

% Neste caso A ee uma disciplina que pertence ao primeiro semestre do curso Curso.
organizaDisciplinas([],_,[[],[]]):- !.
organizaDisciplinas([A|B], Curso, [[A|C], E]):-
    evento(ID,A,_,_,_),
    (horario(ID,_,_,_,_,p1_2); horario(ID,_,_,_,_,p1); horario(ID,_,_,_,_,p2)),
    turno(ID, Curso,_,_), !,
    organizaDisciplinas(B, Curso, [C, E]).

% Neste caso D ee uma disciplina que pertence ao segundo semestre do curso Curso.
organizaDisciplinas([D|B], Curso,[C, [D|E]]):-
    evento(ID,D,_,_,_),
    (horario(ID,_,_,_,_,p3_4); horario(ID,_,_,_,_,p3); horario(ID,_,_,_,_,p4)),
    turno(ID, Curso,_,_), !,
    organizaDisciplinas(B, Curso, [C, E]).


/*
horasCurso(Periodo, Curso, Ano, TotalHoras) ee verdade se TotalHoras for o numero
de horas total dos eventos associadas ao curso Curso, no ano Ano e periodo Periodo
*/
horasCurso(Periodo, Curso, Ano, TotalHoras):-
    relaciona_p_s([Periodo], Lista_p_s),        % A ideia neste predicado ee fazer uma lista
    findall(ID, (member(P_ou_S,Lista_p_s),      % com as duracoes e depois fazer a soma dos
                turno(ID, Curso, Ano,_),        % elementos dessa lista que vai corresponder
                horario(ID,_,_,_,_,P_ou_S)),    % ao Total de Horas.
            ListaIDs),
    sort(ListaIDs, SemRepetidos),
    maplist(duracao, SemRepetidos, ListaHoras),
    sumlist(ListaHoras, TotalHoras).

duracao(ID, Duracao):-
    horario(ID,_,_,_,Duracao,_).


/*
evolucaoHorasCurso(Curso, Evolucao) ee verdade se Evolucao for uma lista de 
tuplos na forma (Ano, Periodo, NumHoras), em que NumHoras ee o total de horas
associadas ao curso Curso, no ano Ano e periodo Periodo. Evolucao encontra-se
ordenada por ano (crescente) e periodo.
*/
evolucaoHorasCurso(Curso, Evolucao):-
    maplist(total_ano(Curso), [1, 2, 3], Junta), % "Iterar pelos 3 anos".
    append(Junta, Evolucao).

total_ano(Curso, Ano, PorPeriodo):-
    maplist(horas_periodo(Curso, Ano), [p1, p2, p3, p4], (PorPeriodo)). % "Iterar pelos 4 periodos".

horas_periodo(Curso, Ano, Periodo, Tuplo):-
    horasCurso(Periodo, Curso, Ano, TotalHoras), % Calculo do numero total de horas para um determinado 
    Tuplo = (Ano, Periodo, TotalHoras).          % curso, num determinado ano, num determinado periodo.


/*
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas)
ee verdade se Horas for o numero de horas sobrepostas entre o evento que tem
inicio em HoraInicioEvento e fim em HoraFimEvento, e o slot que tem inicio
em HoraInicioDada e fim em HoraFimDada. Se nao existirem sobreposicoes o
predicado falha (false).
*/
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas):-
    Aux1 is max(HoraInicioDada, HoraInicioEvento),   % Em todos os casos possiveis, o numero de 
    Aux2 is min(HoraFimDada, HoraFimEvento),         % horas Horas ee dado pela diferenca entre o
    Horas is Aux2 - Aux1,                            % valor maximo entre a HoraInicioDada e a
    Horas > 0.                                       % HoraInicioEvento e o minimo entre a HoraFimDada
                                                     % e a HoraFimEvento. Para tal o numero de horas 
                                                     % Horas tem de ser maior que zero.

/*
numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras)
ee verdade se SomaHoras for o numero de horas ocupadas nas salas do tipo 
TipoSala, no intervalo de tempo definido entre HoraInicio e HoraFim, no dia
da semana DiaSemana, e no periodo Periodo.
*/
numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras):-
    relaciona_p_s([Periodo], Lista_p_s),
    maplist(horas_p_s(TipoSala, DiaSemana, HoraInicio, HoraFim), Lista_p_s, Lista_Horas), % "Iterar pelos periodos/semestres".
    append(Lista_Horas, Junta), sumlist(Junta,SomaHoras).

horas_p_s(TipoSala, DiaSemana, HoraInicio, HoraFim, P_ou_S, Horas_p_s):-
    findall(Horas, (horario(ID, DiaSemana, HoraInicioReal, HoraFimReal,_,P_ou_S),
                    ocupaSlot(HoraInicio, HoraFim, HoraInicioReal, HoraFimReal, Horas),
                    salas(TipoSala, ConjuntoSalas),
                    member(SalaEspecifica, ConjuntoSalas),
                    evento(ID,_,_,_,SalaEspecifica)), 
            Horas_p_s).


/*
ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max) ee verdade se Max for o numero
de horas possiveis de ser ocupadas por salas do tipo TipoSala, no intervalo de
tempo definido entre HoraInicio e HoraFim. Em termos praticos, assume-se que 
Max ee o intervalo tempo dado (HoraFim - HoraInicio), multiplicado pelo numero
de salas em jogo do tipo TipoSala.
*/
ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max):-
    salas(TipoSala, ListaSalas),
    length(ListaSalas, N_Salas),
    Max is (HoraFim - HoraInicio) * N_Salas.


/*
percentagem(SomaHoras, Max, Percentagem) ee verdade se Percentagem for a divisao
de SomaHoras por Max, multiplicada por 100.
*/
percentagem(SomaHoras, Max, Percentagem):-
    Percentagem is (SomaHoras/Max) * 100.


/*
ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados) ee verdade se
Resultados for uma lista ordenada de tuplos do tipo casosCriticos(DiaSemana,
TipoSala, Percentagem) em que DiaSemana, TipoSala e Percentagem sao, respecti-
-vamente, um dia da semana, um tipo de sala e a sua percentagem de ocupacao, 
no intervalo de tempo entre HoraInicio e HoraFim, e supondo que a  percentagem
de ocupacao relativa a esses elementos encontra-se acima de um dado  valor 
critico (Threshold).
*/
ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados):-
    findall(Salas, salas(Salas, _), ListaSalas),
    findall(casosCriticos(DiaSemana,TipoSala, Arredondada),
    (member(TipoSala, ListaSalas), 
    member(DiaSemana, [segunda-feira, terca-feira, quarta-feira, quinta-feira, sexta-feira]),
    member(Periodo, [p1, p1_2, p2, p3, p3_4, p4]),
    numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras),
    ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max),
    percentagem(SomaHoras, Max, Percentagem), % Calculo da percentagem com os valores fornecidos atraves dos
    Percentagem > Threshold,                  % predicados numHorasOcupadas e OcupacaoMax.
    ceiling(Percentagem, Arredondada)), 
    ResAntesDeSort), 
    sort(ResAntesDeSort, Resultados).


/*
ocupacaoMesa(ListaPessoas, ListaRestricoes, OcupacaoMesa) ee verdade se 
ListaPessoas for a lista com o nome das pessoas a sentar aa mesa,
ListaRestricoes for a lista de restricoes a verificar e OcupacaoMesa for 
uma lista com tres listas, em que a primeira contem as pessoas de um lado
da mesa (X1, X2 e X3), a segunda as pessoas aa cabeceira (X4 e X5) e a 
terceira as pessoas do outro lado da mesa (X6, X7 e X8), de modo a que
essas pessoas sao exactamente as da ListaPessoas e verificam todas as 
restricoes de ListaRestricoes.
*/
ocupacaoMesa(ListaPessoas, ListaRestricoes, OcupacaoMesa):-
    permutation(ListaPessoas, [A, B, C, D, E, F, G, H]), % Vai criar permutacoes da mesa.
    X = [[A,B,C], [D, E], [F, G, H]],
    aux_Percorre_Restricoes(ListaRestricoes, X), !,
    OcupacaoMesa = X.

% Vai verificar todas as restricoes dadas para uma dada permutacao.
aux_Percorre_Restricoes([], _ ):-!.
aux_Percorre_Restricoes([Restricao|Resto], X):-
    verifica(Restricao, X),
    aux_Percorre_Restricoes(Resto, X).

% Factos para auxiliar na verificao das restricoes, devolvendo Verdadeiro
% caso a mesa em questao verifique a restricao ou falso caso contrario.
verifica(cab1(NomePessoa), [[_,_,_], [NomePessoa,_], [_,_,_]]).
verifica(cab2(NomePessoa), [[_,_,_],[_,NomePessoa],[_,_,_]]).
verifica(honra(NomePessoa1, NomePessoa2), [[_,_,_],[NomePessoa1,_],[NomePessoa2,_,_]]).
verifica(honra(NomePessoa1, NomePessoa2), [[_,_,NomePessoa2],[_,NomePessoa1],[_,_,_]]).
verifica(lado(NomePessoa1, NomePessoa2), [[NomePessoa1,NomePessoa2,_],[_,_],[_,_,_]]).
verifica(lado(NomePessoa1, NomePessoa2), [[_,NomePessoa1,NomePessoa2],[_,_],[_,_,_]]).
verifica(lado(NomePessoa1, NomePessoa2), [[NomePessoa2,NomePessoa1,_],[_,_],[_,_,_]]).
verifica(lado(NomePessoa1, NomePessoa2), [[_,NomePessoa2,NomePessoa1],[_,_],[_,_,_]]).
verifica(lado(NomePessoa1, NomePessoa2), [[_,_,_],[_,_],[NomePessoa1,NomePessoa2,_]]).
verifica(lado(NomePessoa1, NomePessoa2), [[_,_,_],[_,_],[_,NomePessoa1,NomePessoa2]]).
verifica(lado(NomePessoa1, NomePessoa2), [[_,_,_],[_,_],[NomePessoa2,NomePessoa1,_]]).
verifica(lado(NomePessoa1, NomePessoa2), [[_,_,_],[_,_],[_,NomePessoa2,NomePessoa1]]).
verifica(frente(NomePessoa1, NomePessoa2), [[NomePessoa1,_,_],[_,_],[NomePessoa2,_,_]]).
verifica(frente(NomePessoa1, NomePessoa2), [[_,NomePessoa1,_],[_,_],[_,NomePessoa2,_]]).
verifica(frente(NomePessoa1, NomePessoa2), [[_,_,NomePessoa1],[_,_],[_,_,NomePessoa2]]).
verifica(frente(NomePessoa1, NomePessoa2), [[NomePessoa2,_,_],[_,_],[NomePessoa1,_,_]]).
verifica(frente(NomePessoa1, NomePessoa2), [[_,NomePessoa2,_],[_,_],[_,NomePessoa1,_]]).
verifica(frente(NomePessoa1, NomePessoa2), [[_,_,NomePessoa2],[_,_],[_,_,NomePessoa1]]).
verifica(frente(NomePessoa1, NomePessoa2), [[_,_,_],[NomePessoa1,NomePessoa2],[_,_,_]]).
verifica(frente(NomePessoa1, NomePessoa2), [[_,_,_],[NomePessoa2,NomePessoa1],[_,_,_]]).
verifica(naoFrente(NomePessoa1, NomePessoa2), X):-
    \+ (verifica(frente(NomePessoa1, NomePessoa2), X)).
verifica(naoLado(NomePessoa1, NomePessoa2), X):-
    \+ (verifica(lado(NomePessoa1, NomePessoa2), X)).