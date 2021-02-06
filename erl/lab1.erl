%% -*- coding: utf-8 -*-
-module(lab1).

% nazwa modułu

-compile([export_all]).

% opcje kompilatora, w tym wypadku eksport wszystkich funkcji
% przydatne przy testowaniu
%
%-export([add/2, head/1, sum/1] ).
% lista funkcji jakie będą widoczne dla innych modułów

-vsn(1.0).

% wersja

-kto_jest_najlepszy(ja).%dowolny atom może być wykorzystany jako 'atrybut' modułu
                        %po kompilacji uruchom lab1:module_info().
                        %inne narzędzia mogą korzystać z tych informacji

-import(math,
        [pi/0]).% lista modułów, które są potrzebne.
                % nie jest to konieczne

%funkcje

add(A1, A2) -> A1 + A2.

head([H | _]) -> {glowa, H}.

sum([]) -> 0;
sum([H | T]) -> H + sum(T).

avg(A1, A2) -> (A1 + A2) / 2.

tsum(L) -> tsum(L, 0). %tsum/1

tsum([H | T], S) -> tsum(T, S + H); %tsum/2
tsum([], S) ->
    S.% klauzule funkcji rozdzielane są średnikiem
      % po ostatniej jst kropka

factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

obwod_kola(Promien) ->
    Dwa_pi = 2 * pi(),  % wyrażenie pomocnicze
    Dwa_pi * Promien.   % ostatni element przed '.' lib ';'
                        % to wynik funkcji

% wyświetlamy wyniki funkcji
main(_) ->
    io:format("avarage of ~w and ~w = ~w\n",
              [5, 5, avg(5, 5)]),
    io:format("factorial ~w = ~w\n", [5, factorial(5)]).
