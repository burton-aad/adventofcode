:- use_module(library(clpfd)).

number_digits(Number, 0, [Number]) :- Number in 0..9.
number_digits(Number, N, [Digit|Digits]) :-
        Digit in 0..9,
        N #= N1 + 1,
        Number #= Digit*10^N + Number1,
        Number1 #>= 0,
        N #> 0,
        number_digits(Number1, N1, Digits).

%% True when digit list have a double
pair([H|_], H).
pair([H|Hs], _) :- pair(Hs, H).
pair([]) :- false.
pair([H|Hs]) :- pair(Hs, H).

%% True when digits in list always increase.
%% always_inc(L) :- \+ ( append(_, [A,B|_], L), A > B ).
always_inc([]).
always_inc([_]).
always_inc([A,B|Hs]) :- A #=< B, always_inc([B|Hs]).

sorted(L) :- sort(L, L).

my_digits(Number, List) :-
    atomic_list_concat(List, Atom),
    atom_number(Atom, Number).

valid_num(N) :-
	N #> 0,
	number_digits(N, _, L),
	length(L, 6), !,
	pair(L),
	always_inc(L).
valid_num(_) :- false.

jour04(START, END) :-
	N in START..END, !,
	findall(N, valid_num(N), L),
	length(L, Len), write(Len), nl.
	
