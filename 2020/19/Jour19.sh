#!/bin/bash

in_file=$1

echo "in file '$in_file'"
# Réécriture des règle en prolog
cat $in_file | awk -F"[: \"]+" '
  BEGIN{RS="[|\n]"} /^$/{exit}

  { gsub("[0-9]+", "r&") }

  /:/{ rule = $1 }

  /"/{ print rule "([" $2 "|L], L)."; next }

  {
    if (!$NF) NF -= 1
    printf "%s(X1, X%d) :- ", rule, NF
    for (i=2; i<=NF; i++) {
      if (i > 2) printf ", "
      printf "%s(X%d, X%d)", $i, i-1, i
    }
    print "."
  }' > Jour19.pl

# On lance les prédicat sur tout le fichier d'input mais les règles sont forcément fausse.
cat >> Jour19.pl << EOF

count_r0([], 0).
count_r0([E|L], R) :- string_chars(E, EL), r0(EL, []), !, count_r0(L, N), R is N+1.
count_r0([_|L], R) :- count_r0(L, N), R is N.

%% Partie 2

r0_2(E, S) :- r8_2(E, X), r11_2(X, S).
r8_2(E, S) :- r42(E, S).
r8_2(E, S) :- r42(E, X), r8_2(X, S).
r11_2(E, S) :- r42(E, X), r31(X, S).
r11_2(E, S) :- r42(E, X), r11_2(X, Y), r31(Y, S).

count_r0_2([], 0).
count_r0_2([E|L], R) :- string_chars(E, EL), r0_2(EL, []), !, count_r0_2(L, N), R is N+1.
count_r0_2([_|L], R) :- count_r0_2(L, N), R is N.

main() :-
	read_file_to_string("${in_file}", S, []),
	split_string(S, "\n", "\n", L),
	count_r0(L, N), format("Part 1: ~d~n", [N]),
    count_r0_2(L, N2), format("Part 2: ~d~n", [N2]).

EOF

swipl -g main -t halt -s Jour19.pl
