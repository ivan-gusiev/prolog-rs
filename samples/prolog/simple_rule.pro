q(a, b).
r(b, c).
p(X, Y) :- q(X, Z), r(Z, Y).
?- p(U, V).
# ?- p(a, V). 