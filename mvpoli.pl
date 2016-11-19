is_monomial(m(_C, TD, VPs)) :-
    integer(TD),
    TD >= 0,
    is_list(VPs).

is_varpower(v(Power, VarSymbol)) :-
    integer(Power),
    Power >= 0,
    atom(VarSymbol).

is_polynomial(poly(Monomials)) :-
    is_list(Monomials),
    foreach(member(M, Monomials), is_monomial(M)).