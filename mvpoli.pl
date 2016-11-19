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

get_power(v(Power, VarSymbol), Power) :-
    is_varpower(v(Power, VarSymbol)).

get_totaldegree(m(_, 0, [])) :- !.

get_totaldegree(m(_C, TD, [VP | VPs])) :-
    get_totaldegree(m(_, PartialDegree, VPs)),
    get_power(VP, FirstElementDegree),
    TD is FirstElementDegree + PartialDegree.