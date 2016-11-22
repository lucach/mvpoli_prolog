%%%% -*- Mode: Prolog -*-

%%%% 806976 Chiodini Luca

%%      is_monomial(m(_C, TD, VPs)
%       True if m(C, TD, VPs) is a monomial with a positive total degree
%       and VPs is a list.

is_monomial(m(_C, TD, VPs)) :-
    integer(TD),
    TD >= 0,
    is_list(VPs).

%%      is_varpower(v(Power, VarSymbol))
%       True if v(Power, VarSymbol) is a power of a variable, i.e. Power is
%       a non-negative integer and VarSymbol is an atom.


is_varpower(v(Power, VarSymbol)) :-
    integer(Power),
    Power >= 0,
    atom(VarSymbol).

%%      is_polynomial(poly(Monomials))
%       True if Monomials is a list of valid monomials.

is_polynomial(poly(Monomials)) :-
    is_list(Monomials),
    foreach(member(M, Monomials), is_monomial(M)).

%%      get_power(v(Power, Var), Power)
%       True if v(Power, Var) is a correct varpower and Var is raised to the
%       Power-th power.

get_power(v(Power, VarSymbol), Power) :-
    is_varpower(v(Power, VarSymbol)).

%%      get_totaldegree(m(C, TD, VPs))
%       True if TD is the total degree of the monomial, i.e. the sum of the
%       powers of each variable.

get_totaldegree(m(_, 0, [])) :- !.

get_totaldegree(m(_C, TD, [VP | VPs])) :-
    get_totaldegree(m(_, PartialDegree, VPs)),
    get_power(VP, FirstElementDegree),
    TD is FirstElementDegree + PartialDegree.

%%      pprint_varpower(v(Power, VarSymbol))
%       True after printing to stdout VarSymbol^Power.

pprint_varpower(v(Power, VarSymbol)) :-
    write(VarSymbol^Power).

%%      pprint_varpowers(VPs)
%       True after printing to stdout every varpower with a '*' sign between
%       two of them. If |VPs| = 3, then prints VP1 * VP2 * VP3, without the
%       last '*' (as a human would do on paper).

pprint_varpowers([]) :- !.

pprint_varpowers([VP]) :-
    pprint_varpower(VP),
    !.

pprint_varpowers([VP|VPs]) :-
    format('~@ * ', pprint_varpower(VP)),
    pprint_varpowers(VPs).

%%      pprint_monomial(m(C, _TD, VPs))
%       True after printing to stdout a monomial. A monomial is
%                           C * VPs
%       where, for aesthetics reasons, C (the coefficient) is printed only
%       when not trivial (this means it is not 1).

pprint_monomial(m(C, _TD, VPs)) :-
    C \= 1,
    format('~w * ', C),
    pprint_varpowers(VPs).

pprint_monomial(m(C, _TD, VPs)) :-
    C = 1,
    pprint_varpowers(VPs).

%%      pprint_polynomial(poly(Monomials))
%       True after printing to stdout a polynomial, which is a sequence
%       of monomials separated by the sign ' + '. This predicates behaves as 
%       pprint_varpowers/1, meaning that does not print a ' + ' after the last
%       monomial.

pprint_polynomial(poly([])) :- !.

pprint_polynomial(poly([M])) :-
    pprint_monomial(M),
    !.

pprint_polynomial(poly([M | Monomials])) :-
    format('~@ + ', pprint_monomial(M)),
    pprint_polynomial(poly(Monomials)).