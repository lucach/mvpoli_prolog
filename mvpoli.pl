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

%%      parse_varpower(Expression, v(Power, Variable))
%       True if Expression is in the form Variable^Power.
%       Also true if Expression is in the form without ^Power being
%       explicitly setted. In the latter case, Power defaults to 1.

parse_varpower(Variable, v(1, Variable)) :-
    atomic(Variable).

parse_varpower(Variable^Power, v(Power, Variable)) :-
    atomic(Variable),
    integer(Power).

%%      parse_monomial(Expression, m(C, _, VPs))
%       True if Expression is in the form E1 * E2 * ... * En, where for
%       each Ei (i ranges from 1 to n) parse_varpower(Ei, _) is true.
%       If the first expression is an integer, that number is C. Otherwise,
%       C defaults to 1.

parse_monomial(Coefficient, m(Coefficient, _TD, [])) :-
    integer(Coefficient),
    !.

parse_monomial(Expression, m(1, _TD, [VP])) :-
    parse_varpower(Expression, VP).

parse_monomial(E1 * E2, m(C, TD, VPs)) :-
    parse_varpower(E2, VP),
    parse_monomial(E1, m(C, TD, OtherVPs)),
    append([VP], OtherVPs, VPs).

%%      lexicographicallyCompare(Operator, v(_P1, Var1), v(_P2, Var2))
%       True if Operator is '=' and Var1 equals Var2.
%         or if Operator is '<' and Var1 comes before Var2 in a lex. order
%         or if Operator is '>' and Var2 comes after Var2 in a lex. order

lexicographicallyCompare(<, v(_P1, Var1), v(_P2, Var2)) :-
    Var1 @< Var2,
    !.

lexicographicallyCompare(=, v(_P1, Var1), v(_P2, Var2)) :-
    Var1 = Var2,
    !.

lexicographicallyCompare(>, v(_P1, Var1), v(_P2, Var2)) :-
    Var1 @> Var2,
    !.

%%      as_monomial(Expression, m(C, TD, SortedVPs))
%       True if m(C, TD, SortedVPs) is the monomial corresponding to
%       Expression, with a coefficient C, a total degree TD, and SortedVPs
%       is a list of VarPower sorted using lexicographicallyCompare/3.

as_monomial(Expression, m(C, TD, SortedVPs)) :-
    parse_monomial(Expression, m(C, _, VPs)),
    get_totaldegree(m(C, TD, VPs)),
    predsort(lexicographicallyCompare, VPs, SortedVPs).

%%      coefficients(Polynomial, Coefficients)
%       True if Coefficients is a list where the i-th element is the
%       coefficient of the i-th monomial of Polynomial.

coefficients(poly([]), []) :- !.

coefficients(poly([m(C, _TD, _VPs) | Monomials]), [C | Coefficients]) :-
    coefficients(poly(Monomials), Coefficients).

%%      m_variables(Monomial, Variables)
%       True if Variables is the list containing every variable that
%       appears in Monomial. Variables can contain duplicates and it
%       is not sorted in any specific order.

m_variables(m(_C, _TD, []), []) :- !.

m_variables(m(_C, _TD, [v(_Power, Var) | VPs]), [Var | Vars]) :-
    m_variables(m(_, _, VPs), Vars).

%%      p_variables(Monomials, Variables)
%       True if Variables is a list resulting from the concatenation of
%       the list of variables in every monomial appearing in Monomials,
%       obtained with m_variables/2.

p_variables([], []) :- !.

p_variables([M | Monomials], Vars) :-
    m_variables(M, Vars1),
    p_variables(Monomials, Vars2),
    append(Vars1, Vars2, Vars).

%%      variables(Poly, Variables)
%       True if Variables is a list of variables appearing in every monomial
%       in Poly, it is sorted and does not contain duplicates.

variables(poly(Monomials), SortedVars) :-
    p_variables(Monomials, Vars),
    sort(Vars, SortedVars).
