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

%%      lexicographicallyCompareVP(Operator, v(_P1, Var1), v(_P2, Var2))
%       True if Operator is '=' and Var1 equals Var2.
%         or if Operator is '<' and Var1 comes before Var2 in a lex. order
%         or if Operator is '>' and Var2 comes after Var2 in a lex. order

lexicographicallyCompareVP(<, v(_P1, Var1), v(_P2, Var2)) :-
    Var1 @< Var2,
    !.

lexicographicallyCompareVP(=, v(_P1, Var1), v(_P2, Var2)) :-
    Var1 = Var2,
    !.

lexicographicallyCompareVP(>, v(_P1, Var1), v(_P2, Var2)) :-
    Var1 @> Var2,
    !.

%%      as_monomial(Expression, m(C, TD, SortedVPs))
%       True if m(C, TD, SortedVPs) is the monomial corresponding to
%       Expression, with a coefficient C, a total degree TD, and SortedVPs
%       is a list of VarPower sorted using lexicographicallyCompareVP/3.

as_monomial(Expression, m(C, TD, SortedVPs)) :-
    parse_monomial(Expression, m(C, _, VPs)),
    get_totaldegree(m(C, TD, VPs)),
    predsort(lexicographicallyCompareVP, VPs, SortedVPs).

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

%%      maxdegree(Poly, Degree)
%       True if Degree is the maximum degree of the monomials in Poly.

maxdegree(poly([m(_C, TD, _VPs)]), TD) :- !.

maxdegree(poly([m(_C, FirstMonomialDegree, _VPs) | Monomials]), MaxDegree) :-
    maxdegree(poly(Monomials), Degree),
    MaxDegree is max(FirstMonomialDegree, Degree).

%%      mindegree(Poly, Degree)
%       True if Degree is the minimum degree of the monomials in Poly.

mindegree(poly([m(_C, TD, _VPs)]), TD) :- !.

mindegree(poly([m(_C, FirstMonomialDegree, _VPs) | Monomials]), MinDegree) :-
    mindegree(poly(Monomials), Degree),
    MinDegree is min(FirstMonomialDegree, Degree).

%%      parse_polynomial(Expression, Monomials)
%       True if Expression is in the form E1 Op E2 Op ... Op En, where for
%       each Ei (i ranges from 1 to n) as_monomial(Ei, _) is true and Op is
%       + or -. If Op is -, the following monomial is parsed as usual but its
%       coefficient is considered negated.
%       Monomials is then a list [M1, M2, ..., Mn] where each Mi is the result
%       of the parsing of Ei.

parse_polynomial(M, [ParsedM]) :-
    as_monomial(M, ParsedM).

parse_polynomial(-M, [m(NegCoeff, TD, VPs)]) :-
    as_monomial(M, m(Coeff, TD, VPs)),
    NegCoeff is -Coeff.

parse_polynomial(Monomials + M, [ParsedM | ParsedMonomials]) :-
    as_monomial(M, ParsedM),
    parse_polynomial(Monomials, ParsedMonomials).

parse_polynomial(Monomials - M, [m(NegCoeff, TD, VPs) | ParsedMonomials]) :-
    as_monomial(M, m(Coeff, TD, VPs)),
    NegCoeff is -Coeff,
    parse_polynomial(Monomials, ParsedMonomials).

%%      lexicographicallyCompareMonomials(Operator, VPs1, VPs2)
%       True if Operator is '<' and lexicographicallyCompareVP/3 applied to
%       the first elements of each list is true with Op = '<'.
%       True if Operator is '>' and lexicographicallyCompareVP/3 applied to
%       the first elements of each list is true with Op = '<'.
%       Otherwise, if the first VPs are equal, get rid of them and consider
%       the following ones, repeating the comparison.


lexicographicallyCompareMonomials(<, [], [_VP2 | _VPs2]) :- !.

lexicographicallyCompareMonomials(>, [_VP1 | _VPs1], []) :- !.

lexicographicallyCompareMonomials(< , [VP1 | _VPs1], [VP2 | _VPs2]) :-
    lexicographicallyCompareVP(<, VP1, VP2),
    !.

lexicographicallyCompareMonomials(> , [VP1 | _VPs1], [VP2 | _VPs2]) :-
    lexicographicallyCompareVP(>, VP1, VP2),
    !.

lexicographicallyCompareMonomials(Op , [VP1 | VPs1], [VP2 | VPs2]) :-
    lexicographicallyCompareVP(=, VP1, VP2),
    lexicographicallyCompareMonomials(Op, VPs1, VPs2).

%%      degreeCompareVP(Operator, M1, M2)
%       True if Operator is '<' and monomial M1 has a total degree greater
%                                   than total degree of monomial M2.
%         or if Operator is '>' and monomial M1 has a total degree less
%                                   than total degree of monomial M2.
%       When total degrees are equal, Operator is the Operator resulting from
%       lexicographicallyCompareMonomials/3.

degreeCompareMonomials(<, m(_C1, TD1, _VPs1), m(_C2, TD2, _VPs2)) :-
    TD1 > TD2,
    !.

degreeCompareMonomials(>, m(_C1, TD1, _VPs1), m(_C2, TD2, _VPs2)) :-
    TD1 < TD2,
    !.

degreeCompareMonomials(Op, m(_C1, TD1, VPs1), m(_C2, TD2, VPs2)) :-
    TD1 = TD2,
    lexicographicallyCompareMonomials(Op, VPs1, VPs2).

%%      as_polynomail(Expression, poly(Monomials))
%       True if Monomials is the list that represent every monomial that
%       appears in Expression, as in parse_polynomial/2, and it is sorted
%       using degreeCompareMonomials/3.

as_polynomial(Expression, poly(SortedMonomials)) :-
    parse_polynomial(Expression, Monomials),
    predsort(degreeCompareMonomials, Monomials, SortedMonomials).

%%      computevariableval(v(Power, Var), Variables, VariableValues, Value)
%       Find the Index of Var in list Variables, then gets the corresponding
%       VariableValue in VariableValues at the same position (using that
%       Index).
%       True if Value is VariableValue (as defined above) raised to the
%       Power-th power.

computevariableval(v(Power, Var), Variables, VariableValues, Value) :-
    nth0(Index, Variables, Var),
    !,
    nth0(Index, VariableValues, VariableValue),
    Value is VariableValue ** Power.

%%      computevariableval(VPs, Variables, VariableValues, Value)
%       True if Value is the product of all values of variables in VPs,
%       calculated using computevariableval/4.

computevariablesval([VP], Variables, VariableValues, Value) :-
    computevariableval(VP, Variables, VariableValues, Value),
    !.

computevariablesval([VP | VPs], Variables, VariableValues, TotalValue) :-
    computevariableval(VP, Variables, VariableValues, Value1),
    computevariablesval(VPs, Variables, VariableValues, Value2),
    TotalValue is Value1 * Value2.

%%      computemonomialval(m(Coeff, _TD, VPs), Variables, VariableValues, Value)
%       True if Value is the product between Coeff and the value of all VPs,
%       calculated using computevariablesval/4.

computemonomialval(m(C, _TD, VPs), Variables, VariableValues, TotalValue) :-
    computevariablesval(VPs, Variables, VariableValues, VariableValue),
    TotalValue is C * VariableValue.

%%      computepolyval(Monomials, Variables, VariableValues, Value)
%       True if Value is the *sum* of values of every monomial in Monomials,
%       calculated using computemonomialval/4.

computepolyval([M], Variables, VariableValues, Value) :-
    computemonomialval(M, Variables, VariableValues, Value),
    !.

computepolyval([M | Monomials], Variables, VariableValues, TotalValue) :-
    computemonomialval(M, Variables, VariableValues, Value1),
    computepolyval(Monomials, Variables, VariableValues, Value2),
    TotalValue is Value1 + Value2.

%%      polyval(Polynomial, VariableValues, Value)
%       True if Value is the value of the polynomial Polynomial in the
%       n-dimensional point represented by the list VariableValues. The i-th
%       value in VariableValues matches with the i-th variable resulting from
%       variables/2.

polyval(poly(Monomials), VariableValues, Value) :-
    !,
    variables(poly(Monomials), Variables),
    computepolyval(Monomials, Variables, VariableValues, Value).

polyval(Expression, VariableValues, Value) :-
    as_polynomial(Expression, Poly),
    polyval(Poly, VariableValues, Value).
