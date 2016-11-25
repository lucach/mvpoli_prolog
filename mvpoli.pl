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
%       True after printing to stdout VarSymbol^Power. Avoids printing ^Power
%       if Power is 1.

pprint_varpower(v(1, VarSymbol)) :-
    write(VarSymbol),
    !.

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

%%      lexicographicallyCompareMonomialsWithoutEqual(Operator, VPs1, VPs2)
%       True if Operator is '<' and lexicographicallyCompareVP/3 applied to
%       the first elements of each list is true with Op = '<'.
%       True if Operator is '>' and lexicographicallyCompareVP/3 applied to
%       the first elements of each list is true with Op = '<'.
%       Otherwise, if the first VPs are equal, get rid of them and consider
%       the following ones, repeating the comparison.

lexicographicallyCompareMonomialsWithoutEqual(<, [], _) :- !.

lexicographicallyCompareMonomialsWithoutEqual(>, [_VP1 | _VPs1], []) :- !.

lexicographicallyCompareMonomialsWithoutEqual(< , [VP1 | _VPs1], [VP2 | _VPs2]) :-
    lexicographicallyCompareVP(<, VP1, VP2),
    !.

lexicographicallyCompareMonomialsWithoutEqual(> , [VP1 | _VPs1], [VP2 | _VPs2]) :-
    lexicographicallyCompareVP(>, VP1, VP2),
    !.

lexicographicallyCompareMonomialsWithoutEqual(Op , [VP1 | VPs1], [VP2 | VPs2]) :-
    lexicographicallyCompareVP(=, VP1, VP2),
    lexicographicallyCompareMonomialsWithoutEqual(Op, VPs1, VPs2).


%%      degreeCompareMonomialsWithoutEqual(Operator, M1, M2)
%       True if Operator is '<' and monomial M1 has a total degree greater
%                                   than total degree of monomial M2.
%         or if Operator is '>' and monomial M1 has a total degree less
%                                   than total degree of monomial M2.
%       When total degrees are equal, Operator is the Operator resulting from
%       lexicographicallyCompareMonomialsWithoutEqual/3.

degreeCompareMonomialsWithoutEqual(<, m(_C1, TD1, _VPs1), m(_C2, TD2, _VPs2)) :-
    TD1 > TD2,
    !.

degreeCompareMonomialsWithoutEqual(>, m(_C1, TD1, _VPs1), m(_C2, TD2, _VPs2)) :-
    TD1 < TD2,
    !.

degreeCompareMonomialsWithoutEqual(Op, m(_C1, TD1, VPs1), m(_C2, TD2, VPs2)) :-
    TD1 = TD2,
    lexicographicallyCompareMonomialsWithoutEqual(Op, VPs1, VPs2).

%%      polyReduce(Poly, PolyReduced)
%       True if PolyReduced represents the same monomials in Poly combining
%       similar terms.
%       Definition: two monomials are similar iff they share the same
%                   varpowers; i.e., they differ only from coefficient.
%       Two similar monomials can be compressed in one monomial whose
%       coefficient is the sum of the two original coefficient.
%       Note: this predicate assumes that Poly is sorted using
%             degreeCompareMonomialsWithoutEqual/3.

polyReduce(poly([M]), poly([M])) :- !.

polyReduce(poly([m(C1, TD, VP), m(C2, TD, VP) | Monomials]), poly(ReducedM)) :-
    !,
    C3 is C1 + C2,
    polyReduce(poly([m(C3, TD, VP) | Monomials]), poly(ReducedM)).

polyReduce(poly([M1, M2 | Monomials]), poly([M1 | ReducedM])) :-
    polyReduce(poly([M2 | Monomials]), poly(ReducedM)).

%%      as_polynomail(Expression, poly(Monomials))
%       True if Monomials is the list that represent every monomial that
%       appears in Expression, as in parse_polynomial/2, sorted using
%       degreeCompareMonomialsWithoutEqual/3 and reduced with polyReduce/2.

as_polynomial(Expression, poly(ReducedMonomials)) :-
    parse_polynomial(Expression, Monomials),
    predsort(degreeCompareMonomialsWithoutEqual, Monomials, SortedMonomials),
    polyReduce(poly(SortedMonomials), poly(ReducedMonomials)).

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


%%      polysum(Poly1, Poly2, Result)
%       True if Result is the polynomial sum of Poly1 and Poly2. Note that
%       Poly1 and Poly2 can also be monomials.

polysum(m(C, TD, VPs), Poly2, Result) :-
    polysum(poly([m(C, TD, VPs)]), Poly2, Result),
    !.

polysum(Poly1, m(C, TD, VPs), Result) :-
    polysum(Poly1, poly([m(C, TD, VPs)]), Result),
    !.

polysum(poly(Monomials1), poly(Monomials2), poly(ResultMonomials)) :-
    append(Monomials1, Monomials2, Monomials3),
    predsort(degreeCompareMonomialsWithoutEqual, Monomials3, SortedMonomials3),
    polyReduce(poly(SortedMonomials3), poly(ResultMonomials)).

%%      negateCoeff(Monomial, NegatedMonomial)
%       True if NegatedMonomial is Monomial with a negated coefficient.

negateCoeff(m(Coeff, TD, VPs), m(NegCoeff, TD, VPs)) :-
    NegCoeff is -Coeff.

%%      polyminus(Poly1, Poly2, Result)
%       True if Result is the polynomial dif. of Poly1 and Poly2. Note that
%       Poly1 and Poly2 can also be monomials.

polyminus(Poly1, poly(Monomials2), Result) :-
    maplist(negateCoeff, Monomials2, NegMonomials2),
    polysum(Poly1, poly(NegMonomials2), Result).

polyminus(Poly1, m(C, TD, VPs), Result) :-
    negateCoeff(m(C, TD, VPs), m(NegCoeff, TD, VPs)),
    polysum(Poly1, poly([m(NegCoeff, TD, VPs)]), Result).

%%      varpowersReduce(VPs, ReducedVPs)
%       True if ReducedVPs represents the same varpowers in VPs combining
%       equal variables.
%       Definition: two varpowers are equal iff they have the same variable.
%       Two equal varpowers can be compressed in one varpower whose exponent
%       is the sum of the two original exponents.
%       Note: this predicate assumes that VPs is sorted using
%             lexicographicallyCompareVPWithoutEqual/3.

varpowersReduce([v(Power, Var)], [v(Power, Var)]) :- !.

varpowersReduce([v(P1, Var), v(P2, Var) | VPs], ReducedVPs) :-
    !,
    P3 is P1 + P2,
    varpowersReduce([v(P3, Var) | VPs], ReducedVPs).

varpowersReduce([VP1, VP2 | VPs], [VP1 | ReducedVPs]) :-
    varpowersReduce([VP2 | VPs], ReducedVPs).

%%      lexicographicallyCompareVPWithoutEqual(Operator, v(_P1, Var1), v(_P2, Var2))
%       True if Operator is '<' and Var1 comes before Var2 in a lex. order
%         or if Operator is '>' and Var2 comes after Var2 in a lex. order

lexicographicallyCompareVPWithoutEqual(<, v(_P1, Var1), v(_P2, Var2)) :-
    Var1 @=< Var2,
    !.

lexicographicallyCompareVPWithoutEqual(>, v(_P1, Var1), v(_P2, Var2)) :-
    Var1 @> Var2,
    !.

%%      monomialTimesMonomial(M1, M2, MResult)
%       True if MResult is the monomial coming from the product of M1 times M2.
%       The resulting monomial coefficient is the product of the coefficients,
%       varpowers are the union of the original varpowers (reduced if needed).

monomialTimesMonomial(m(C1, _TD1, VPs1), m(C2, _TD2, VPs2), m(C3, TD3, VPs3)) :-
    C3 is C1 * C2,
    append(VPs1, VPs2, VPs),
    predsort(lexicographicallyCompareVPWithoutEqual, VPs, SortedVPs),
    varpowersReduce(SortedVPs, VPs3),
    get_totaldegree(m(C3, TD3, VPs3)).

%%      monomialTimesPoly(Monomial, Poly, PolyResult)
%       True if PolyResult is the polynomial coming from the product of
%       Monomial times Poly.
%       The resulting monomial is the sum of Monomial times each monomial in
%       Poly.

monomialTimesPoly(_, poly([]), poly([])) :- !.

monomialTimesPoly(m(C, TD, VPs), poly([M]), poly([Result])) :-
    monomialTimesMonomial(m(C, TD, VPs), M, Result),
    !.

monomialTimesPoly(m(C, TD, VPs), poly([M | Monomials]), poly(Result)) :-
    monomialTimesMonomial(m(C, TD, VPs), M, FirstM),
    monomialTimesPoly(m(C, TD, VPs), poly(Monomials), poly(OtherMs)),
    append([FirstM], OtherMs, Result).

%%      polytimes(Poly1, Poly2, Result)
%       True if Result is the polynomial product of Poly1 times Poly2. Note
%       that Poly1 and Poly2 can also be monomials.
%       The product is obtained as the sum of each monomial in Poly1
%       multiplied with Poly2 using monomialTimesPoly/3. The resulting
%       polynomial is then sorted and reduced with polyReduce/2 if needed.

polytimes(poly([]), poly(_), poly([])) :- !.

polytimes(m(C, TD, VPs), Poly2, PolyResult) :-
    polytimes(poly([m(C, TD, VPs)]), Poly2, PolyResult),
    !.

polytimes(Poly1, m(C, TD, VPs), PolyResult) :-
    polytimes(Poly1, poly([m(C, TD, VPs)]), PolyResult),
    !.

polytimes(poly([M | M1]), poly(M2), ReducedPoly) :-
    monomialTimesPoly(M, poly(M2), poly(FirstMonomials)),
    polytimes(poly(M1), poly(M2), poly(OtherMonomials)),
    append(FirstMonomials, OtherMonomials, Monomials),
    predsort(degreeCompareMonomialsWithoutEqual, Monomials, SortedMonomials),
    polyReduce(poly(SortedMonomials), ReducedPoly).
