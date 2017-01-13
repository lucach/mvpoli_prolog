mvpoli_prolog
-------------

This is an implementation of a prolog library to handle polynomials.

Author
------
Luca Chiodini (luca <at> chiodini <dot> org)

Development
-----------
This library has been developed using GIT versioning system. It has been tested
using swipl (SWI-Prolog, 64 bits, Version 7.2.3).
Report of "unit tests" is available at https://jenkins.chiodini.org/ .

Usage
-----
Many predicates are provided to do basic algebraic operations with polynomials.
Here are some examples:

- as_monomial/2 is true when the second arg is the result of parsing of a
  superficial representation of a monomial.

  ?- as_monomial(y * s^3 * t^3, M).
  M = m(1, 7, [v(3, s), v(3, t), v(1, y)]).

- as_polynomial/2 is true when the second arg is the result of parsing of a
  superficial representation of a polynomial.

  ?- as_polynomial(-y+x*y, P).
  P = poly([m(-1, 1, [v(1, y)]), m(1, 2, [v(1, x), v(1, y)])]).

- coefficients/2 is true when the second arg is a list of the coefficients
  appearing in a polynomial.

  ?- coefficients(poly([m(-4, 0, []), m(1, 2, [v(1, x), v(1, y)])]), C).
  C = [-4, 1].

- variables/2 is true when the second arg is a list of all variable symbols
  appearing in a polynomial.

  ?- variables(-x+2*x*y, V).
  V = [x, y].

- maxdegree/2 is true when the second arg is the maximum degree of a monomial
  appearing in the polynomial.

  ?- maxdegree(5*y*z+4*x^2*y+4*r*w^3, D).
  D = 4.

- mindegree/2 is true when the second arg is the minimum degree of a monomial
  appearing in the polynomial.

  ?- mindegree(5*y*z+4*x^2*y+4*r*w^3, D).
  D = 2.

- polyval/3 is true when the third arg is the the value of the polynomial
  computed in the n-dimensional point provided as second argument.

  ?- polyval(x^2 * y + 3 * x, [2, 2], V).
  V = 14.

- polyplus/3 is true when the third arg is the polynomial sum of the two
  polynomials provided.

  ?- polyplus(x+y, 2*x-y, P).
  P = poly([m(3, 1, [v(1, x)])]).

- polyminus/3 is true when the third arg is the polynomial difference of the
  two polynomials provided.

  ?- polyminus(x+y, 2*x-y, P).
  P = poly([m(-1, 1, [v(1, x)]), m(2, 1, [v(1, y)])]).

- polytimes/3 is true when the third arg is the polynomial product of the two
  polynomials provided.

  ?- polytimes(x^2, x+y, P).
  P = poly([m(1, 3, [v(2, x), v(1, y)]), m(1, 3, [v(3, x)])]).

- monomials/2 is true when the second arg is a list of monomials appearing in
  a polynomial.

  ?- monomials(z*y+a*b, Ms).
  Ms = [m(1, 2, [v(1, a), v(1, b)]), m(1, 2, [v(1, y), v(1, z)])].

