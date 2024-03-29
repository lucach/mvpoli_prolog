%%%% as_monomial

as_monomial(3 * y * w * t^3, m(3, 5, [v(3, t), v(1, w), v(1, y)])).
as_monomial(y * s^3 * t^3, m(1, 7, [v(3, s), v(3, t), v(1, y)])).
as_monomial(42, m(42, 0, [])).
as_monomial(2*cos(0)*x, m(2.0, 1, [v(1, x)])).
as_monomial(cos(0), m(1.0, 0, [])).
as_monomial(x^0, m(1, 0, [])).
as_monomial(x^3 * y^0 * z^2, m(1, 5, [v(3, x), v(2, z)])).
as_monomial(a^2 * a^2, m(1, 4, [v(4, a)])).
as_monomial(radius^2, m(1, 2, [v(2, radius)])).
as_monomial(-x, m(-1, 1, [v(1, x)])).
as_monomial(-2*x, m(-2, 1, [v(1, x)])).
as_monomial(0*x, m(0, 0, [])).

%%%% coefficients

coefficients(poly([m(1, 3, [v(3, x)])]), [1]).
coefficients(x^3, [1]).
coefficients(poly([m(-4, 0, []), m(1, 2, [v(1, x), v(1, y)]), m(1, 7, [v(3, s), v(3, t), v(1, y)])]), [-4, 1, 1]).
coefficients(poly([]), [0]).

%%%% variables

variables(poly([m(-1, 1, [v(1, x)]), m(1, 2, [v(1, x), v(1, y)])]), [x, y]).
variables(poly([m(5, 2, [v(1, y), v(1, z)]), m(4, 3, [v(2, x), v(1, y)]), m(4, 4, [v(1, r), v(3, w)])]), [r, w, x, y, z]).
variables(-x+2*x*y, [x, y]).

%%%% maxdegree

maxdegree(poly([m(5, 2, [v(1, y), v(1, z)]), m(4, 3, [v(2, x), v(1, y)]), m(4, 4, [v(1, r), v(3, w)])]), 4).
maxdegree(5*y*z+4*x^2*y+4*r*w^3, 4).
maxdegree(poly([]), 0).

%%%% mindegree

mindegree(poly([m(5, 2, [v(1, y), v(1, z)]), m(4, 3, [v(2, x), v(1, y)]), m(4, 4, [v(1, r), v(3, w)])]), 2).
mindegree(5*y*z+4*x^2*y+4*r*w^3, 2).
mindegree(poly([]), 0).

%%%% as_polynomial

as_polynomial(y^4 * z * x^5 - y * z * r + y^4 * r * z^5, poly([m(-1, 3, [v(1, r), v(1, y), v(1, z)]), m(1, 10, [v(1, r), v(4, y), v(5, z)]), m(1, 10, [v(5, x), v(4, y), v(1, z)])])).
as_polynomial(y * s^3 * t^3 - 4 + x * y, poly([m(-4, 0, []), m(1, 2, [v(1, x), v(1, y)]), m(1, 7, [v(3, s), v(3, t), v(1, y)])])).
as_polynomial(-1 * x + x * y, poly([m(-1, 1, [v(1, x)]), m(1, 2, [v(1, x), v(1, y)])])).
as_polynomial(x * y^2 + x^2 * y, poly([m(1, 3, [v(1, x), v(2, y)]), m(1, 3, [v(2, x), v(1, y)])])).
as_polynomial(a*c+a^2+a*b+a, poly([m(1, 1, [v(1, a)]), m(1, 2, [v(1, a), v(1, b)]), m(1, 2, [v(1, a), v(1, c)]), m(1, 2, [v(2, a)])])).
as_polynomial(0*a+x, poly([m(1, 1, [v(1, x)])])).
as_polynomial(a^2 + a*c + a + a*z, poly([m(1, 1, [v(1, a)]), m(1, 2, [v(1, a), v(1, c)]), m(1, 2, [v(1, a), v(1, z)]), m(1, 2, [v(2, a)])])).
as_polynomial(0+0, poly([])).
as_polynomial(0, poly([])).

%%%% polyval

polyval(x^2 * y, [2, 2], 8).
polyval(x^2 * y + 3 * x, [2, 2], 14).
polyval(m(1, 2, [v(2, a)]), [3], 9).
polyval(3, [], 3).
polyval(x, [4, 5], 4).

%%%% polyplus

polyplus(m(2, 1, [v(1, a)]), m(3, 1, [v(1, a)]), poly([m(5, 1, [v(1, a)])])).
polyplus(m(3, 1, [v(1, a)]), m(-3, 1, [v(1, a)]), poly([])).
polyplus(3*a, poly([]), poly([m(3, 1, [v(1, a)])])).
polyplus(x+y, 2*x-y, poly([m(3, 1, [v(1, x)])])).

%%%% polyminus

polyminus(m(2, 1, [v(1, a)]), m(3, 1, [v(1, a)]), poly([m(-1, 1, [v(1, a)])])).
polyminus(m(3, 1, [v(1, a)]), m(3, 1, [v(1, a)]), poly([])).
polyminus(3*a, poly([]), poly([m(3, 1, [v(1, a)])])).
polyminus(x+y, x+y, poly([])).

%%%% polytimes

polytimes(y*s^3*t^3, -1*x+x*y, poly([m(-1, 8, [v(3, s), v(3, t), v(1, x), v(1, y)]), m(1, 9, [v(3, s), v(3, t), v(1, x), v(2, y)])])).
polytimes(a+b, a+b, poly([m(2, 2, [v(1, a), v(1, b)]), m(1, 2, [v(2, a)]), m(1, 2, [v(2, b)])])).
polytimes(x^2, x+y, poly([m(1, 3, [v(2, x), v(1, y)]), m(1, 3, [v(3, x)])])).

%%%% monomials

monomials(z*y+a*b, [m(1, 2, [v(1, a), v(1, b)]), m(1, 2, [v(1, y), v(1, z)])]).
monomials(x+x, [m(2, 1, [v(1, x)])]).
