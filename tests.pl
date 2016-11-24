is_varpower(v(3, a)).
m(_, _, [v(2, x), v(2, y)]) = m(_, _, VPs), foreach(member(VP, VPs), is_varpower(VP)).
get_power(v(2, x), 2).
get_totaldegree(m(_, 0, [])).
get_totaldegree(m(_, 5, [v(3, t), v(1, w), v(1, y)])).
as_monomial(3 * y * w * t^3, m(3, 5, [v(3, t), v(1, w), v(1, y)])).
as_monomial(y * s^3 * t^3, m(1, 7, [v(3, s), v(3, t), v(1, y)])).
as_monomial(42, m(42, 0, [])).
coefficients(poly([m(1, 3, [v(3, x)])]), [1]).
coefficients(poly([m(1, 7, [v(3, s), v(3, t), v(1, y)]), m(1, 2, [v(1, x), v(1, y)]), m(-4, 0, [])]), [1, 1, -4]).
variables(poly([m(4, 2, [v(1,x), v(1,y)]), m(5,2,[v(1,z), v(1,y)]), m(6, 2, [v(1,w), v(1,r)])]), [r, w, x, y, z]).
maxdegree(poly([m(4, 3, [v(2,x), v(1,y)]), m(5,2,[v(1,z), v(1,y)]), m(4, 4, [v(3,w), v(1,r)])]), 4).
mindegree(poly([m(4, 3, [v(2,x), v(1,y)]), m(5,2,[v(1,z), v(1,y)]), m(4, 4, [v(3,w), v(1,r)])]), 2).
