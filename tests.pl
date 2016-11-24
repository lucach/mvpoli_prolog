is_varpower(v(3, a)).
m(_, _, [v(2, x), v(2, y)]) = m(_, _, VPs), foreach(member(VP, VPs), is_varpower(VP)).
get_power(v(2, x), P), P = 2.
get_totaldegree(m(_, TD, [])), TD = 0.
get_totaldegree(m(_, TD, [v(3, t), v(1, w), v(1, y)])), TD = 5.
as_monomial(3 * y * w * t^3, M), M = m(3, 5, [v(3, t), v(1, w), v(1, y)]).
as_monomial(y * s^3 * t^3, M), M = m(1, 7, [v(3, s), v(3, t), v(1, y)]).
as_monomial(42, QD), QD = m(42, 0, []).
coefficients(poly([m(1, 3, [v(3, x)])]), [1]).
coefficients(poly([m(1, 7, [v(3, s), v(3, t), v(1, y)]), m(1, 2, [v(1, x), v(1, y)]), m(-4, 0, [])]), [1, 1, -4]).
