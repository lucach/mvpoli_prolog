is_varpower(v(3, a)).
m(_, _, [v(2, x), v(2, y)]) = m(_, _, VPs), foreach(member(VP, VPs), is_varpower(VP)).
get_power(v(2, x), P), P = 2.
get_totaldegree(m(_, TD, [])), TD = 0.
get_totaldegree(m(_, TD, [v(3, t), v(1, w), v(1, y)])), TD = 5.
