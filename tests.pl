is_varpower(v(3, a)).
m(_, _, [v(2, x), v(2, y)]) = m(_, _, VPs), foreach(member(VP, VPs), is_varpower(VP)).
