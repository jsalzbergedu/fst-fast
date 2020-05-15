-- (aa/bb)x
local l = l.l(print_interpreter)
l:rule('A'):is(l:lit('aa'))
 :rule('B'):is(l:lit('bb'))
 :rule('K'):is(l:lit('x'))
 :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
 :create()
end
