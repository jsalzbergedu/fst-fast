local l = require("language")
local sc_to_cs = require("sc_to_cs")

----------------------------------------------------------------------------
-- Test the sc_to_cs interpreter
---------------------------------------------------------------------------
print()
print("Testing the sc_to_cs interpreter")
do
   local l = l.l()
   l:rule('A'):is(l:lit('aa'))
    :rule('B'):is(l:lit('bb'))
    :rule('K'):is(l:lit('x'))
    :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
    :create(sc_to_cs)(print_syntax)
end

l.assert_pegreg_interpreter(sc_to_cs)
