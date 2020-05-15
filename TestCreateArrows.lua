local Test = {}

function Test:testRequires()
   return {"TestEnumerate"}
end

function Test:srcRequires()
   return {
      "language",
      "expand_ref",
      "expand_string",
      "mark_fin",
      "enumerate",
      "create_arrows",
      "flatten",
      "print_fst",
   }
end

--------------------------------------------------------------------------------
-- Test the arrows interpreter
--------------------------------------------------------------------------------
function Test:testCreateArrows()
   local l = self.language
   local expand_ref = self.expand_ref
   local expand_string = self.expand_string
   local mark_fin = self.mark_fin
   local enumerate = self.enumerate
   local create_arrows = self.create_arrows
   local flatten = self.flatten
   local print_fst = self.print_fst
   print()
   print("Testing the create arrows interpreter")
   do
      local l = l.l()
      local arrows = l:rule('A'):is(l:lit('aa'))
         :rule('B'):is(l:lit('bb'))
         :rule('K'):is(l:lit('x'))
         :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
         :create(expand_ref)(expand_string)(mark_fin)(enumerate)(create_arrows)(flatten)(print_fst)
   end
end

function Test:testCreateNInterpreter()
   self.TestEnumerate:assertNInterpreter(self.create_arrows)
end

return Test
