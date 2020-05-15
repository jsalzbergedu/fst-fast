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
      "create_states",
      "flatten",
      "print_fst"
   }
end

function Test:testCreateStateOutput()
   local l = self.language
   local expand_ref = self.expand_ref
   local expand_string = self.expand_string
   local mark_fin = self.mark_fin
   local enumerate = self.enumerate
   local create_states = self.create_states
   local flatten = self.flatten
   local print_fst = self.print_fst
   --------------------------------------------------------------------------------
   -- Test the create states interpreter
   --------------------------------------------------------------------------------
   print()
   print("Testing the create states interpreter")
   do
      local interpreter = expand_string.create()
      local l = l.l(interpreter)
      l:rule('A'):is(l:lit('aa'))
         :rule('B'):is(l:lit('bb'))
         :rule('K'):is(l:lit('x'))
         :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('A')))
         :create(expand_ref)(expand_string)(mark_fin)(enumerate)(create_states)(flatten)(print_fst)
   end
end

function Test:testCreateStatesInterpreter()
   self.TestEnumerate:assertNInterpreter(self.create_states)
end

return Test
