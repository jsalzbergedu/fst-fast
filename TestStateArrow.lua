local Test = {}

function Test:srcRequires()
   return {
      "language",
      "expand_ref",
      "expand_string",
      "add_left_right",
      "mark_fin",
      "enumerate",
      "state_arrow",
      "flatten",
      "print_n",
      "print_fst"
   }
end

function Test:testRequires()
   return {
      "TestEnumerate"
   }
end

function Test:testStateArrowOutput()
   local l = self.language
   local expand_ref = self.expand_ref
   local expand_string = self.expand_string
   local add_left_right = self.add_left_right
   local mark_fin = self.mark_fin
   local enumerate = self.enumerate
   local state_arrow = self.state_arrow
   local flatten = self.flatten
   local print_n = self.print_n
   local print_fst = self.print_fst
   print()
   print("Testing the create states and arrow interpreter")
   print("From:")
   do
      local l = l.l()
      local arrows = l:rule('A'):is(l:lit('aa'))
         :rule('B'):is(l:lit('bb'))
         :rule('K'):is(l:lit('x'))
         :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
         :create(expand_ref)(expand_string)(add_left_right)(mark_fin)(enumerate)(print_n)
   end
   print("To:")
   do
      local l = l.l()
      local arrows = l:rule('A'):is(l:lit('aa'))
         :rule('B'):is(l:lit('bb'))
         :rule('K'):is(l:lit('x'))
         :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
         :create(expand_ref)(expand_string)(add_left_right)(mark_fin)(enumerate)(state_arrow)(flatten)(print_fst)
   end
end

function Test:testStateArrowInterpreter()
   self.TestEnumerate:assertNInterpreter(self.state_arrow)
end

return Test
