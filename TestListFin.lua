local Test = {}

function Test:testRequires()
   return {
      "TestEnumerate"
   }
end

function Test:srcRequires()
   return {
      "language",
      "expand_ref",
      "expand_string",
      "mark_fin",
      "enumerate",
      "remark_fin",
      "list_fin"
   }
end

function Test:testListFin()
   local l = self.language
   local expand_ref = self.expand_ref
   local expand_string = self.expand_string
   local mark_fin = self.mark_fin
   local enumerate = self.enumerate
   local remark_fin = self.remark_fin
   local list_fin = self.list_fin

   do
      print()
      print("Testing the list finstates interpreter")
      local l = l.l()
      local states = l:rule('A'):is(l:lit('aa'))
         :rule('B'):is(l:lit('bb'))
         :rule('K'):is(l:lit('x'))
         :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('A')))
         :create(expand_ref)(expand_string)(mark_fin)(enumerate)(remark_fin)(list_fin)
      for i, v in ipairs(states) do
         print(i, v)
      end
   end
end

function Test:testListFinInterpreter()
   self.TestEnumerate:assertNInterpreter(self.list_fin)
end

return Test
