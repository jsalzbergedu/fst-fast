local Test = {}

function Test:testRequires()
   return {"TestAddLeftRight", "TestPrintSyntax"}
end

function Test:srcRequires()
   return {
      "language",
      "expand_ref",
      "expand_string",
      "mark_fin",
      "print_fin"
   }
end

----------------------------------------------------------------------------
-- Test the fin print
---------------------------------------------------------------------------
function Test:testMarkFinOutput()
   local l = self.language
   local expand_ref = self.expand_ref
   local expand_string = self.expand_string
   local mark_fin = self.mark_fin
   local print_fin = self.print_fin

   print()
   print("Testing the mark final interpreters")
   do
      local l = l.l()
      l:rule('A'):is(l:lit('aa'))
         :rule('B'):is(l:lit('bb'))
         :rule('K'):is(l:lit('x'))
         :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
         :create(expand_ref)(expand_string)(mark_fin)(print_fin)
   end

   do
      local l = l.l()
      l:rule('A'):is(l:lit('aa'))
         :rule('B'):is(l:lit('bb'))
         :rule('K'):is(l:lit('x'))
         :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('A')))
         :create(expand_ref)(expand_string)(mark_fin)(print_fin)
   end
end

function Test:assertFinInterpreter(interpreter)
   self.TestAddLeftRight:assertLRInterpreter(interpreter)
   local luaunit = self.luaunit
   luaunit.assertEquals(type(interpreter.fin), "function", "No fin")
   luaunit.assertEquals(type(interpreter.notfin), "function", "No notfin")
end

function Test:testMarkfinInterpreter()
   self.TestPrintSyntax:assertPegregInterpreter(self.mark_fin)
end

function Test:testPrintInterpreter()
   self:assertFinInterpreter(self.print_fin)
end

return Test
