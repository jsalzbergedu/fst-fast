local Test = {}

function Test:testRequires()
   return {"TestPrintSyntax"}
end

function Test:srcRequires()
   return {
      "language",
      "expand_string",
      "print_syntax",
   }
end

---------------------------------------------------------------------------
-- Test the expand string interpreter
---------------------------------------------------------------------------
function Test:testExpandString()
   local l = self.language
   local expand_string = self.expand_string
   local print_syntax = self.print_syntax

   print()
   print("Testing the expand string interpreter")
   do
      local l = l.l()
      l:rule('A'):is(l:lit('aa'))
         :rule('B'):is(l:lit('bb'))
         :rule('K'):is(l:lit('x'))
         :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
         :create(expand_string)(print_syntax)
   end
end

function Test:testExpandStringInterpreter()
   self.TestPrintSyntax:assertPegregInterpreter(self.expand_string)
end

return Test
