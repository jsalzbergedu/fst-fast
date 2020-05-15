local Test = {}

function Test:testRequires()
   return {"TestPrintSyntax"}
end

function Test:srcRequires()
   return {
      "expand_ref",
      "expand_string",
      "print_syntax",
      "language",
   }
end

function Test:testExpandRef()
   local expand_ref = self.expand_ref
   local expand_string = self.expand_string
   local print_syntax = self.print_syntax
   local l = self.language

   ----------------------------------------------------------------------------
   -- Test the expand ref interpreter
   ---------------------------------------------------------------------------
   print()
   print("Testing the expand ref interpreter")
   do
      local l = l.l()
      l:rule('A'):is(l:lit('aa'))
         :rule('B'):is(l:lit('bb'))
         :rule('K'):is(l:lit('x'))
         :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
         :create(expand_ref)(expand_string)(print_syntax)
   end
end

function Test:testExpandRefInterpreter()
   self.TestPrintSyntax:assertPegregInterpreter(self.expand_ref)
end

return Test
