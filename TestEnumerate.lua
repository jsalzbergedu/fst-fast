local Test = {}

function Test:testRequires()
   return {"TestMarkFin"}
end

function Test:srcRequires()
   return {
      "language",
      "expand_ref",
      "expand_string",
      "mark_fin",
      "enumerate",
      "print_n"
   }
end

function Test:testEnumerateOutput()
   local l = self.language
   local expand_ref = self.expand_ref
   local expand_string = self.expand_string
   local mark_fin = self.mark_fin
   local enumerate = self.enumerate
   local print_n = self.print_n

   ----------------------------------------------------------------------------
   -- Testing the enumerate interpreter
   ---------------------------------------------------------------------------
   print()
   print("Testing the enumerate interpreters")
   do
      local l = l.l()
      l:rule('A'):is(l:lit('aa'))
         :rule('B'):is(l:lit('bb'))
         :rule('K'):is(l:lit('x'))
         :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
         :create(expand_ref)(expand_string)(mark_fin)(enumerate)(print_n)
   end

   do
      local l = l.l()
      l:rule('A'):is(l:lit('aa'))
         :rule('B'):is(l:lit('bb'))
         :rule('K'):is(l:lit('x'))
         :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('A')))
         :create(expand_ref)(expand_string)(mark_fin)(enumerate)(print_n)
   end
end

--------------------------------------------------------------------------------
-- Assert that the interpreter can interpret
-- PEGREG extended to include fin and enumeration
--------------------------------------------------------------------------------
function Test:assertNInterpreter(interpreter)
   local luaunit = self.luaunit
   self.TestMarkFin:assertFinInterpreter(interpreter)
   luaunit.assertEquals(type(interpreter.mark_n), "function", "No mark_n")
end

function Test:testEnumerateInterpreter()
   self.TestMarkFin:assertFinInterpreter(self.enumerate)
end

function Test:testPrintNInterpreter()
   local print_n = self.print_n
   Test:assertNInterpreter(print_n)
end

function Test:testEnumerateInterpreter()
   local enumerate = self.enumerate
   self.TestMarkFin:assertFinInterpreter(enumerate)
end

return Test
