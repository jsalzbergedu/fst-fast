local Test = {}

function Test:testRequires()
   return {"TestPrintSyntax"}
end

function Test:srcRequires()
   return {"language",
           "add_left_right",
           "expand_string",
           "expand_ref",
           "print_lr"}
end

function Test:testLROutput()
   local l = self.language
   local add_left_right = self.add_left_right
   local expand_string = self.expand_string
   local expand_ref = self.expand_ref
   local print_lr = self.print_lr
   print()
   print("Testing the left right interpreter")
   do
      local l = l.l()
      l:rule('A'):is(l:lit('aa'))
         :rule('B'):is(l:lit('bb'))
         :rule('K'):is(l:lit('x'))
         :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
         :create(expand_ref)(expand_string)(add_left_right)(print_lr)
   end
end

--------------------------------------------------------------------------------
-- Ensures that the interpreter has PEGREG extended with left_right
--------------------------------------------------------------------------------
function Test:assertLRInterpreter(interpreter)
   self.TestPrintSyntax:assertPegregInterpreter(interpreter)
   local luaunit = self.luaunit
   luaunit.assertEquals(type(interpreter.left), "function", "No left")
   luaunit.assertEquals(type(interpreter.right), "function", "No right")
end

function Test:testAddLRInterpreter()
   local add_left_right = self.add_left_right
   self.TestPrintSyntax:assertPegregInterpreter(add_left_right)
end

function Test:testPrintLrInterpreter()
   local print_lr = self.print_lr
   Test:assertLRInterpreter(print_lr)
end

----------------------------------------------------------------------------
-- Test the left right interpreter
---------------------------------------------------------------------------

return Test
