local Test = {}

--------------------------------------------------------------------------------
-- A method to assert that the interpreter can interpret
-- the PEGREG dsl in its entirety
--------------------------------------------------------------------------------
function Test:testRequires()
   return {}
end

function Test:srcRequires()
   return {"language", "print_syntax"}
end

function Test:testPrintSyntaxOutput()
   local l = self.language
   local print_syntax = self.print_syntax
   do
      local l = l.l()
      l:rule('A'):is(l:lit('aa'))
         :rule('B'):is(l:lit('bb'))
         :rule('K'):is(l:lit('x'))
         :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
         :create(print_syntax)
   end
end

function Test:assertPegregInterpreter(interpreter)
   local luaunit = self.luaunit
   local must_have = {
      "e",
      "lit",
      "seq",
      "choice",
      "grammar",
      "ref",
      "create",
      "star"
   }
   for _, v in ipairs(must_have) do
      if type(interpreter[v]) ~= "function" then
         luaunit.fail("No " .. v)
      end
   end
end


function Test:testPegregInterpreter()
   local l = self.language
   local luaunit = self.luaunit
   local print_syntax = self.print_syntax
   Test:assertPegregInterpreter(print_syntax)
end


return Test
