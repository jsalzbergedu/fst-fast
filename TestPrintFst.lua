local Test = {}

function Test:srcRequires()
   return {"fst_language", "print_fst"}
end

function Test:testRequires()
   return {}
end


function Test:testPrintFstOutput(test, fstl, print_fst)
   local fstl = self.fst_language
   local print_fst = self.print_fst
   do
      local l = fstl.l()

      l:state(0, false)
         :state(1, false)
         :state(2, true)
         :arrow(0, 1, 'a', 'a')
         :arrow(1, 2, 'b', 'b')
         :create(print_fst)
   end
end

--------------------------------------------------------------------------------
-- Assert that the state interpreter has all the functions required
--------------------------------------------------------------------------------
function Test:assertSLInterpreter(interpreter)
   local luaunit = self.luaunit
   luaunit.assertEquals(type(interpreter.state), "function", "No State")
   luaunit.assertEquals(type(interpreter.arrow), "function", "No Arrow")
   luaunit.assertEquals(type(interpreter.pair), "function", "No Pair")
   luaunit.assertEquals(type(interpreter.null), "function", "No Null")
   luaunit.assertEquals(type(interpreter.create), "function", "No Create")
end

function Test:testPrintFstInterpreter()
   Test:assertSLInterpreter(self.print_fst)
end

return Test
