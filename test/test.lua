local fst_fast = require("fst_fast")
local luaunit = require("luaunit")

function testInstructionTape()
   local it = fst_fast.instruction_tape.open()

   -- State 0
   do
      local ins = it:instr(6)
      ins:initial()
      do
         local fse = ins:get_outgoing('a')
         fse:set_outstate(1)
         fse:set_outchar('a')
      end
      ins:finish()
   end

   -- State 1
   do
      local ins = it:instr(6)
      ins:initial()
      do
         local fse = ins:get_outgoing('a')
         fse:set_outstate(2)
         fse:set_outchar('a')
      end
      do
         local fse = ins:get_outgoing('b')
         fse:set_outstate(4)
         fse:set_outchar('b')
      end
      ins:finish()
   end

   -- State 2
   do
      local ins = it:instr(6)
      do
         local fse = ins:get_outgoing('x')
         fse:set_outstate(3)
         fse:set_outchar('x')
      end
      ins:finish()
   end

   -- State 3
   do
      local ins = it:instr(6)
      ins:final()
   do end
   ins:finish()
   end

   -- State 4
   do
      local ins = it:instr(6)
      do
         local fse = ins:get_outgoing('x')
         fse:set_outstate(5)
         fse:set_outchar('x')
      end
      ins:finish()
   end

   -- State 5
   do
      local ins = it:instr(6)
      ins:final()
   do end
   ins:finish()
   end

   -- State 6
   do
      local ins = it:instr(6)
   do end
   ins:finish()
   end

   -- Match string
   local outstr, match_success, matched_states = it:match_string("aax")

   luaunit.assertEquals(outstr, "aax", "Outstr incorrect")

   luaunit.assertTrue(match_success, "Match should succeed")

   luaunit.assertEquals(matched_states, {1, 2, 3})

   it:close()

end

os.exit(luaunit.LuaUnit.run())
