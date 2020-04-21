local fst_fast = require("library")

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
outstr, match_success, matched_states = it:match_string("aax")

for i = 1, #outstr do
   the_char = outstr:sub(i, i)
   the_state = matched_states[i]
   print("The char ", the_char, " matched with the state ", the_state)
end

-- Develop from here on out in Lua
-- OR build up C to lua interface

it:close()
