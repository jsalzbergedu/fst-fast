fst_fast = require("fst_fast")

-- Simplest case

instruction_tape = fst_fast.get_instruction_tape()

fst_fast.create_pegreg_diffmatch(instruction_tape)

outstr, match_success, matched_states = fst_fast.match_string("aax", instruction_tape);

print("Outstr is: ", outstr)

print("Match success is: ", match_success)

for i = 1, #outstr do
   the_char = outstr:sub(i, i)
   the_state = matched_states[i]
   print("The char ", the_char, " matched with the state ", the_state)
end

fst_fast.instruction_tape_destroy(instruction_tape)

--- Create the same little guy

instrtape = fst_fast.get_instruction_tape()
print("instruciton tape is now", instruction_tape)

-- State 0
do
   fst_fast.fse_clear_instr(instrtape, 6)
   fst_fast.fse_set_initial_flags(instrtape)
   do
      local fse = fst_fast.fse_get_outgoing(instrtape, 'a')
      fst_fast.fse_set_outstate(fse, 1)
      fst_fast.fse_set_outchar(fse, 'a')
   end
   fst_fast.fse_finish(instrtape)
end

-- State 1
do
   fst_fast.fse_clear_instr(instrtape, 6)
   do
      local fse = fst_fast.fse_get_outgoing(instrtape, 'a')
      fst_fast.fse_set_outstate(fse, 2)
      fst_fast.fse_set_outchar(fse, 'a')
   end
   do
      local fse = fst_fast.fse_get_outgoing(instrtape, 'b')
      fst_fast.fse_set_outstate(fse, 4)
      fst_fast.fse_set_outchar(fse, 'b')
   end
   fst_fast.fse_finish(instrtape)
end

-- State 2
do
   fst_fast.fse_clear_instr(instrtape, 6);
   do
      local fse = fst_fast.fse_get_outgoing(instrtape, 'x')
      fst_fast.fse_set_outstate(fse, 3)
      fst_fast.fse_set_outchar(fse, 'x')
   end
   fst_fast.fse_finish(instrtape)
end

-- State 3
do
   fst_fast.fse_clear_instr(instrtape, 6)
   fst_fast.fse_set_final_flags(instrtape)
   do end
   fst_fast.fse_finish(instrtape)
end


-- State 4
do
   fst_fast.fse_clear_instr(instrtape, 6)
   do
      local fse = fst_fast.fse_get_outgoing(instrtape, 'x')
      fst_fast.fse_set_outstate(fse, 5)
      fst_fast.fse_set_outchar(fse, 'x')
   end
   fst_fast.fse_finish(instrtape)
end

-- State 5
do
   fst_fast.fse_clear_instr(instrtape, 6)
   fst_fast.fse_set_final_flags(instrtape)
   do end
   fst_fast.fse_finish(instrtape)
end

-- State 6
do
   fst_fast.fse_clear_instr(instrtape, 6)
   do end
   fst_fast.fse_finish(instrtape)
end

fst_fast.create_pegreg_diffmatch(instrtape)

outstr, match_success, matched_states = fst_fast.match_string("aax", instrtape)

print("Outstr is: ", outstr)

print("Match success is: ", match_success)

for i = 1, #outstr do
   the_char = outstr:sub(i, i)
   the_state = matched_states[i]
   print("The char ", the_char, " matched with the state ", the_state)
end

fst_fast.instruction_tape_destroy(instrtape)
