fst_fast = require("fst_fast")

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
