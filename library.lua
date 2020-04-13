local fst_fast = {}
local c_lib = require("fst_fast")

fst_fast.instruction_tape = {}

--------------------------------------------------------------------------------
-- Create an instruction tape.
-- This is the "low level" functionality that
-- wraps the unsafe C functions and creates a heirarchical organization.
--------------------------------------------------------------------------------
function fst_fast.instruction_tape.open()
   local tape = {}
   -----------------------------------------------------------------------------
   -- The pointer to the instruction tape
   -----------------------------------------------------------------------------
   tape.__it = c_lib.get_instruction_tape()
   -----------------------------------------------------------------------------
   -- Whether the instruction tape is valid
   -----------------------------------------------------------------------------
   tape.__it_isvalid = true
   -----------------------------------------------------------------------------
   -- A method to close (destroy) the instruction tape.
   -----------------------------------------------------------------------------
   function tape:close()
      assert(self.__it_isvalid, "Instruction tape already closed")
      c_lib.instruction_tape_destroy(self.__it)
      self.__it_isvalid = false;
   end
   -----------------------------------------------------------------------------
   -- A method to clear a new instruction.
   -- The number that this method takes should be the error state.
   -----------------------------------------------------------------------------
   function tape:instr(num)
      local ins = {}
      print("ins is: ", ins)
      --------------------------------------------------------------------------
      -- Whether or not the instruction is valid
      --------------------------------------------------------------------------
      ins.__in_isvalid = true

      print("ins is: ", ins)
      --------------------------------------------------------------------------
      -- A method that sets the state's flags to initial
      --------------------------------------------------------------------------
      function ins:initial()
         assert(tape.__it_isvalid, "Instruction tape invalidated")
         c_lib.fse_set_initial_flags(tape.__it)
      end

      print("ins is: ", ins)
      --------------------------------------------------------------------------
      -- A function that sets the final flags
      --------------------------------------------------------------------------
      function ins:final()
         assert(tape.__it_isvalid, "Instruction tape invalidated")
         assert(self.__in_isvalid, "Instruction invalidated")
         c_lib.fse_set_final_flags(tape.__it)
      end

      print("ins is: ", ins)
      --------------------------------------------------------------------------
      -- A method that finishes an instruction and advances the tape
      --------------------------------------------------------------------------
      function ins:finish()
         assert(tape.__it_isvalid, "Instruction tape invalidated")
         c_lib.fse_finish(tape.__it)
         self.__in_isvalid = false;
      end

      print("ins is: ", ins)
      --------------------------------------------------------------------------
      -- A method that gets the outgoing instruction.
      -- The letter that this method takes is the
      -- incoming letter to transition on.
      --------------------------------------------------------------------------
      function ins:get_outgoing(c)
         assert(tape.__it_isvalid, "Instruction tape invalidated")
         assert(self.__in_isvalid, "Instruction invalidated")
         local outgoing = {}
         outgoing.__fse = c_lib.fse_get_outgoing(tape.__it, c)
         -----------------------------------------------------------------------
         -- A method that sets the out state of the transition
         -----------------------------------------------------------------------
         function outgoing:set_outstate(num)
            assert(tape.__it_isvalid, "Instruction tape invalidated")
            assert(ins.__in_isvalid, "Instruction invalidated")
            c_lib.fse_set_outstate(self.__fse, num)
         end

         -----------------------------------------------------------------------
         -- A method that sets the output character of the transition
         -----------------------------------------------------------------------
         function outgoing:set_outchar(c)
            assert(tape.__it_isvalid, "Instruction tape invalidated")
            assert(ins.__in_isvalid, "Instruction invalidated")
            c_lib.fse_set_outchar(self.__fse, c);
         end
         return outgoing
      end

      assert(tape.__it_isvalid, "Instruction tape already closed")
      c_lib.fse_clear_instr(tape.__it, num)
      return ins
   end

   return tape
end

return fst_fast
