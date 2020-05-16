local fst_fast = {}
local c_lib = require("fst_fast_system")

fst_fast.instruction_tape = {}

-- FUTURE: Implement kleene star,
--         test regexes
--         Then add cont-pass layer and possessive ordered
--         choice, possesive star as layers

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
      --------------------------------------------------------------------------
      -- Whether or not the instruction is valid
      --------------------------------------------------------------------------
      ins.__in_isvalid = true

      --------------------------------------------------------------------------
      -- A method that sets the state's flags to initial
      --------------------------------------------------------------------------
      function ins:initial()
         assert(tape.__it_isvalid, "Instruction tape invalidated")
         c_lib.fse_set_initial_flags(tape.__it)
      end

      --------------------------------------------------------------------------
      -- A function that sets the final flags
      --------------------------------------------------------------------------
      function ins:final()
         assert(tape.__it_isvalid, "Instruction tape invalidated")
         assert(self.__in_isvalid, "Instruction invalidated")
         c_lib.fse_set_final_flags(tape.__it)
      end

      --------------------------------------------------------------------------
      -- A method that finishes an instruction and advances the tape
      --------------------------------------------------------------------------
      function ins:finish()
         assert(tape.__it_isvalid, "Instruction tape invalidated")
         c_lib.fse_finish(tape.__it)
         self.__in_isvalid = false;
      end

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

      assert(tape.__it_isvalid, "Instruction tape invalidated")
      c_lib.fse_clear_instr(tape.__it, num)
      return ins
   end

   ------------------------------------------------------------------------------
   -- Match a string against the tape
   ------------------------------------------------------------------------------
   function tape:match_string(str)
      return c_lib.match_string(str, self.__it)
   end

   return tape
end

-- Want to go from
--
-- A <- aa
-- B <- ab
-- K <- x
-- (A/B)K
--
-- (or (A|B)K)
-- local l = require("language")

-- While not enforcable, the types for interpreters work like so:
-- Interpreter takes in two types, the "working" type
-- and the "target" type. The working and target type can be the same
-- All methods but grammar and create take in and return the "working" type.
-- Grammar takes in the working type and converts it to the target type.
-- Create takes in the target type.
-- Keeping the working and target types seperate allows
-- for the use of, for example, tables in the working type
-- and, for example, strings in the target type
-- while still retaining the layering of interpreters.

-- local print_syntax = require("print_syntax")

-- l.assert_pegreg_interpreter(print_syntax)

-- Current language:
-- (create (rules (rule K (lit x))
--                (rule B (lit bb)))
--         (grammar (seq (choice (ref A) (ref B)) (ref K))))
--
-- Some neccessary translations before compiling:
-- Make strings sequences
-- (create (rules (rule K (lit x))
--                (rule A (seq (lit a) (lit a)))
--                (rule B (seq (lit b) (lit b))))
--         (grammar (seq (choice (ref A) (ref B)) (ref K))))
--
-- Expand refs
-- (create (grammar (seq (choice (seq (lit a) (lit a))
--                               (seq (lit b) (lit b)))
--                       (lit x))))
-- Seq marks all left as definitley not final,
-- choice marks left and right as potentially final,
-- and grammar marks outermost term as final.
-- So
--
-- (create (grammar (seq (choice (seq (lit a) (lit a))
--                               (seq (lit b) (lit b)))
--                       (fin (lit x)))))
--
-- is
--
-- (create (grammar (seq (choice (seq (lit a) (lit a))
--                               (seq (lit b) (lit b)))
--                       (fin (lit x)))))
--
-- Also, translate (seq (choice A B) K)
-- to (choice (seq A K) (seq B K)).
-- this will remove any cases where a pair
-- of arrows would have reconverged on a state,
-- allowing for reconversion.
--
-- To optimize, this could be done
-- before and after enumeration,
-- and the transform could be changed to simply reference K.

-- local sc_to_cs = require("sc_to_cs")

-- local expand_string = require("expand_string")

-- local expand_ref = require("expand_ref")

-- local add_left_right = require("add_left_right")

-- l.assert_pegreg_interpreter(add_left_right)

-- local print_lr = require("print_lr")

-- local mark_fin = require("mark_fin")

-- local print_fin = require("print_fin")

-- local print_fst = require("print_fst")

-- Now that we have all of the "states" of our nondeterministic FST,
-- we will want to enumerate them.
-- We will be using a depth first search to do this.

----------------------------------------------------------------------------
-- An interpreter that performs a depth first search
-- and numbers all the nodes.
-- Adds the method "mark_n"
---------------------------------------------------------------------------
-- local enumerate = require("enumerate")

-- local print_n = require("print_n")

-- local list_fin = require("list_fin")

-- local remark_fin = require("remark_fin")

-- local fstl = require("fst_language")

-- local flatten = require("flatten")

-- local create_arrows = require("create_arrows")

-- local create_states = require("create_states")

-- local state_arrow = require("state_arrow")

--------------------------------------------------------------------------------
-- An interpreter that compiles to a tape
--------------------------------------------------------------------------------
-- local compiler_interpreter = {}

-- translate 'aa' into
-- seq(lit('a'), seq(lit('a'), K))
-- which is
-- seq(-a:a->(q0), seq(-a:a->(q0), K))
-- which is
-- seq(-a:a->(q0), seq(-a:a->))
-- seq(seq(lit('a'), lit('a')), F)
-- then
-- seq(-a:a-> (q0) -0:0-> ((K)),
--     -a:a-> (q0) -0:0-> ((K)),
--     -0:0-> )
-- then
-- seq(-a:a->(q0)-a:a->(q1)-0:0->((F)))

-- function compiler_interpreter.lit(lit)
--    assert(type(lit) == "string")
--    local out = {}
--    out['kind'] = compiler_syntax.kinds.lit
--    out['data'] = lit
--    local litsize = #lit
--    out['states'] =
--    for i in 0, litsize do

--    end
-- end


fst_fast.l = l

return fst_fast
