local fst_fast = {}
local c_lib = require("fst_fast")

fst_fast.instruction_tape = {}

runtimes = 0

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
-- A <- aa
-- B <- ab
-- K <- x
-- (A/B)K
--
-- l.rule('A').is('aa')
--  .rule('B').is('bb')
--  .rule('K').is('x')
--  .grammar('(A/B)K')
--  .create(interpreter)

-- l.rule(A).is('aa')
--  .rule(B).is('bb')
--  .rule(K).is('x')
--  .grammar((A+B)*K)
--  .create(interpreter)

-- l.rule(A).is('aa')
--  .rule(B).is('bb')
--  .rule(K).is('x')
--  .grammar(sequence(choice(A, B), K))
--  .create(interpreter)
--
-- local l = language(i)
-- l:rule('A'):is(l:lit('aa'))
--  :rule('B'):is(l:lit('bb'))
--  :rule('K'):is(l:lit('x'))
--  :grammar(l:seq(l:choice('A', 'B'), 'K'))
--  :create()

--------------------------------------------------------------------------------
-- A set of methods for creating languages.
--------------------------------------------------------------------------------
local l = {}

--------------------------------------------------------------------------------
-- A set of methods for creating languages.
--------------------------------------------------------------------------------
function l.l(interpreter)
   local l = {}
   l.rules = {}
   function l:rule(name)
      local rule = {}
      function rule:is(tobind)
         l.rules[name] = tobind
         return l
      end
      return rule
   end

   ----------------------------------------------------------------------------
   -- Create a grammar
   ----------------------------------------------------------------------------
   function l:grammar(item)
      l.grammar = interpreter.grammar(item)
      return l
   end

   ----------------------------------------------------------------------------
   -- Create language out out grammar and rules
   ----------------------------------------------------------------------------
   function l:create()
      interpreter.create(l.rules, l.grammar)
   end

   ----------------------------------------------------------------------------
   -- Literal
   ----------------------------------------------------------------------------
   function l:lit(str)
      return interpreter.lit(str)
   end

   ----------------------------------------------------------------------------
   -- Sequence
   ----------------------------------------------------------------------------
   function l:seq(rule1, rule2)
      return interpreter.seq(rule1, rule2)
   end

   ----------------------------------------------------------------------------
   -- A set of methods for creating languages.
   ----------------------------------------------------------------------------
   function l:choice(rule1, rule2)
      return interpreter.choice(rule1, rule2)
   end

   ----------------------------------------------------------------------------
   -- A set of methods for creating languages.
   ----------------------------------------------------------------------------
   function l:ref(rule)
      return interpreter.ref(rule, l.rules)
   end

   ----------------------------------------------------------------------------
   -- Empty transition
   ----------------------------------------------------------------------------
   function l:e(rule)
      return interpreter.e()
   end

   return l
end

local syntax = {}
syntax.kinds = {}
local kindn = 0
syntax.kinds.e = kindn
kindn = kindn + 1
syntax.kinds.seq = kindn
kindn = kindn + 1
syntax.kinds.lit = kindn
kindn = kindn + 1
syntax.kinds.create = kindn
kindn = kindn + 1
syntax.kinds.choice = kindn
kindn = kindn + 1
syntax.kinds.ref = kindn
kindn = kindn + 1
syntax.kinds.grammar = kindn
kindn = kindn + 1
syntax.kinds.star = kindn

--------------------------------------------------------------------------------
-- A method to assert that the interpreter can interpret
-- the PEGREG dsl in its entirety
--------------------------------------------------------------------------------
local function assert_pegreg_interpreter(interpreter)
   assert(type(interpreter.e) == "function")
   assert(type(interpreter.lit) == "function")
   assert(type(interpreter.seq) == "function")
   assert(type(interpreter.choice) == "function")
   assert(type(interpreter.grammar) == "function")
   assert(type(interpreter.ref) == "function")
   assert(type(interpreter.create) == "function")
   assert(type(interpreter.star) == "function")
end

local print_interpreter = {}

function print_interpreter.e()
   local out = "(empty)"
   return out
end

function print_interpreter.lit(lit)
   local out = string.format("(lit %s)", lit)
   return out
end

function print_interpreter.seq(rule1, rule2)
   local out = string.format("(seq %s %s)", rule1, rule2)
   return out
end

function print_interpreter.choice(rule1, rule2)
   local out = string.format("(choice %s %s)", rule1, rule2)
   return out
end

function print_interpreter.grammar(item)
   local out = string.format("(grammar %s)", item)
   return out
end

function print_interpreter.ref(rule, rules)
   local out = string.format("(ref %s)", rule)
   return out
end

function print_interpreter.star(item)
   local out = string.format("(star %s)", rule)
   return out
end

function print_interpreter.create(rules, grammar)
   local it, tbl = pairs(rules)
   local k, v = it(tbl, nil)
   local rulesstr
   if tbl ~= nil then
      rulesstr = string.format("(rule %s %s)", k, v)
   else
      rulesstr = ""
   end
   k, v = it(tbl, k)
   while k ~= nil do
      rulesstr = rulesstr .. string.format(" (rule %s %s)", k, v)
      k, v = it(tbl, k)
   end
   local out = string.format("(create (rules %s) %s)", rulesstr, grammar)
   print(out)
   return out
end


----------------------------------------------------------------------------
-- Test the print interpreter
----------------------------------------------------------------------------
print()
print("Testing the print interpreter")
do
local l = l.l(print_interpreter)
l:rule('A'):is(l:lit('aa'))
 :rule('B'):is(l:lit('bb'))
 :rule('K'):is(l:lit('x'))
 :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
 :create()
end

assert_pegreg_interpreter(print_interpreter)


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

--------------------------------------------------------------------------------
-- Transform all sequences of the form
-- seq(choice(A, B), K)
-- to the form
-- choice(seq(A, K), seq(B, K))
--------------------------------------------------------------------------------
local sc_to_cs = {}

function sc_to_cs.create(interpreter)
   local sc_to_cs = {}
   function sc_to_cs.e()
      return {
         kind = syntax.kinds.e
      }
   end

   function sc_to_cs.seq(rule1, rule2)
      return {
         kind = syntax.kinds.seq,
         rule1 = rule1,
         rule2 = rule2
      }
   end

   function sc_to_cs.lit(lit)
      return {
         kind = syntax.kinds.lit,
         data = lit
      }
   end

   function sc_to_cs.choice(rule1, rule2)
      return {
         kind = syntax.kinds.choice,
         rule1 = rule1,
         rule2 = rule2
      }
   end

   function sc_to_cs.star(item)
      return {
         kind = syntax.kinds.star,
         data = item
      }
   end

   function sc_to_cs.ref(rule, rules)
      return {
         kind = syntax.kinds.ref,
         rule = rule,
         rules = rules
      }
   end

   local function rebuild(rule)
      if rule.kind == syntax.kinds.lit then
         return interpreter.lit(rule.data)
      end
      if rule.kind == syntax.kinds.star then
         return interpreter.star(rebuild(rule.data))
      end
      if rule.kind == syntax.kinds.seq then
         if rule.rule1.kind == syntax.kinds.choice then
            return interpreter.choice(interpreter.seq(rebuild(rule.rule1.rule1),
                                                      rebuild(rule.rule2)),
                                      interpreter.seq(rebuild(rule.rule1.rule2),
                                                      rebuild(rule.rule2)))
         end
         return interpreter.seq(rebuild(rule.rule1),
                                rebuild(rule.rule2))
      end
      if rule.kind == syntax.kinds.choice then
         return interpreter.choice(rebuild(rule.rule1),
                                   rebuild(rule.rule2))
      end
      if rule.kind == syntax.kinds.ref then
         return interpreter.ref(rule.rule, rule.rules)
      end
      return interpreter.e()
   end

   function sc_to_cs.grammar(item)
      return interpreter.grammar(rebuild(item))
   end

   function sc_to_cs.create(rules, grammar)
      return interpreter.create(rules, grammar)
   end

   return sc_to_cs
end

----------------------------------------------------------------------------
-- Test the sc_to_cs interpreter
---------------------------------------------------------------------------
print()
print("Testing the sc_to_cs interpreter")
do
local l = l.l(sc_to_cs.create(print_interpreter))
l:rule('A'):is(l:lit('aa'))
 :rule('B'):is(l:lit('bb'))
 :rule('K'):is(l:lit('x'))
 :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
 :create()
end

assert_pegreg_interpreter(sc_to_cs.create(print_interpreter))

----------------------------------------------------------------------------
-- Expand strings into sequences
---------------------------------------------------------------------------
local expand_string = {}

function expand_string.create(interpreter)
   local expand_string = {}
   function expand_string.e()
      return interpreter.e()
   end

   function expand_string.seq(rule1, rule2)
      return interpreter.seq(rule1, rule2)
   end

   function expand_string_req(str, c, acc)
      if str == "" then
         return expand_string.seq(interpreter.lit(c), acc)
      end
      return expand_string_req(str:sub(1, #str - 1),
                               str:sub(#str, #str),
                               expand_string.seq(interpreter.lit(c), acc))
   end

   function expand_string.lit(lit)
      assert(type(lit) == "string")
      if lit == "" then
         return expand_string.e()
      end
      if #lit == 1 then
         return interpreter.lit(lit)
      end
      local acc = expand_string.e()
      return expand_string_req(lit:sub(1, #lit - 1),
                               lit:sub(#lit, #lit),
                               acc)
   end

   function expand_string.choice(rule1, rule2)
      return interpreter.choice(rule1, rule2)
   end

   function expand_string.grammar(item)
      return interpreter.grammar(item)
   end

   function expand_string.ref(rule, rules)
      return interpreter.ref(rule, rules)
   end

   function expand_string.create(rules, grammar)
      return interpreter.create(rules, grammar)
   end

   function expand_string.star(item)
      return interpreter.star(item)
   end

   return expand_string
end


----------------------------------------------------------------------------
-- Test the expand string interpreter
---------------------------------------------------------------------------
print()
print("Testing the expand string interpreter")
do
local l = l.l(expand_string.create(print_interpreter))
l:rule('A'):is(l:lit('aa'))
 :rule('B'):is(l:lit('bb'))
 :rule('K'):is(l:lit('x'))
 :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
 :create()
end

assert_pegreg_interpreter(expand_string.create(print_interpreter))


----------------------------------------------------------------------------
-- An interpreter that expands refs.
-- Removes the neccessity for
-- ref on the interpreter it is called on.
---------------------------------------------------------------------------
local expand_ref = {}

function expand_ref.create(interpreter)
   local expand_ref = {}
   function expand_ref.e()
      return {
         kind = syntax.kinds.e
      }
   end

   function expand_ref.seq(rule1, rule2)
      return {
         kind = syntax.kinds.seq,
         rule1 = rule1,
         rule2 = rule2
      }
   end

   function expand_ref.lit(lit)
      return {
         kind = syntax.kinds.lit,
         data = lit
      }
   end

   function expand_ref.choice(rule1, rule2)
      return {
         kind = syntax.kinds.choice,
         rule1 = rule1,
         rule2 = rule2
      }
   end

   function expand_ref.ref(rule, rules)
      return {
         kind = syntax.kinds.ref,
         rule = rule,
         rules = rules
      }
   end

   function expand_ref.star(item)
      return {
         kind = syntax.kinds.star,
         data = item
      }
   end

   local function rebuild(rule)
      if rule.kind == syntax.kinds.lit then
         return interpreter.lit(rule.data)
      end
      if rule.kind == syntax.kinds.star then
         return interpreter.star(rebuild(rule.data))
      end
      if rule.kind == syntax.kinds.seq then
         return interpreter.seq(rebuild(rule.rule1),
                                rebuild(rule.rule2))
      end
      if rule.kind == syntax.kinds.choice then
         return interpreter.choice(rebuild(rule.rule1),
                                   rebuild(rule.rule2))
      end
      if rule.kind == syntax.kinds.ref then
         return rebuild(rule.rules[rule.rule])
      end
      return interpreter.e()
   end


   function expand_ref.grammar(item)
      return interpreter.grammar(rebuild(item))
   end


   function expand_ref.create(rules, grammar)
      return interpreter.create(rules, grammar)
   end

   function expand_ref.star(item)
      return interpreter.star(item)
   end

   return expand_ref
end


----------------------------------------------------------------------------
-- Test the expand ref interpreter
---------------------------------------------------------------------------
print()
print("Testing the expand ref interpreter")
do
local l = l.l(expand_ref.create(expand_string.create(print_interpreter)))
l:rule('A'):is(l:lit('aa'))
 :rule('B'):is(l:lit('bb'))
 :rule('K'):is(l:lit('x'))
 :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
 :create()
end

assert_pegreg_interpreter(expand_ref.create(print_interpreter))


----------------------------------------------------------------------------
-- An interpreter that marks things as final.
-- It adds the method "fin", which
-- its interpreter must implement.
---------------------------------------------------------------------------
local mark_fin = {}

function mark_fin.create(interpreter)
   local mark_fin = {}

   function mark_fin.fin(item)
      interpreter.fin(item)
   end

   function mark_fin.e()
      return {
         kind = syntax.kinds.e,
         data = interpreter.e()
      }
   end

   function mark_fin.lit(lit)
      return {
         kind = syntax.kinds.lit,
         data = interpreter.lit(lit)
      }
   end

   function mark_fin.seq(rule1, rule2)
      return {
         kind = syntax.kinds.seq,
         rule1 = rule1,
         rule2 = rule2,
      }
   end


   function mark_fin.choice(rule1, rule2)
      return {
         kind = syntax.kinds.choice,
         rule1 = rule1,
         rule2 = rule2,
      }
   end

   function mark_fin.star(item)
      return {
         kind = syntax.kinds.star,
         data = item
      }
   end

   function mark_fin.ref(rule, rules)
      return rules[rule]
   end

   function marklasts_rec(item, mark)
      if item.kind == syntax.kinds.seq then
         return interpreter.seq(marklasts_rec(item.rule1, false),
                                marklasts_rec(item.rule2, mark))
      end

      if item.kind == syntax.kinds.choice then
         return interpreter.choice(marklasts_rec(item.rule1, mark),
                                   marklasts_rec(item.rule2, mark))
      end

      if item.kind == syntax.kinds.star then
         return interpreter.star(marklasts_rec(item.data, mark))
      end

      if mark then
         return interpreter.fin(item.data)
      else
         return item.data
      end
   end

   function marklasts(item)
      return marklasts_rec(item, true)
   end

   function mark_fin.grammar(item)
      return interpreter.grammar(marklasts(item))
   end

   function mark_fin.create(rules, grammar)
      return interpreter.create(rules, grammar)
   end


   return mark_fin
end

----------------------------------------------------------------------------
-- A print interpreter adding fin functionality
---------------------------------------------------------------------------
local fin_print_interpreter = {}

function fin_print_interpreter.e()
   return print_interpreter.e()
end

function fin_print_interpreter.lit(lit)
   return print_interpreter.lit(lit)
end

function fin_print_interpreter.seq(rule1, rule2)
   return print_interpreter.seq(rule1, rule2)
end

function fin_print_interpreter.choice(rule1, rule2)
   return print_interpreter.choice(rule1, rule2)
end

function fin_print_interpreter.grammar(item)
   return print_interpreter.grammar(item)
end

function fin_print_interpreter.ref(rule, rules)
   return print_interpreter.ref(rule, rules)
end

function fin_print_interpreter.create(rules, grammar)
   return print_interpreter.create(rules, grammar)
end

function fin_print_interpreter.star(item)
   return print_interpreter.star(rules, grammar)
end

function fin_print_interpreter.fin(item)
   local out = string.format("(fin %s)", item)
   return out
end


--------------------------------------------------------------------------------
-- Assert that the interpreter can interpret
-- PEGREG extended to include fin
--------------------------------------------------------------------------------
local function assert_fin_interpreter(interpreter)
   assert(type(interpreter.e) == "function")
   assert(type(interpreter.lit) == "function")
   assert(type(interpreter.seq) == "function")
   assert(type(interpreter.choice) == "function")
   assert(type(interpreter.grammar) == "function")
   assert(type(interpreter.ref) == "function")
   assert(type(interpreter.create) == "function")
   assert(type(interpreter.star) == "function")
   assert(type(interpreter.fin) == "function")
end

----------------------------------------------------------------------------
-- Test the fin print
---------------------------------------------------------------------------
print()
print("Testing the mark final interpreters")
do
   local l = l.l(expand_string.create(expand_ref.create(mark_fin.create(fin_print_interpreter))))
   l:rule('A'):is(l:lit('aa'))
      :rule('B'):is(l:lit('bb'))
      :rule('K'):is(l:lit('x'))
      :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
      :create()
end

do
   local l = l.l(expand_string.create(expand_ref.create(mark_fin.create(fin_print_interpreter))))
   l:rule('A'):is(l:lit('aa'))
      :rule('B'):is(l:lit('bb'))
      :rule('K'):is(l:lit('x'))
      :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('A')))
      :create()
end

assert_fin_interpreter(fin_print_interpreter)
assert_fin_interpreter(mark_fin.create(fin_print_interpreter))



-- Now that we have all of the "states" of our nondeterministic FST,
-- we will want to enumerate them.
-- We will be using a depth first search to do this.

----------------------------------------------------------------------------
-- An interpreter that performs a depth first search
-- and numbers all the nodes.
-- Adds the method "mark_n"
---------------------------------------------------------------------------
local enumerate = {}

function enumerate.create(interpreter)
   local enumerate = {}
   local finkind = {}

   function enumerate.fin(item)
      return {
         kind = finkind,
         data = item
      }
   end

   function enumerate.e()
      return {kind = syntax.kinds.e}
   end

   function enumerate.seq(rule1, rule2)
      return {
         kind = syntax.kinds.seq,
         rule1 = rule1,
         rule2 = rule2
      }
   end

   function enumerate.lit(lit)
      return {
         kind = syntax.kinds.lit,
         data = lit
      }
   end

   function enumerate.choice(rule1, rule2)
      return {
         kind = syntax.kinds.choice,
         rule1 = rule1,
         rule2 = rule2
      }
   end

   function enumerate.star(item)
      return {
         kind = syntax.kinds.star,
         data = item
      }
   end

   function enumerate.mark_n(item)
      return interpreter.mark_n(item)
   end

   function enumerate.ref(item)
      return interpreter.ref(item)
   end

   local function rebuild(item)
      if item.kind == finkind then
         return interpreter.fin(rebuild(item.data))
      end
      local n = item.n
      if item.kind == syntax.kinds.lit then
         return interpreter.mark_n(interpreter.lit(item.data), n)
      end
      if item.kind == syntax.kinds.seq then
         return interpreter.mark_n(interpreter.seq(rebuild(item.rule1),
                                                   rebuild(item.rule2)),
                                   n)
      end
      if item.kind == syntax.kinds.choice then
         return interpreter.mark_n(interpreter.choice(rebuild(item.rule1),
                                                      rebuild(item.rule2)),
                                   n)
      end
      if item.kind == syntax.kinds.star then
         return interpreter.mark_n(interpreter.star(rebuild(item.data)))
      end
      return interpreter.mark_n(interpreter.e(), n)
   end

   function enumerate.grammar(item)
      local stack = {[1] = item}
      local n = 0
      while #stack > 0 do
         local top = stack[#stack]
         stack[#stack] = nil
         if top.kind == finkind then
            stack[#stack + 1] = top.data
         else
            top.n = n
            n = n + 1
            if top.kind == syntax.kinds.seq or
               top.kind == syntax.kinds.choice
            then
               stack[#stack + 1] = top.rule2
               stack[#stack + 1] = top.rule1
            end
         end
      end
      local rebuilt = rebuild(item);
      return interpreter.grammar(rebuilt)
   end

   function enumerate.create(rules, grammar)
      return interpreter.create(rules, grammar)
   end


   return enumerate
end

----------------------------------------------------------------------------
-- A print interpreter that adds mark_n functionality
---------------------------------------------------------------------------
local n_print_interpreter = {}

function n_print_interpreter.e()
   return fin_print_interpreter.e()
end

function n_print_interpreter.lit(lit)
   return fin_print_interpreter.lit(lit)
end

function n_print_interpreter.seq(rule1, rule2)
   return fin_print_interpreter.seq(rule1, rule2)
end

function n_print_interpreter.choice(rule1, rule2)
   return fin_print_interpreter.choice(rule1, rule2)
end

function n_print_interpreter.grammar(item)
   return fin_print_interpreter.grammar(item)
end

function n_print_interpreter.ref(rule, rules)
   return fin_print_interpreter.ref(rule, rules)
end

function n_print_interpreter.create(rules, grammar)
   return fin_print_interpreter.create(rules, grammar)
end

function n_print_interpreter.fin(item)
   return fin_print_interpreter.fin(item)
end

function n_print_interpreter.star(item)
   return fin_print_interpreter.star(item)
end

function n_print_interpreter.mark_n(item, n)
   local out = string.format("(%d %s)", n, item)
   return out
end

--------------------------------------------------------------------------------
-- Assert that the interpreter can interpret
-- PEGREG extended to include fin and enumeration
--------------------------------------------------------------------------------
local function assert_n_interpreter(interpreter)
   assert(type(interpreter.e) == "function")
   assert(type(interpreter.lit) == "function")
   assert(type(interpreter.seq) == "function")
   assert(type(interpreter.choice) == "function")
   assert(type(interpreter.grammar) == "function")
   assert(type(interpreter.ref) == "function")
   assert(type(interpreter.create) == "function")
   assert(type(interpreter.star) == "function")
   assert(type(interpreter.fin) == "function")
   assert(type(interpreter.mark_n) == "function")
end

----------------------------------------------------------------------------
-- Testing the enumerate interpreter
---------------------------------------------------------------------------
print()
print("Testing the enumerate interpreters")
do
   local l = l.l(expand_string.create(expand_ref.create(mark_fin.create(enumerate.create(n_print_interpreter)))))
   l:rule('A'):is(l:lit('aa'))
      :rule('B'):is(l:lit('bb'))
      :rule('K'):is(l:lit('x'))
      :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
      :create()
end

do
   local l = l.l(expand_string.create(expand_ref.create(mark_fin.create(enumerate.create(n_print_interpreter)))))
   l:rule('A'):is(l:lit('aa'))
      :rule('B'):is(l:lit('bb'))
      :rule('K'):is(l:lit('x'))
      :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('A')))
      :create()
end

assert_n_interpreter(n_print_interpreter)
assert_n_interpreter(enumerate.create(n_print_interpreter))

--------------------------------------------------------------------------------
-- Now that we have all of these layers,
-- we can output states and arrows for _unordered_ choice.
-- States and arrows are another language; they are made of these elements:
-- (states N L)
-- (arrow FROM TO IN OUT)
-- (create (states N) (arrow FROM TO IN OUT) (arrow ...) ...)
-- Where N is the number of states,
-- L is the list of final states,
-- FROM and TO are the state it comes from and goes to,
-- and IN and OUT are the input and output characters.
--------------------------------------------------------------------------------
-- SL stands for state language.
local sl = {}



----------------------------------------------------------------------------
-- An interpreter that compiles to a tape
---------------------------------------------------------------------------
local compiler_interpreter = {}

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
