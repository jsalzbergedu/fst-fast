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

local function add_to_string(str, tbl)
   local mt = getmetatable(tbl)
   if mt == nil then
      mt = {}
   end
   function mt:__tostring()
      return str
   end
   setmetatable(tbl, mt)
end

local syntax = {}
syntax.kinds = {}
syntax.kinds.e = {}
add_to_string("E", syntax.kinds.e)
syntax.kinds.seq = {}
add_to_string("Seq", syntax.kinds.seq)
syntax.kinds.lit = {}
add_to_string("Lit", syntax.kinds.lit)
syntax.kinds.create = {}
add_to_string("Create", syntax.kinds.create)
syntax.kinds.choice = {}
add_to_string("Choice", syntax.kinds.choice)
syntax.kinds.ref = {}
add_to_string("Ref", syntax.kinds.ref)
syntax.kinds.grammar = {}
add_to_string("Grammar", syntax.kinds.grammar)
syntax.kinds.star = {}
add_to_string("Star", syntax.kinds.star)

--------------------------------------------------------------------------------
-- A method to assert that the interpreter can interpret
-- the PEGREG dsl in its entirety
--------------------------------------------------------------------------------
local function assert_pegreg_interpreter(interpreter)
   assert(type(interpreter.e) == "function", "No E")
   assert(type(interpreter.lit) == "function", "No Lit")
   assert(type(interpreter.seq) == "function", "No Seq")
   assert(type(interpreter.choice) == "function", "No Choice")
   assert(type(interpreter.grammar) == "function", "No Grammar")
   assert(type(interpreter.ref) == "function", "No Ref")
   assert(type(interpreter.create) == "function", "No Create")
   assert(type(interpreter.star) == "function", "No Star")
end

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
-- An interpreter to help create translations
--------------------------------------------------------------------------------
local translate = {}

function translate.e()
   local e = {}
   e.kind = syntax.kinds.e
   return e
end

function translate.seq(rule1, rule2)
   local seq = {}
   seq.kind = syntax.kinds.seq
   seq.children = {rule1, rule2}
   return seq
end

function translate.lit(lit)
   local out = {}
   out.kind = syntax.kinds.lit
   out.children = {lit}
   return out
end

function translate.choice(rule1, rule2)
   local choice = {}
   choice.kind = syntax.kinds.choice
   choice.children = {rule1, rule2}
   return choice
end

function translate.star(item)
   local star = {}
   star.kind = syntax.kinds.star
   star.children = {item}
   return star
end

function translate.ref(rule, rules)
   local ref = {}
   ref.kind = syntax.kinds.ref
   ref.children = {rule, rules}
   return ref
end

local function translate_default(o, interpreter)
   local function rebuild(rule)
      local success, rebuilt = o(rule, rebuild)
      if success then
         return rebuilt
      end
      if rule.kind == syntax.kinds.lit then
         return interpreter.lit(rule.children[1])
      end
      if rule.kind == syntax.kinds.star then
         return interpreter.star(rebuild(rule.children[1]))
      end
      if rule.kind == syntax.kinds.seq then
         return interpreter.seq(rebuild(rule.children[1]),
                                rebuild(rule.children[2]))
      end
      if rule.kind == syntax.kinds.choice then
         return interpreter.choice(rebuild(rule.children[1]),
                                   rebuild(rule.children[2]))
      end
      if rule.kind == syntax.kinds.ref then
         return interpreter.ref(rule.children[1], rule.children[2])
      end
      return interpreter.e()
   end
   return rebuild
end

function translate.grammar(item)
   function translate_grammar(o, interpreter)
      return interpreter.grammar(translate_default(o, interpreter)(item))
   end
   return translate_grammar
end

function translate.create(rules, grammar)
   function translate_create(o, interpreter)
      return interpreter.create(rules, grammar(o, interpreter))
   end
   return translate_create
end


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
      return translate.e()
   end

   function sc_to_cs.seq(rule1, rule2)
      return translate.seq(rule1, rule2)
   end

   function sc_to_cs.lit(lit)
      return translate.lit(lit)
   end

   function sc_to_cs.choice(rule1, rule2)
      return translate.choice(rule1, rule2)
   end

   function sc_to_cs.star(item)
      return translate.star(item)
   end

   function sc_to_cs.ref(rule, rules)
      return translate.ref(rule, rules)
   end

   local function rebuild_rec(rule, rebuild)
      -- seq(choice(A, B), K)
      -- to the form
      -- choice(seq(A, K), seq(B, K))
      -- rebuild two sides first to ensure K in hash table

      if rule.children[1].kind == syntax.kinds.choice then
         local a = rule.children[1].children[1]
         local b = rule.children[1].children[2]
         local k = rule.children[2]
         return interpreter.choice(interpreter.seq(rebuild(a),
                                                   rebuild(k)),
                                   interpreter.seq(rebuild(b),
                                                   rebuild(k)))
      end
      return interpreter.seq(rebuild(rule.children[1]),
                             rebuild(rule.children[2]))

   end

   local function rebuild(rule, rebuild)
      if rule.kind == syntax.kinds.seq then
         return true, rebuild_rec(rule, rebuild)
      end
      return false
   end

   function sc_to_cs.grammar(item)
      return translate.grammar(item)
   end

   function sc_to_cs.create(rules, grammar)
      return translate.create(rules, grammar)(rebuild, interpreter)
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

   function expand_string_rec(str, c, acc)
      if str == "" then
         return expand_string.seq(interpreter.lit(c), acc)
      end
      return expand_string_rec(str:sub(1, #str - 1),
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
      return expand_string_rec(lit:sub(1, #lit - 1),
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
      return translate.e()
   end

   function expand_ref.seq(rule1, rule2)
      return translate.seq(rule1, rule2)
   end

   function expand_ref.lit(lit)
      return translate.lit(lit)
   end

   function expand_ref.choice(rule1, rule2)
      return translate.choice(rule1, rule2)
   end

   function expand_ref.ref(rule, rules)
      return translate.ref(rule, rules)
   end

   function expand_ref.star(item)
      return translate.star(item)
   end

   function expand_ref.star(item)
      return translate.star(item)
   end

   local function rebuild(rule, rebuild)
      if rule.kind == syntax.kinds.ref then
         return true, rebuild(rule.children[2][rule.children[1]])
      end
      return false
   end

   function expand_ref.grammar(item)
      return translate.grammar(item)
   end

   function expand_ref.create(rules, grammar)
      return translate.create(rules, grammar)(rebuild, interpreter)
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
-- It adds the method "fin", (and notfin)
-- its interpreter must implement.
---------------------------------------------------------------------------
local mark_fin = {}

function mark_fin.create(interpreter)
   local mark_fin = {}

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
         local out = interpreter.seq(marklasts_rec(item.rule1, false),
                                     marklasts_rec(item.rule2, mark))
         return interpreter.notfin(out)
      end

      if item.kind == syntax.kinds.choice then
         local out = interpreter.choice(marklasts_rec(item.rule1, mark),
                                        marklasts_rec(item.rule2, mark))
         return interpreter.notfin(out)
      end

      if item.kind == syntax.kinds.star then
         local out = interpreter.star(marklasts_rec(item.data, mark))
         return interpreter.notfin(out)
      end

      if mark then
         return interpreter.fin(item.data)
      else
         return interpreter.notfin(item.data)
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
   assert(false, "Refs ought to be expanded")
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

function fin_print_interpreter.notfin(item)
   -- Less cluttered. The information is still there if we need it
   return item
end

--------------------------------------------------------------------------------
-- Assert that the interpreter can interpret
-- PEGREG extended to include fin
--------------------------------------------------------------------------------
local function assert_fin_interpreter(interpreter)
   assert_pegreg_interpreter(interpreter)
   assert(type(interpreter.fin) == "function", "No Fin")
   assert(type(interpreter.notfin) == "function", "No Notfin")
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
assert_pegreg_interpreter(mark_fin.create(fin_print_interpreter))


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
   local notfinkind = {}

   function enumerate.fin(item)
      return {
         kind = finkind,
         data = item
      }
   end

   function enumerate.notfin(item)
      return {
         kind = notfinkind,
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

   function enumerate.ref(rule, rules)
      assert(false, "Refs ought to be expanded")
   end

   local function rebuild(item)
      if item.kind == finkind then
         return interpreter.fin(rebuild(item.data))
      end
      if item.kind == notfinkind then
         return interpreter.notfin(rebuild(item.data))
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
         if top.kind == finkind or
            top.kind == notfinkind
         then
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

function n_print_interpreter.notfin(item)
   return fin_print_interpreter.notfin(item)
end

function n_print_interpreter.star(item)
   return fin_print_interpreter.star(item)
end

function n_print_interpreter.mark_n(item, n)
   local out = string.format("(# %d %s)", n, item)
   return out
end


--------------------------------------------------------------------------------
-- Assert that the interpreter can interpret
-- PEGREG extended to include fin and enumeration
--------------------------------------------------------------------------------
local function assert_n_interpreter(interpreter)
   assert_fin_interpreter(interpreter)
   assert(type(interpreter.mark_n) == "function", "No Mark_n")
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
-- (state N FINAL)
-- (arrow FROM TO INPUT OUTPUT)
-- (create (pair (state N FINAL) ...) (pair (arrow FROM TO INPUT OUTPUT) ...))
-- Where N is the number of the state,
-- FINPUTAL is whether the state is final or not,
-- L is the list of final states,
-- FROM and TO are the state it comes from and goes to,
-- and INPUT and OUTPUT are the input and output characters.
-- These states and arrows will be made into lists of PAIRs
-- ending with NULLs.
--------------------------------------------------------------------------------
local sl = {}

function sl.l(interpreter)
   local l = {}
   l.states = {}
   l.arrows = {}

   function l:state(n, final)
      self.states[#self.states + 1] = interpreter.state(n, final)
      return self
   end

   function l:arrow(from, to, input, output)
      self.arrows[#self.arrows + 1] = interpreter.arrow(from, to, input, output)
      return self
   end

   function l:create()
      local states = interpreter.null()
      for _, v in ipairs(self.states) do
         states = interpreter.pair(v, states)
      end

      local arrows = interpreter.null()
      for _, v in ipairs(self.arrows) do
         arrows = interpreter.pair(v, arrows)
      end

      interpreter.create(states, arrows)
      return self
   end

   return l
end

--------------------------------------------------------------------------------
-- Assert that the state interpreter has all the functions required
--------------------------------------------------------------------------------
local function assert_sl_interpreter(interpreter)
   assert(type(interpreter.state) == "function")
   assert(type(interpreter.arrow) == "function")
   assert(type(interpreter.pair) == "function")
   assert(type(interpreter.null) == "function")
   assert(type(interpreter.create) == "function")
end

--------------------------------------------------------------------------------
-- A print interpreter for the states
--------------------------------------------------------------------------------
local state_print_interpreter = {}

function state_print_interpreter.state(n, final)
   return string.format("(state %d %s)", n, final)
end

function state_print_interpreter.arrow(from, to, input, output)
   return string.format("(arrow %d %d %s %s)", from, to, input, output)
end

function state_print_interpreter.pair(fst, snd)
   return string.format("(pair %s %s)", fst, snd)
end

function state_print_interpreter.null()
   return "null"
end

function state_print_interpreter.create(states, arrows)
   local out = string.format("(create (states %s) (arrows %s))", states, arrows)
   print(out)
   return out
end

--------------------------------------------------------------------------------
-- Test the state print interpreter
--------------------------------------------------------------------------------
print()
print("Testing the state print interpreter")
do
   local sl = sl.l(state_print_interpreter)

   sl:state(0, false)
      :state(1, false)
      :state(2, true)
      :arrow(0, 1, 'a', 'a')
      :arrow(1, 2, 'b', 'b')
      :create()

   assert_sl_interpreter(state_print_interpreter)
end

--------------------------------------------------------------------------------
-- Rules that transform enumerated, finally-marked output
-- to arrows,
-- fin/notfin is abbreviated as "nfin"
-- arrows (nfin a) = (arrows a)
-- arrows (# N1 (empty)) = (lambda (N) {(arrow N N1 '' '')})
-- arrows (# N1 (lit a)) = (lambda (N) {(arrow N N1 a a)})
--
-- arrows (# N1 (seq (nfin (# N2 a)) (nfin (# N3 b)))) = (lambda (N)
--                              (arrow N N1 '' '')
--                              and append
--                              ((arrows a) N1)
--                              and append
--                              (fold append (map (arrows b) (fins a))))
-- arrows (# N1 (choice a b)) = (lambda (N)
--                               (arrow N N1 '' '')
--                                and append
--                                ((arrows a) N1)
--                                and append
--                                ((arrows b) N1)
-- TODO star
--------------------------------------------------------------------------------
local arrows = {}

--------------------------------------------------------------------------------
-- An interpreter that creates lists of
-- numbers of final states.
--------------------------------------------------------------------------------
local fin_lst = {}

function fin_lst.e()
   return {}
end

function fin_lst.lit(item)
   return {}
end

function fin_lst.star(item)
   return item
end

function fin_lst.seq(item1, item2)
   local out = {}
   for _, v in ipairs(item1) do
      table.insert(out, v)
   end
   for _, v in ipairs(item2) do
      table.insert(out, v)
   end
   return out
end

function fin_lst.choice(item1, item2)
   local out = {}
   for _, v in ipairs(item1) do
      table.insert(out, v)
   end
   for _, v in ipairs(item2) do
      table.insert(out, v)
   end
   return out
end

function fin_lst.ref(rule, rules)
   assert(false, "Rules should be expanded")
end

function fin_lst.notfin(item)
   local out = {}
   for _, v in ipairs(item) do
      table.insert(out, v)
   end
   return out
end

function fin_lst.fin(item)
   local out = fin_lst.notfin(item)
   table.insert(out, item.n)
   return out
end

function fin_lst.mark_n(n, item)
   local out = {}
   for _, v in ipairs(item) do
      table.insert(out, v)
   end
   out.n = n
   return out
end

function fin_lst.create(rules, grammar)
   return grammar
end


function fin_lst.grammar(item)
   return item
end

assert_n_interpreter(fin_lst)

local mark_fin_enumerated = {}

function mark_fin_enumerated.create(interpreter)
   local mark_fin_old = mark_fin.create(interpreter)

   local mark_fin = {}

   function mark_fin.e()
      return mark_fin_old.e()
   end

   function mark_fin.lit(lit)
      return mark_fin_old.lit(lit)
   end

   function mark_fin.seq(rule1, rule2)
      return mark_fin_old.seq(rule1, rule2)
   end

   function mark_fin.choice(rule1, rule2)
      return mark_fin_old.choice(rule1, rule2)
   end

   function mark_fin.star(item)
      return mark_fin_old.star(item)
   end

   function mark_fin.ref(rule, rules)
      assert(false, "Ref should be expanded")
   end

   function mark_fin.grammar(item)
      return mark_fin_old.grammar(marklasts(item))
   end

   function mark_fin.create(rules, grammar)
      return mark_fin_old.create(marklasts(item))
   end

   function mark_fin.fin(item)
      return interpreter.fin(item)
   end

   function mark_fin.notfin(item)
      return interpreter.notfin(item)
   end

   function mark_fin.mark_n(item)
      return interpreter.mark_n(item)
   end

   return mark_fin
end

assert_n_interpreter(mark_fin_enumerated.create(fin_lst))

function arrows.create(state_interpreter)
   local finkind = {}
   local notfinkind = {}
   local nkind = {}

   local arrows = {}

   function arrows.e()
      return {
         kind = syntax.kinds.e
      }
   end

   function arrows.seq(rule1, rule2)
      return {
         kind = syntax.kinds.seq,
         rule1 = rule1,
         rule2 = rule2
      }
   end

   function arrows.lit(lit)
      return {
         kind = syntax.kinds.lit,
         data = lit
      }
   end

   function arrows.choice(rule1, rule2)
      return {
         kind = syntax.kinds.choice,
         rule1 = rule1,
         rule2 = rule2
      }
   end

   function arrows.star(item)
      return {
         kind = syntax.kinds.star,
         data = item
      }
   end

   function arrows.ref(rule, rules)
      assert(false, "Refs should be expanded")
   end

   function arrows.fin(item)
      return {
         kind = finkind,
         item = item
      }
   end

   function arrows.notfin(item)
      return {
         kind = notfinkind,
         item = item
      }
   end

   function arrows.mark_n(n, item)
      return {
         kind = nkind,
         n = n,
         item = item
      }
   end

   local function rebuild(rule, interpreter)
      if rule.kind == syntax.kinds.lit then
         return interpreter.lit(rule.data)
      end
      if rule.kind == syntax.kinds.star then
         return interpreter.star(rebuild(rule.data))
      end
      if rule.kind == syntax.kinds.seq then
         interpreter.seq(rebuild(rule.rule1), reubild(rule.rule2))
      end
      if rule.kind == syntax.kinds.choice then
         return interpreter.choice(rebuild(rule.rule1),
                                   rebuild(rule.rule2))
      end
      if rule.kind == finkind then
         return interpreter.fin(rebuild(rule.item))
      end
      if rule.kind == notfinkind then
         return interpreter.notfin(rebuild(rule.item))
      end
      if rule.kind == nkind then
         return interpreter.mark_n(rule.n, rebuild(rule.item))
      end
      return interpreter.e()
   end

   function arrows.grammar(item)

      if item.kind == finkind or
         item.kind == notfinkind
      then
         return arrows.grammar(item.item)
      end

      if item.kind == nkind then
         local n1 = item.n
         if item.item.kind == syntax.kinds.e then
            local function empty_t(n)
               return {state_interpreter.arrow(n, n1, '', '')}
            end
            return empty_t
         end
         if item.item.kind == syntax.kinds.lit then
            local function lit_t(n)
               return {state_interpreter.arrow(n, n1, item.item.data, item.item.data)}
            end
            return lit_t
         end
         if item.item.kind == syntax.kinds.seq then
            local n2 = item.item.rule1.item.n
            local n3 = item.item.rule2.item.n

            local fins = rebuild(item.item.rule1,
                                 mark_fin_enumerated.create(fin_lst))
            local function seq_t(n)
               local out = {}
               local nton1 = state_interpreter.arrow(n, n1, '', '')
               table.insert(out, nton1)
               for _, v in ipairs(arrows.grammar(item.item.rule1)(n1)) do
                  table.insert(out, v)
               end
               for _, v in ipairs(fins) do
                  for _, o in ipairs(arrows.grammar(item.item.rule2)(v)) do
                     table.insert(out, o)
                  end
               end
               return out
            end
            return seq_t
         end
         if item.item.kind == syntax.kinds.choice then
            local function choice_t(n)
               local out = {}
               local nton1 = state_interpreter.arrow(n, n1, '', '')
               table.insert(out, nton1)

               local a = arrows.grammar(item.item.rule1)
               local b = arrows.grammar(item.item.rule2)
               for _, v in ipairs(a(n1)) do
                  table.insert(out, v)
               end
               for _, v in pairs(b(n1)) do
                  table.insert(out, v)
               end
               return out
            end
            return choice_t
         end
      end
      rebuild(item)
   end

   function arrows.create(rules, grammar)
      return grammar
   end

   function arrows.star(item)
      assert(false, "TODO star")
   end

   return arrows
end

assert_n_interpreter(arrows.create(state_print_interpreter))

--------------------------------------------------------------------------------
-- An interpreter that takes in enumerated, finally-marked
-- PEGREG syntax and translates it to calls to the
-- state interpreter
--------------------------------------------------------------------------------
local create_states = {}

function create_states.create(state_interpreter)
   assert_sl_interpreter(state_interpreter)

   local create_states = {}
   local nkind = {}
   local finkind = {}
   local notfinkind = {}

   function create_states.e()
      return {
         kind = syntax.kinds.e
      }
   end

   function create_states.lit(lit)
      return {
         kind = syntax.kinds.lit,
         data = lit
      }
   end

   function create_states.seq(rule1, rule2)
      return {
         kind = syntax.kinds.seq,
         rule1 = rule1,
         rule2 = rule2
      }
   end

   function create_states.choice(rule1, rule2)
      return {
         kind = syntax.kinds.choice,
         rule1 = rule1,
         rule2 = rule2
      }
   end


   function create_states.star(item)
      return {
         kind = syntax.kinds.star,
         item = item
      }
   end

   function create_states.fin(item)
      return {
         kind = finkind,
         item = item
      }
   end

   function create_states.notfin(item)
      return {
         kind = notfinkind,
         item = item
      }
   end

   function create_states.mark_n(item, n)
      return {
         kind = nkind,
         item = item,
         n = n
      }
   end

   function create_states.ref(rule, rules)
      assert(false, "Refs ought to be expanded")
   end

   function create_states.grammar(item)
      local sstack = {[1] = item}
      local states = state_interpreter.null()
      local final = false
      while #sstack > 0 do
         local top = sstack[#sstack]
         sstack[#sstack] = nil
         if top.kind == finkind or
            top.kind == notfinkind
         then
            local state = state_interpreter.state(top.item.n, true)
            states = state_interpreter.pair(state, states)
         elseif top.kind == nkind then
            local state = state_interpreter.state(top.n, false)
            states = state_interpreter.pair(state, states)
            sstack[#sstack + 1] = top.item
         elseif top.kind == syntax.kinds.seq or
                top.kind == syntax.kinds.choice
         then
            sstack[#sstack + 1] = top.rule1
            sstack[#sstack + 1] = top.rule2
         end
      end

      local astack = {[1] = item}
      local arrows = state_interpreter.null()
      return state_interpreter.create(states, arrows)
   end

   function create_states.create(rules, grammar)
      -- Convert to a state language here
      return grammar
   end

   return create_states
end

--------------------------------------------------------------------------------
-- Test the create states interpreter
--------------------------------------------------------------------------------
print()
print("Testing the create states interpreter")
do
   local interpreter = expand_string.create(expand_ref.create(mark_fin.create(enumerate.create(create_states.create(state_print_interpreter)))))
   local l = l.l(interpreter)
   l:rule('A'):is(l:lit('aa'))
      :rule('B'):is(l:lit('bb'))
      :rule('K'):is(l:lit('x'))
      :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('A')))
      :create()
end

--------------------------------------------------------------------------------
-- An interpreter that compiles to a tape
--------------------------------------------------------------------------------
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
