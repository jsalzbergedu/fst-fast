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
function l.l()
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
      self.grammar = function(interpreter)
         return interpreter.grammar(item(interpreter))
      end
      return l
   end

   ----------------------------------------------------------------------------
   -- Create language out out grammar and rules
   ----------------------------------------------------------------------------
   function l:create(interpreter)
      return interpreter.create(l.grammar(interpreter))
   end

   ----------------------------------------------------------------------------
   -- Literal
   ----------------------------------------------------------------------------
   function l:lit(str)
      return function (interpreter)
         return interpreter.lit(str)
      end
   end

   ----------------------------------------------------------------------------
   -- Sequence
   ----------------------------------------------------------------------------
   function l:seq(rule1, rule2)
      return function (interpreter)
         return interpreter.seq(rule1(interpreter), rule2(interpreter))
      end
   end

   ----------------------------------------------------------------------------
   -- A set of methods for creating languages.
   ----------------------------------------------------------------------------
   function l:choice(rule1, rule2)
      return function (interpreter)
         return interpreter.choice(rule1(interpreter), rule2(interpreter))
      end
   end

   ----------------------------------------------------------------------------
   -- A set of methods for creating languages.
   ----------------------------------------------------------------------------
   function l:ref(rule)
      return function (interpreter)
         return interpreter.ref(rule, self.rules)
      end
   end

   ----------------------------------------------------------------------------
   -- Empty transition
   ----------------------------------------------------------------------------
   function l:e(rule)
      return function (interpreter)
         return interpreter.e()
      end
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

function print_interpreter.create(grammar)
   local out = string.format("(create %s)", grammar)
   print(out)
   return out
end


----------------------------------------------------------------------------
-- Test the print interpreter
----------------------------------------------------------------------------
print()
print("Testing the print interpreter")
do
local l = l.l()
l:rule('A'):is(l:lit('aa'))
 :rule('B'):is(l:lit('bb'))
 :rule('K'):is(l:lit('x'))
 :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
 :create(print_interpreter)
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

function sc_to_cs.e()
   return function(f, t, data, interpreter)
      return f(false, interpreter.e(), nil, nil)
   end
end

function sc_to_cs.lit(lit)
   return function(interpreter, f, a, b)
      return f(false, interpreter.lit(lit), nil, nil)
   end
end

local function extract(t, data, a, b)
   return t, data, a, b
end

function sc_to_cs.choice(rule1, rule2)
   return function (interpreter, f, a, b)
      local _, erule1, _, _ = rule1(interpreter, extract, nil, nil)
      local _, erule2, _, _ = rule2(interpreter, extract, nil, nil)
      return f(true, interpreter.choice(erule1, erule2), erule1, erule2)
   end
end

function sc_to_cs.star(item)
   return function (interpreter, f, a, b)
      local _, item, _, _ = item(interpreter, extract)
      return f(false, interpreter.star(item), nil, nil)
   end
end

function sc_to_cs.ref(rule, rules)
   return function (interpreter, f, a, b)
      local t, _, a, b = (rules[rule])(interpreter, extract, nil, nil)
      return f(t, interpreter.ref(rule, rules), a, b)
   end
end

function sc_to_cs.seq(rule1, rule2)
   return function (interpreter, f, a, b)
      local t, left, a, b = rule1(interpreter, extract, nil, nil)
      local _, k, _, _ = rule2(interpreter, extract, nil, nil)
      if t then
         local newleft = interpreter.seq(a, k)
         local newright = interpreter.seq(b, k)
         local newout = interpreter.choice(newleft, newright)
         return f(false, newout, nil, nil)
      end
      return f(false, interpreter.seq(left, k), nil, nil)
   end
end

function sc_to_cs.grammar(item)
   return function (interpreter)
      local _, item, _, _ = item(interpreter, extract, nil, nil)
      return interpreter.grammar(item)
   end
end

function sc_to_cs.create(grammar)
   return function (interpreter)
      return interpreter.create(grammar(interpreter))
   end
end

----------------------------------------------------------------------------
-- Test the sc_to_cs interpreter
---------------------------------------------------------------------------
print()
print("Testing the sc_to_cs interpreter")
do
   local l = l.l()
   l:rule('A'):is(l:lit('aa'))
    :rule('B'):is(l:lit('bb'))
    :rule('K'):is(l:lit('x'))
    :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
    :create(sc_to_cs)(print_interpreter)
end

assert_pegreg_interpreter(sc_to_cs)



----------------------------------------------------------------------------
-- Expand strings into sequences
---------------------------------------------------------------------------
local expand_string = {}

function expand_string.e()
   return function (interpreter)
      return interpreter.e()
   end
end

function expand_string.seq(rule1, rule2)
   return function (interpreter)
      return interpreter.seq(rule1(interpreter),
                             rule2(interpreter))
   end
end

local function expand_string_rec(interpreter, str, c, acc)
   if str == "" then
      return interpreter.seq(interpreter.lit(c), acc)
   end
   return expand_string_rec(interpreter,
                            str:sub(1, #str - 1),
                            str:sub(#str, #str),
                            interpreter.seq(interpreter.lit(c), acc))
end

function expand_string.lit(lit)
   return function (interpreter)
      assert(type(lit) == "string")
      if lit == "" then
         return interpreter.e()
      end
      if #lit == 1 then
         return interpreter.lit(lit)
      end
      local acc = interpreter.e()
      return expand_string_rec(interpreter,
                               lit:sub(1, #lit - 1),
                               lit:sub(#lit, #lit),
                               acc)
   end
end

function expand_string.choice(rule1, rule2)
   return function(interpreter)
      return interpreter.choice(rule1(interpreter), rule2(interpreter))
   end
end

function expand_string.grammar(item)
   return function(interpreter)
      return interpreter.grammar(item(interpreter))
   end
end

function expand_string.ref(rule, rules)
   return function(interpreter)
      return interpreter.ref(rule, rules)
   end
end

function expand_string.create(grammar)
   return function(interpreter)
      return interpreter.create(grammar(interpreter))
   end
end

function expand_string.star(item)
   return function(interpreter)
      return interpreter.star(item(interpreter))
   end
end

----------------------------------------------------------------------------
-- Test the expand string interpreter
---------------------------------------------------------------------------
print()
print("Testing the expand string interpreter")
do
local l = l.l()
l:rule('A'):is(l:lit('aa'))
 :rule('B'):is(l:lit('bb'))
 :rule('K'):is(l:lit('x'))
 :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
 :create(expand_string)(print_interpreter)
end

assert_pegreg_interpreter(expand_string)


----------------------------------------------------------------------------
-- An interpreter that expands refs.
-- Removes the neccessity for
-- ref on the interpreter it is called on.
---------------------------------------------------------------------------
local expand_ref = {}
function expand_ref.e()
   return function (i)
      return i.e()
   end
end

function expand_ref.seq(rule1, rule2)
   return function (i)
      return i.seq(rule1(i), rule2(i))
   end
end

function expand_ref.lit(lit)
   return function (i)
      return i.lit(lit)
   end
end

function expand_ref.choice(rule1, rule2)
   return function (i)
      return i.choice(rule1(i), rule2(i))
   end
end

function expand_ref.ref(rule, rules)
   return rules[rule]
end

function expand_ref.star(item)
   return function (i)
      i.star(item(i))
   end
end

function expand_ref.grammar(item)
   return function (interpreter)
      return interpreter.grammar(item(interpreter))
   end
end

function expand_ref.create(grammar)
   return function (interpreter)
      return interpreter.create(grammar(interpreter))
   end
end

----------------------------------------------------------------------------
-- Test the expand ref interpreter
---------------------------------------------------------------------------
print()
print("Testing the expand ref interpreter")
do
local l = l.l()
l:rule('A'):is(l:lit('aa'))
 :rule('B'):is(l:lit('bb'))
 :rule('K'):is(l:lit('x'))
 :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
 :create(expand_ref)(expand_string)(print_interpreter)
end

assert_pegreg_interpreter(expand_ref)


--------------------------------------------------------------------------------
-- Add left and right to seq and choice
--------------------------------------------------------------------------------
local add_left_right = {}

function add_left_right.e()
   return function (i)
      return i.e()
   end
end

function add_left_right.lit(lit)
   return function (i)
      return i.lit(lit)
   end
end

function add_left_right.seq(rule1, rule2)
   return function (i)
      return i.seq(i.left(rule1(i)), i.right(rule2(i)))
   end
end

function add_left_right.choice(rule1, rule2)
   return function (i)
     return i.choice(i.left(rule1(i)), i.right(rule2(i)))
   end
end

function add_left_right.grammar(item)
   return function (i)
      return i.grammar(item(i))
   end
end

function add_left_right.ref(rule, rules)
   return rules[rule]
end

function add_left_right.create(grammar)
   return function (i)
      return i.create(grammar(i))
   end
end

function add_left_right.star(item)
   return function (i)
      return i.star(item(i))
   end
end

assert_pegreg_interpreter(add_left_right)

local lr_print_interpreter = {}
for k, v in pairs(print_interpreter) do
   lr_print_interpreter[k] = v
end

function lr_print_interpreter.left(item)
   return string.format("(left %s)", item)
end

function lr_print_interpreter.right(item)
   return string.format("(right %s)", item)
end

--------------------------------------------------------------------------------
-- Ensures that the interpreter has PEGREG extended with left_right
--------------------------------------------------------------------------------
local function assert_leftright_interpreter(interpreter)
   assert_pegreg_interpreter(interpreter)
   assert(type(interpreter.left) == "function", "No Left")
   assert(type(interpreter.right) == "function", "No Right")
end

----------------------------------------------------------------------------
-- Test the left right interpreter
---------------------------------------------------------------------------
print()
print("Testing the left right interpreter")
do
local l = l.l()
l:rule('A'):is(l:lit('aa'))
 :rule('B'):is(l:lit('bb'))
 :rule('K'):is(l:lit('x'))
 :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
 :create(expand_ref)(expand_string)(add_left_right)(lr_print_interpreter)
end

assert_leftright_interpreter(lr_print_interpreter)

----------------------------------------------------------------------------
-- An interpreter that marks things as final.
-- It adds the method "fin", (and notfin)
-- its interpreter must implement.
---------------------------------------------------------------------------
local mark_fin = {}

function mark_fin.e()
   return function (t, interpreter)
      if t then
         return interpreter.fin(interpreter.e())
      else
         return interpreter.notfin(interpreter.e())
      end
   end
end

function mark_fin.lit(lit)
   return function (t, interpreter)
      if t then
         return interpreter.fin(interpreter.lit(lit))
      else
         return interpreter.notfin(interpreter.lit(lit))
      end
   end
end

function mark_fin.seq(rule1, rule2)
   return function (t, interpreter)
      return interpreter.notfin(interpreter.seq(rule1(false, interpreter),
                                                rule2(t, interpreter)))
   end
end


function mark_fin.choice(rule1, rule2)
   return function (t, interpreter)
      return interpreter.notfin(interpreter.choice(rule1(t, interpreter), rule2(t, interpreter)))
   end
end

function mark_fin.star(item)
   return function (t, interpreter)
      return interpreter.notfin(interpreter.star(item(t, interpreter)))
   end
end

function mark_fin.left(item)
   return function (t, interpreter)
      return interpreter.notfin(interpreter.left(item(t, interpreter)))
   end
end

function mark_fin.right(item)
   return function (t, interpreter)
      return interpreter.notfin(interpreter.right(item(t, interpreter)))
   end
end

function mark_fin.ref(rule, rules)
   assert(false, "Refs should be expanded")
end

function mark_fin.grammar(item)
   return function (interpreter)
      return interpreter.grammar(item(true, interpreter))
   end
end

function mark_fin.create(grammar)
   return function (interpreter)
      return interpreter.create(grammar(interpreter))
   end
end

----------------------------------------------------------------------------
-- A print interpreter adding fin functionality
---------------------------------------------------------------------------
local fin_print_interpreter = {}

for k, v in pairs(lr_print_interpreter) do
   fin_print_interpreter[k] = v
end

function fin_print_interpreter.fin(item)
   local out = string.format("(fin %s)", item)
   return out
end

function fin_print_interpreter.notfin(item)
   -- Less cluttered. The information is still there if we need it
   local out = string.format("(notfin %s)", item)
   return out
end

--------------------------------------------------------------------------------
-- Assert that the interpreter can interpret
-- PEGREG extended to include fin
--------------------------------------------------------------------------------
local function assert_fin_interpreter(interpreter)
   assert_leftright_interpreter(interpreter)
   assert(type(interpreter.fin) == "function", "No Fin")
   assert(type(interpreter.notfin) == "function", "No Notfin")
end

----------------------------------------------------------------------------
-- Test the fin print
---------------------------------------------------------------------------
print()
print("Testing the mark final interpreters")
do
   local l = l.l()
   l:rule('A'):is(l:lit('aa'))
      :rule('B'):is(l:lit('bb'))
      :rule('K'):is(l:lit('x'))
      :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
      :create(expand_ref)(expand_string)(mark_fin)(fin_print_interpreter)
end

do
   local l = l.l()
   l:rule('A'):is(l:lit('aa'))
      :rule('B'):is(l:lit('bb'))
      :rule('K'):is(l:lit('x'))
      :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('A')))
      :create(expand_ref)(expand_string)(mark_fin)(fin_print_interpreter)
end

assert_fin_interpreter(fin_print_interpreter)
assert_pegreg_interpreter(mark_fin)


-- Now that we have all of the "states" of our nondeterministic FST,
-- we will want to enumerate them.
-- We will be using a depth first search to do this.

----------------------------------------------------------------------------
-- An interpreter that performs a depth first search
-- and numbers all the nodes.
-- Adds the method "mark_n"
---------------------------------------------------------------------------
local enumerate = {}


local function extract(item, sum)
   return item, sum
end

function enumerate.fin(item)
   return function (i, n, k)
      local child, sum_child = item(i, n, extract)
      return k(i.fin(child), sum_child)
   end
end

function enumerate.notfin(item)
   return function (i, n, k)
      local child, sum_child = item(i, n, extract)
      return k(i.notfin(child), sum_child)
   end
end

function enumerate.e()
   return function (i, n, k)
      return k(i.mark_n(n + 1, i.e()), n + 1)
   end
end


function enumerate.lit(lit)
   return function (i, n, k)
      return k(i.mark_n(n + 1, i.lit(lit)), n + 1)
   end
end

function enumerate.seq(rule1, rule2)
   return function (i, n, k)
      local mark_self = n + 1
      local left, sum_left = rule1(i, n + 1, extract)
      local right, sum_right = rule2(i, sum_left, extract)
      return k(i.mark_n(mark_self, i.seq(left, right)), sum_right)
   end
end

function enumerate.choice(rule1, rule2)
   return function (i, n, k)
      local mark_self = n + 1
      local left, sum_left = rule1(i, n + 1, extract)
      local right, sum_right = rule2(i, sum_left, extract)
      return k(i.mark_n(mark_self, i.choice(left, right)), sum_right)
   end
end

function enumerate.star(item)
   return function (i, n, k)
      local mark_self = n + 1
      local child, sum_child = item(i, n + 1, extract)
      return k(i.mark_n(mark_self, i.star(child)), sum_child)
   end
end

function enumerate.right(item)
   return function (i, n, k)
      local mark_self = n + 1
      local child, sum_child = item(i, n + 1, extract)
      return k(i.mark_n(mark_self, i.right(child)), sum_child)
   end
end

function enumerate.left(item)
   return function (i, n, k)
      local mark_self = n + 1
      local child, sum_child = item(i, n + 1, extract)
      return k(i.mark_n(mark_self, i.left(child)), sum_child)
   end
end

function enumerate.ref(rule, rules)
   assert(false, "Refs ought to be expanded")
end

function enumerate.grammar(item)
   return function (i)
      local item, _ = item(i, -1, extract)
      return i.grammar(item)
   end
end

function enumerate.create(grammar)
   return function (i)
      return i.create(grammar(i))
   end
end

----------------------------------------------------------------------------
-- A print interpreter that adds mark_n functionality
---------------------------------------------------------------------------
local n_print_interpreter = {}

for k, v in pairs(fin_print_interpreter) do
   n_print_interpreter[k] = v
end

function n_print_interpreter.mark_n(n, item)
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
   local l = l.l()
   l:rule('A'):is(l:lit('aa'))
      :rule('B'):is(l:lit('bb'))
      :rule('K'):is(l:lit('x'))
      :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
      :create(expand_ref)(expand_string)(mark_fin)(enumerate)(n_print_interpreter)
end

do
   local l = l.l()
   l:rule('A'):is(l:lit('aa'))
      :rule('B'):is(l:lit('bb'))
      :rule('K'):is(l:lit('x'))
      :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('A')))
      :create(expand_ref)(expand_string)(mark_fin)(enumerate)(n_print_interpreter)
end

assert_n_interpreter(n_print_interpreter)
assert_fin_interpreter(enumerate)

--------------------------------------------------------------------------------
-- An interpreter that returns a list of final states
--------------------------------------------------------------------------------
local list_finstates = {}
print("list_finstates is:", list_finstates)

function list_finstates.e()
   return function (f)
      return f({}, false)
   end
end

function list_finstates.lit(lit)
   return function (f)
      return f({}, false)
   end
end

local function extract(lst, t)
   return lst, t
end

function list_finstates.seq(rule1, rule2)
   return function (f)
      local lst1, _ = rule1(extract)
      local lst2, _ = rule2(extract)
      local out = {}
      for _, v in ipairs(lst1) do
         table.insert(out, v)
      end
      for _, v in ipairs(lst2) do
         table.insert(out, v)
      end
      return f(out, false)
   end
end

function list_finstates.choice(rule1, rule2)
   return list_finstates.seq(rule1, rule2)
end

function list_finstates.grammar(item)
   local lst, _ = item(extract)
   return lst
end

function list_finstates.ref(rule, rules)
   assert(false, "Refs should be expanded")
end

function list_finstates.create(grammar)
   return grammar
end

function list_finstates.star(item)
   return item
end

function list_finstates.left(item)
   return item
end

function list_finstates.right(item)
   return item
end

function list_finstates.fin(item)
   local lst, _ = item(extract)
   return function (f)
      return f(lst, true)
   end
end

function list_finstates.notfin(item)
   local lst, _ = item(extract)
   return function (f)
      return f(lst, false)
   end
end

function list_finstates.mark_n(n, item)
   return function (f)
      local lst, t = item(extract)
      local out = {}
      for _, v in ipairs(lst) do
         table.insert(out, v)
      end
      if t then
         table.insert(out, n)
      end
      return f(out, false)
   end
end

assert_n_interpreter(list_finstates)

--------------------------------------------------------------------------------
-- An interpreter that remarks final states
--------------------------------------------------------------------------------
local remark_fin = {}
print("remark fin is: ", remark_fin)
for k, v in pairs(mark_fin) do
   remark_fin[k] = v
end

function remark_fin.mark_n(n, item)
   return function (t, i)
      return i.mark_n(n, item(t, i))
   end
end

function remark_fin.fin(item)
   return item
end

function remark_fin.notfin(item)
   return item
end

assert_n_interpreter(remark_fin)

--------------------------------------------------------------------------------
-- Test list finstates
--------------------------------------------------------------------------------
do
   print()
   print("Testing the list finstates interpreter")
   local l = l.l()
   local states = l:rule('A'):is(l:lit('aa'))
      :rule('B'):is(l:lit('bb'))
      :rule('K'):is(l:lit('x'))
      :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('A')))
      :create(expand_ref)(expand_string)(mark_fin)(enumerate)(list_finstates)
   for i, v in ipairs(states) do
      print(i, v)
   end
end

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
   assert(type(interpreter.state) == "function", "No State")
   assert(type(interpreter.arrow) == "function", "No Arrow")
   assert(type(interpreter.pair) == "function", "No Pair")
   assert(type(interpreter.null) == "function", "No Null")
   assert(type(interpreter.create) == "function", "No Create")
end

--------------------------------------------------------------------------------
-- A print interpreter for the states
--------------------------------------------------------------------------------
local state_print_interpreter = {}

function state_print_interpreter.state(n, final)
   return string.format("(state %d %s)", n, final)
end

function state_print_interpreter.arrow(from, to, input, output)
   if input == '' then
      input = 'ε'
   end
   if output == '' then
      output = 'ε'
   end
   return string.format("(arrow %d %d %s %s)", from, to, input, output)
end

function state_print_interpreter.pair(fst, snd)
   if snd == "" then
      return fst
   end
   return string.format("%s %s", fst, snd)
end

function state_print_interpreter.null()
   return ""
end

function state_print_interpreter.create(states, arrows)
   local out = string.format("(create (states (%s)) (arrows (%s)))", states, arrows)
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

local flatten = {}

function flatten.state(n, final)
   return function (s, paired)
      return s.pair(s.state(n, final), paired)
   end
end

function flatten.arrow(from, to, input, output)
   return function (s, paired)
      return s.pair(s.arrow(from, to, input, output), paired)
   end
end

function flatten.pair(fst, snd)
   return function (s, paired)
      return fst(s, snd(s, paired))
   end
end

function flatten.null()
   return function (s, paired)
      return paired
   end
end

function flatten.create(states, arrows)
   return function (s)
      local states = states(s, s.null())
      local arrows = arrows(s, s.null())
      return s.create(states, arrows)
   end
end

assert_sl_interpreter(flatten)

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
local create_arrows = {}

function create_arrows.e()
   return function (s, f)
      local function empty_arrow(previous)
         return function (current)
            return s.arrow(previous, current, '', '')
         end
      end
      return f(empty_arrow,
               remark_fin.e())
   end
end

function create_arrows.lit(lit)
   return function (s, f)
      local function lit_arrow(previous)
         return function (current)
            return s.arrow(previous, current, lit, lit)
         end
      end
      return f(lit_arrow,
               remark_fin.lit(lit))
   end
end

local function extract(arrows, finstates)
   return arrows, finstates
end

function create_arrows.seq(rule1, rule2)
   return function (s, f)
      local arrows_1, finstates_1 = rule1(s, extract)
      local arrows_2, finstates_2 = rule2(s, extract)
      local function seq_arrow(previous)
         return function (current)
            -- Go from previous to current
            local arrows_out = s.pair(s.arrow(previous, current, '', ''),
                                      s.null())
            -- Go from current to left
            arrows_out = s.pair(arrows_1(current), arrows_out)
            -- Go from fins left to right
            local fins_left = remark_fin.create(remark_fin.grammar(finstates_1))(list_finstates)
            for _, v in ipairs(fins_left) do
               arrows_out = s.pair(arrows_2(v), arrows_out)
            end
            return arrows_out
         end
      end
      return f(seq_arrow, remark_fin.seq(finstates_1, finstates_2))
   end
end

function create_arrows.left(item)
   return function (s, f)
      local _, finstates = item(s, extract)
      local function left_arrow(previous)
         return function (current)
            local arrows, _ = item(s, extract)
            local out = s.pair(s.arrow(previous, current, '', ''), s.null())
            out = s.pair(arrows(current), out)
            return out
         end
      end
      return f(left_arrow,
               remark_fin.left(finstates))
   end
end

function create_arrows.right(item)
   return function (s, f)
      local _, finstates = item(s, extract)
      local function right_arrow(previous)
         return function (current)
            local arrows, _ = item(s, extract)
            local out = s.pair(s.arrow(previous, current, '', ''), s.null())
            out = s.pair(arrows(current), out)
            return out
         end
      end
      return f(right_arrow,
               remark_fin.right(finstates))
   end
end

function create_arrows.fin(item)
   return function (s, f)
      local arrows, finstates = item(s, extract)
      return f(arrows, remark_fin.fin(finstates))
   end
end

function create_arrows.notfin(item)
   return function (s, f)
      local arrows, finstates = item(s, extract)
      return f(arrows, remark_fin.notfin(finstates))
   end
end

function create_arrows.mark_n(n, item)
   return function (s, f)
      local arrows, finstates = item(s, extract)
      local function mark_n_arrow(previous)
         return arrows(previous)(n)
      end
      return f(mark_n_arrow, remark_fin.mark_n(n, finstates))
   end
end

function create_arrows.star(rule1)
   assert(false, "TODO")
end

function create_arrows.choice(rule1, rule2)
   return function (s, f)
      local arrows_1, finstates_1 = rule1(s, extract)
      local arrows_2, finstates_2 = rule2(s, extract)
      local function seq_arrow(previous)
         return function (current)
            -- Go from previous to current
            local arrows_out = s.pair(s.arrow(previous, current, '', ''),
                                      s.null())
            -- Go from current to left
            arrows_out = s.pair(arrows_1(current), arrows_out)
            -- Go from current to right
            arrows_out = s.pair(arrows_2(current), arrows_out)
            return arrows_out
         end
      end
      return f(seq_arrow, remark_fin.choice(finstates_1, finstates_2))
   end
end

function create_arrows.grammar(item)
   return function (s)
      local arrows, _ = item(s, extract)
      arrows = arrows(-1)
      return s.create(s.null(), arrows)
   end
end

function create_arrows.ref(item)
   assert(false, "Refs should be expanded")
end

function create_arrows.create(grammar)
   return grammar
end

--------------------------------------------------------------------------------
-- Test the arrows interpreter
--------------------------------------------------------------------------------
print()
print("Testing the arrows interpreter")
do
   local l = l.l()
   local arrows = l:rule('A'):is(l:lit('aa'))
      :rule('B'):is(l:lit('bb'))
      :rule('K'):is(l:lit('x'))
      :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
      :create(expand_ref)(expand_string)(mark_fin)(enumerate)(create_arrows)(flatten)(state_print_interpreter)
end

assert_n_interpreter(create_arrows)
--------------------------------------------------------------------------------
-- An interpreter that takes in enumerated, finally-marked
-- PEGREG syntax and translates it to calls to the
-- state interpreter
--------------------------------------------------------------------------------
local create_states = {}

function create_states.e()
   return function (i, t)
      return i.null()
   end
end

function create_states.lit(lit)
   return function (i, t)
      return i.null()
   end
end

function create_states.seq(rule1, rule2)
   return function (i, t)
      local left = rule1(i, t)
      local right = rule2(i, t)
      return i.pair(left, right)
   end
end

function create_states.choice(rule1, rule2)
   return function (i, t)
      local left = rule1(i, t)
      local right = rule2(i, t)
      return i.pair(left, right)
   end
end

function create_states.star(item)
   return item
end

function create_states.left(item)
   return item
end

function create_states.right(item)
   return item
end

function create_states.fin(item)
   return function (i, t)
      return item(i, true)
   end
end

function create_states.notfin(item)
   return function (i, t)
      return item(i, false)
   end
end

function create_states.mark_n(n, item)
   return function (i, t)
      return i.pair(i.state(n, t), item(i, t))
   end
end

function create_states.grammar(item)
   return function (i)
      return item(i, false)
   end
end

function create_states.ref(rule, rules)
   assert(false, "Refs ought to be expanded")
end

function create_states.create(grammar)
   return function (i)
      return i.create(grammar(i), i.null())
   end
end

--------------------------------------------------------------------------------
-- Test the create states interpreter
--------------------------------------------------------------------------------
print()
print("Testing the create states interpreter")
do
   local interpreter = expand_string.create()
   local l = l.l(interpreter)
   l:rule('A'):is(l:lit('aa'))
      :rule('B'):is(l:lit('bb'))
      :rule('K'):is(l:lit('x'))
      :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('A')))
      :create(expand_ref)(expand_string)(mark_fin)(enumerate)(create_states)(flatten)(state_print_interpreter)
end

--------------------------------------------------------------------------------
-- States and arrows
--------------------------------------------------------------------------------
local state_arrow = {}

function state_arrow.e()
   return function (f)
      return f(create_states.e(), create_arrows.e())
   end
end

function state_arrow.lit(lit)
   return function(f)
      return f(create_states.lit(lit), create_arrows.lit(lit))
   end
end

local function extract(states, arrows)
   return states, arrows
end

function state_arrow.seq(item1, item2)
   local states_1, arrows_1 = item1(extract)
   local states_2, arrows_2 = item2(extract)
   return function (f)
      return f(create_states.seq(states_1, states_2), create_arrows.seq(arrows_1, arrows_2))
   end
end

function state_arrow.choice(item1, item2)
   local states_1, arrows_1 = item1(extract)
   local states_2, arrows_2 = item2(extract)
   return function (f)
      return f(create_states.choice(states_1, states_2), create_arrows.choice(arrows_1, arrows_2))
   end
end

function state_arrow.grammar(item)
   return item
end

function state_arrow.ref(rule, rules)
   assert(false, "Refs should be expanded")
end

function state_arrow.create(grammar)
   local states, arrows = grammar(extract)
   return function (s)
      local arrows = arrows(s, function (a, b) return a end)(-1)
      return s.create(states, arrows)
   end
end

function state_arrow.left(item)
   local states, arrows = item(extract)
   return function (f)
      return f(create_states.left(states), create_arrows.left(arrows))
   end
end

function state_arrow.right(item)
   local states, arrows = item(extract)
   return function (f)
      return f(create_states.right(states), create_arrows.right(arrows))
   end
end

function state_arrow.fin(item)
   local states, arrows = item(extract)
   return function (f)
      return f(create_states.fin(states), create_arrows.fin(arrows))
   end
end

function state_arrow.notfin(item)
   local states, arrows = item(extract)
   return function (f)
      return f(create_states.notfin(states), create_arrows.notfin(arrows))
   end
end

function state_arrow.mark_n(n, item)
   local states, arrows = item(extract)
   return function (f)
      return f(create_states.mark_n(n, states), create_arrows.mark_n(n, arrows))
   end
end

function state_arrow.star(item)
   assert(false, "TODO")
end

assert_n_interpreter(state_arrow)
--------------------------------------------------------------------------------
-- Test the create states and arrow interpreter
--------------------------------------------------------------------------------
print()
print("Testing the create states and arrow interpreter")
print("From:")
do
   local l = l.l()
   local arrows = l:rule('A'):is(l:lit('aa'))
      :rule('B'):is(l:lit('bb'))
      :rule('K'):is(l:lit('x'))
      :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
      :create(expand_ref)(expand_string)(add_left_right)(mark_fin)(enumerate)(n_print_interpreter)
end
print("To:")
do
   local l = l.l()
   local arrows = l:rule('A'):is(l:lit('aa'))
      :rule('B'):is(l:lit('bb'))
      :rule('K'):is(l:lit('x'))
      :grammar(l:seq(l:choice(l:ref('A'), l:ref('B')), l:ref('K')))
      :create(expand_ref)(expand_string)(add_left_right)(mark_fin)(enumerate)(state_arrow)(flatten)(state_print_interpreter)
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
