local luaunit = require("luaunit")
local lfs = require("lfs")

local src_requires = {}
local test_requires = {}
local name_to_suite = {}

do
   for file in lfs.dir(".") do
      if file:sub(1, 4) == "Test" and
         file:sub(-4, -1) == ".lua"
      then
         local suite = dofile(file)
         local name = file:sub(1, -5)
         name_to_suite[name] = suite
         _G[name] = suite
         print("name is: ", name)
         test_requires[name] = suite:testRequires()
         suite.luaunit = luaunit
         for _, v in ipairs(suite:srcRequires()) do
            if src_requires[v] == nil then
               src_requires[v] = require(v)
            end
            suite[v] = src_requires[v]
         end
      end
   end
   for name, requires in pairs(test_requires) do
      local suite = name_to_suite[name]
      for _, v in ipairs(requires) do
         suite[v] = name_to_suite[v]
      end
   end
end

os.exit(luaunit.LuaUnit.run())
