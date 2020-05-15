local Test = {}

function Test:srcRequires()
   return {"fst_language", "flatten"}
end

function Test:testRequires()
   return {"TestPrintFst"}
end


function Test:testFlattenInterpreter()
   local fstl = require("fst_language")
   local flatten = require("flatten")
   TestPrintFst:assertSLInterpreter(flatten)
end

return Test
