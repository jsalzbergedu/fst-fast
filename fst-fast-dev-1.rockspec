rockspec_format = "3.0"

package = "fst-fast"
version = "dev-1"
source = {
   url = "git+https://github.com/jsalzbergedu/fst-fast.git"
}

description = {
   homepage = "https://github.com/jsalzbergedu/fst-fast",
   license = "MIT License"
}

build = {
   type = "builtin",
   modules = {
      fst_fast = "src/fst_fast.lua"
   }
}

test_dependencies = {
   "luaunit >= 3",
   "luafilesystem >= 1.8"
}

test = {
   type = "command",
   script = "test/test.lua"
}
