#!/usr/bin/env lua

local lfs = require("lfs")
local alt_getopt = require("alt_getopt")

local function is_directory(path)
  return lfs.attributes(path, "mode") == "directory"
end

local function merge_tests(name, inp, expected)
  local test = io.open(name:lower():gsub(" ", "_") .. ".test", "w")
  test:write("lunamark -r mmd\n")
  test:write("<<<\n")
  test:write(inp)
  test:write(">>>\n")
  test:write(expected)
  test:close()
end

local function find_tests(path)
  local tests = {}
  for f in lfs.dir(path) do
    local fpath = path .. "/" .. f
    local name, ext = f:match("^([^\.]*)\.(.*)$")
    if name and tests[name] == nil then
      tests[name] = {}
    end
    if name and f ~= "." and f ~= ".." then
      if is_directory(fpath) then
        find_tests(fpath)
      else
        local fh = io.open(fpath, "r")
        tests[name][ext] = fh:read("*all"):gsub("\r","")
      end
    end
  end
  for name,cont in pairs(tests) do
    if cont["text"] and cont["html"] then
      merge_tests(name, cont["text"], cont["html"])
    end
  end
end

-- main program

local usage = [[
Usage: import_mmd_tests.lua [options] - import multimarkdown tests

Options:
  --dir,-d PATH      Directory containing .test files (default 'tests')
  --help,-h          This message
]]

local long_opts = {
  dir = "d",
  help = "h"
}

local short_opts = "d:h"
local optarg,optind = alt_getopt.get_opts(arg, short_opts, long_opts)

if optarg.h then
  io.write(usage)
  os.exit(0)
end

local testdir = optarg.d or "tests"

find_tests(testdir)
