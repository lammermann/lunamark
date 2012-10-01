-- (c) 2009-2011 John MacFarlane, Hans Hagen.  Released under MIT license.
-- See the file LICENSE in the source for details.

local markdown = require("lunamark.reader.markdown")
local util = require("lunamark.util")
local R = require("lunamark.reader.generic")
local lpeg = require("lpeg")

util.import_from(R)

local M = {}

--- Create a new multimarkdown parser.
--
-- It extends [lunamark.reader.markdown].
--
-- Sets the options:
--
--     `notes`
--     :   Enable footnotes as in pandoc.
--
--     `definition_lists`
--     :   Enable definition lists as in pandoc.
function M.new(writer, options)
  local options = options or {}

  -- Set default options
  -- Enable footnotes as in pandoc.
  options.notes = 1
  -- Enable definition lists as in pandoc.
  options.definition_lists = 1

  ------------------------------------------------------------------------------
  -- MetaData
  ------------------------------------------------------------------------------

  local metadatakey = lpeg.Ct(letter * (space + alphanumeric + dash + underscore)^0)
  local metadatavalue = spacechar^0 * lpeg.Ct((any - ((newline * metadatakey) + blankline))^1)
  local metadata = (metadatakey * colon * metadatavalue)^0

  ------------------------------------------------------------------------------
  -- Exported conversion function
  ------------------------------------------------------------------------------

  -- inp is a string; line endings are assumed to be LF (unix-style)
  -- and tabs are assumed to be expanded.
  return function(inp)
      -- multimarkdown metadata
      local metatable, rest = lpeg.match(metadata, inp)
      for key,val in pairs(metatable) do
        print(key)
        writer.set_metadata(key, val)
      end
      --inp = rest
      -- return default markdown parser
      local result = markdown.new(writer, options)(inp)
      return util.rope_to_string(result), writer.get_metadata()
  end

end

return M
