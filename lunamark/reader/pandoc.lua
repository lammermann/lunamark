-- (c) 2009-2011 John MacFarlane, Hans Hagen.  Released under MIT license.
-- See the file LICENSE in the source for details.

local markdown = require("lunamark.reader.markdown")

local M = {}

--- Create a new pandoc parser.
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
--
--     `pandoc_title_blocks`
--     :   Parse pandoc-style title block at the beginning of document:
--
--             % Title
--             % Author1; Author2
--             % Date
function M.new(writer, options)
  local options = options or {}

  -- Set default options
  -- Enable footnotes as in pandoc.
  options.notes = 1
  -- Enable definition lists as in pandoc.
  options.definition_lists = 1
  -- Parse pandoc-style title block at the beginning of document:
  --
  --     % Title
  --     % Author1; Author2
  --     % Date
  options.pandoc_title_blocks = 1


  -- return default markdown parser
  return markdown.new(writer, options)

end

return M
