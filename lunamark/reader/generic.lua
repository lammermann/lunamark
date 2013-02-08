-- (c) 2009-2011 John MacFarlane, Hans Hagen.  Released under MIT license.
-- See the file LICENSE in the source for details.

local util = require("lunamark.util")
local lpeg = require("lpeg")
local lower, upper, gsub, rep, gmatch, format, length =
  string.lower, string.upper, string.gsub, string.rep, string.gmatch,
  string.format, string.len
local P, R, S, V, C, Cg, Cb, Cmt, Cc, Cf, Ct, B, Cs =
  lpeg.P, lpeg.R, lpeg.S, lpeg.V, lpeg.C, lpeg.Cg, lpeg.Cb,
  lpeg.Cmt, lpeg.Cc, lpeg.Cf, lpeg.Ct, lpeg.B, lpeg.Cs
local lpegmatch = lpeg.match

local M = {}

local rope_to_string = util.rope_to_string

------------------------------------------------------------------------------
-- Generic parsers
------------------------------------------------------------------------------

M.percent                = P("%")
M.asterisk               = P("*")
M.dash                   = P("-")
M.plus                   = P("+")
M.underscore             = P("_")
M.period                 = P(".")
M.hash                   = P("#")
M.ampersand              = P("&")
M.backtick               = P("`")
M.less                   = P("<")
M.more                   = P(">")
M.space                  = P(" ")
M.squote                 = P("'")
M.dquote                 = P('"')
M.lparent                = P("(")
M.rparent                = P(")")
M.lbracket               = P("[")
M.rbracket               = P("]")
M.circumflex             = P("^")
M.slash                  = P("/")
M.equal                  = P("=")
M.colon                  = P(":")
M.semicolon              = P(";")
M.comma                  = P(",")
M.exclamation            = P("!")
M.tilde                  = P("~")

M.digit                  = R("09")
M.hexdigit               = R("09","af","AF")
M.letter                 = R("AZ","az")
M.alphanumeric           = R("AZ","az","09")
M.keyword                = M.letter * M.alphanumeric^0

M.any                    = P(1)
M.fail                   = M.any - 1
M.always                 = P("")

M.escapable              = S("\\`*_{}[]()+_.!<>#-~:^")
M.anyescaped             = P("\\") / "" * M.escapable
                         + M.any

M.tab                    = P("\t")
M.spacechar              = S("\t ")
M.spacing                = S(" \n\r\t")
M.newline                = P("\n")
M.nonspacechar           = M.any - M.spacing
M.tightblocksep          = P("\001")

M.specialchar            = M.fail

M.normalchar             = M.any -
                         (M.specialchar + M.spacing + M.tightblocksep)
M.optionalspace          = M.spacechar^0
M.spaces                 = M.spacechar^1
M.eof                    = - M.any
M.linechar               = P(1 - M.newline)

M.blankline              = M.optionalspace * M.newline / "\n"
M.blanklines             = M.blankline^0
M.skipblanklines         = (M.optionalspace * M.newline)^0
M.sp                     = M.spacing^0
M.spnl                   = M.optionalspace * (M.newline * M.optionalspace)^-1
M.line                   = M.linechar^0 * M.newline
                         + M.linechar^1 * M.eof
M.nonemptyline           = M.line - M.blankline

-- change syntax recursivly
--
-- *    `opts` is a table with parsing options.
--      If there is already an alter_syntax function in this table, they will
--      be chained together.
--
-- *    `fun` is a function from syntax table to syntax table,
--      allowing the user to change or extend the reader syntax.
--      For an example, see the documentation for `lunamark`.
--      It can take the following parameters:
--
--      `syntax`
--      :   The table with the gramma definition to change.
--
--      `writer`
--      :   A writer table (see [lunamark.writer.generic]).
--
--      `opts`
--      :   A table with parsing options.
function M.alter_syntax(opts, fun)
  local opts = opts or {}

  if opts.alter_syntax then
    local alter_syntax = opts.alter_syntax
    opts.alter_syntax = function(syntax, writer, opts)
      syntax = fun(syntax, writer, opts)
      syntax = alter_syntax(syntax, writer, opts)
    end
  else
    opts.alter_syntax = fun
  end
end

--- Create a new generic parser.
--
-- *   `writer` is a writer table (see [lunamark.writer.generic]).
--
-- *   `options` is a table with parsing options.
--     The following fields are significant:
--
--     `alter_syntax`
--     :   Function from syntax table to syntax table,
--         allowing the user to change or extend the reader syntax.
--         For an example, see the documentation for `lunamark`.
--
--     `references`
--     :   A table of references to be used in resolving links
--         in the document.  The keys should be all lowercase, with
--         spaces and newlines collapsed into single spaces.
--         Example:
--
--             { foo: { url = "/url", title = "my title" },
--               bar: { url = "http://fsf.org" } }
--
--
-- *   Returns a converter function that converts a generic string
--     using `writer`, returning the parsed document as first result,
--     and a table containing any extracted metadata as the second
--     result. The converter assumes that the input has unix
--     line endings (newline).  If the input might have DOS
--     line endings, a simple `gsub("\r","")` should take care of them.
function M.new(writer, options)
  local options = options or {}

  ------------------------------------------------------------------------------

  local syntax
  local blocks
  local inlines

  function M.parse_blocks(str)
    local res = lpegmatch(blocks, str)
    if res == nil
      then error(format("parse_blocks failed on:\n%s", str:sub(1,20)))
      else return res
    end
  end

  function M.parse_inlines(str)
    local res = lpegmatch(inlines, str)
    if res == nil
      then error(format("parse_inlines failed on:\n%s", str:sub(1,20)))
      else return res
    end
  end

  function parse_inlines_no_link(str)
    local res = lpegmatch(inlines_no_link, str)
    if res == nil
      then error(format("parse_inlines_no_link failed on:\n%s", str:sub(1,20)))
      else return res
    end
  end

  ------------------------------------------------------------------------------
  -- Blank
  ------------------------------------------------------------------------------

  local Blank          = M.blankline / ""
                       + V("Comment") / ""
                       + (M.tightblocksep / "\n")

  ------------------------------------------------------------------------------

  local Plain = M.linechar^1 / writer.plain -- Prints everything as it comes

  local Block     = V("Block")
  local Inline    = V("Inline")

  syntax =
    { "Blocks",

      Blocks                = Blank^0 *
                              Block^-1 *
                              (Blank^0 / function() return writer.interblocksep end * Block)^0 *
                              Blank^0 *
                              M.eof,

      Blank                 = Blank,

      Comment               = M.fail,

      Block                 = V("Preprocess")
                            + V("Blockquote")
                            + V("Verbatim")
                            + V("PassThrough")
                            + V("HorizontalRule")
                            + V("BulletList")
                            + V("OrderedList")
                            + V("Header")
                            + V("DefinitionList")
                            + V("Table")
                            + V("DisplayHtml")
                            + V("Paragraph")
                            + V("Plain"),

      Preprocess            = M.fail,
      Blockquote            = M.fail,
      Verbatim              = M.fail,
      PassThrough           = M.fail,
      HorizontalRule        = M.fail,
      BulletList            = M.fail,
      OrderedList           = M.fail,
      Header                = M.fail,
      DefinitionList        = M.fail,
      Table                 = M.fail,
      DisplayHtml           = M.fail,
      Paragraph             = M.fail,
      Plain                 = Plain,

      Inline                = V("InlineComment")
                            + V("Strong")
                            + V("Space")
                            + V("Endline")
                            + V("UlOrStarLine")
                            + V("Emph")
                            + V("NoteRef")
                            + V("Link")
                            + V("Image")
                            + V("Code")
                            + V("AutoLinkUrl")
                            + V("AutoLinkEmail")
                            + V("InlineHtml")
                            + V("HtmlEntity")
                            + V("EscapedChar")
                            + V("Smart")
                            + V("Symbol")
                            + V("Str"),

      InlineComment         = M.fail,
      Space                 = M.fail,
      Endline               = M.fail,
      UlOrStarLine          = M.fail,
      Strong                = M.fail,
      Emph                  = M.fail,
      NoteRef               = M.fail,
      Link                  = M.fail,
      Image                 = M.fail,
      Code                  = M.fail,
      AutoLinkUrl           = M.fail,
      AutoLinkEmail         = M.fail,
      InlineHtml            = M.fail,
      HtmlEntity            = M.fail,
      EscapedChar           = M.fail,
      Smart                 = M.fail,
      Symbol                = M.fail,
      Str                   = M.Plain
    }

  if options.alter_syntax and type(options.alter_syntax) == "function" then
    syntax = options.alter_syntax(syntax, writer, options)
  end

  if options.debug then
    for k, p in pairs(syntax) do
      local enter = Cmt(P(true), function(s, p, ...)
        print("ENTER", k) return p end);
      local leave = Cmt(P(true), function(s, p, ...)
        print("LEAVE", k) return p end) * (P("k") - P "k");
      if type(p) ~= "string" then
        syntax[k] = Cmt(enter * p + leave, function(s, p, ...)
          print("---", k, "---") print(p, s:sub(1, p-1)) return p end)
      end
    end 
  end

  blocks = Ct(syntax)

  local inlines_t = util.table_copy(syntax)
  inlines_t[1] = "Inlines"
  inlines_t.Inlines = Inline^0 * (M.spacing^0 * M.eof / "")
  inlines = Ct(inlines_t)

  inlines_no_link_t = util.table_copy(inlines_t)
  inlines_no_link_t.Link = M.fail
  inlines_no_link = Ct(inlines_no_link_t)

  ------------------------------------------------------------------------------
  -- Exported conversion function
  ------------------------------------------------------------------------------

  -- inp is a string; line endings are assumed to be LF (unix-style)
  -- and tabs are assumed to be expanded.
  return function(inp)
      references = options.references or {}
      -- optional function for pre parsing
      if options.pre then
        inp = options.pre(inp)
      end
      local result = { writer.start_document(), M.parse_blocks(inp), writer.stop_document() }
      return rope_to_string(result), writer.get_metadata()
  end

end

return M
