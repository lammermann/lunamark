-- (c) 2009-2011 John MacFarlane, Hans Hagen.  Released under MIT license.
-- See the file LICENSE in the source for details.

local R = require("lunamark.reader.generic")
local lpeg = require("lpeg")
local lower, upper, gsub, rep, gmatch, format, length =
  string.lower, string.upper, string.gsub, string.rep, string.gmatch,
  string.format, string.len

local M = {}

local function delimited_block(delimiter)
  --local deli_line = ((1 - #(R.linechar - delimiter)^1) * delimiter^4 * R.newline)
  local deli_line = (delimiter^4 * R.newline)
  return deli_line * lpeg.C((R.any - deli_line)^0) * deli_line
end

local function inline_macro(keyword)
  return keyword * R.colon * lpeg.Ct(R.nonspacechar^1)
    * R.lbracket * lpeg.Ct(R.nonspacechar^1) / attributes * R.rbracket
end

local function block_macro(keyword)
  return keyword * R.colon * R.colon * lpeg.Ct(R.nonspacechar^1)
    * R.lbracket * lpeg.Ct(R.nonspacechar^1) / attributes * R.rbracket
end

function add_asciidoc_syntax(syntax, writer, options)
  ------------------------------------------------------------------------------
  -- Comments
  ------------------------------------------------------------------------------

  local CommentLine    = R.slash^2 * R.line
  local CommentBlock   = delimited_block(R.slash)
  local Comment        = (CommentBlock + CommentLine) / ""

  ------------------------------------------------------------------------------
  -- Headers
  ------------------------------------------------------------------------------

  -- parse Atx heading start and return level
  local HeadingStart = #R.equal * lpeg.C(R.equal^-6) * -R.equal / length

  -- parse setext header ending and return level
  local HeadingLevel = R.equal^1 * lpeg.Cc(1)
                     + R.dash^1 * lpeg.Cc(2)
                     + R.tilde^1 * lpeg.Cc(3)
                     + R.circumflex^1 * lpeg.Cc(4)
                     + R.plus^1 * lpeg.Cc(5)

  local function strip_atx_end(s)
    return s:gsub("[#%s]*\n$","")
  end

  -- parse atx header
  local AtxHeader = lpeg.Cg(HeadingStart,"level")
                     * R.optionalspace
                     * (lpeg.C(R.line) / strip_atx_end / parse_inlines)
                     * lpeg.Cb("level")
                     / writer.header

  -- parse setext header
  local SetextHeader = #(R.line * lpeg.S("=-~^+"))
                     * lpeg.Ct(R.line / parse_inlines)
                     * HeadingLevel
                     * R.optionalspace * R.newline
                     / writer.header

  -----------------------------------------------------------------------------
  -- Parsers used for asciidoc lists
  -----------------------------------------------------------------------------

  -- gobble spaces to make the whole bullet or enumerator four spaces wide:
  local function gobbletofour(s,pos,c)
      if length(c) >= 3
         then return lpegmatch(space^-1,s,pos)
      elseif length(c) == 2
         then return lpegmatch(space^-2,s,pos)
      else return lpegmatch(space^-3,s,pos)
      end
  end

  local bulletchar = lpeg.C(R.plus + R.asterisk + R.dash)

  local bullet     = ( bulletchar * #R.spacing * (R.tab + R.space^-3)
                     + R.space * bulletchar * #R.spacing * (R.tab + R.space^-2)
                     + R.space * R.space * bulletchar * #R.spacing * (R.tab + R.space^-1)
                     + R.space * R.space * R.space * bulletchar * #R.spacing
                     ) * -bulletchar

  local dig = R.digit

  local enumerator = lpeg.C(dig^3 * R.period) * #R.spacing
                   + lpeg.C(dig^2 * R.period) * #R.spacing * (R.tab + R.space^1)
                   + lpeg.C(dig * R.period) * #R.spacing * (R.tab + R.space^-2)
                   + R.space * lpeg.C(dig^2 * R.period) * #R.spacing
                   + R.space * lpeg.C(dig * R.period) * #R.spacing * (R.tab + R.space^-1)
                   + R.space * R.space * lpeg.C(dig^1 * R.period) * #R.spacing
                   + lpeg.C(R.period^3) * #R.spacing
                   + lpeg.C(R.period^2) * #R.spacing * (R.tab + R.space^1)
                   + lpeg.C(R.period) * #R.spacing * (R.tab + R.space^-2)
                   + R.space * lpeg.C(R.period^2) * #R.spacing
                   + R.space * lpeg.C(R.period) * #R.spacing * (R.tab + R.space^-1)
                   + R.space * R.space * lpeg.C(R.period^1) * #R.spacing

  local indent                 = R.space^-3 * R.tab
                               + lpeg.P("    ") / ""
  local indentedline           = indent    /"" * lpeg.C(R.linechar^1 * R.newline^-1)
  local optionallyindentedline = indent^-1 /"" * lpeg.C(R.linechar^1 * R.newline^-1)

  -- block followed by 0 or more optionally
  -- indented blocks with first line indented.
  local function indented_blocks(bl)
    return lpeg.Cs( bl
             * (R.blankline^1 * indent * -R.blankline * bl)^0
             * R.blankline^1 )
  end

  ------------------------------------------------------------------------------
  -- Lists
  ------------------------------------------------------------------------------

  local starter = bullet + enumerator

  -- we use \001 as a separator between a tight list item and a
  -- nested list under it.
  local NestedList            = lpeg.Cs((optionallyindentedline - starter)^1)
                              / function(a) return "\001"..a end

  local ListBlockLine         = optionallyindentedline
                                - R.blankline - (indent^-1 * starter)

  local ListBlock             = R.line * ListBlockLine^0

  local ListContinuationBlock = R.blanklines * (indent / "") * ListBlock

  local function TightListItem(starter)
      return (lpeg.Cs(starter / "" * ListBlock * NestedList^-1) / R.parse_blocks)
             * -(R.blanklines * indent)
  end

  local function LooseListItem(starter)
      return lpeg.Cs( starter / "" * ListBlock * lpeg.Cc("\n")
             * (NestedList + ListContinuationBlock^0)
             * (R.blanklines / "\n\n")
             ) / R.parse_blocks
  end

  local BulletList = ( lpeg.Ct(TightListItem(bullet)^1)
                       * lpeg.Cc(true) * R.skipblanklines * -bullet
                     + lpeg.Ct(LooseListItem(bullet)^1)
                       * lpeg.Cc(false) * R.skipblanklines ) / writer.bulletlist

  local function ordered_list(s,tight,startnum)
    if options.startnum then
      startnum = tonumber(listtype) or 1  -- fallback for '#'
    else
      startnum = nil
    end
    return writer.orderedlist(s,tight,startnum)
  end

  local OrderedList = lpeg.Cg(enumerator, "listtype") *
                      ( lpeg.Ct(TightListItem(lpeg.Cb("listtype")) * TightListItem(enumerator)^0)
                        * lpeg.Cc(true) * R.skipblanklines * -enumerator
                      + lpeg.Ct(LooseListItem(lpeg.Cb("listtype")) * LooseListItem(enumerator)^0)
                        * lpeg.Cc(false) * R.skipblanklines
                      ) * lpeg.Cb("listtype") / ordered_list

  local defstartchar = lpeg.P("::") + lpeg.P(";;")
  local defstart     = ( #R.spacing * (R.tab + R.space^-3)
                     + R.space * #R.spacing * (R.tab + R.space^-2)
                     + R.space * R.space * #R.spacing * (R.tab + R.space^-1)
                     + R.space * R.space * R.space * #R.spacing
                     )

  local dlchunk = lpeg.Cs(R.line * (indentedline - R.blankline)^0)

  local function definition_list_item(term, defs, tight)
    return { term = parse_inlines(term), definitions = defs }
  end

  local DefinitionListItemLoose = lpeg.C((R.linechar - defstartchar)^1) * defstartchar * R.skipblanklines
                           * lpeg.Ct((defstart * indented_blocks(dlchunk) / R.parse_blocks)^1)
                           * lpeg.Cc(false)
                           / definition_list_item

  local DefinitionListItemTight = lpeg.C((R.linechar - defstartchar)^1) * defstartchar * R.newline
                           * lpeg.Ct((defstart * dlchunk / R.parse_blocks)^1)
                           * lpeg.Cc(true)
                           / definition_list_item

  local DefinitionList =  ( lpeg.Ct(DefinitionListItemLoose^1) * lpeg.Cc(false)
                          +  lpeg.Ct(DefinitionListItemTight^1)
                             * (R.skipblanklines * -DefinitionListItemLoose * lpeg.Cc(true))
                          ) / writer.definitionlist

  ------------------------------------------------------------------------------
  -- Inline elements
  ------------------------------------------------------------------------------

  local Str       = R.normalchar^1 / writer.string

  local Endline   = R.newline * -( -- newline, but not before...
                        R.blankline -- paragraph break
                      + R.tightblocksep  -- nested list
                      + R.eof       -- end of document
                    ) * R.spacechar^0 / writer.space

  local Space     = R.spacechar^2 * Endline / writer.linebreak
                  + R.spacechar^1 * Endline^-1 * R.eof / ""
                  + R.spacechar^1 * Endline^-1 * R.optionalspace / writer.space

  --local DirectImage   = lpeg.P("image::")
  --                    * (tag / parse_inlines)
  --                    * spnl
  --                    * lparent
  --                    * (url + Cc(""))  -- link can be empty [foo]()
  --                    * optionaltitle
  --                    * rparent
  --                    / writer.image

  --local IndirectImage  = exclamation * tag * (C(spnl) * tag)^-1 / indirect_image

  --local Image         = DirectImage + IndirectImage

  ------------------------------------------------------------------------------
  -- Block elements
  ------------------------------------------------------------------------------
  local nonindentspace         = R.space^-3 * - R.spacechar

  local Verbatim       = delimited_block(R.period) / writer.verbatim

  local Paragraph      = nonindentspace * lpeg.Ct(syntax.Inline^1) * R.newline
                       * ( R.blankline^1
                         + #R.hash
                         + #(R.more * R.space^-1)
                         )
                       / writer.paragraph

  -- use DisplayHtml for passthroug blocks
  local DisplayHtml    = delimited_block(R.plus) / writer.plain

  ------------------------------------------------------------------------------

  syntax.Header         = AtxHeader + SetextHeader
  syntax.Comment        = Comment
  syntax.BulletList     = BulletList
  syntax.OrderedList    = OrderedList
  syntax.DefinitionList = DefinitionList
  syntax.DisplayHtml    = DisplayHtml
  syntax.Paragraph      = Paragraph
  syntax.Str            = Str
  syntax.Endline        = Endline
  syntax.Space          = Space

  return syntax
end

function M.new(writer, options)
  local options = options or {}

  R.alter_syntax(options, add_asciidoc_syntax)

  -- return default parser
  return R.new(writer, options)

end

return M
