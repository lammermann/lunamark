-- (c) 2009-2011 John MacFarlane, Hans Hagen.  Released under MIT license.
-- See the file LICENSE in the source for details.

local generic = require("lunamark.reader.generic")
local util = require("lunamark.util")
local lpeg = require("lpeg")
local lower, upper, gsub, rep, gmatch, format, length =
  string.lower, string.upper, string.gsub, string.rep, string.gmatch,
  string.format, string.len
local P, R, S, V, C, Cg, Cb, Cmt, Cc, Cf, Ct, B, Cs =
  lpeg.P, lpeg.R, lpeg.S, lpeg.V, lpeg.C, lpeg.Cg, lpeg.Cb,
  lpeg.Cmt, lpeg.Cc, lpeg.Cf, lpeg.Ct, lpeg.B, lpeg.Cs

util.import_from(generic)


local M = {}

local function delimited_block(delimiter)
  --local deli_line = ((1 - #(linechar - delimiter)^1) * delimiter^4 * newline)
  local deli_line = (delimiter^4 * newline)
  return deli_line * lpeg.C((any - deli_line)^0) * deli_line
end

local function inline_macro(keyword)
  return keyword * colon * lpeg.Ct(nonspacechar^1)
    * lbracket * lpeg.Ct(nonspacechar^1) / attributes * rbracket
end

local function block_macro(keyword)
  return keyword * colon * colon * lpeg.Ct(nonspacechar^1)
    * lbracket * lpeg.Ct(nonspacechar^1) / attributes * rbracket
end

function add_asciidoc_syntax(syntax, writer, options)
  ------------------------------------------------------------------------------
  -- Comments
  ------------------------------------------------------------------------------

  local CommentLine    = slash^2 * line
  local CommentBlock   = delimited_block(slash)
  local Comment        = (CommentBlock + CommentLine) / ""

  ------------------------------------------------------------------------------
  -- Headers
  ------------------------------------------------------------------------------

  -- parse Atx heading start and return level
  local HeadingStart = #equal * lpeg.C(equal^-6) * -equal / length

  -- parse setext header ending and return level
  local HeadingLevel = equal^1 * lpeg.Cc(1)
                     + dash^1 * lpeg.Cc(2)
                     + tilde^1 * lpeg.Cc(3)
                     + circumflex^1 * lpeg.Cc(4)
                     + plus^1 * lpeg.Cc(5)

  local function strip_atx_end(s)
    return s:gsub("[#%s]*\n$","")
  end

  -- parse atx header
  local AtxHeader = lpeg.Cg(HeadingStart,"level")
                     * optionalspace
                     * (lpeg.C(line) / strip_atx_end / parse_inlines)
                     * lpeg.Cb("level")
                     / writer.header

  -- parse setext header
  local SetextHeader = #(line * lpeg.S("=-~^+"))
                     * lpeg.Ct(line / parse_inlines)
                     * HeadingLevel
                     * optionalspace * newline
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

  local bulletchar = lpeg.C(plus + asterisk + dash)

  local bullet     = ( bulletchar * #spacing * (tab + space^-3)
                     + space * bulletchar * #spacing * (tab + space^-2)
                     + space * space * bulletchar * #spacing * (tab + space^-1)
                     + space * space * space * bulletchar * #spacing
                     ) * -bulletchar

  local dig = digit

  local enumerator = lpeg.C(dig^3 * period) * #spacing
                   + lpeg.C(dig^2 * period) * #spacing * (tab + space^1)
                   + lpeg.C(dig * period) * #spacing * (tab + space^-2)
                   + space * lpeg.C(dig^2 * period) * #spacing
                   + space * lpeg.C(dig * period) * #spacing * (tab + space^-1)
                   + space * space * lpeg.C(dig^1 * period) * #spacing
                   + lpeg.C(period^3) * #spacing
                   + lpeg.C(period^2) * #spacing * (tab + space^1)
                   + lpeg.C(period) * #spacing * (tab + space^-2)
                   + space * lpeg.C(period^2) * #spacing
                   + space * lpeg.C(period) * #spacing * (tab + space^-1)
                   + space * space * lpeg.C(period^1) * #spacing

  local indent                 = space^-3 * tab
                               + lpeg.P("    ") / ""
  local indentedline           = indent    /"" * lpeg.C(linechar^1 * newline^-1)
  local optionallyindentedline = indent^-1 /"" * lpeg.C(linechar^1 * newline^-1)

  -- block followed by 0 or more optionally
  -- indented blocks with first line indented.
  local function indented_blocks(bl)
    return lpeg.Cs( bl
             * (blankline^1 * indent * -blankline * bl)^0
             * blankline^1 )
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
                                - blankline - (indent^-1 * starter)

  local ListBlock             = line * ListBlockLine^0

  local ListContinuationBlock = blanklines * (indent / "") * ListBlock

  local function TightListItem(starter)
      return (lpeg.Cs(starter / "" * ListBlock * NestedList^-1) / generic.parse_blocks)
             * -(blanklines * indent)
  end

  local function LooseListItem(starter)
      return lpeg.Cs( starter / "" * ListBlock * lpeg.Cc("\n")
             * (NestedList + ListContinuationBlock^0)
             * (blanklines / "\n\n")
             ) / generic.parse_blocks
  end

  local BulletList = ( lpeg.Ct(TightListItem(bullet)^1)
                       * lpeg.Cc(true) * skipblanklines * -bullet
                     + lpeg.Ct(LooseListItem(bullet)^1)
                       * lpeg.Cc(false) * skipblanklines ) / writer.bulletlist

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
                        * lpeg.Cc(true) * skipblanklines * -enumerator
                      + lpeg.Ct(LooseListItem(lpeg.Cb("listtype")) * LooseListItem(enumerator)^0)
                        * lpeg.Cc(false) * skipblanklines
                      ) * lpeg.Cb("listtype") / ordered_list

  local defstartchar = lpeg.P("::") + lpeg.P(";;")
  local defstart     = ( #spacing * (tab + space^-3)
                     + space * #spacing * (tab + space^-2)
                     + space * space * #spacing * (tab + space^-1)
                     + space * space * space * #spacing
                     )

  local dlchunk = lpeg.Cs(line * (indentedline - blankline)^0)

  local function definition_list_item(term, defs, tight)
    return { term = parse_inlines(term), definitions = defs }
  end

  local DefinitionListItemLoose = lpeg.C((linechar - defstartchar)^1) * defstartchar * skipblanklines
                           * lpeg.Ct((defstart * indented_blocks(dlchunk) / generic.parse_blocks)^1)
                           * lpeg.Cc(false)
                           / definition_list_item

  local DefinitionListItemTight = lpeg.C((linechar - defstartchar)^1) * defstartchar * newline
                           * lpeg.Ct((defstart * dlchunk / generic.parse_blocks)^1)
                           * lpeg.Cc(true)
                           / definition_list_item

  local DefinitionList =  ( lpeg.Ct(DefinitionListItemLoose^1) * lpeg.Cc(false)
                          +  lpeg.Ct(DefinitionListItemTight^1)
                             * (skipblanklines * -DefinitionListItemLoose * lpeg.Cc(true))
                          ) / writer.definitionlist

  ------------------------------------------------------------------------------
  -- Inline elements
  ------------------------------------------------------------------------------

  local Str       = normalchar^1 / writer.string

  local Endline   = newline * -( -- newline, but not before...
                        blankline -- paragraph break
                      + tightblocksep  -- nested list
                      + eof       -- end of document
                    ) * spacechar^0 / writer.space

  local Space     = spacechar^2 * Endline / writer.linebreak
                  + spacechar^1 * Endline^-1 * eof / ""
                  + spacechar^1 * Endline^-1 * optionalspace / writer.space

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
  local nonindentspace         = space^-3 * - spacechar

  local Verbatim       = delimited_block(period) / writer.verbatim

  local Paragraph      = nonindentspace * lpeg.Ct(syntax.Inline^1) * newline
                       * ( blankline^1
                         + #hash
                         + #(more * space^-1)
                         )
                       / writer.paragraph

  -- use DisplayHtml for passthroug blocks
  local DisplayHtml    = delimited_block(plus) / writer.plain

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

  generic.alter_syntax(options, add_asciidoc_syntax)

  -- return default parser
  return generic.new(writer, options)

end

return M
