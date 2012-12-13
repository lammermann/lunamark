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
local unicode = require("unicode")
local utf8 = unicode.utf8
local io = io

util.import_from(generic)


local M = {}

local function delimited_block(start, finish)
  local finish = finish or start
  local deli_start  = start * newline
  local deli_finish = newline * finish * newline
  return deli_start * C((any - deli_finish)^0) * deli_finish
end

function add_asciidoc_syntax(syntax, writer, options)
  ------------------------------------------------------------------------------
  -- Helpers for AttributeLists
  ------------------------------------------------------------------------------
  local UnquotedField = Cs((any - dquote - comma - rbracket - lbracket)^0)
  local QuotedField = dquote * Cs((any - dquote)^0) * dquote
  local NamedField = Cg(Cs((nonspacechar - equal)^1) * equal * QuotedField)
  local Sep = optionalspace * comma * optionalspace
 
  local Attrs = ((QuotedField * Sep)^0 *
      Cf(Ct("") * (NamedField * Sep)^0 * NamedField, rawset))
    + ((QuotedField * Sep)^0 * QuotedField)
    + ((UnquotedField * Sep)^0 * UnquotedField)

  function attrlist(label)
    --local label = label or P("")
    return lbracket * optionalspace * Cg(Attrs, "attrs") * optionalspace * rbracket
            * optionalspace
  end

  -- Title for Block Elements
  local BlockTitle = period * C(linechar^1) * newline

  ------------------------------------------------------------------------------
  -- Macros
  ------------------------------------------------------------------------------
  local target = C((nonspacechar-colon * any-(lbracket+rbracket+newline))^0)

  local function block_macro(keyword)
    return P(keyword) * P("::") * target * attrlist() * newline
  end
  
  local function inline_macro(keyword)
    return P(keyword) * colon * target * attrlist()
  end

  ------------------------------------------------------------------------------
  -- Helpers for links and references
  ------------------------------------------------------------------------------
  -- Normalize a asciidoc reference tag.  (Make lowercase, and collapse
  -- adjacent whitespace characters.)
  local function normalize_tag(tag)
    return utf8.lower(gsub(rope_to_string(tag), "[ \n\r\t]+", " "))
  end

  -- List of references defined in the document
  local references

  -- add a reference to the list
  local function register_link(tag,url,title)
      references[normalize_tag(tag)] = { url = url, title = title }
      return ""
  end

  -- lookup link reference and return either
  -- the link or nil and fallback text.
  local function lookup_reference(label,sps,tag)
      local tagpart
      if not tag then
          tag = label
          tagpart = ""
      elseif tag == "" then
          tag = label
          tagpart = "[]"
      else
          tagpart = {"[", generic.parse_inlines(tag), "]"}
      end
      if sps then
        tagpart = {sps, tagpart}
      end
      local r = references[normalize_tag(tag)]
      if r then
        return r
      else
        return nil, {"[", generic.parse_inlines(label), "]", tagpart}
      end
  end

  -- lookup link reference and return a link, if the reference is found,
  -- or a bracketed label otherwise.
  local function indirect_link(label,sps,tag)
    return function()
      local r,fallback = lookup_reference(label,sps,tag)
      if r then
        return writer.link(parse_inlines_no_link(label), r.url, r.title)
      else
        return fallback
      end
    end
  end

  ------------------------------------------------------------------------------
  -- System Macros
  ------------------------------------------------------------------------------
  local function include(path)
    -- @todo find path relative to source file
    local f = io.open(path, "r")
    local inpt = {}
    if f then
      table.insert(inpt, f:read("*all"))
    else
      util.err("Could not open file '" .. path .. "'", 7)
    end
    local inp = table.concat(inpt, "\n")
    if inp:find("\r",1,true) then
      inp = inp:gsub("\r\n","\n") -- convert DOS line endings
    end
    return inp
  end

  local inc   = block_macro("include")
    / function(path) return generic.parse_blocks(include(path) .. "\n") end
  local inc1   = block_macro("include1") / include
  local eval   = block_macro("eval")     / "TODO"
  local sys    = block_macro("sys")      / "TODO"
  local sys2   = block_macro("sys2")     / "TODO"

  local function conditional(start, finish)
    local deli_start     = P(start) * P("::") * Cg(target, "t1")
                           * Cg(attrlist(), "attrs") * newline
    local deli_finish_1  = newline * P(finish) * P("::") * Cg(target, "t2")
    local deli_finish_2  = Cg(attrlist(), "ignored") * newline
    local deli_finish    = deli_finish_1 * deli_finish_2
    local deli_finish_eq = Cmt(deli_finish_1 * Cb("t1") * Cb("t2")
                               * deli_finish_2,
                           function (s, i, a, b) return a == b end)
    return deli_start * Cg((any - deli_finish_eq)^0, "content") * deli_finish
           * Cb("t1") * Cb("attrs") * Cb("content")
  end

  local function if_defined(target, attrs, content)
    local t = options.metadata[target]
    if t then return generic.parse_blocks(content .. "\n\n") end
    return generic.parse_blocks(content .. "\n\n")
  end

  local function if_ndefined(target, attrs, content)
    local t = options.metadata[target]
    if not t then return generic.parse_blocks(content .. "\n\n") end
    return ""
  end

  local ifdef  = conditional("ifdef", "endif")  / if_defined
  local ifndef = conditional("ifndef", "endif") / if_ndefined
  local ifeval = conditional("ifeval", "endif") / "TODO"

  ------------------------------------------------------------------------------
  -- Delimited Blocks
  ------------------------------------------------------------------------------
  local PassThrough  = delimited_block(plus^4)       / writer.plain
  local ListingBlock = delimited_block(dash^4)       / writer.code
  local LiteralBlock = delimited_block(period^4)     / writer.verbatim
  local QuoteBlock   = delimited_block(underscore^4) / writer.blockquote
  local ExampleBlock = delimited_block(equal^4)      / writer.code

  ------------------------------------------------------------------------------
  -- Comments
  ------------------------------------------------------------------------------

  local CommentLine    = slash^2 * line
  local CommentBlock   = delimited_block(slash^4)
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
                     * (lpeg.C(line) / strip_atx_end / generic.parse_inlines)
                     * lpeg.Cb("level")
                     / writer.header

  -- parse setext header
  local SetextHeader = #(line * lpeg.S("=-~^+"))
                     * lpeg.Ct(line / generic.parse_inlines)
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
    return { term = generic.parse_inlines(term), definitions = defs }
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
  -- Tables
  ------------------------------------------------------------------------------
  local Table = delimited_block("|" * equal^4) / "TODO"

  ------------------------------------------------------------------------------
  -- Inline elements
  ------------------------------------------------------------------------------
  -- XRef 
  local XRef    = P("<<") * (UnquotedField * Sep)^0 * UnquotedField * P(">>")
                  / indirect_link
  local XRefM   = inline_macro("xref") / indirect_link

  -- Local Links
  local LocalLink = inline_macro("link") / writer.link

  local InlineComment = (linechar - (optionalspace * slash^2))^1
                        / generic.parse_inlines
                        * optionalspace * slash^2 * linechar^0

  local Str       = normalchar^1 / writer.string

  local Endline   = newline * -( -- newline, but not before...
                        blankline -- paragraph break
                      + tightblocksep  -- nested list
                      + eof       -- end of document
                    ) * spacechar^0 / writer.space

  local Space     = spacechar^2 * Endline / writer.linebreak
                  + spacechar^1 * Endline^-1 * eof / ""
                  + spacechar^1 * Endline^-1 * optionalspace / writer.space
  local Strong    = asterisk * C((any - asterisk)^0) * asterisk
                  / writer.strong
  local Emph      = underscore * C((any - underscore)^0) * underscore
                      / writer.emphasis
                    + squote * C((any - squote)^0) * squote / writer.emphasis

  local InlineImage      = inline_macro("image") / writer.image
  local BlockImage       = block_macro("image")  / writer.image

  --local IndirectImage  = exclamation * tag * (C(spnl) * tag)^-1 / indirect_image

  --local Image         = DirectImage + IndirectImage

  ------------------------------------------------------------------------------
  -- Block elements
  ------------------------------------------------------------------------------
  -- BlockID 
  local BlockId = P("[[") * (UnquotedField * Sep)^0 * UnquotedField * P("]]")
                  * newline / register_link
  local Anchor  = inline_macro("anchor") / register_link

  local nonindentspace = space^-3 * - spacechar

  local ParagraphEnd   = newline * ( blankline^1 + #hash + #(more * space^-1))
  local Paragraph      = nonindentspace * (C((any-ParagraphEnd)^1)
                          / generic.parse_inlines) * ParagraphEnd
                       / writer.paragraph

  -- use DisplayHtml for passthroug blocks
  local DisplayHtml    = delimited_block(plus) / writer.plain

  ------------------------------------------------------------------------------

  syntax.Preprocess     = inc + inc1 + ifdef + ifndef
  syntax.Verbatim       = PassThrough
  syntax.Header         = AtxHeader + SetextHeader
  syntax.Comment        = Comment
  syntax.BulletList     = BulletList
  syntax.OrderedList    = OrderedList
  syntax.DefinitionList = DefinitionList
  syntax.DisplayHtml    = DisplayHtml
  syntax.Paragraph      = Paragraph
  syntax.InlineComment  = InlineComment
  syntax.Str            = Str
  syntax.Strong         = Strong
  syntax.Emph           = Emph
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
