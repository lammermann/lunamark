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
    if label then
      return lbracket * optionalspace * label * (Sep * Cg(Attrs, "attrs"))^-1
              * optionalspace * rbracket * optionalspace
    end
    return lbracket * optionalspace * Cg(Attrs, "attrs") * optionalspace * rbracket
            * optionalspace
  end

  ------------------------------------------------------------------------------
  -- Helpers for links and references
  ------------------------------------------------------------------------------
  -- Normalize a asciidoc reference tag.  (Make lowercase, and collapse
  -- adjacent whitespace characters.)
  local function normalize_tag(tag)
    return utf8.lower(gsub(util.rope_to_string(tag), "[ \n\r\t]+", " "))
  end

  -- List of references defined in the document
  local references = {}

  -- add a reference to the list
  local function register_link(tag,url,title)
    references[normalize_tag(tag)] = { url = url, title = title }
    return ""
  end

  -- lookup link reference and return either
  -- the link or nil and fallback text.
  local function lookup_reference(tag, label)
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
      local r = references[normalize_tag(tag)]
      if r then
        return r
      else
        return nil, {"[", generic.parse_inlines(label), "]", tagpart}
      end
  end

  -- lookup link reference and return a link, if the reference is found,
  -- or a bracketed label otherwise.
  local function indirect_link(tag, label)
    local label = label or tag
    local r,fallback = lookup_reference(tag, label)
    if r then
      return writer.link(generic.parse_inlines_no_link(label), r.url, r.title)
    else
      return fallback
    end
  end

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
  ------------------------------------------------------------------------------
  -- Text Formating
  ------------------------------------------------------------------------------

  -- Quoted Text
  local function constraint_quote(qchar)
    local wordending  = spacing + period + dash + lbracket + rbracket + lparent + rparent
                        + eof
    return attrlist()^-1 * qchar * C((any - (qchar * wordending))^0) * qchar
  end

  local function unconstraint_quote(qchar)
    return attrlist()^-1 * qchar * qchar * C((any - (qchar * qchar))^0) * qchar * qchar
  end

  local Emph      = unconstraint_quote(underscore) / writer.emphasis
                    + constraint_quote(squote) / writer.emphasis
                    + constraint_quote(underscore) / writer.emphasis
  local Strong    = unconstraint_quote(asterisk) / writer.strong
                    + constraint_quote(asterisk) / writer.strong
  local Mono      = unconstraint_quote(plus) / writer.monospace
                    + constraint_quote(plus) / writer.monospace
  local Unquoted  = unconstraint_quote(hash) / writer.styled
                    + constraint_quote(hash) / writer.styled

  -- Superscript and Subscript
  local SuperScript = circumflex * C((any - circumflex)^0) * circumflex
                      / writer.super
  local SubScript = tilde * C((any - tilde)^0) * tilde
                      / writer.sub
  local SuperSub  = C((any - SuperScript - SubScript)^0) / generic.parse_inlines
                    * (SuperScript + SubScript)

  local Quote = Emph + Strong + Mono + Unquoted + SuperSub

  -- Replacements
  local Copyright    = P("(C)") / "©"
  local Trademark    = P("(TM)") / "™"
  local RegTrademark = P("(R)") / "®"
  local EmDash       = P("--") / "—"
  local Ellipsis     = P("...") / ". . ."
  local RightArrow   = P("->") / "→"
  local LeftArrow    = P("<-") / "←"
  local RDoubleArrow = P("=>") / "⇒"
  local LDoubleArrow = P("<=") / "⇐"

  local Replacements = Copyright
                     + Trademark
                     + RegTrademark
                     + EmDash
                     + Ellipsis
                     + RightArrow
                     + LeftArrow
                     + RDoubleArrow
                     + LDoubleArrow

  local Replacement = C((any - Replacements)^0) / generic.parse_inlines
                      * Replacements

  ------------------------------------------------------------------------------
  -- Titles
  ------------------------------------------------------------------------------

  -- parse Atx title start and return level
  local TitleStart = #equal * C(equal^-6) * -equal / length

  -- parse setext title ending and return level
  local TitleLevel = equal^1 * Cc(1)
                     + dash^1 * Cc(2)
                     + tilde^1 * Cc(3)
                     + circumflex^1 * Cc(4)
                     + plus^1 * Cc(5)

  local function strip_atx_end(s)
    return s:gsub("[=%s]*\n$","")
  end

  -- parse atx title
  local AtxTitle = Cg(TitleStart,"level")
                     * optionalspace
                     * (C(line) / strip_atx_end / generic.parse_inlines)
                     * Cb("level")
                     / writer.header

  -- parse setext title
  local SetextTitle = #(line * S("=-~^+"))
                     * Ct(line / generic.parse_inlines)
                     * TitleLevel
                     * optionalspace * newline
                     / writer.header

  local Title    = AtxTitle + SetextTitle

  ------------------------------------------------------------------------------
  -- Block Titles
  ------------------------------------------------------------------------------

  local BlockTitle = period * C(linechar^1) * newline

  ------------------------------------------------------------------------------
  -- BlockId Element
  ------------------------------------------------------------------------------

  local BlockId = P("[[") * UnquotedField * (Sep * UnquotedField)^-1
                  * P("]]") * newline

  local function block_element(pattern)
    return Cg(Ct(BlockId^-1), "id") * Cg(Ct(BlockTitle^-1), "btitle") * pattern
            * Cb("id") * Cb("btitle")
  end

  ------------------------------------------------------------------------------
  -- Paragraphs
  ------------------------------------------------------------------------------

  local function paragraph_block(para, id, title)
    local attrs = {}
    if id[1] then
      local tag = normalize_tag(id[1])
      local url = "#" .. tag
      register_link(tag, url, id[2])
      attrs.id = tag
      return writer.block(para, attrs)
    end
    return para
  end

  local ParagraphEnd  = ( newline * blankline^1)
                      + eof
                      + V("DelimitedBlock")
                      + V("List")

  -- normal Paragraph
  local NormalPara    = (C((any-ParagraphEnd)^1)
                         / generic.parse_inlines) * ParagraphEnd
                      / writer.paragraph

  -- literal Paragraph
  local LiteralPara   = (attrlist("literal") * newline
                        * C((any-ParagraphEnd)^1) * ParagraphEnd)
                        / writer.verbatim
                      + (spacechar^1
                        * C((any-ParagraphEnd)^1) / function(s)
                            return s:gsub("\n%s*","\n") end
                          * ParagraphEnd)
                        / writer.verbatim

  local Paragraphs    = LiteralPara + NormalPara
  local Paragraph     = block_element(Paragraphs) / paragraph_block

  ------------------------------------------------------------------------------
  -- Delimited Blocks
  ------------------------------------------------------------------------------
  local function delimited_block(start, finish)
    local finish = finish or start
    local deli_start  = start * newline
    local deli_finish = newline * finish * newline
    return deli_start * C((any - deli_finish)^0) * deli_finish
  end

  local PassThrough     = delimited_block(plus^4)       / writer.plain
  local ListingBlock    = delimited_block(dash^4)       / writer.verbatim
  local LiteralBlock    = delimited_block(period^4)     / writer.verbatim
  local QuoteBlock      = delimited_block(underscore^4) / writer.blockquote
  local ExampleBlock    = delimited_block(equal^4)      / writer.code

  local DelimitedBlock  = PassThrough
                          + ListingBlock
                          + LiteralBlock
                          + QuoteBlock
                          + ExampleBlock

  ------------------------------------------------------------------------------
  -- Macros
  ------------------------------------------------------------------------------
  local target = C((nonspacechar-colon * any-(lbracket+rbracket+newline))^0)

  local function block_macro(keyword)
    return P(keyword) * P("::") * target * attrlist() * (newline + eof)
  end
  
  local function inline_macro(keyword)
    return P(keyword) * colon * target * attrlist()
  end

  -- Inline Macros
  ----------------

  -- Footnotes
  local rawnotes = {}

  -- like indirect_link
  local function lookup_note(ref)
    local found = rawnotes[normalize_tag(ref)]
    if found then
      return writer.note(generic.parse_blocks(found))
    else
      return {"[^", ref, "]"}
    end
  end

  local function register_note(ref,rawnote)
    rawnotes[normalize_tag(ref)] = rawnote
    return ""
  end

  local function direct_note(target, attrs)
    return writer.note(generic.parse_blocks(attrs))
  end

  local function footnoteref(target, attrs)
    local ref = attrs[1]
    local rawnote = attrs[2]
    if rawnote then
      register_note(ref, rawnote)
    end
    return lookup_note(ref)
  end

  local Footnote    = inline_macro("footnote")    * Cb("attrs") / direct_note
  local FootnoteRef = inline_macro("footnoteref") * Ct(Cb("attrs")) / footnoteref

  -- URLs
  local function url_link(url, label)
    local label = label or url
    return writer.link(label, url)
  end

  local Link  = Cs(( P("http://")
                    + P("https://")
                    + P("ftp:://")
                    + P("file:://")
                  )
                * C(any - spacing -attrlist())^1)
                * (attrlist() * Cb("attrs"))^-1
              / url_link

  local EMail   = Cs((nonspacechar - at)^1 * at
                    * (nonspacechar - at - attrlist())^1)
  local MailTo  = ( ( P("mailto:") * EMail
                      * attrlist() * Cb("attrs"))
                    + EMail
                  )
                / url_link

  -- Internal Cross References
  local function anchor_link(tag, title)
    local tag = normalize_tag(tag)
    local url = "#" .. tag
    register_link(tag, url, title)
    return writer.anchor(tag)
  end

  local Anchor  = inline_macro("anchor") * Cb("attrs") / anchor_link

  local XRField = Cs((any - Sep - P(">>"))^0)
  local XRef1   = P("<<") * XRField * (Sep * XRField)^-1 * P(">>")
  local XRef2   = inline_macro("xref") * Cb("attrs")
  local XRef    = ( XRef1 + XRef2 ) / indirect_link

  -- Local Links
  local function locallink(target, attrs)
    local attrs = attrs or {}
    local label = attrs[1] or ""
    --return writer.link(label, target, attrs[1])
    return writer.link(label, target)
  end

  local LocalLink = inline_macro("link") * Ct(Cb("attrs")) / locallink

  -- Images
  local function inline_img(target, attrs)
    local attrs = attrs or {}
    local label = attrs[1] or ""
    if type(label) ~= "string" then label = "" end
    local sty = {}
    if attrs[#attrs] and type(attrs[#attrs]) == "table" then
      sty = attrs[#attrs]
    end
    if sty.alt and label == "" then
      label = sty.alt
    end
    sty.width = sty.width or sty.scaledwidth
    local title = ""
    return writer.image(label, target, title, sty)
  end

  local InlineImage = inline_macro("image") * Ct(Cb("attrs")) / inline_img

  local InlineComment = (linechar - (optionalspace * slash^2) - Link)^1
                        / generic.parse_inlines
                        * optionalspace * slash^2 * linechar^0

  local InlineMacro   = Anchor
                        + XRef
                        + Footnote
                        + FootnoteRef
                        + Link
                        + MailTo
                        + LocalLink
                        + InlineImage

  -- Block Macros
  ---------------

  -- Images
  local function block_img(target, attrs, id, btitle)
    local attrs = attrs or {}
    local title = btitle[1] or ""
    local label = attrs[1] or ""
    if type(label) ~= "string" then label = "" end
    local sty = {}
    if attrs[#attrs] and type(attrs[#attrs]) == "table" then
      sty = attrs[#attrs]
    end
    sty.id = id[1] or sty.id
    if sty.alt and label == "" then
      label = sty.alt
    end
    sty.width = sty.width or sty.scaledwidth
    return writer.blockimage(label, target, title, sty)
  end

  local BlockImage = block_element(block_macro("image") * Ct(Cb("attrs"))) / block_img

  -- Comment Lines
  local CommentLine  = slash^2 * line
  local CommentBlock = delimited_block(slash^4)

  local Comment      = CommentBlock + CommentLine

  -- System Macros
  ----------------
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
  local inc1  = block_macro("include1") / include
  local eval  = block_macro("eval")     / "TODO"
  local sys   = block_macro("sys")      / "TODO"
  local sys2  = block_macro("sys2")     / "TODO"

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

  local BlockMacro = inc + inc1 + ifdef + ifndef
                      + BlockImage

  ------------------------------------------------------------------------------
  -- Tables
  ------------------------------------------------------------------------------

  local Multiplier = Cg(C(digit^1) * asterisk, "multiplier")^-1

  local halignvals = {
    ["<"] = "left",
    ["^"] = "center",
    [">"] = "right",
  }
  local HAlign = Cg(C(S("<^>")) / halignvals, "align")
  local valignvals = {
    ["<"] = "top",
    ["^"] = "center",
    [">"] = "bottom",
  }
  local VAlign = Cg(C(S("<^>")) / valignvals, "valign")
  local Align  = HAlign^-1  * (period * VAlign)^-1

  local Width = Cg(Cs(digit^1) * percent^-1, "width")^-1

  local cstyles = {
    e = writer.emphasis,
    s = writer.strong,
  }
  local CStyle = Cg(C(S("demshalv")) / cstyles, "style")^-1

  -- Coloumn Specifiers
  ---------------------
  local function process_colls(cols)
    local out = {}
    local j = 1
    for i,c in ipairs(cols) do
      c.multiplier = c.multiplier or 1
      for k=1, c.multiplier do
        out[j] = c
        j = j + 1
      end
    end
    return out
  end

  local ColSpecifier = Ct(Multiplier * Align * Width * CStyle)

  local Cols = Ct(ColSpecifier * (Sep * ColSpecifier)^0) / process_colls

  -- Cell Specifiers
  ------------------
  local CSpan  = Cg(digit^1, "colspan")
  local RSpan  = Cg(digit^1, "rowspan")
  local Span   = CSpan^-1 * (period * RSpan)^-1 * plus
  local CMulti = Cg(digit^1, "colmulti")
  local RMulti = Cg(digit^1, "rowmulti")
  local Multi  = CMulti^-1 * (period * RMulti)^-1 * asterisk

  local CellSpecifier = Ct((Span + Multi)^-1 * Align * CStyle)

  -- Row Formats
  --------------
  local function csvrow(deli)
    local deli = deli or comma
    deli = P(deli)
    local cell = Ct(Cg((any - newline - deli)^1, "content"))
    return Ct((cell * deli)^0 * cell) * newline^-1
  end

  local function psvrow(deli)
    local deli = deli or "|"
    deli = spacechar^0 * Cg(CellSpecifier * -P("\\") * P(deli), "sty") * spacechar^0
    local cell = Ct(deli * Cg(Cs((any - deli - (newline * deli))^0), "content"))
    return Ct(cell^1 * newline)^0 * Ct(cell^1)
  end

  local function tablerow(format, deli)
    local format = format or "psv"
    local row = psvrow(deli)
    if format == "csv" then
      row = csvrow(deli)
    end
    local fun = function(path)
      local pattern = row^1
      return pattern:match(include(path))
    end
    return Ct(((block_macro("include") / fun) + row)^1)
  end

  local function process_table(tdata, attrs, id, title)
    local title = title[1] or ""
    local sty = {}
    if type(attrs) == "table" then
      if type(attrs[#attrs]) == "table" then
        sty = attrs[#attrs]
      end
    end
    if sty.frame == "topbot" then sty.frame = "hsides" end
    local pattern = tablerow(sty.format, sty.separator)
    local rows = pattern:match(tdata) or {}
    if not sty.cols then
      local fun = function (x)
        local t = {}
        for i=1,x do t[i] = {} end
        return t
      end
      sty.cols = fun(#rows[1])
    elseif type(sty.cols) == "string" then
      sty.cols = Cols:match(sty.cols)
    end
    if sty.options then
      local options = lpeg.match(Ct(C((any-comma)^1)*(comma*C((any-comma)^1))^0), sty.options)
      sty.options = {}
      for _,o in ipairs(options) do
        sty.options[o] = true
      end
    end
    for _,r in ipairs(rows) do
      local cidx = 1
      for _,c in ipairs(r) do
        c.content = c.content or ""
        c.sty = c.sty or {}
        for k,v in pairs(sty.cols[cidx] or {}) do
          c.sty[k] = c.sty[k] or v
        end
        local fun = c.sty.style or generic.parse_inlines
        c.content = fun(c.content)
        cidx = cidx + (c.sty.colspan or 1)
      end
    end
    return writer.table(rows, title, sty)
  end

  local TableBlock  = (attrlist() * newline)^-1 * delimited_block("|" * equal^4)
                      * Ct(Cb("attrs"))

  local Table = block_element(TableBlock) / process_table

  ------------------------------------------------------------------------------
  -- Attribute Entry
  ------------------------------------------------------------------------------

  local function attribute_entry(name, is_set, value)
    local value = value or true
    local name  = normalize_tag(name)
    if is_set == false then
      writer.set_metadata(name, false)
    else
      writer.set_metadata(name, value)
    end
    return ""
  end

  local EndAttEntry     = ((exclamation * Cc(false)) + Cc(true)) * colon
  local AttVal          = optionalspace
                          * C((linechar - newline
                            - (spacechar^1 * plus * newline))^1)
  local AttributeValue  = Cs((AttVal * spacechar^1 * plus * newline)^0
                          * AttVal)

  local AttributeEntry  = colon * C((any - (newline + EndAttEntry))^1)
                          * EndAttEntry
                          * AttributeValue^-1 * blankline
                        / attribute_entry

  ------------------------------------------------------------------------------
  -- Attribute References
  ------------------------------------------------------------------------------

  local function attribute_ref(name)
    local name  = normalize_tag(name)
    return generic.parse_inlines(writer.get_metadata()[name])
  end

  local AttributeRef  = lcbrace * C((any-newline-rcbrace)^1) * rcbrace
                      / attribute_ref

  ------------------------------------------------------------------------------

  local Blank          = blankline / ""
                       + V("Comment") / ""

  syntax =
    { "Document",

      Document          = V("Header")^-1
                          --* V("Preamble")^-1
                          * V("Blocks"),
                          --* V("Section")^0,

      Blocks                = Blank^0 *
                              V("Block")^-1 *
                              (Blank^0 / function() return writer.interblocksep end * V("Block"))^0 *
                              Blank^0 *
                              eof,

      Header            = V("Title")
                          * ( V("AuthorInfo") * V("RevisionInfo")^-1)^-1,

      AuthorInfo        = V("FirstName")
                          * ( V("MiddleName")^-1 * V("LastName") )^-1
                          * V("EmailAddress")^-1,

      RevisionInfo      = V("RevisionNumber")^-1
                          * V("RevisionDate")
                          * V("RevisionRemark")^-1,

      Block             = V("Comment")
                          + V("Title")
                          + V("BlockMacro")
                          + V("List")
                          + V("DelimitedBlock")
                          + V("Table")
                          + V("AttributeEntry")
                          + V("AttributeList")
                          + V("BlockTitle")
                          + V("Paragraph"),

      Inline            = V("InlineComment")
                          + V("Space")
                          + V("Endline")
                          + V("SpecialChar")
                          + V("Quote")
                          + V("SpecialWord")
                          + V("Replacement")
                          + V("Attribute")
                          + V("InlineMacro")
                          + V("Replacement2"),

      ---------------------------------------------
      Title             = Title,
      FirstName         = fail,
      MiddleName        = fail,
      LastName          = fail,
      EmailAddress      = fail,
      RevisionNumber    = fail,
      RevisionDate      = fail,
      RevisionInfo      = fail,
      RevisionRemark    = fail,

      Comment           = Comment,
      BlockTitle        = fail,
      Paragraph         = Paragraph,
      DelimitedBlock    = DelimitedBlock,
      BlockMacro        = BlockMacro,
      InlineMacro       = InlineMacro,
      List              = fail,
      Table             = Table,
      SpecialChar       = fail,
      SpecialWord       = fail,
      Replacement       = Replacement,
      Replacement2      = fail,
      Attribute         = AttributeRef,
      AttributeEntry    = AttributeEntry,
      AttributeList     = fail,
      ListTerm          = fail,
      ListParagraph     = fail,
      ListContinuation  = fail,
      ItemText          = fail,

      InlineComment     = InlineComment,
      Quote             = Quote,
      Space             = Space,
      Endline           = Endline,
      Replacement2      = Str
    }

  return syntax
end

function M.new(writer, options)
  local options = options or {}

  generic.alter_syntax(options, add_asciidoc_syntax)

  -- return default parser
  return generic.new(writer, options)

end

return M
