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
    return qchar * C((any - (qchar * (spacing + period)))^0) * qchar
  end

  local function unconstraint_quote(qchar)
    return qchar * qchar * C((any - (qchar * qchar))^0) * qchar * qchar
  end

  local Emph      = unconstraint_quote(underscore) / writer.emphasis
                    + constraint_quote(squote) / writer.emphasis
                    + constraint_quote(underscore) / writer.emphasis
  local Strong    = unconstraint_quote(asterisk) / writer.strong
                    + constraint_quote(asterisk) / writer.strong
  local Mono      = unconstraint_quote(plus) / writer.monospace
                    + constraint_quote(plus) / writer.monospace
  local Quote = Emph + Strong + Mono

  ------------------------------------------------------------------------------
  -- Titles
  ------------------------------------------------------------------------------

  -- parse Atx title start and return level
  local TitleStart = #equal * C(equal^-6) * -equal / length

  -- parse setext header ending and return level
  local TitleLevel = equal^1 * Cc(1)
                     + dash^1 * Cc(2)
                     + tilde^1 * Cc(3)
                     + circumflex^1 * Cc(4)
                     + plus^1 * Cc(5)

  local function strip_atx_end(s)
    return s:gsub("[=%s]*\n$","")
  end

  -- parse atx header
  local AtxTitle = Cg(TitleStart,"level")
                     * optionalspace
                     * (C(line) / strip_atx_end / generic.parse_inlines)
                     * Cb("level")
                     / writer.header

  -- parse setext header
  local SetextTitle = #(line * S("=-~^+"))
                     * Ct(line / generic.parse_inlines)
                     * TitleLevel
                     * optionalspace * newline
                     / writer.header

  local Title    = AtxTitle + SetextTitle

  ------------------------------------------------------------------------------
  -- Paragraphs
  ------------------------------------------------------------------------------

  local ParagraphEnd  = ( newline * blankline^1)
                      + eof
                      + V("DelimitedBlock")
                      + V("List")

  -- normal Paragraph
  local NormalPara    = (C((any-ParagraphEnd)^1)
                         / generic.parse_inlines) * ParagraphEnd
                      / writer.paragraph

  local Paragraph     = NormalPara

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
  local ListingBlock    = delimited_block(dash^4)       / writer.code
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
    return P(keyword) * P("::") * target * attrlist() * newline
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

  local InlineComment = (linechar - (optionalspace * slash^2))^1
                        / generic.parse_inlines
                        * optionalspace * slash^2 * linechar^0

  local InlineMacro   = Footnote
                        + FootnoteRef

  -- Block Macros
  ---------------
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

  local BlockMacro = inc + inc1 + ifdef + ifndef

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
      Table             = fail,
      SpecialChar       = fail,
      SpecialWord       = fail,
      Replacement       = fail,
      Replacement2      = fail,
      Attribute         = fail,
      AttributeEntry    = fail,
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
