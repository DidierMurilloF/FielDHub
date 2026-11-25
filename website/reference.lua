function Div(div)
  local blocks = div.content
  if div.identifier ~= "" then
    blocks:insert(1, pandoc.RawBlock("html", '<span id="' .. div.identifier .. '"></span>'))
  end
  return blocks
end
function Span(span)
  return span.content
end
-- Write argument lists as Markdown definition lists, which GitHub-flavored
-- Markdown lacks: the term, then each definition indented after a colon.
function DefinitionList(list)
  local lines = {}
  for _, item in ipairs(list.content) do
    table.insert(lines, "**" .. pandoc.utils.stringify(item[1]) .. "**")
    for _, definition in ipairs(item[2]) do
      local text = pandoc.write(pandoc.Pandoc(definition), "gfm", { wrap_text = "wrap-none" })
      table.insert(lines, ":   " .. text:gsub("%s+$", ""):gsub("\n", "\n    "))
    end
    table.insert(lines, "")
  end
  return pandoc.RawBlock("markdown", table.concat(lines, "\n"))
end
