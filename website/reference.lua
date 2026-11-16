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
