-- Keep Pandoc's vignette heading IDs when writing GitHub-flavored Markdown.
function Header(heading)
  if heading.identifier ~= "" then
    heading.content:insert(pandoc.RawInline("html", " {#" .. heading.identifier .. "}"))
  end
  return heading
end
