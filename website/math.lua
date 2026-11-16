-- Write maths as $...$ and $$...$$ for pymdownx.arithmatex. Pandoc 3.1.5 and
-- newer write GitHub's $`...`$ and ```math forms, which arithmatex ignores.
function Math(m)
  local delim = m.mathtype == "DisplayMath" and "$$" or "$"
  return pandoc.RawInline("gfm", delim .. m.text .. delim)
end
