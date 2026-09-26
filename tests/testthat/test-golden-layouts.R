# Golden snapshots of the field layouts of every catalogue design whose
# layout is built from its field book at plot time. See helper-layouts.R.

for (name in layout_entries()) {
  local({
    entry <- name
    test_that(paste("the field layouts of", entry, "are unchanged"), {
      skip_unless_golden_platform()
      expect_snapshot(print_layouts(catalogue_design(entry)))
    })
  })
}
