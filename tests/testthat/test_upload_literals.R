test_that("upload examples retain their literal published contents", {
  expect_identical(upload_format_example("optim"), data.frame(
    ENTRY = 1:9, NAME = c("CHECK1", "CHECK2", "CHECK3", "GenotypeA", "GenotypeB", "GenotypeC",
                          "GenotypeD", "GenotypeE", "GenotypeF"),
    REPS = factor(c(10, 10, 10, 1, 1, 1, 1, 1, 1))))
  expect_identical(upload_format_example("spd"), data.frame(
    WHOLEPLOT = c("NFung", "Fung1", "Fung2", "Fung3", "Fung4", "", "", "", "", ""),
    SUBPLOT = c("Beans1", "Beans2", "Beans3", "Beans4", "Beans5", "Beans6", "Beans7", "Beans8", "Beans9", "Beans10")))
  expect_identical(upload_format_example("sspd"), data.frame(
    WHOLPLOT = c("IRR_NO", "IRR_Yes", "", "", "", "", "", "", "", ""),
    SUBPLOT = c("NFung", "Fung1", "Fung2", "Fung3", "Fung4", "", "", "", "", ""),
    SUB_SUBPLOT = c("Beans1", "Beans2", "Beans3", "Beans4", "Beans5", "Beans6", "Beans7", "Beans8", "Beans9", "Beans10")))
  expect_identical(upload_format_example("multi_loc_prep"), data.frame(
    ENTRY = 1:10, NAME = c("Genotype-A", "Genotype-B", "Genotype-C", "Genotype-D", "Genotype-E",
                           "Genotype-F", "Genotype-G", "Genotype-H", "Genotype-I", "Genotype-J")))
})

test_that("upload HTML keeps each literal module identifier", {
  skip_if_not_installed("shiny")
  ids <- list(
    alpha = c("owndata_alpha", "file.alpha", "sep.alpha"),
    crd = c("owndatacrd", "file.CRD", "sep.crd"),
    mdiag = c("list_entries_multiple", "file_multiple", "sep.DIAGONALS"),
    sdiag = c("owndataDIAGONALS", "file1", "sep.DIAGONALS"),
    factorial = c("owndata", "file.FD", "sep.fd"),
    ibd = c("owndataibd", "file.IBD", "sep.ibd"),
    lsd = c("owndataLSD", "file.LSD", "sep.lsd"),
    multi_loc_prep = c("multi_prep_data", "file_multi_prep", "sep_multi_prep"),
    optim = c("owndataOPTIM", "file3", "sep.OPTIM"),
    prep = c("owndataPREPS", "file.preps", "sep.preps"),
    arcbd = c("owndata_a_rcbd", "file1_a_rcbd", "sep.a_rcbd"),
    rcbd = c("owndatarcbd", "file.RCBD", "sep.rcbd"),
    rect = c("owndata_rectangular", "file.rectangular", "sep.rectangular"),
    rcd = c("owndataRCD", "file.RCD", "sep.rcd"),
    sparse_allocation = c("input_sparse_data", "sparse_file", "sparse_file_sep"),
    spd = c("owndataSPD", "file.SPD", "sep.spd"),
    square = c("owndata_square", "file.square", "sep.square"),
    sspd = c("owndataSSPD", "file.SSPD", "sep.sspd"),
    strip = c("owndataSTRIP", "file.STRIP", "sep.strip")
  )
  for (design in names(ids)) {
    html <- as.character(app_upload_ui(shiny::NS("x"), design))
    for (id in ids[[design]]) expect_match(html, paste0('id="x-', id, '"'), fixed = TRUE, info = design)
  }
})
