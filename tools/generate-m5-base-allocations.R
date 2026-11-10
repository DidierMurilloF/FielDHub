# Record the allocation results that the switch from blocksdesign to the block
# engine (M5) must leave unchanged: field books built from an allocation passed
# in (sparse_list / optim_list), and one-location allocations, which never
# searched. Run once from the package root of the commit before M5 (eca15e6),
# where do_optim() still calls blocksdesign::blocks():
#   Rscript tools/generate-m5-base-allocations.R
# It writes tests/testthat/fixtures/m5-base-allocations.rds.
pkgload::load_all(".", quiet = TRUE)
sparse_list <- do_optim("sparse", lines = 120, l = 4, copies_per_entry = 3,
                        add_checks = TRUE, checks = 4, seed = 21)
prep_list <- do_optim("prep", lines = 150, l = 3, copies_per_entry = 4, add_checks = TRUE,
                      checks = 4, rep_checks = c(2, 2, 2, 2), seed = 22)
fixtures <- list(
  sparse_list = sparse_list,
  sparse_given = sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4,
                                   sparse_list = sparse_list, seed = 31, year = 2026)$fieldBook,
  prep_list = prep_list,
  prep_given = multi_location_prep(lines = 150, l = 3, copies_per_entry = 4, checks = 4,
                                   rep_checks = c(2, 2, 2, 2), optim_list = prep_list,
                                   seed = 32, year = 2026)$fieldBook,
  prep_one_location = multi_location_prep(lines = 150, l = 1, copies_per_entry = 2, checks = 4,
                                          rep_checks = c(2, 2, 2, 2), seed = 11,
                                          year = 2026)$fieldBook,
  optim_one_prep = do_optim("prep", lines = 20, l = 1, copies_per_entry = 3, seed = 5)$allocation,
  optim_one_sparse = do_optim("sparse", lines = 20, l = 1, copies_per_entry = 1, add_checks = TRUE,
                              checks = 2, seed = 6)$allocation
)
saveRDS(fixtures, "tests/testthat/fixtures/m5-base-allocations.rds", version = 2)
