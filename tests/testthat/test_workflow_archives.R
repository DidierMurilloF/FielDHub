archive_test_inputs <- function(simulated = TRUE) {
  design <- RCBD(t = 4, reps = 2, seed = 17)
  book <- field_layout(design)
  simulation <- if (simulated) simulate_classic_field_book(book, 10, 20, "YIELD", 27) else NULL
  if (!is.null(simulation)) book <- simulation$field_book
  list(design = design, book = book, simulation = simulation,
       layout = list(parameters = list(layout = 1, planter = "serpentine", stacked = "vertical"),
                     selected = 1))
}

test_that("workflow archives preserve design, displayed data, and simulation records", {
  input <- archive_test_inputs()
  out <- new_workflow_archive(input$design, input$book, input$book,
                              input$simulation, input$layout, "field_book")
  expect_identical(out$schema_version, 1L)
  expect_identical(out$design, input$design)
  expect_identical(out$field_book, input$book)
  expect_identical(out$simulation, input$simulation)
  expect_identical(out$layout, input$layout)
  expect_identical(out$export$data, input$book)
  expect_identical(out$export$kind, "field_book")
  expect_identical(out$software$R, as.character(getRversion()))
  expect_identical(out$software$packages[["FielDHub"]], as.character(utils::packageVersion("FielDHub")))
})

test_that("archive callbacks preserve CSV bytes and include safe reproducibility files", {
  skip_if_not_installed("zip")
  input <- archive_test_inputs()
  handlers <- csv_archive_handlers(
    filename = function() "trial.csv", data = function() input$book,
    design = function() input$design, field_book = function() input$book,
    simulation = function() input$simulation, layout = function() input$layout
  )
  directory <- tempfile("fieldhub-archive-test-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  file <- file.path(directory, handlers$filename())
  expect_identical(basename(file), "trial.zip")
  set.seed(92)
  before_seed <- .Random.seed
  before_options <- options()
  before_directory <- getwd()
  handlers$content(file)
  expect_identical(.Random.seed, before_seed)
  expect_identical(options(), before_options)
  expect_identical(getwd(), before_directory)
  expect_setequal(zip::zip_list(file)$filename,
                  c("data.csv", "workflow.rds", "reproduce.R", "README.txt"))
  extracted <- file.path(directory, "contents")
  utils::unzip(file, exdir = extracted)
  original <- file.path(directory, "original.csv")
  utils::write.csv(input$book, original, row.names = FALSE)
  expect_identical(unname(tools::md5sum(original)),
                   unname(tools::md5sum(file.path(extracted, "data.csv"))))
  saved <- readRDS(file.path(extracted, "workflow.rds"))
  expect_identical(saved$design, input$design)
  expect_identical(saved$simulation, input$simulation)
  old <- setwd(extracted)
  on.exit(setwd(old), add = TRUE, after = FALSE)
  env <- new.env(parent = globalenv())
  sys.source("reproduce.R", envir = env)
  expect_identical(env$design, input$design)
  expect_identical(env$simulation, input$simulation)
  expect_identical(env$exported_data, input$book)
})

test_that("archive callbacks read the current output only when downloading", {
  input <- archive_test_inputs(FALSE)
  count <- 0L
  provider <- function() {count <<- count + 1L; input$book}
  handlers <- csv_archive_handlers(function() "layout.csv", provider,
                                   function() input$design, function() input$book,
                                   kind = "layout")
  expect_identical(count, 0L)
  expect_identical(handlers$filename(), "layout.zip")
  expect_identical(count, 0L)
})

test_that("malformed archive inputs fail before creating a download", {
  input <- archive_test_inputs()
  with_list <- input$book
  with_list$BAD <- rep(list(1), nrow(with_list))
  for (bad in list(NULL, list(), input$book[FALSE, ], with_list)) {
    expect_error(new_workflow_archive(input$design, input$book, bad),
                 class = "fieldhub_input_error")
  }
  expect_error(new_workflow_archive(NULL, input$book, input$book), class = "fieldhub_input_error")
  expect_error(new_workflow_archive(input$design, input$book, input$book, simulation = list()),
               class = "fieldhub_input_error")
  expect_error(new_workflow_archive(input$design, input$book, input$book, kind = "unknown"),
               class = "fieldhub_input_error")
  bad_layout <- list(parameters = list(layout = quote(stop("unsafe"))))
  expect_error(new_workflow_archive(input$design, input$book, input$book, layout = bad_layout),
               class = "fieldhub_input_error")
  handlers <- csv_archive_handlers(function() "trial.csv", function() NULL,
                                   function() input$design, function() input$book)
  file <- tempfile(fileext = ".zip")
  expect_error(handlers$content(file), class = "fieldhub_input_error")
  expect_false(file.exists(file))
})

test_that("archive filenames cannot introduce paths or ambiguous formats", {
  expect_identical(csv_archive_filename("trial.csv"), "trial.zip")
  expect_identical(csv_archive_filename("Trial.CSV"), "Trial.zip")
  for (name in list(NULL, "", NA_character_, c("a.csv", "b.csv"), "../a.csv", "a/b.csv", "a\\b.csv", "x.txt")) {
    expect_error(csv_archive_filename(name), class = "fieldhub_input_error")
  }
})

test_that("layout results record the effective layout parameters", {
  x <- RCBD(t = 4, reps = 2, l = 2, plotNumber = c(101, 1001), seed = 17)
  out <- plot_layout(x, layout = 1, planter = "cartesian", stacked = "horizontal", l = 2)
  expect_identical(out$layout_metadata,
                   list(parameters = list(layout = 1, planter = "cartesian", stacked = "horizontal"), selected = 2))
})
