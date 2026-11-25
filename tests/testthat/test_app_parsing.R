# App text inputs (plot starts, factor levels, check counts, replicates per
# check, p-rep groups) become validated numbers, or a classed
# fieldhub_input_error whose message names the value the user typed. The app
# shows that message through validate_design().

# Evaluate `call` and return list(value =) or list(error =). A warning raised
# on the way is turned into an unclassed error, so a case also fails when a
# parser leaks a base R warning ("NAs introduced by coercion") before failing.
parse_outcome <- function(call) {
  tryCatch(
    withCallingHandlers(list(value = eval(call)), warning = function(w) {
      stop("leaked warning: ", conditionMessage(w), call. = FALSE)
    }),
    error = function(e) list(error = e)
  )
}

# The starting-plot parser of every generic app page
plot_start_parsers <- local({
  specs <- Filter(Negate(is.null), lapply(fieldhub_app_registry(), `[[`, "spec"))
  parsers <- lapply(unique(unlist(specs)), function(spec) {
    control <- Filter(function(x) identical(x$id, "plot_start"), design_app_spec(spec)$controls)
    if (length(control) != 1L) return(function(text, state) stop("no single plot_start control"))
    control[[1L]]$parse
  })
  stats::setNames(parsers, unique(unlist(specs)))
})

ok <- function(name, call, value) list(name = name, call = call, value = value)
bad <- function(name, call, message = NULL) list(name = name, call = call, message = message)

parse_cases <- c(
  list(
    # parse_whole_numbers(): plot starts, factor levels -----------------------
    ok("whole: one value", quote(parse_whole_numbers("101", "Plot start")), 101),
    ok("whole: spaced list", quote(parse_whole_numbers(" 2, 2 ,3", "Factors")), c(2, 2, 3)),
    bad("whole: names bad token", quote(parse_whole_numbers("2,x,3", "Factors")),
        "Factors could not read \"x\" as a whole number"),
    bad("whole: bare word", quote(parse_whole_numbers("abc", "Plot start")), "\"abc\""),
    bad("whole: empty string", quote(parse_whole_numbers("", "Plot start")), "cannot be blank"),
    bad("whole: NULL", quote(parse_whole_numbers(NULL, "Plot start")), "cannot be blank"),
    bad("whole: NA", quote(parse_whole_numbers(NA_character_, "Plots")), "Plots"),
    bad("whole: two strings", quote(parse_whole_numbers(c("1", "2"), "Plots")), "Plots"),
    bad("whole: list", quote(parse_whole_numbers(list("1"), "Plots")), "Plots"),
    bad("whole: inner empty token", quote(parse_whole_numbers("2,,3", "Factors")), "empty value"),
    bad("whole: trailing comma", quote(parse_whole_numbers("2,", "Plots"))),
    bad("whole: trailing comma and spaces", quote(parse_whole_numbers("2,  ", "Plots"))),
    bad("whole: trailing comma after list", quote(parse_whole_numbers("2,3,", "Plots"))),
    bad("whole: fraction", quote(parse_whole_numbers("2.5", "Factors")),
        "could not read \"2.5\" as a whole number"),
    bad("whole: zero", quote(parse_whole_numbers("0", "Plot start")),
        "could not read \"0\" as a whole number"),

    # parse_rep_checks(): replicates per check --------------------------------
    ok("rep checks: one value recycled", quote(parse_rep_checks("2", n_checks = 3)), c(2, 2, 2)),
    ok("rep checks: padded value recycled", quote(parse_rep_checks(" 5 ", n_checks = 2)), c(5, 5)),
    ok("rep checks: one value per check", quote(parse_rep_checks("2,3", n_checks = 2)), c(2, 3)),
    # bounds on replication live downstream, not in the parser
    ok("rep checks: large value", quote(parse_rep_checks("1e9", n_checks = 2)), c(1e9, 1e9)),
    # "2,abc" used to drop "abc" and recycle 2 to both checks
    bad("rep checks: bad token", quote(parse_rep_checks("2,abc", n_checks = 2)),
        "could not read \"abc\""),
    bad("rep checks: too many values", quote(parse_rep_checks("2,3,4", n_checks = 2)),
        "1 value or 2 values.*got 3"),
    bad("rep checks: empty", quote(parse_rep_checks("", n_checks = 2)), "blank"),
    bad("rep checks: spaces", quote(parse_rep_checks("   ", n_checks = 2)), "blank"),
    bad("rep checks: NULL", quote(parse_rep_checks(NULL, n_checks = 2)), "blank"),
    bad("rep checks: inner empty token", quote(parse_rep_checks("2,,3", n_checks = 2)), "empty value"),
    bad("rep checks: trailing comma", quote(parse_rep_checks("2,", 2))),
    bad("rep checks: trailing comma and spaces", quote(parse_rep_checks("2,  ", 2))),
    bad("rep checks: trailing comma after list", quote(parse_rep_checks("2,3,", 2))),
    bad("rep checks: NA", quote(parse_rep_checks(NA_character_, 2))),
    bad("rep checks: two strings", quote(parse_rep_checks(c("1", "2"), 2))),
    bad("rep checks: list", quote(parse_rep_checks(list("1"), 2))),
    bad("rep checks: zero", quote(parse_rep_checks("0", 2))),
    bad("rep checks: negative", quote(parse_rep_checks("-1", 2))),
    bad("rep checks: fraction", quote(parse_rep_checks("1.5", 2))),
    bad("rep checks: Inf", quote(parse_rep_checks("Inf", 2))),
    bad("rep checks: NaN", quote(parse_rep_checks("NaN", 2))),
    bad("rep checks: overflow", quote(parse_rep_checks("1e309", 2))),
    # the number of checks is validated before the value is recycled
    bad("rep checks: NA check count", quote(parse_rep_checks("2", NA_real_)), "Checks"),
    bad("rep checks: zero check count", quote(parse_rep_checks("2", 0)), "Checks"),
    bad("rep checks: negative check count", quote(parse_rep_checks("2", -1)), "Checks"),
    bad("rep checks: fractional check count", quote(parse_rep_checks("2", 1.5)), "Checks"),
    bad("rep checks: text check count", quote(parse_rep_checks("2", "2")), "Checks"),
    bad("rep checks: two check counts", quote(parse_rep_checks("2", c(1, 2))), "Checks"),

    # parse_n_checks(): number of checks --------------------------------------
    ok("n checks: whole number", quote(parse_n_checks(2)), 2L),
    bad("n checks: NA", quote(parse_n_checks(NA)), "blank"),
    bad("n checks: NULL", quote(parse_n_checks(NULL))),
    bad("n checks: zero", quote(parse_n_checks(0))),
    bad("n checks: negative", quote(parse_n_checks(-1)), "whole number"),
    bad("n checks: fraction", quote(parse_n_checks(1.5))),
    bad("n checks: text", quote(parse_n_checks("2"))),
    bad("n checks: two values", quote(parse_n_checks(c(1, 2)))),
    bad("n checks: Inf", quote(parse_n_checks(Inf))),
    bad("n checks: beyond integer range", quote(parse_n_checks(.Machine$integer.max + 1))),

    # parse_rep_groups(): p-rep entries and replicates per group --------------
    ok("rep groups: two groups", quote(parse_rep_groups("75,150", "2,1")[c("repGens", "repUnits", "total_plots")]),
       list(repGens = c(75, 150), repUnits = c(2, 1), total_plots = 300)),
    # "75,abc" became c(75, NA) and failed later in 1:sum(NA)
    bad("rep groups: bad entry count", quote(parse_rep_groups("75,abc", "2,1")),
        "Entries per replicate group could not read \"abc\""),
    bad("rep groups: bad replicate count", quote(parse_rep_groups("75,150", "2,x")), "Replicates per group"),
    bad("rep groups: unequal lengths", quote(parse_rep_groups("75,150", "2")), "same number of values"),
    bad("rep groups: blank", quote(parse_rep_groups("", "2")), "blank")
  ),
  # every generic app page reads its starting plots with the strict reader
  unlist(lapply(names(plot_start_parsers), function(spec) list(
    ok(paste("plot start:", spec), bquote(plot_start_parsers[[.(spec)]]("101, 201", list())), c(101, 201)),
    bad(paste("plot start bad token:", spec), bquote(plot_start_parsers[[.(spec)]]("101,x", list())),
        "Starting plot number")
  )), recursive = FALSE)
)

test_that("app text parsers return the typed numbers or a classed error naming the problem", {
  for (case in parse_cases) {
    got <- parse_outcome(case$call)
    if (!is.null(case$value)) {
      expect_equal(got$value, case$value, info = case$name)
    } else {
      expect(
        inherits(got$error, "fieldhub_input_error") &&
          (is.null(case$message) || grepl(case$message, conditionMessage(got$error))),
        sprintf("case %s: got %s", case$name,
                if (is.null(got$error)) "no error" else
                  paste0(class(got$error)[1], ": ", conditionMessage(got$error)))
      )
    }
  }
})
