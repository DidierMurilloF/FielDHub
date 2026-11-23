# Summary of the onestage stage-rule prototype sweep against the os baseline
# (phase 2 plan Task 4 Step 5; spec 6.1, 6.2 search count, 7). A measurement
# tool, not a test: it prints the tables the four rules of Step 5 are decided
# from and writes the measurements.
#
#   Rscript tools/summarise-onestage-stages.R [proto_dir] [baseline.csv.gz] [out.csv.gz]
#
# Defaults: ~/fieldhub-libs/runs/phase2/proto (the proto-*.csv shards written
# by tools/prototype-onestage.R), tools/nested-goldens-os-baseline.csv.gz and
# tools/onestage-stage-measurements.csv.gz. It needs no package: only base R
# and tests/testthat/helper-nested-goldens.R. Nothing here draws random
# numbers, so two runs on the same inputs give the same file.
#
# What is compared. The prototype models `iterations = 200`, so each
# (shape, coding, swap set, S) is judged against the baseline rows recorded
# with iterations == 200, on the seeds where onestage ran, with the onestage
# rules of nested_acceptance(bootstrap = FALSE): worst seed, constant
# baseline, optimum (against the recorded bound), errors, and the fallback
# count rule. Two rules of spec 7 are not in that mode and are handled here:
# - Latinization (clash counts): the prototype does not record clashes, so
#   `clash_ok` is not measured and is left out of every comparison.
# - Non-inferiority on the mean: with 8 (t <= 100) or 2 (t = 200, 300)
#   prototype seeds the bootstrap of spec 7 is not run. `ni_ok` is the same
#   margin applied to the point estimate: candidate mean - baseline mean >=
#   -max(2e-6, 0.5 x baseline SD), for row, column and joint A and D. It is
#   more lenient than the bootstrap's 95% lower bound. `ni_clear` marks the
#   failures that are not seed noise: the upper one-sided 95% limit of the
#   difference (normal, both sides' standard errors) is still below the margin.
# t = 300 has no iterations == 200 baseline (the grid records it at 20 only),
# so it has no rule outcome here; a second table compares S = 1 with the
# iterations == 20 baseline (iterations = 20 runs one joint search, spec 6.2),
# which is the only baseline t = 300 has.
#
# Output file: one CSV with a `record` column.
# - record == "summary": one row per shape x coding x swap set x S, with the
#   seed counts, full-rank counts, candidate and baseline means, the rule
#   outcomes and the mean work counters.
# - record == "work": the S = 1 work counters of every (shape, coding, seed,
#   swap set), which the work fit of rule 4 is made from (counters are sums
#   over the S searches, so only S = 1 is one search).
arguments <- commandArgs(trailingOnly = TRUE)
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
argument <- function(i, default) if (length(arguments) >= i) arguments[i] else default
proto_dir <- path.expand(argument(1, "~/fieldhub-libs/runs/phase2/proto"))
baseline_path <- argument(2, file.path(script_dir, "nested-goldens-os-baseline.csv.gz"))
out_path <- argument(3, file.path(script_dir, "onestage-stage-measurements.csv.gz"))
source(file.path(script_dir, "..", "tests", "testthat", "helper-nested-goldens.R"))
options(width = 200, stringsAsFactors = FALSE)

efficiencies <- c("row_D", "row_A", "col_D", "col_A", "joint_D", "joint_A")
rules <- c("worst_ok", "constant_ok", "optimum_ok", "errors_ok", "fallback_ok")
swap_sets <- c("floor", "row_preserving")
search_counts <- c(1L, 2L, 5L, 10L)
tol <- nested_golden_tolerance
section <- function(title) cat("\n", strrep("=", 78), "\n", title, "\n", strrep("=", 78), "\n", sep = "")
show <- function(x, ...) print(x, row.names = FALSE, ...)

# ---- inputs ------------------------------------------------------------------
files <- sort(Sys.glob(file.path(proto_dir, "proto-*.csv")))
if (length(files) == 0L) stop("No proto-*.csv under ", proto_dir)
proto <- do.call(rbind, lapply(files, utils::read.csv))
proto <- proto[order(proto$coding, proto$t, proto$k, proto$r, proto$seed, proto$swap_set, proto$S), ]
rownames(proto) <- NULL
unit <- paste(proto$coding, proto$t, proto$k, proto$r, proto$seed, proto$swap_set, proto$S)
if (anyDuplicated(unit)) stop("Duplicate prototype rows.")
shape_id <- paste(proto$coding, proto$t, proto$k, proto$r)
complete <- tapply(paste(proto$swap_set, proto$S), paste(shape_id, proto$seed), function(x) length(unique(x)))
if (any(complete != 8L)) stop("A prototype unit does not hold its 8 rows.")
baseline <- utils::read.csv(gzfile(baseline_path))

section("Inputs")
cat("prototype:", length(files), "shards,", nrow(proto), "rows,", length(unique(shape_id)), "shape x coding,",
    length(unique(paste(shape_id, proto$seed))), "units\n")
seeds_by_t <- unique(data.frame(t = proto$t, id = shape_id, seed = proto$seed))
seeds_by_t <- tapply(seeds_by_t$seed, seeds_by_t$id, length)
cat("seeds per shape x coding:", paste(names(table(seeds_by_t)), "seeds on", table(seeds_by_t), collapse = "; "), "\n")
cat("baseline:", nrow(baseline), "seed rows;", sum(baseline$iterations == 200), "at iterations 200,",
    sum(baseline$iterations == 20), "at iterations 20\n")

# ---- acceptance of every swap set and S against a baseline ------------------
# Prototype rows in the form nested_summary() reads. A unit with no full-rank
# search is what the call would return as a fallback (onestage_infeasible).
# The prototype's own bound and clash counts do not exist: NA.
as_records <- function(x, iterations) {
  data.frame(caller = paste0("onestage_", x$coding), t = x$t, k = x$k, r = x$r, seed = x$seed,
             D = x$col_D, A = x$col_A, bound = NA_real_,
             row_D = x$row_D, row_A = x$row_A, col_D = x$col_D, col_A = x$col_A,
             joint_D = x$joint_D, joint_A = x$joint_A,
             seconds = x$seconds_stage1 + x$seconds_repair + x$seconds_joint,
             status = ifelse(x$full_rank, "ok", "fallback"), message = "", iterations = iterations,
             row_bound = NA_real_, col_bound = NA_real_, row_clash = NA_real_, col_clash = NA_real_,
             optimiser_seconds = NA_real_)
}
judge <- function(iterations, counts) {
  base <- nested_summary(baseline[baseline$iterations == iterations, ])
  out <- list()
  for (set in swap_sets) for (S in counts) {
    x <- proto[proto$swap_set == set & proto$S == S, ]
    cand <- nested_summary(as_records(x, iterations))
    j <- match(nested_shape_key(cand), nested_shape_key(base))
    row <- data.frame(coding = sub("onestage_", "", cand$caller), t = cand$t, k = cand$k, r = cand$r,
                      swap_set = set, S = S, seeds = cand$seeds + cand$fallbacks,
                      full_rank_seeds = cand$seeds, baseline_seeds = base$seeds[j],
                      baseline_fallbacks = base$fallbacks[j])
    has <- !is.na(j) & !is.na(row$baseline_seeds) & row$baseline_seeds > 0L
    accepted <- nested_acceptance(base[j[!is.na(j)], ], cand, bootstrap = FALSE)
    a <- match(nested_shape_key(cand), nested_shape_key(base[j[!is.na(j)], ]))
    for (rule in rules) row[[rule]] <- accepted[[rule]][a]
    # with no baseline seed that ran onestage only the count rules say anything
    for (rule in c("worst_ok", "constant_ok", "optimum_ok")) row[[rule]][!has] <- NA
    ni <- rep(TRUE, nrow(row))
    clear <- rep(FALSE, nrow(row))
    for (e in efficiencies) {
      cm <- cand[[paste0(e, "_mean")]]
      bm <- base[[paste0(e, "_mean")]][j]
      bsd <- base[[paste0(e, "_sd")]][j]
      csd <- cand[[paste0(e, "_sd")]]
      margin <- pmax(2e-6, 0.5 * bsd)
      se <- sqrt(ifelse(is.na(csd), 0, csd)^2 / pmax(cand$seeds, 1L) + bsd^2 / pmax(base$seeds[j], 1L))
      row[[paste0(e, "_mean")]] <- cm
      row[[paste0(e, "_min")]] <- cand[[paste0(e, "_min")]]
      row[[paste0("base_", e, "_mean")]] <- bm
      row[[paste0("base_", e, "_sd")]] <- bsd
      row[[paste0("base_", e, "_min")]] <- base[[paste0(e, "_min")]][j]
      ok <- cm - bm >= -margin
      ni <- ni & ok
      clear <- clear | (!ok & cm - bm + stats::qnorm(0.95) * se < -margin)
    }
    row$ni_ok <- ifelse(has & row$full_rank_seeds > 0L, ni, NA)
    row$ni_clear <- ifelse(has & row$full_rank_seeds > 0L, clear, NA)
    row$ni_fails <- vapply(seq_len(nrow(row)), function(i) {
      if (is.na(row$ni_ok[i]) || row$ni_ok[i]) return("")
      bad <- vapply(efficiencies, function(e) {
        row[[paste0(e, "_mean")]][i] - row[[paste0("base_", e, "_mean")]][i] <
          -max(2e-6, 0.5 * row[[paste0("base_", e, "_sd")]][i])
      }, logical(1))
      paste(efficiencies[bad], collapse = " ")
    }, character(1))
    out[[length(out) + 1L]] <- row
  }
  do.call(rbind, out)
}
judged <- judge(200L, search_counts)
judged$rule_holds <- mapply(onestage_rule_holds, judged$t, judged$k, judged$r, judged$coding == "latinized")
judged$compared <- !is.na(judged$ni_ok)
all_rules <- c(rules, "ni_ok")
fails <- function(x, rule) sum(!x[[rule]], na.rm = TRUE)
shape_label <- function(x) paste0(x$t, "/", x$k, "/", x$r)

section("Shapes and what can be compared (iterations = 200 baseline)")
one <- judged[judged$swap_set == "floor" & judged$S == 10L, ]
cat("shape x coding measured:", nrow(one), "\n")
cat("  5.1b fails (no joint stage; the call falls back):", sum(!one$rule_holds), "\n")
cat("  5.1b holds:", sum(one$rule_holds), "\n")
cat("    with an iterations = 200 baseline seed that ran onestage (efficiency rules apply):", sum(one$compared), "\n")
cat("    t = 300, no iterations = 200 baseline:", sum(one$rule_holds & one$t == 300), "\n")
cat("    5.1b holds but the baseline fell back on every seed:",
    sum(one$rule_holds & one$t < 300 & !one$compared), "\n")
odd <- one[one$rule_holds & one$t < 300 & !one$compared, ]
if (nrow(odd) > 0L) cat("     ", paste(odd$coding, shape_label(odd), collapse = "; "), "\n")

# ---- rule 3: repair ----------------------------------------------------------
section("Rule 3. Repair: full rank after repair on every seed where 5.1b holds")
holds <- proto[proto$rule_holds, ]
repair <- do.call(rbind, lapply(split(holds, list(holds$coding, holds$swap_set), drop = TRUE), function(x) {
  s10 <- x[x$S == 10L, ]
  s1 <- x[x$S == 1L, ]
  data.frame(coding = x$coding[1], swap_set = x$swap_set[1], units = nrow(s10),
             searches = 10L * nrow(s10),
             searches_full_rank_at_start = sum(s10$searches_full_rank_start),
             searches_full_rank_after_repair = sum(s10$searches_full_rank_after_repair),
             searches_full_rank_final = sum(s10$searches_full_rank_final),
             units_search1_singular = sum(!s1$full_rank_start),
             units_search1_repaired = sum(!s1$full_rank_start & s1$full_rank_after_repair),
             units_returned_full_rank_S1 = sum(s1$full_rank),
             units_returned_full_rank_S10 = sum(s10$full_rank))
}))
show(repair)
not_holds <- proto[!proto$rule_holds, ]
cat("where 5.1b fails:", sum(not_holds$S == 10L & not_holds$swap_set == "floor"), "units x 2 swap sets;",
    "searches that end full rank:", sum(not_holds$searches_full_rank_final[not_holds$S == 10L]), "\n")
rule3_ok <- all(holds$full_rank) && all(holds$searches_full_rank_after_repair == holds$S) &&
  all(holds$searches_full_rank_final == holds$S)
cat("RULE 3:", if (rule3_ok) "PASS - every search of every seed is full rank after repair wherever 5.1b holds"
    else "FAIL - spec-level failure", "\n")

# ---- rule 1: swap set --------------------------------------------------------
section("Rule 1. Swap set: floor against row_preserving")
cat("Shapes x codings failing each rule, of", sum(one$compared), "compared (ni_ok: point estimate, see the header)\n")
counts <- do.call(rbind, lapply(split(judged[judged$compared, ], list(judged$swap_set[judged$compared],
                                                                      judged$S[judged$compared])), function(x) {
  row <- data.frame(swap_set = x$swap_set[1], S = x$S[1])
  for (rule in all_rules) row[[sub("_ok", "_fail", rule)]] <- fails(x, rule)
  row$ni_clear_fail <- sum(x$ni_clear)
  row$any_rule_fail <- sum(!Reduce(`&`, x[rules]))
  row$any_fail <- sum(!Reduce(`&`, x[all_rules]))
  row
}))
show(counts[order(counts$S, counts$swap_set), ])

# Joint D of the two sets on every measured shape. The margin is 0.5 x the
# baseline SD of joint D at iterations = 200; t = 300 has only the iterations =
# 20 baseline, whose SD is used there. A constant baseline gives a margin of 0;
# the golden tolerance 1.5e-7 is allowed for arithmetic noise.
base20 <- nested_summary(baseline[baseline$iterations == 20, ])
sd20 <- base20$joint_D_sd[match(paste0("onestage_", judged$coding, "|", judged$t, "|", judged$k, "|", judged$r),
                                nested_shape_key(base20))]
judged$joint_D_sd_used <- ifelse(is.na(judged$base_joint_D_sd), sd20, judged$base_joint_D_sd)
pair <- merge(judged[judged$swap_set == "floor", ], judged[judged$swap_set == "row_preserving", ],
              by = c("coding", "t", "k", "r", "S"), suffixes = c(".floor", ".rp"))
pair <- pair[pair$full_rank_seeds.floor > 0L, ]
pair$margin <- pmax(0.5 * pair$joint_D_sd_used.floor, tol)
pair$joint_D_gap <- pair$joint_D_mean.rp - pair$joint_D_mean.floor
pair$within <- abs(pair$joint_D_gap) <= pair$margin
pair$not_below <- pair$joint_D_gap >= -pair$margin
pair$keeps_rules <- Reduce(`&`, lapply(rules, function(rule) {
  f <- pair[[paste0(rule, ".floor")]]
  is.na(f) | !f | pair[[paste0(rule, ".rp")]]
}))
pair$keeps_ni <- is.na(pair$ni_ok.floor) | !pair$ni_ok.floor | pair$ni_ok.rp
cat("\nrow_preserving against floor, per S (shapes x codings with a full-rank design:",
    sum(pair$S == 10L), "; margin known on", sum(pair$S == 10L & !is.na(pair$margin)), ")\n")
rule1 <- do.call(rbind, lapply(split(pair, pair$S), function(x) {
  data.frame(S = x$S[1],
             loses_a_rule_floor_passes = sum(!x$keeps_rules),
             loses_ni_floor_passes = sum(!x$keeps_ni),
             joint_D_below_floor_by_more_than_margin = sum(!x$not_below, na.rm = TRUE),
             joint_D_outside_margin_either_way = sum(!x$within, na.rm = TRUE),
             mean_gap = mean(x$joint_D_gap),
             median_gap_in_baseline_SD = stats::median((x$joint_D_gap / x$joint_D_sd_used.floor)[
               !is.na(x$joint_D_sd_used.floor) & x$joint_D_sd_used.floor > 1e-9]))
}))
show(rule1, digits = 3)
p10 <- pair[pair$S == 10L, ]
lost <- p10[!p10$keeps_rules, ]
cat("\nS = 10: shapes where row_preserving fails a rule the floor set passes:\n")
if (nrow(lost) > 0L) {
  lost$rules_lost <- vapply(seq_len(nrow(lost)), function(i) {
    paste(sub("_ok", "", rules[vapply(rules, function(rule) {
      isTRUE(lost[[paste0(rule, ".floor")]][i]) && !lost[[paste0(rule, ".rp")]][i]
    }, logical(1))]), collapse = " ")
  }, character(1))
  show(data.frame(coding = lost$coding, shape = shape_label(lost), rules_lost = lost$rules_lost,
                  joint_D_floor = lost$joint_D_mean.floor, joint_D_rp = lost$joint_D_mean.rp,
                  baseline_joint_D = lost$base_joint_D_mean.floor, baseline_SD = lost$base_joint_D_sd.floor),
       digits = 7)
}
below <- p10[!is.na(p10$not_below) & !p10$not_below, ]
cat("\nS = 10: shapes where row_preserving mean joint D is below the floor set's by more than the margin:",
    nrow(below), "\n")
if (nrow(below) > 0L) {
  below <- below[order(below$joint_D_gap / below$margin), ]
  show(utils::head(data.frame(coding = below$coding, shape = shape_label(below), seeds = below$seeds.floor,
                              joint_D_floor = below$joint_D_mean.floor, joint_D_rp = below$joint_D_mean.rp,
                              gap = below$joint_D_gap, margin = below$margin), 15L), digits = 6)
  cat("(the 15 largest in units of the margin)\n")
}
rule1_adopt <- all(p10$keeps_rules) && all(p10$keeps_ni) && all(p10$within, na.rm = TRUE)
cat("RULE 1:", if (rule1_adopt) "row_preserving meets both conditions on every shape: ADOPT (changes the spec's rule)"
    else "row_preserving does not meet the conditions on every shape: KEEP THE FLOORS (spec unchanged)", "\n")

# ---- rule 2: search count ----------------------------------------------------
section("Rule 2. Search count at iterations = 200 (floor set)")
floor_rows <- judged[judged$swap_set == "floor" & judged$compared, ]
floor_rows$joint_D_z <- (floor_rows$joint_D_mean - floor_rows$base_joint_D_mean) / floor_rows$base_joint_D_sd
floor_rows$size <- cut(floor_rows$t, c(0, 16, 30, 48, 100, 200), labels = c("t<=16", "20-30", "36-48", "50-100", "200"))
cat("Mean joint D minus the baseline's, in baseline SDs (shapes whose baseline SD > 1e-9); the margin is -0.5\n")
trend <- do.call(rbind, lapply(split(floor_rows, list(floor_rows$S, floor_rows$size, floor_rows$coding), drop = TRUE),
                               function(x) {
  z <- x$joint_D_z[x$base_joint_D_sd > 1e-9]
  data.frame(coding = x$coding[1], size = x$size[1], S = x$S[1], shapes = nrow(x), varying = length(z),
             median = stats::median(z), lowest = min(z), below_margin = sum(z < -0.5),
             ni_fail = sum(!x$ni_ok), ni_clear_fail = sum(x$ni_clear),
             rule_fail = sum(!Reduce(`&`, x[rules])))
}))
show(trend[order(trend$coding, trend$size, trend$S), ], digits = 3)

s10 <- floor_rows[floor_rows$S == 10L, ]
hard <- s10[!Reduce(`&`, s10[rules]), ]
cat("\nS = 10, floor: shapes failing a rule of nested_acceptance(bootstrap = FALSE):", nrow(hard), "\n")
if (nrow(hard) > 0L) {
  show(data.frame(coding = hard$coding, shape = shape_label(hard), seeds = hard$seeds,
                  worst = hard$worst_ok, constant = hard$constant_ok, optimum = hard$optimum_ok,
                  row_A_min = hard$row_A_min, base_row_A_min = hard$base_row_A_min,
                  col_A_min = hard$col_A_min, base_col_A_min = hard$base_col_A_min,
                  joint_D_min = hard$joint_D_min, base_joint_D_min = hard$base_joint_D_min), digits = 7)
}
soft <- s10[!s10$ni_ok, ]
cat("\nS = 10, floor: shapes failing non-inferiority on the mean (point estimate):", nrow(soft),
    "; clear of seed noise:", sum(soft$ni_clear), "\n")
if (nrow(soft) > 0L) {
  in_sd <- function(e) round((soft[[paste0(e, "_mean")]] - soft[[paste0("base_", e, "_mean")]]) /
                               pmax(soft[[paste0("base_", e, "_sd")]], 4e-6), 2)
  show(data.frame(coding = soft$coding, shape = shape_label(soft), seeds = soft$seeds, clear = soft$ni_clear,
                  row_A = in_sd("row_A"), col_A = in_sd("col_A"), joint_D = in_sd("joint_D"),
                  joint_A = in_sd("joint_A"), fails = soft$ni_fails))
  cat("(row_A, col_A, joint_D, joint_A: candidate mean - baseline mean in baseline SDs, the SD floored at 4e-6\n",
      " so that the margin max(2e-6, 0.5 SD) is -0.5 in every column)\n")
}
needs_larger <- s10[!s10$ni_ok | !Reduce(`&`, s10[rules]), ]
cat("\nShapes that need a larger S before rule 2 can be decided:", nrow(needs_larger), "\n")
for (coding in unique(needs_larger$coding)) {
  x <- needs_larger[needs_larger$coding == coding, ]
  cat(" ", coding, ":", paste(shape_label(x), collapse = " "), "\n")
}
cat("  t values involved:", paste(sort(unique(needs_larger$t)), collapse = ","), "\n")
rule2_ok <- nrow(needs_larger) == 0L
cat("RULE 2:", if (rule2_ok) "S = 10 passes on every shape: KEEP 10 (spec unchanged)"
    else "S = 10 fails on the shapes above: OPEN until a larger S is run on them (controller job)", "\n")

# ---- S = 1 against the iterations = 20 baseline (supplement) -----------------
section("Supplement. S = 1 against the iterations = 20 baseline (one joint search; the only baseline of t = 300)")
judged20 <- judge(20L, 1L)
judged20$compared <- !is.na(judged20$ni_ok)
supplement <- do.call(rbind, lapply(split(judged20[judged20$compared, ], judged20$swap_set[judged20$compared]),
                                    function(x) {
  row <- data.frame(swap_set = x$swap_set[1], compared = nrow(x))
  for (rule in all_rules) row[[sub("_ok", "_fail", rule)]] <- fails(x, rule)
  row$ni_clear_fail <- sum(x$ni_clear)
  row
}))
show(supplement)
t300 <- judged20[judged20$t == 300 & judged20$swap_set == "floor", ]
in_sd20 <- function(e) round((t300[[paste0(e, "_mean")]] - t300[[paste0("base_", e, "_mean")]]) /
                               pmax(t300[[paste0("base_", e, "_sd")]], 4e-6), 2)
cat("\nt = 300, floor, S = 1, 2 seeds (differences in baseline SDs):\n")
show(data.frame(coding = t300$coding, shape = shape_label(t300), worst = t300$worst_ok,
                constant = t300$constant_ok, optimum = t300$optimum_ok, ni = t300$ni_ok,
                row_A = in_sd20("row_A"), col_A = in_sd20("col_A"), joint_D = in_sd20("joint_D"),
                joint_A = in_sd20("joint_A")))

# ---- rule 4: work ------------------------------------------------------------
section("Rule 4. Work of one search (S = 1 rows, floor set, shapes where 5.1b holds)")
work <- proto[proto$S == 1L, ]
work$n <- work$t * work$r
work$nrows <- work$k
counters <- c(outer(c("accepted", "passes", "evaluations"), c("stage1_rows", "stage1_cols", "repair", "joint"),
                    function(what, stage) paste(stage, what, sep = "_")))
cat("S = 1 runs:", nrow(work), "; passes at the prototype's cap of 60 cycles per descent:",
    sum(work[grep("_passes$", counters, value = TRUE)] >= 60, na.rm = TRUE), "\n")
cat("Model: E[count] = exp(b0) x r^b1 x nrows^b2 x ncols^b3 (quasi-Poisson, log link, one row per run).\n",
    "n = r x nrows x ncols, so a separate exponent for n is not identified: it is b1, b2 and b3 together.\n",
    "Fitted on t <= 200; t = 300 (3 shapes per coding, 2 seeds) is held out.\n",
    "R2 and the relative errors are on shape means (t <= 200): R2 = 1 - SSE / SST on the count scale;\n",
    "max_rel_error = largest |predicted / observed - 1| over shapes with an observed mean >= 10 accepted swaps\n",
    "(>= 2 passes); max_run_ratio = largest observed / predicted over single runs (the factor an admission\n",
    "model needs on top of the mean); t300_ratio = observed / predicted mean at 300/10/r, r = 2, 3, 4.\n", sep = "")
fit_one <- function(data, counter, floor_mean) {
  data$y <- data[[counter]]
  train <- data[data$t <= 200, ]
  fit <- stats::glm(y ~ log(r) + log(nrows) + log(ncols), family = stats::quasipoisson(), data = train)
  data$predicted <- stats::predict(fit, data, type = "response")
  means <- stats::aggregate(cbind(y, predicted) ~ t + nrows + ncols + r, data, mean)
  inside <- means[means$t <= 200, ]
  held <- means[means$t == 300, ]
  held <- held[order(held$r), ]
  big <- inside$y >= floor_mean
  data.frame(counter = counter, runs = nrow(train), shapes = nrow(inside),
             b0 = unname(stats::coef(fit)[1]), b_r = unname(stats::coef(fit)[2]),
             b_nrows = unname(stats::coef(fit)[3]), b_ncols = unname(stats::coef(fit)[4]),
             R2 = 1 - sum((inside$y - inside$predicted)^2) / sum((inside$y - mean(inside$y))^2),
             max_rel_error = max(abs(inside$predicted[big] / inside$y[big] - 1)),
             max_run_ratio = max(train$y / stats::predict(fit, train, type = "response")),
             t300_ratio = paste(sprintf("%.2f", held$y / held$predicted), collapse = " "))
}
fit_rows <- work[work$swap_set == "floor" & work$rule_holds, ]
fits <- list()
for (coding in c("nested", "latinized")) {
  x <- fit_rows[fit_rows$coding == coding, ]
  stages <- if (coding == "latinized") c("stage1_rows", "stage1_cols", "joint") else "joint"
  for (stage in stages) for (what in c("accepted", "passes")) {
    fits[[length(fits) + 1L]] <- cbind(coding = coding, fit_one(x, paste(stage, what, sep = "_"),
                                                               if (what == "accepted") 10 else 2))
  }
}
fits <- do.call(rbind, fits)
show(fits, digits = 3)
cat("Stage 1 of the nested coding is the installed twostage; it does not expose its work (NA in every row).\n")

cat("\nPlain envelopes over single runs (5.1b holds, floor set, all t):\n")
envelope <- do.call(rbind, lapply(split(fit_rows, fit_rows$coding), function(x) {
  data.frame(coding = x$coding[1], runs = nrow(x),
             joint_accepted_per_plot_max = max(x$joint_accepted / x$n),
             joint_passes_max = max(x$joint_passes),
             stage1_rows_accepted_per_plot_max = max(x$stage1_rows_accepted / x$n),
             stage1_rows_passes_max = max(x$stage1_rows_passes),
             stage1_cols_accepted_per_plot_max = max(x$stage1_cols_accepted / x$n),
             stage1_cols_passes_max = max(x$stage1_cols_passes))
}))
show(envelope, digits = 3)

cat("\nRepair (search 1, 5.1b holds): too rare for a regression; the envelope is the model.\n")
repair_work <- do.call(rbind, lapply(split(work[work$rule_holds, ], list(work$coding[work$rule_holds],
                                                                         work$swap_set[work$rule_holds])), function(x) {
  ran <- x[!x$full_rank_start, ]
  data.frame(coding = x$coding[1], swap_set = x$swap_set[1], runs = nrow(x), repairs = nrow(ran),
             largest_t_repaired = if (nrow(ran) > 0L) max(ran$t) else NA,
             accepted_max = max(x$repair_accepted), accepted_per_plot_max = max(x$repair_accepted / x$n),
             passes_max = max(x$repair_passes))
}))
show(repair_work, digits = 3)
cat("Where 5.1b fails the repair runs to a local optimum without success (not part of an admitted build):\n")
fail_rows <- work[!work$rule_holds, ]
cat("  runs", nrow(fail_rows), "; accepted max", max(fail_rows$repair_accepted), "; accepted per plot max",
    format(max(fail_rows$repair_accepted / fail_rows$n), digits = 3), "; passes max", max(fail_rows$repair_passes), "\n")

# ---- the measurements file ---------------------------------------------------
mean_work <- stats::aggregate(proto[counters], proto[c("coding", "t", "k", "r", "swap_set", "S")],
                              function(v) if (all(is.na(v))) NA_real_ else mean(v))
names(mean_work)[match(counters, names(mean_work))] <- paste0(counters, "_mean")
rank_counts <- stats::aggregate(proto[c("searches_full_rank_start", "searches_full_rank_after_repair",
                                        "searches_full_rank_final")],
                                proto[c("coding", "t", "k", "r", "swap_set", "S")], sum)
stage1 <- stats::aggregate(proto[paste0("stage1_", efficiencies)], proto[c("coding", "t", "k", "r", "swap_set", "S")],
                           function(v) if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE))
names(stage1)[-(1:6)] <- paste0("stage1_", efficiencies, "_mean")
summary_rows <- judged[setdiff(names(judged), c("compared", "joint_D_sd_used"))]
by <- c("coding", "t", "k", "r", "swap_set", "S")
summary_rows <- merge(merge(merge(summary_rows, rank_counts, by = by), stage1, by = by), mean_work, by = by)
summary_rows$ncols <- summary_rows$t / summary_rows$k
summary_rows$record <- "summary"
work_rows <- work[c("coding", "t", "k", "ncols", "r", "seed", "swap_set", "S", "rule_holds", "full_rank_start",
                    "full_rank_after_repair", "repair_swaps", "full_rank", counters)]
work_rows$record <- "work"
columns <- union(c("record", by, "ncols", "seed"), union(names(summary_rows), names(work_rows)))
fill <- function(x) {
  for (column in setdiff(columns, names(x))) x[[column]] <- NA
  x[columns]
}
out <- rbind(fill(summary_rows), fill(work_rows))
out <- out[order(out$record, out$coding, out$t, out$k, out$r, out$swap_set, out$S, out$seed), ]
numeric <- vapply(out, is.double, logical(1))
out[numeric] <- lapply(out[numeric], signif, 10)
connection <- gzfile(out_path, "wb")
utils::write.csv(out, connection, row.names = FALSE, na = "")
close(connection)

section("Written")
cat(out_path, ":", sum(out$record == "summary"), "summary rows,", sum(out$record == "work"), "work rows,",
    length(columns), "columns\n")
cat("\nDecisions: rule 1", if (rule1_adopt) "ADOPT row_preserving" else "keep the floors",
    "| rule 2", if (rule2_ok) "keep 10" else "OPEN (larger S needed)",
    "| rule 3", if (rule3_ok) "pass" else "FAIL", "| rule 4 fit above\n")
