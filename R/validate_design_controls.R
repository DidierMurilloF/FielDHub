#' Read the sidebar controls of a design page
#'
#' @description
#' Every classic design page (\code{mod_design_ui()}/\code{mod_design_server()})
#' reads its sidebar through \code{read_design_controls()}: a named list of the
#' raw values Shiny delivers for the page's controls goes in, the parsed
#' values the design's argument builder reads come out, or a classed
#' \code{fieldhub_input_error} naming the control's label.
#'
#' Shiny's own encodings are handled the same way for every control:
#' \code{NULL} (an input that has not been rendered yet), logical \code{NA}
#' (a cleared \code{numericInput}) and \code{""} (a cleared
#' \code{textInput}) are blank. A blank required control is an error naming
#' its label; a blank seed means an automatic seed; a blank flag takes its
#' default.
#'
#' A control is a list built by the \code{ctl_*()} constructors
#' (R/app_controls.R) with at least \code{id}, \code{label} and \code{parse}
#' (\code{function(value, values)}, called with the raw value and the values
#' parsed so far). Optional fields: \code{generated_only} (skipped when the
#' entries come from an uploaded file), \code{enabled_by} (the id of a flag
#' control read earlier; while that flag is off the control is not read and
#' takes \code{disabled_value}), \code{depends_on} and \code{options} (a
#' select whose choices are computed from other controls, see
#' \code{design_control_choices()}). A control without \code{parse} (a
#' preview) has no value.
#'
#' @name design_controls
#' @noRd
NULL

#' A control's name for messages: its label without the trailing colon
#' @noRd
control_label_name <- function(label) {
  sub("[[:space:]]*:?[[:space:]]*$", "", label)
}

#' Whether a raw Shiny value is blank
#'
#' @description \code{NULL}, an empty vector, a scalar \code{NA} of any type
#' and an empty or whitespace-only string.
#' @noRd
is_blank_control_value <- function(value) {
  if (length(value) == 0L) return(TRUE)
  if (!is.atomic(value) || length(value) != 1L) return(FALSE)
  is.na(value) || (is.character(value) && !nzchar(trimws(value)))
}

#' Abort with a message naming a control
#' @noRd
control_abort <- function(label, ...) {
  fieldhub_abort(control_label_name(label), ..., data = list(control = label),
                 call = NULL)
}

#' Parse a whole-number count typed in a numericInput
#'
#' @description The engines validate the range (the control's minimum is only
#' a hint in the browser), so this only makes sure there is one whole number
#' to send.
#' @param value The raw value.
#' @param label The control's label.
#' @return The number, as Shiny delivered it.
#' @noRd
parse_control_number <- function(value, label) {
  if (is_blank_control_value(value)) control_abort(label, " cannot be blank.")
  if (!is.numeric(value) || length(value) != 1L || !is.null(dim(value)) ||
      !is.finite(value) || value != trunc(value)) {
    control_abort(label, " must be one whole number.")
  }
  value
}

#' Evaluate a parser, naming the control in any input error it raises
#'
#' @description The shared parsers (\code{parse_whole_numbers()},
#' \code{parse_rep_checks()}, ...) write messages that already name the
#' control; this adds the \code{control} field every control error carries.
#' @param label The control's label.
#' @param expr The parse.
#' @noRd
with_control_label <- function(label, expr) {
  tryCatch(expr, fieldhub_input_error = function(e) {
    fields <- unclass(e)[setdiff(names(e), c("message", "call"))]
    if (is.null(fields$control)) fields$control <- label
    fieldhub_abort(conditionMessage(e), data = fields, call = NULL,
                   class = setdiff(class(e), c("fieldhub_error", "error", "condition")))
  })
}

#' Parse a comma-separated list of whole numbers typed in a textInput
#' @inheritParams parse_control_number
#' @return A numeric vector.
#' @noRd
parse_control_whole_numbers <- function(value, label) {
  if (is_blank_control_value(value)) control_abort(label, " cannot be blank.")
  with_control_label(label, parse_whole_numbers(value, control_label_name(label)))
}

#' Parse a number of checks typed in a numericInput
#'
#' @description The same rule as \code{parse_n_checks()}: one whole number
#' from 1 up.
#' @inheritParams parse_control_number
#' @return The count, as an integer.
#' @noRd
parse_control_checks <- function(value, label) {
  count <- parse_control_number(value, label)
  if (count < 1 || count > .Machine$integer.max) {
    control_abort(label, " must be a whole number from 1 to ", .Machine$integer.max, ".")
  }
  as.integer(count)
}

#' Parse the number of replicates of each p-rep group
#'
#' @description One whole number per group of \code{"# of Entries Per Rep
#' Group"} (see \code{parse_rep_groups()}).
#' @inheritParams parse_control_number
#' @param groups The parsed entries per group.
#' @return A numeric vector as long as \code{groups}.
#' @noRd
parse_control_rep_units <- function(value, label, groups) {
  units <- parse_control_whole_numbers(value, label)
  if (length(units) != length(groups)) {
    control_abort(label, " must have one value per group of entries (", length(groups),
                  "); got ", length(units), ".")
  }
  units
}

#' Parse the per-factor level counts of a generated factorial design
#' @inheritParams parse_control_number
#' @return A numeric vector of at least two counts.
#' @noRd
parse_control_factor_counts <- function(value, label) {
  counts <- parse_control_whole_numbers(value, label)
  if (length(counts) < 2L) control_abort(label, ": more than one factor needs to be specified.")
  counts
}

#' Parse comma-separated names typed in a textInput
#'
#' @description Split on commas exactly as the design pages always have
#' (names are not trimmed, so a name keeps the spaces typed in it).
#' @inheritParams parse_control_number
#' @param split Whether the text is a comma-separated list; the
#'   optimized-arrangement and p-rep pages take the whole text as one name.
#' @param trim Whether to drop the spaces around each name (the multiple
#'   diagonal page always has).
#' @return A character vector.
#' @noRd
parse_control_names <- function(value, label, split = TRUE, trim = FALSE) {
  if (is_blank_control_value(value)) control_abort(label, " cannot be blank.")
  if (!is.character(value) || length(value) != 1L) {
    control_abort(label, " must be one comma-separated text value.")
  }
  names <- if (split) as.vector(unlist(strsplit(value, ",", fixed = TRUE))) else value
  if (trim) trimws(names) else names
}

#' Parse the value of a selectInput
#' @inheritParams parse_control_number
#' @param choices The values the select offers.
#' @return The chosen value (a string).
#' @noRd
parse_control_choice <- function(value, label, choices) {
  if (is_blank_control_value(value)) control_abort(label, " cannot be blank.")
  if (!is.character(value) || length(value) != 1L || !value %in% as.character(choices)) {
    control_abort(label, " must be one of: ", paste(as.character(choices), collapse = ", "), ".")
  }
  value
}

#' Parse a select offering computed whole-number options (block sizes, rows)
#'
#' @description \code{block_size_choices()} offers "No Options Available"
#' when no option fits the entries; running then reports it.
#' @inheritParams parse_control_number
#' @return The chosen number.
#' @noRd
parse_control_option <- function(value, label) {
  if (is_blank_control_value(value)) control_abort(label, " cannot be blank.")
  if (identical(value, no_block_size_option())) {
    fieldhub_abort("No options for this combination of treatments!",
                   data = list(control = label), call = NULL)
  }
  number <- suppressWarnings(as.numeric(value))
  if (length(number) != 1L || !is.finite(number) || number != trunc(number)) {
    control_abort(label, " must be one whole number.")
  }
  number
}

#' Parse a checkboxInput
#' @inheritParams parse_control_number
#' @param default The value of a blank (not yet rendered) checkbox.
#' @return \code{TRUE} or \code{FALSE}.
#' @noRd
parse_control_flag <- function(value, label, default) {
  if (is_blank_control_value(value)) return(default)
  if (!is.logical(value) || length(value) != 1L) control_abort(label, " must be TRUE or FALSE.")
  value
}

#' Parse the optional random seed
#'
#' @description Blank (\code{NULL}, \code{NA}, \code{""}) means an automatic
#' seed and reads as \code{NULL}; the app then draws one from its private
#' stream (\code{app_design_seed()}), which records it in the result.
#' @inheritParams parse_control_number
#' @return The seed, or \code{NULL}.
#' @noRd
parse_control_seed <- function(value, label) {
  tryCatch(read_app_seed(value), fieldhub_input_error = function(e) {
    control_abort(label, " must be one number, or blank for an automatic seed.")
  })
}

#' Parse the raw values of a design page's controls
#'
#' @param spec A design page spec (\code{design_app_spec()}); only its
#'   \code{controls} are read.
#' @param raw Named list of raw input values, by control id. A missing
#'   entry reads as \code{NULL}.
#' @param uploaded Whether the entries come from an uploaded file: controls
#'   marked \code{generated_only} are then not read.
#' @param only Optional control ids to read (the others are skipped).
#' @return Named list of parsed values, in control order.
#' @noRd
read_design_controls <- function(spec, raw, uploaded = FALSE, only = NULL) {
  if (!is.list(raw) || (length(raw) > 0L && is.null(names(raw)))) {
    fieldhub_abort("Control values must be a named list.", class = "fieldhub_internal_error")
  }
  validate_flag(uploaded, "uploaded")
  values <- list()
  for (control in spec$controls) {
    if (is.null(control$parse)) next
    if (!is.null(only) && !control$id %in% only) next
    if (uploaded && isTRUE(control$generated_only)) next
    if (!is.null(control$enabled_by) && !isTRUE(values[[control$enabled_by]])) {
      values[control$id] <- list(control$disabled_value)
      next
    }
    values[control$id] <- list(control$parse(raw[[control$id]], values))
  }
  values
}

#' Choices of a select computed from other controls or an uploaded file
#'
#' @param spec A design page spec.
#' @param control The select control (with \code{depends_on} and
#'   \code{options}).
#' @param raw Named list of raw input values.
#' @param data The shaped upload, or \code{NULL} on the generated path. With
#'   an upload, the controls read only for generated entries are not read.
#' @return \code{list(choices = , selected = )}, or a classed error when the
#'   controls it depends on cannot be read.
#' @noRd
design_control_choices <- function(spec, control, raw, data = NULL) {
  values <- read_design_controls(spec, raw, uploaded = !is.null(data), only = control$depends_on)
  control$options(values, data)
}

#' Choices of a step select, from the values of the run it follows
#'
#' @description A step is a select (or flag) a spatial design page offers
#' after Run! (field dimensions, per-location dimensions) or after
#' Randomize! (percentage of checks); its choices are computed from the
#' values of that run.
#' @param step The step control (with \code{options}).
#' @param values The values of the run (\code{spec$values}, plus the
#'   allocation of the run and the steps read before).
#' @param data The shaped upload, or \code{NULL}.
#' @return \code{list(choices = , selected = )}, per location for a
#'   per-location select, or a classed error explaining why nothing fits.
#' @noRd
design_step_choices <- function(step, values, data = NULL) {
  step$options(values, data)
}

#' Whether a step holds one of its current choices
#'
#' @description A select keeps its previous value until its new choices
#' reach the browser; a step is only read once it holds one of them.
#' Numbers are compared as numbers (the percentage of checks arrives as
#' text).
#' @param value The raw value (a list of values for a per-location select).
#' @param choices The current choices (a list for a per-location select).
#' @return \code{TRUE} or \code{FALSE}.
#' @noRd
step_choice_ready <- function(value, choices) {
  if (is.list(choices)) {
    return(is.list(value) && length(value) == length(choices) &&
             all(vapply(seq_along(choices), function(i) {
               step_choice_ready(value[[i]], choices[[i]])
             }, logical(1))))
  }
  if (is_blank_control_value(value) || !is.atomic(value) || length(value) != 1L) return(FALSE)
  options <- unname(choices)
  if (is.numeric(options)) {
    number <- suppressWarnings(as.numeric(value))
    return(is.finite(number) && any(abs(options - number) < 1e-6))
  }
  as.character(value) %in% as.character(options)
}

#' Read the steps of one stage of a spatial design page
#'
#' @description Each step's \code{parse(value, values)} returns the values
#' it sets for the argument builder (a named list, such as
#' \code{list(nrows = , ncols = )}), or one value stored under the step's
#' id. A step \code{enabled_by} a flag that is off is skipped.
#' @param steps The step controls of the stage.
#' @param raw Named list of raw step values, by step id.
#' @param values The values of the run.
#' @param choices Named list of the current choices of each step with
#'   options.
#' @return \code{values} with the steps read, or \code{NULL} while a step
#'   does not hold one of its current choices yet.
#' @noRd
read_design_steps <- function(steps, raw, values, choices = list()) {
  for (step in steps) {
    if (!is.null(step$enabled_by) && !isTRUE(values[[step$enabled_by]])) next
    value <- raw[[step$id]]
    if (!is.null(step$options)) check_step_choices(step, choices[[step$id]]$choices)
    if (!is.null(step$options) && !step_choice_ready(value, choices[[step$id]]$choices)) return(NULL)
    parsed <- step$parse(value, values)
    if (is.list(parsed)) values[names(parsed)] <- parsed else values[step$id] <- list(parsed)
  }
  values
}

#' Check that a step has something to choose from
#'
#' @description A select with no choices (a location no field size fits)
#' can never hold one, so reading it would wait for ever; this explains it
#' instead.
#' @param step The step control.
#' @param choices Its current choices (a list, one per location, for a
#'   per-location select).
#' @return \code{NULL}, or a classed input error naming the step (and the
#'   location).
#' @noRd
check_step_choices <- function(step, choices) {
  label <- step$label
  if (is.list(choices)) {
    empty <- which(lengths(choices) == 0L)
    if (length(empty) > 0L) {
      control_abort(paste(label, min(empty)), ": no field size fits this location. ",
                    "Allow filler plots or change the entries or checks.")
    }
  } else if (length(choices) == 0L) {
    control_abort(label, " has no option that fits.")
  }
  invisible(NULL)
}

#' The raw values of steps that start from their selected choice
#'
#' @description Each Randomize! starts its steps from the choice the page
#' selects, written as the select sends it back (text).
#' @param steps The step controls with options.
#' @param choices Named list of each step's \code{list(choices = , selected
#'   = )}.
#' @return Named list of raw values, by step id.
#' @noRd
selected_step_values <- function(steps, choices) {
  ids <- vapply(steps, `[[`, character(1), "id")
  stats::setNames(lapply(ids, function(id) as.character(choices[[id]]$selected)), ids)
}

#' Parse a field size offered as "rows x columns"
#' @inheritParams parse_control_number
#' @return \code{c(rows, columns)}.
#' @noRd
parse_control_dimensions <- function(value, label) {
  if (is_blank_control_value(value)) control_abort(label, " cannot be blank.")
  parts <- if (is.character(value) && length(value) == 1L) {
    suppressWarnings(as.numeric(unlist(strsplit(value, " x ", fixed = TRUE))))
  }
  if (length(parts) != 2L || any(!is.finite(parts)) || any(parts < 1) || any(parts != trunc(parts))) {
    control_abort(label, " must be a field size such as \"10 x 12\".")
  }
  parts
}

#' The raw values a design page starts with
#'
#' @description What Shiny delivers before the user changes anything: each
#' control's default, with a computed select on the choice it selects first
#' and a blank seed.
#' @param spec A design page spec.
#' @return Named list of raw values, by control id.
#' @noRd
design_control_defaults <- function(spec) {
  raw <- list()
  for (control in spec$controls) {
    if (is.null(control$parse)) next
    if (!is.null(control$options)) {
      raw[control$id] <- list(design_control_choices(spec, control, raw)$selected)
    } else {
      raw[control$id] <- list(control$value)
    }
  }
  raw
}

#' Parse the RCBD "Reps per Check" text input
#' @inheritParams parse_control_number
#' @param checks The parsed number of checks.
#' @return One replication count per check.
#' @noRd
parse_control_rep_checks <- function(value, label, checks) {
  if (is_blank_control_value(value)) control_abort(label, " cannot be blank.")
  with_control_label(label, parse_rep_checks(value, checks))
}
