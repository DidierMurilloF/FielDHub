#' Every module's own upload identifiers, error wording and dialog note
#'
#' @description Every design module has always let a user "Import entries'
#' list?" with the same toggle + file + separator controls, shown the same
#' family of format-example dialogs, and reported a bad upload the same way
#' (\code{app_upload_error()}, then \code{app_read_upload()} below). Only
#' the input ids (kept as each module has always had them, so server code
#' elsewhere in the module, and later tasks, keep working), the design's own
#' missing-columns wording and its dialog note differ.
#'
#' \code{design} is the module's own upload key. Most match the design key
#' \code{upload_validation_rule()}/\code{read_design_upload()} use directly
#' (\code{validation_design}); \code{"multi_loc_prep"} and
#' \code{"sparse_allocation"} are modules that share another module's
#' upload validation rule (\code{"sdiag"}) but keep their own ids, wording
#' and example table (see \code{upload_format_example()}), so they get keys
#' of their own here.
#'
#' @param design One of the module upload keys.
#' @return A list with \code{toggle}, \code{file}, \code{sep} (input ids,
#'   unnamespaced), \code{validation_design}, \code{missing_columns} and
#'   \code{note} (a string, or \code{NULL}), plus \code{note_tag} ("h4", the
#'   default, or "h5").
#' @noRd
app_upload_spec <- function(design) {
  controls_first_note <- "Note that the controls must be in the first rows of the CSV file."
  consecutive_entries_note <- "Entry numbers can be any set of consecutive positive numbers."
  entry_name_columns <- "Data input needs at least two columns: ENTRY and NAME"
  entry_name_reps_columns <- "Data input needs at least three columns with: ENTRY, NAME and REPS."

  spec <- switch(design,
    alpha = list(toggle = "owndata_alpha", file = "file.alpha", sep = "sep.alpha",
                 missing_columns = entry_name_columns, note = consecutive_entries_note),
    crd = list(toggle = "owndatacrd", file = "file.CRD", sep = "sep.crd",
               missing_columns = "Data input needs at least two columns: TREATMENT and REP.",
               note = "Note that only the TREATMENT column is required."),
    mdiag = list(toggle = "list_entries_multiple", file = "file_multiple", sep = "sep.DIAGONALS",
                 missing_columns = entry_name_columns, note = controls_first_note),
    sdiag = list(toggle = "owndataDIAGONALS", file = "file1", sep = "sep.DIAGONALS",
                 missing_columns = entry_name_columns, note = controls_first_note),
    factorial = list(toggle = "owndata", file = "file.FD", sep = "sep.fd",
                     missing_columns = "Data input needs at least two column: FACTOR and LEVEL",
                     note = NULL),
    ibd = list(toggle = "owndataibd", file = "file.IBD", sep = "sep.ibd",
               missing_columns = entry_name_columns, note = consecutive_entries_note),
    lsd = list(toggle = "owndataLSD", file = "file.LSD", sep = "sep.lsd",
               missing_columns = "Data input needs at least one column: ROW, COLUMN, and  TREATMENT",
               note = NULL),
    multi_loc_prep = list(toggle = "multi_prep_data", file = "file_multi_prep", sep = "sep_multi_prep",
                          validation_design = "sdiag", missing_columns = entry_name_reps_columns,
                          note = "Remark: If you want to include checks, please add them in the first rows of the file.",
                          note_tag = "h5"),
    optim = list(toggle = "owndataOPTIM", file = "file3", sep = "sep.OPTIM",
                 missing_columns = entry_name_reps_columns, note = controls_first_note),
    prep = list(toggle = "owndataPREPS", file = "file.preps", sep = "sep.preps",
                missing_columns = entry_name_reps_columns, note = NULL),
    arcbd = list(toggle = "owndata_a_rcbd", file = "file1_a_rcbd", sep = "sep.a_rcbd",
                 missing_columns = "Data input needs at least three columns with: ENTRY and NAME.",
                 note = controls_first_note),
    rcbd = list(toggle = "owndatarcbd", file = "file.RCBD", sep = "sep.rcbd",
                missing_columns = "Data input needs at least one column: TREATMENT",
                note = paste("Note that only the TREATMENT column is required. When repeated",
                             "checks are enabled, the first rows of the file are taken as the checks.")),
    rect = list(toggle = "owndata_rectangular", file = "file.rectangular", sep = "sep.rectangular",
                missing_columns = entry_name_columns, note = consecutive_entries_note),
    rcd = list(toggle = "owndataRCD", file = "file.RCD", sep = "sep.rcd",
               missing_columns = entry_name_columns, note = consecutive_entries_note),
    sparse_allocation = list(toggle = "input_sparse_data", file = "sparse_file", sep = "sparse_file_sep",
                             validation_design = "sdiag", missing_columns = entry_name_columns,
                             note = controls_first_note),
    spd = list(toggle = "owndataSPD", file = "file.SPD", sep = "sep.spd",
               missing_columns = "Data input needs at least two column: WHOLEPLOT and SUBPLOT",
               note = NULL),
    square = list(toggle = "owndata_square", file = "file.square", sep = "sep.square",
                  missing_columns = entry_name_columns, note = consecutive_entries_note),
    sspd = list(toggle = "owndataSSPD", file = "file.SSPD", sep = "sep.sspd",
                missing_columns = "Data input needs at least two column: WHOLEPLOT, SUBPLOT, and SUB_SUBPLOT",
                note = NULL),
    strip = list(toggle = "owndataSTRIP", file = "file.STRIP", sep = "sep.strip",
                 missing_columns = "Data input needs at least two column: Hplot and Vplot",
                 note = NULL),
    fieldhub_abort("Unknown upload design: ", design)
  )
  if (is.null(spec$validation_design)) spec$validation_design <- design
  if (is.null(spec$note_tag)) spec$note_tag <- "h4"
  spec
}

#' The shared "Import entries' list?" toggle, file input and separator
#'
#' @description Every module's own copy of this control, unified behind
#' canonical labels ("Import entries' list?", "Upload a CSV File:",
#' "Separator") and moved out of each module. Ids come from
#' \code{app_upload_spec()}, so each module keeps the input ids it has
#' always had.
#'
#' @param ns The module's namespace function.
#' @param design One of the module upload keys (see \code{app_upload_spec()}).
#' @return A \code{shiny::tagList()}.
#' @noRd
app_upload_ui <- function(ns, design) {
  spec <- app_upload_spec(design)
  shiny::tagList(
    shiny::radioButtons(
      inputId = ns(spec$toggle),
      label = "Import entries' list?",
      choices = c("Yes", "No"),
      selected = "No",
      inline = TRUE
    ),
    shiny::conditionalPanel(
      condition = paste0("input.", spec$toggle, " == 'Yes'"),
      ns = ns,
      shiny::fluidRow(
        shiny::column(
          7,
          shiny::fileInput(ns(spec$file), label = "Upload a CSV File:", multiple = FALSE)
        ),
        shiny::column(
          5,
          shiny::radioButtons(
            ns(spec$sep), "Separator",
            choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
            selected = ","
          )
        )
      )
    )
  )
}

#' The shared entries-format dialog
#'
#' @description Every module's own \code{entriesInfoModal_*()}, unified: the
#' same title/instructions, the design's own example table
#' (\code{upload_format_example()}) and its own note, if any.
#'
#' @param design One of the module upload keys (see \code{app_upload_spec()}).
#' @return A \code{shiny::modalDialog()}.
#' @noRd
app_upload_dialog <- function(design) {
  spec <- app_upload_spec(design)
  note_tag <- if (identical(spec$note_tag, "h5")) shiny::h5 else shiny::h4
  shiny::modalDialog(
    title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
    shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
    shiny::renderTable(upload_format_example(design), bordered = TRUE, align = 'c', striped = TRUE),
    if (!is.null(spec$note)) note_tag(spec$note),
    easyClose = FALSE
  )
}

#' Open the entries-format dialog when a module's toggle switches to "Yes"
#'
#' @description Every module's own \code{toListen}/\code{observeEvent()}
#' pair that opened its format dialog, unified into one observer.
#'
#' @param input The module's \code{input}.
#' @param design One of the module upload keys (see \code{app_upload_spec()}).
#' @noRd
app_upload_dialog_observer <- function(input, design) {
  spec <- app_upload_spec(design)
  toggled <- shiny::reactive(input[[spec$toggle]])
  shiny::observeEvent(toggled(), {
    if (identical(toggled(), "Yes")) shiny::showModal(app_upload_dialog(design))
  })
}

#' Read and validate the file behind a module's own upload controls
#'
#' @description Every module's own
#' \code{load_file()}/\code{names(data_ingested) == "dataUp"}/
#' \code{app_upload_error()} sequence, unified: reads the file at the ids
#' \code{app_upload_ui()} wired up for \code{design}
#' (\code{read_design_upload()}) and reports a failed upload through the
#' app's one problem path (\code{app_attempt()}/\code{app_report_problem()})
#' instead of returning a flag for the caller to branch on.
#'
#' @param input The module's \code{input}.
#' @param design One of the module upload keys (see \code{app_upload_spec()}).
#' @param check Whether to apply the design's column/uniqueness rule at all;
#'   \code{FALSE} for a module that lets the same entries repeat (passed
#'   straight through to \code{read_design_upload()}).
#' @return \code{list(data = <data.frame>)}, or \code{NULL} when the upload
#'   failed (already reported) or no file has been chosen yet.
#' @noRd
app_read_upload <- function(input, design, check = TRUE) {
  spec <- app_upload_spec(design)
  file <- input[[spec$file]]
  shiny::req(file)
  shiny::req(input[[spec$sep]])
  app_attempt(read_design_upload(
    file[["datapath"]], input[[spec$sep]], design = spec$validation_design,
    missing_columns = spec$missing_columns, name = file[["name"]], check = check
  ))
}
