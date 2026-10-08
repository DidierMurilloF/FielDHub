#' Each module's upload identifiers and shared format rule
#' @noRd
app_upload_spec <- function(design) {
  rule <- upload_rule(design)
  if (is.null(rule)) fieldhub_abort("Unknown upload design: ", design)
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
  spec <- as.list(stats::setNames(ids[[design]], c("toggle", "file", "sep")))
  spec$validation_design <- if (design %in% c("multi_loc_prep", "sparse_allocation")) "sdiag" else design
  c(spec, rule[c("missing_columns", "note", "note_tag")])
}

#' The shared "Import entries' list?" toggle, file input and separator
#'
#' @description Shared upload bindings with presentation arguments for each
#' original input panel. Ids come from
#' \code{app_upload_spec()}, so each module keeps the input ids it has
#' always had.
#'
#' @param ns The module's namespace function.
#' @param design One of the module upload keys (see \code{app_upload_spec()}).
#' @param part Render both parts, just the toggle, or just the file row.
#' @param file_width Bootstrap columns for the file input (out of twelve).
#' @param gutters Use the original wider left-column gutter.
#' @param separator_gutter Use the original narrow right-column gutter.
#' @param toggle_label,file_label Original visible labels.
#' @return A \code{shiny::tagList()}.
#' @noRd
app_upload_ui <- function(ns, design, part = c("all", "toggle", "file"),
                          file_width = 7, gutters = FALSE, separator_gutter = TRUE,
                          toggle_label = "Import entries' list?", file_label = "Upload a CSV File:") {
  spec <- app_upload_spec(design)
  part <- match.arg(part)
  shiny::tagList(
    if (part != "file") shiny::radioButtons(
      inputId = ns(spec$toggle),
      label = toggle_label,
      choices = c("Yes", "No"),
      selected = "No",
      inline = TRUE
    ),
    if (part != "toggle") shiny::conditionalPanel(
      condition = paste0("input.", spec$toggle, " == 'Yes'"),
      ns = ns,
      shiny::fluidRow(
        shiny::column(
          file_width, class = if (gutters) "fieldhub-input-left",
          shiny::fileInput(ns(spec$file), label = file_label, multiple = FALSE)
        ),
        shiny::column(
          12 - file_width, class = if (gutters && separator_gutter) "fieldhub-input-right",
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
    title = shiny::div(shiny::tags$h3("Important message", class = "fieldhub-input-error")),
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
