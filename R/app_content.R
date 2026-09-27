#' Documentation topics derived from the app's design catalogue
#' @noRd
fieldhub_help_topics <- function() {
  registry <- fieldhub_app_registry()
  engine <- vapply(registry, `[[`, character(1), "engine")
  data.frame(
    label = vapply(registry, `[[`, character(1), "label"),
    group = vapply(registry, `[[`, character(1), "group"),
    engine = engine,
    help = paste0('help("', engine, '", package = "FielDHub")'),
    url = paste0("https://didiermurillof.github.io/FielDHub/reference/", engine, ".html"),
    stringsAsFactors = FALSE
  )
}

#' Maintained metadata for the app's About page
#' @noRd
fieldhub_about_info <- function() {
  description <- utils::packageDescription("FielDHub")
  list(version = description$Version, authors = description$Author,
       license = description$License, issues = description$BugReports)
}

#' Accessible links that do not give the new tab access to the app
#' @noRd
app_external_link <- function(label, url) {
  shiny::tags$a(label, href = url, target = "_blank", rel = "noopener noreferrer")
}

#' Shared Help page with online references and offline R help commands
#' @noRd
app_help_ui <- function() {
  topics <- fieldhub_help_topics()
  groups <- lapply(unique(topics$group), function(group) {
    entries <- topics[topics$group == group, , drop = FALSE]
    shiny::tags$section(
      shiny::tags$h3(group),
      shiny::tags$ul(lapply(seq_len(nrow(entries)), function(i) {
        shiny::tags$li(
          app_external_link(entries$label[i], entries$url[i]),
          shiny::tags$br(), shiny::tags$code(entries$help[i])
        )
      }))
    )
  })
  shiny::tags$section(
    class = "fieldhub-help",
    shiny::tags$h2("FielDHub documentation"),
    shiny::tags$p("Online reference links require an internet connection. The R help commands below use the documentation installed with FielDHub and also work offline."),
    shiny::tags$div(class = "fieldhub-help-topics", groups),
    shiny::tags$h3("Project resources"),
    shiny::tags$ul(
      shiny::tags$li(app_external_link("Release notes", "https://didiermurillof.github.io/FielDHub/news/index.html")),
      shiny::tags$li(app_external_link("Report a problem", fieldhub_about_info()$issues)),
      shiny::tags$li(app_external_link("Source code", "https://github.com/DidierMurilloF/FielDHub")),
      shiny::tags$li(app_external_link("Contributing guide", "https://didiermurillof.github.io/FielDHub/CONTRIBUTING.html")),
      shiny::tags$li(app_external_link("Code of conduct", "https://didiermurillof.github.io/FielDHub/CODE_OF_CONDUCT.html")),
      shiny::tags$li(app_external_link("FielDHub paper", "https://joss.theoj.org/papers/10.21105/joss.03122")),
      shiny::tags$li(app_external_link("CRAN package", "https://cran.r-project.org/package=FielDHub"))
    )
  )
}

#' About page combining package credits and maintained team profiles
#' @noRd
app_about_ui <- function() {
  info <- fieldhub_about_info()
  shiny::tagList(
    shiny::tags$section(
      class = "fieldhub-package-info",
      shiny::tags$h2(paste("FielDHub", info$version)),
      shiny::tags$p(shiny::tags$strong("Package authors and contributors: "), info$authors),
      shiny::tags$p(shiny::tags$strong("License: "), info$license)
    ),
    htmltools::includeHTML(system.file("app/www/aboutUs.html", package = "FielDHub"))
  )
}
