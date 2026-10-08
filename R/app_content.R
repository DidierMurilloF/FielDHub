#' Documentation topics derived from the app's design catalogue
#' @noRd
fieldhub_help_topics <- function() {
  registry <- fieldhub_app_registry()
  engine <- vapply(registry, `[[`, character(1), "engine")
  reference <- paste0("https://didiermurillof.github.io/FielDHub/reference/", engine, ".html")
  article <- ifelse(engine %in% c("CRD", "RCBD"), tolower(engine), engine)
  data.frame(
    label = vapply(registry, `[[`, character(1), "label"),
    group = vapply(registry, `[[`, character(1), "group"),
    engine = engine,
    help = paste0('help("', engine, '", package = "FielDHub")'),
    url = reference,
    guide = ifelse(engine == "latin_rectangle", reference,
                   paste0("https://didiermurillof.github.io/FielDHub/articles/", article, ".html")),
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

#' Legacy documentation-and-links layout, with optional offline R help
#' @noRd
app_help_ui <- function() {
  topics <- fieldhub_help_topics()
  groups <- lapply(unique(topics$group), function(group) {
    entries <- topics[topics$group == group, , drop = FALSE]
    shiny::tags$section(
      class = "fieldhub-help-group",
      shiny::tags$h3(group),
      shiny::tags$ul(lapply(seq_len(nrow(entries)), function(i) {
        shiny::tags$li(app_external_link(sub("^New - ", "", entries$label[i]), entries$guide[i]))
      }))
    )
  })
  shiny::div(class = "fieldhub-info-page fieldhub-help",
    shiny::div(class = "fieldhub-info-content",
      shiny::tags$h2("FielDHub Documentation"),
      shiny::tags$p(class = "fieldhub-info-intro",
        "Guides and examples for the Shiny app and the standalone R functions."),
      shiny::div(class = "fieldhub-help-layout",
        shiny::tags$section(`aria-label` = "Design documentation",
          shiny::div(class = "fieldhub-help-topics", groups)),
        shiny::tags$aside(class = "fieldhub-help-resources", `aria-label` = "Project resources",
          shiny::tags$h3("Other Links"),
          shiny::tags$ul(
            shiny::tags$li(app_external_link("View on CRAN", "https://cran.r-project.org/package=FielDHub")),
            shiny::tags$li(app_external_link("Browse source code", "https://github.com/DidierMurilloF/FielDHub")),
            shiny::tags$li(app_external_link("FielDHub paper", "https://joss.theoj.org/papers/10.21105/joss.03122")),
            shiny::tags$li(app_external_link("Report a problem", fieldhub_about_info()$issues)),
            shiny::tags$li(app_external_link("Release notes", "https://didiermurillof.github.io/FielDHub/news/index.html"))
          ),
          shiny::tags$h3("Community"),
          shiny::tags$ul(
            shiny::tags$li(app_external_link("Contributing guide", "https://didiermurillof.github.io/FielDHub/CONTRIBUTING.html")),
            shiny::tags$li(app_external_link("Code of conduct", "https://didiermurillof.github.io/FielDHub/CODE_OF_CONDUCT.html")),
            shiny::tags$li(app_external_link("Video tutorials", "https://www.youtube.com/channel/UC4i3oTcU58Za42DbWwqF9Kg")),
            shiny::tags$li(app_external_link("NDSU Big Data Pipeline", "https://sites.google.com/ndsu.edu/plsc-bpdm/home"))
          )
        )
      ),
      shiny::tags$details(class = "fieldhub-offline-help",
        shiny::tags$summary("Using FielDHub from R (including offline help)"),
        shiny::tags$p("Online guides require an internet connection. These commands open the documentation installed with FielDHub and also work offline."),
        shiny::tags$dl(lapply(seq_len(nrow(topics)), function(i) shiny::tagList(
          shiny::tags$dt(sub("^New - ", "", topics$label[i])),
          shiny::tags$dd(shiny::tags$code(topics$help[i])))))
      )
    ),
    htmltools::HTML(fieldhub_footer())
  )
}

#' About page combining package credits and maintained team profiles
#' @noRd
app_about_ui <- function() {
  info <- fieldhub_about_info()
  team <- fieldhub_team()
  htmltools::htmlTemplate(system.file("app/www/aboutUs.html", package = "FielDHub"),
    metadata = shiny::div(class = "fieldhub-package-info",
      shiny::span(paste("Version", info$version)),
      shiny::span(paste("License:", info$license))),
    team = app_team_ui(team), footer = htmltools::HTML(fieldhub_footer()))
}

#' Welcome content with the current footer
#' @noRd
app_home_ui <- function() {
  htmltools::htmlTemplate(system.file("app/www/home.html", package = "FielDHub"),
                          footer = htmltools::HTML(fieldhub_footer()))
}
