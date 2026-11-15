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

#' Replay notes for the designs that have them, from the design registry
#' @noRd
app_help_replay_notes <- function(topics = fieldhub_help_topics()) {
  notes <- design_replay_notes()
  shown <- topics[topics$engine %in% names(notes), , drop = FALSE]
  if (!nrow(shown)) return(NULL)
  shiny::tags$section(class = "fieldhub-help-notes", `aria-label` = "Reproduction notes",
    shiny::tags$h3("Reproduction code"),
    lapply(seq_len(nrow(shown)), function(i) shiny::tagList(
      shiny::tags$h4(sub("^New - ", "", shown$label[i])),
      shiny::tags$ul(lapply(notes[[shown$engine[i]]], shiny::tags$li)))))
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
      app_help_replay_notes(topics),
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
    metadata = shiny::tags$aside(class = "fieldhub-about-aside", `aria-label` = "About the package",
      shiny::tags$section(class = "fieldhub-package-info",
        shiny::h3(class = "fieldhub-about-label", "Package"),
        shiny::tags$dl(
          shiny::tags$dt("Version"), shiny::tags$dd(info$version),
          shiny::tags$dt("License"), shiny::tags$dd(info$license),
          shiny::tags$dt("Source"),
          shiny::tags$dd(app_external_link("GitHub", "https://github.com/DidierMurilloF/FielDHub")),
          shiny::tags$dt("Issues"), shiny::tags$dd(app_external_link("Report a problem", info$issues)))),
      shiny::tags$section(
        shiny::h3(class = "fieldhub-about-label", "Cite FielDHub"),
        shiny::p("Run ", shiny::tags$code('citation("FielDHub")'), " in R, or read the ",
                 app_external_link("FielDHub paper", "https://joss.theoj.org/papers/10.21105/joss.03122"),
                 " in the Journal of Open Source Software.")),
      app_team_others(team)),
    team = app_team_ui(team), footer = htmltools::HTML(fieldhub_footer()))
}

#' Welcome content with the current footer
#' @noRd
app_home_ui <- function(registry = fieldhub_app_registry()) {
  htmltools::htmlTemplate(system.file("app/www/home.html", package = "FielDHub"),
                          design_count = length(registry),
                          version = as.character(utils::packageVersion("FielDHub")),
                          designs = app_home_designs(registry),
                          footer = htmltools::HTML(fieldhub_footer()))
}

#' The Welcome page's design chooser
#'
#' One card per design menu, listing its designs in menu order. Each design
#' links to its tab (\code{data-fieldhub-tab}, handled by
#' \code{home-links.js}); a "New - " label shows as a New badge.
#' @param registry The app's design registry.
#' @noRd
app_home_designs <- function(registry = fieldhub_app_registry()) {
  titles <- c(`Unreplicated Designs` = "Unreplicated",
              `Partially Replicated Designs` = "Partially replicated",
              `Lattice Designs` = "Lattice",
              `Other Designs` = "Other designs")
  blurbs <- c(`Unreplicated Designs` = "Test many entries once, with replicated checks spread across the field.",
              `Partially Replicated Designs` = "Replicate a share of the entries, at one location or several.",
              `Lattice Designs` = "Resolvable incomplete blocks for many entries.",
              `Other Designs` = "Complete blocks, Latin, factorial, split-plot and row-column designs.")
  groups <- vapply(registry, `[[`, character(1), "group")
  shiny::div(class = "fieldhub-home-families",
    lapply(unique(groups), function(group) {
      entries <- registry[groups == group]
      count <- length(entries)
      shiny::tags$article(class = "fieldhub-home-family",
        shiny::div(class = "fieldhub-home-family-top",
          shiny::h3(if (group %in% names(titles)) titles[[group]] else group),
          shiny::span(class = "fieldhub-home-count", paste(count, if (count == 1L) "design" else "designs"))),
        if (group %in% names(blurbs)) shiny::p(blurbs[[group]]),
        shiny::tags$ul(lapply(entries, function(entry) {
          is_new <- startsWith(entry$label, "New - ")
          shiny::tags$li(shiny::tags$a(href = "#", `data-fieldhub-tab` = entry$label,
            shiny::span(sub("^New - ", "", entry$label)),
            if (is_new) shiny::span(class = "fieldhub-home-new", "New")))
        })))
    }))
}
