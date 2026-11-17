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

#' Licence for display: "MIT + file LICENSE" reads as "MIT"
#' @noRd
fieldhub_license_label <- function(license) {
  sub("[[:space:]]*\\+[[:space:]]*file[[:space:]]+LICEN[CS]E$", "", license)
}

#' The green banner that opens the Welcome, Help and About pages: kicker,
#' title and lead on NDSU green, beside a photograph.
#' @noRd
app_page_banner <- function(kicker, title, lead, photo, alt) {
  shiny::tags$header(class = "fh-banner",
    shiny::div(class = "fh-banner-copy",
      shiny::p(class = "fh-kicker", kicker),
      shiny::h1(title),
      shiny::p(class = "fh-banner-sub", lead)),
    shiny::tags$figure(class = "fh-banner-photo fh-pos-mid",
      shiny::tags$img(src = paste0("www/", photo), alt = alt)))
}

#' Help: where to start, a guide, reference page and offline R help command
#' for every design, and project links.
#' @noRd
app_help_ui <- function() {
  topics <- fieldhub_help_topics()
  unit <- fieldhub_unit()
  start <- function(title, text, url) {
    shiny::tags$a(class = "fh-start-card", href = url, target = "_blank", rel = "noopener noreferrer",
                  shiny::tags$strong(title), shiny::span(text))
  }
  groups <- lapply(unique(topics$group), function(group) {
    entries <- topics[topics$group == group, , drop = FALSE]
    shiny::tags$section(class = "fh-guide-group",
      shiny::h3(group),
      shiny::tags$ul(lapply(seq_len(nrow(entries)), function(i) {
        shiny::tags$li(
          shiny::tags$a(class = "fh-guide-name", href = entries$guide[i], target = "_blank",
                        rel = "noopener noreferrer", sub("^New - ", "", entries$label[i])),
          shiny::tags$a(class = "fh-guide-ref", href = entries$url[i], target = "_blank",
                        rel = "noopener noreferrer", "Reference"),
          shiny::tags$code(entries$help[i]))
      })))
  })
  links <- function(...) shiny::tags$ul(lapply(list(...), shiny::tags$li))
  shiny::div(class = "fh-page fh-help",
    app_page_banner("Help", "Guides and documentation",
      "A guide for every design, the function reference, and the R help that works offline.",
      "help-banner.jpg",
      "A clipboard with blank sheets and a pencil on the soil at the edge of green wheat plots, with white plot stakes along the alley."),
    shiny::div(class = "fh-inner", shiny::div(class = "fh-layout",
      shiny::div(class = "fh-main",
        shiny::div(class = "fh-start",
          start("FielDHub website", "All guides, worked examples and the function reference.",
                "https://didiermurillof.github.io/FielDHub/"),
          start("Video tutorials", "Walk-throughs of the app, design by design.",
                "https://www.youtube.com/channel/UC4i3oTcU58Za42DbWwqF9Kg"),
          start("Report a problem", "Found a bug or missing a design? Open an issue on GitHub.",
                fieldhub_about_info()$issues)),
        shiny::tags$section(class = "fh-guides", `aria-label` = "Design documentation",
          shiny::h2("Design guides"),
          shiny::p("Guides open on the FielDHub website and need an internet connection. The R command beside each design opens the help installed with the package, and works offline."),
          groups)),
      shiny::tags$aside(class = "fh-aside", `aria-label` = "Project resources",
        shiny::tags$section(shiny::h2(class = "fh-label", "Package"),
          links(app_external_link("View on CRAN", "https://cran.r-project.org/package=FielDHub"),
                app_external_link("Browse source code", "https://github.com/DidierMurilloF/FielDHub"),
                app_external_link("FielDHub paper", "https://joss.theoj.org/papers/10.21105/joss.03122"),
                app_external_link("Release notes", "https://didiermurillof.github.io/FielDHub/news/index.html"))),
        shiny::tags$section(shiny::h2(class = "fh-label", "Community"),
          links(app_external_link("Contributing guide", "https://didiermurillof.github.io/FielDHub/CONTRIBUTING.html"),
                app_external_link("Code of conduct", "https://didiermurillof.github.io/FielDHub/CODE_OF_CONDUCT.html"),
                shiny::tagList(app_external_link(unit$name, unit$url), ", NDSU"))),
        shiny::tags$section(shiny::h2(class = "fh-label", "In R"),
          shiny::p(shiny::tags$code("library(FielDHub)")),
          shiny::p(shiny::tags$code("run_app()")),
          shiny::p(class = "fh-small", "Every design in this app is also an R function with the same arguments."))))),
    htmltools::HTML(fieldhub_footer()))
}

#' About: the team, package facts, where FielDHub is developed, how to cite
#' it and the photo credits (the footer's "Photo credits" link points here).
#' @noRd
app_about_ui <- function() {
  info <- fieldhub_about_info()
  team <- fieldhub_team()
  unit <- fieldhub_unit()
  htmltools::htmlTemplate(system.file("app/www/aboutUs.html", package = "FielDHub"),
    metadata = shiny::tags$aside(class = "fh-aside", `aria-label` = "About the package",
      shiny::tags$section(class = "fh-package",
        shiny::h2(class = "fh-label", "Package"),
        shiny::tags$dl(
          shiny::tags$dt("Version"), shiny::tags$dd(info$version),
          shiny::tags$dt("License"), shiny::tags$dd(fieldhub_license_label(info$license)),
          shiny::tags$dt("Source"),
          shiny::tags$dd(app_external_link("GitHub", "https://github.com/DidierMurilloF/FielDHub")),
          shiny::tags$dt("Issues"), shiny::tags$dd(app_external_link("Report a problem", info$issues)))),
      shiny::tags$section(
        shiny::h2(class = "fh-label", "Developed at"),
        shiny::p(app_external_link(unit$name, unit$url), ", North Dakota State University."),
        shiny::p(class = "fh-small", unit$description)),
      shiny::tags$section(
        shiny::h2(class = "fh-label", "Cite FielDHub"),
        shiny::p("Run ", shiny::tags$code('citation("FielDHub")'), " in R, or read the ",
                 app_external_link("FielDHub paper", "https://joss.theoj.org/papers/10.21105/joss.03122"),
                 " in the Journal of Open Source Software.")),
      shiny::tags$section(id = "photo-credits",
        shiny::h2(class = "fh-label", "Photo credits"),
        shiny::p(class = "fh-small",
          "Welcome: aerial view of an NDSU breeding trial, by ",
          app_external_link(unit$name, unit$url), ", NDSU; cropped."),
        shiny::p(class = "fh-small",
          "Help and About: illustrative images (AI-generated), not photographs of real trials."))),
    team = app_team_ui(team), footer = htmltools::HTML(fieldhub_footer()))
}

#' Welcome page: banner, how it works, the design chooser, one example
#' trial (map, field book, simulated response, R call) and teaching. The
#' example's images and field-book rows are rendered by dev/page_images.R.
#' @noRd
app_home_ui <- function(registry = fieldhub_app_registry()) {
  fragment <- function(name) htmltools::HTML(paste(readLines(system.file("app/www", name, package = "FielDHub"),
                                                             warn = FALSE), collapse = "\n"))
  htmltools::htmlTemplate(system.file("app/www/home.html", package = "FielDHub"),
                          design_count = length(registry),
                          designs = app_home_designs(registry),
                          field_book = fragment("home-fieldbook.html"),
                          heat_scale = fragment("home-heat-scale.html"),
                          footer = htmltools::HTML(fieldhub_footer()))
}

#' The Welcome page's design chooser
#'
#' One row per design menu: a small layout of that kind of design, a short
#' description, and its designs in menu order. Each design links to its tab
#' (\code{data-fieldhub-tab}, handled by \code{home-links.js}); a "New - "
#' label shows as a New badge.
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
  thumbs <- c(`Unreplicated Designs` = "thumb-unreplicated.svg",
              `Partially Replicated Designs` = "thumb-prep.svg",
              `Lattice Designs` = "thumb-lattice.svg",
              `Other Designs` = "thumb-other.svg")
  groups <- vapply(registry, `[[`, character(1), "group")
  shiny::div(class = "fh-families",
    lapply(unique(groups), function(group) {
      entries <- registry[groups == group]
      count <- length(entries)
      shiny::tags$article(class = "fh-family",
        shiny::div(class = "fh-family-intro",
          if (group %in% names(thumbs)) {
            shiny::tags$img(src = paste0("www/", thumbs[[group]]), width = 170, height = 98, alt = "")
          },
          shiny::div(
            shiny::h3(if (group %in% names(titles)) titles[[group]] else group),
            shiny::span(class = "fh-count", paste(count, if (count == 1L) "design" else "designs")),
            if (group %in% names(blurbs)) shiny::p(blurbs[[group]]))),
        shiny::tags$ul(lapply(entries, function(entry) {
          is_new <- startsWith(entry$label, "New - ")
          shiny::tags$li(shiny::tags$a(href = "#", `data-fieldhub-tab` = entry$label,
            shiny::span(sub("^New - ", "", entry$label)),
            if (is_new) shiny::span(class = "fh-new", "New")))
        })))
    }))
}
