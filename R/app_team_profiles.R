#' Local portraits and existing contributor profiles, keyed by family name
#' @noRd
app_team_profiles <- function() {
  list(
    Murillo = list(photo = "didier-murillo.jpg",
      bio = paste("I am a statistician and programmer at NDSU, where I provide statistical consulting,",
        "develop R/Shiny applications, and support breeding teams with database maintenance and",
        "automated statistical analysis. My work covers experimental design, linear mixed models, and",
        "genomic and phenomic prediction. I wrote most of FielDHub's code and led the development of",
        "this Shiny app."),
      url = "https://www.linkedin.com/in/didier-murillo-25a7ab113/",
      github = "https://github.com/DidierMurilloF"),
    Gezan = list(photo = "Salvador.jpg",
      bio = paste("I am a statistical quantitative geneticist with experience in Linear Mixed Models",
        "applied to plant and animal breeding. I usually work providing guidance and leadership to",
        "breeding teams in projects related to the development of applications and analysis of large",
        "datasets, such as the NDSU breeding programs."),
      url = "https://www.linkedin.com/in/salvador-a-gezan-54768a1a/"),
    Heilman = list(photo = "ana.jpg",
      bio = paste("I am a plant breeder and data scientist with experience in statistical analysis and",
        "data visualizations. I support plant breeders/faculty, staff and students with their data",
        "analytic needs and I lead the expansion of technology transfer and deployment of software",
        "products that supports NDSU breeding operations."),
      url = "https://www.linkedin.com/in/ana-mar%C3%ADa-heilman-morales-a416a91b/"),
    Seefeldt = list(photo = "matthew.jpg",
      bio = paste("I am a mathematician and undergraduate student at NDSU. I contributed to the updated",
        "version of FielDHub, adding functionality for heatmaps and experiments over multiple",
        "locations, among other things. I also wrote documentation and vignettes for FielDHub."),
      url = "https://www.linkedin.com/in/matthew-seefeldt-1a98481a2/"),
    Aparicio = list(photo = "johan.jpg",
      bio = paste("I am a statistician, and I am very passionate about statistical analysis, web",
        "development, and visualizations, which is combined with my desire for accelerating precise",
        "decision-making processes. In plant breeding I currently lead work on the development of",
        "applications designed to automate routine agricultural tasks."),
      url = "https://www.linkedin.com/in/johan-steven-aparicio-arce-b68976193/"),
    Horsley = list(photo = "richard.jpg",
      bio = paste("I am the Head of the Department of Plant Sciences and barley breeder at North Dakota",
        "State University. The primary goal of my breeding project is to release and develop two-rowed",
        "and six-rowed malting barley varieties acceptable to barley producers in North Dakota,",
        "adjacent states, and the malting and brewing industry. I am also the leader of the Breeding",
        "Pipeline Database Managers team and I provide guidelines and support on the statistical analysis."),
      url = "https://www.linkedin.com/in/richard-horsley-1047301b/"),
    Walk = list(url = "https://www.linkedin.com/in/tomwalk/")
  )
}

#' The people the About page shows
#'
#' @param team Credited people (\code{fieldhub_team()}).
#' @return A list: \code{team} (shown people, historical package attribution
#'   left out), \code{lead} (the row of the maintainer with a portrait, or
#'   \code{NA}), \code{members} (the other rows with a portrait, in profile
#'   order) and \code{others} (rows without a portrait).
#' @noRd
app_team_people <- function(team = fieldhub_team()) {
  profiles <- app_team_profiles()
  # The app's displayed team is separate from historical package attribution.
  team <- team[team$name != "Jean-Marc Montpetit" & !grepl("(^|, )cph($|, )", team$roles), , drop = FALSE]
  family <- sub("^.* ", "", team$name)
  portraits <- names(Filter(function(profile) !is.null(profile$photo), profiles))
  featured <- match(portraits, family)
  featured <- featured[!is.na(featured)]
  maintainer <- featured[grepl("(^|, )cre($|, )", team$roles[featured])]
  lead <- if (length(maintainer)) maintainer[1L] else NA_integer_
  list(team = team, family = family, lead = lead, members = setdiff(featured, lead),
       others = setdiff(seq_len(nrow(team)), featured))
}

#' Readable roles of a credited person
#' @noRd
app_team_roles <- function(roles) {
  role_labels <- c(cre = "Maintainer", aut = "Author", ctb = "Contributor", cph = "Copyright holder")
  roles <- strsplit(roles, ", ", fixed = TRUE)[[1L]]
  labels <- unname(role_labels[roles])
  labels[is.na(labels)] <- roles[is.na(labels)]
  paste(labels, collapse = ", ")
}

#' A person's profile links
#' @noRd
app_team_links <- function(profile) {
  shiny::p(class = "fh-contact",
    if (!is.null(profile$url)) app_external_link("LinkedIn", profile$url),
    if (!is.null(profile$github)) app_external_link("GitHub", profile$github))
}

#' Initials shown in place of a missing portrait
#' @noRd
app_team_initials <- function(name) {
  words <- strsplit(name, " ", fixed = TRUE)[[1L]]
  toupper(paste(substr(words[c(1L, length(words))], 1L, 1L), collapse = ""))
}

#' A team member's card: portrait (or initials), name, roles, a bio clamped
#' to a few lines with a "Read more" toggle that needs no JavaScript, and
#' profile links.
#' @noRd
app_team_card <- function(name, roles, profile) {
  id <- paste0("fh-bio-", gsub("[^a-z0-9]+", "-", tolower(name)))
  avatar <- if (!is.null(profile$photo)) {
    shiny::tags$img(class = "fh-avatar", src = paste0("www/", profile$photo), alt = "", width = 72, height = 72)
  } else {
    shiny::span(class = "fh-avatar fh-avatar-mono", `aria-hidden` = "true", app_team_initials(name))
  }
  shiny::tags$li(class = "fh-card",
    avatar,
    shiny::div(class = "fh-card-head", shiny::h3(name), shiny::p(class = "fh-role", app_team_roles(roles))),
    if (!is.null(profile$bio)) shiny::div(class = "fh-card-body",
      shiny::tags$input(class = "fh-bio-toggle", type = "checkbox", id = id),
      shiny::p(class = "fh-bio fh-bio-clamp", profile$bio),
      shiny::tags$label(class = "fh-bio-label", `for` = id,
        shiny::span(class = "fh-more", "Read more"), shiny::span(class = "fh-less", "Show less"),
        shiny::span(class = "sr-only", paste(" about", name)))),
    app_team_links(profile))
}

#' The maintainer's profile and a card for everyone else
#'
#' The maintainer leads with a full portrait; the others follow as cards,
#' people with a portrait first in profile order, then the rest with their
#' initials.
#' @noRd
app_team_ui <- function(team = fieldhub_team()) {
  people <- app_team_people(team)
  team <- people$team
  profiles <- app_team_profiles()
  profile_of <- function(i) profiles[[people$family[i]]] %||% list()
  lead <- if (!is.na(people$lead)) {
    i <- people$lead
    profile <- profile_of(i)
    shiny::tags$article(class = "fh-lead",
      shiny::tags$img(src = paste0("www/", profile$photo), alt = team$name[i], width = 168, height = 210),
      shiny::div(class = "fh-person",
        shiny::div(shiny::h2(team$name[i]), shiny::p(class = "fh-role", app_team_roles(team$roles[i]))),
        shiny::p(class = "fh-bio", profile$bio),
        app_team_links(profile)))
  }
  others <- c(people$members, people$others)
  cards <- if (length(others)) shiny::tags$section(class = "fh-team", `aria-labelledby` = "fh-team-title",
    shiny::h2(class = "fh-section-title", id = "fh-team-title", "Team"),
    shiny::tags$ul(class = "fh-cards", lapply(others, function(i) {
      app_team_card(team$name[i], team$roles[i], profile_of(i))
    })))
  shiny::div(class = "fh-main", lead, cards)
}
