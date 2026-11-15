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
  shiny::div(class = "fieldhub-team-contact",
    if (!is.null(profile$url)) app_external_link("LinkedIn", profile$url),
    if (!is.null(profile$github)) app_external_link("GitHub", profile$github))
}

#' The maintainer's profile and the team list
#'
#' The maintainer leads with a full portrait; the others with a portrait
#' follow in a two-column list. People without a portrait are listed by
#' \code{app_team_others()}.
#' @noRd
app_team_ui <- function(team = fieldhub_team()) {
  people <- app_team_people(team)
  team <- people$team
  profiles <- app_team_profiles()
  profile_of <- function(i) profiles[[people$family[i]]]
  lead <- if (!is.na(people$lead)) {
    i <- people$lead
    profile <- profile_of(i)
    shiny::tags$article(class = "fieldhub-team-profile fieldhub-team-lead",
      shiny::tags$img(class = "fieldhub-team-lead-photo", src = paste0("www/", profile$photo),
                      alt = team$name[i]),
      shiny::div(class = "fieldhub-team-lead-body",
        shiny::tags$header(
          shiny::h3(team$name[i]),
          shiny::p(class = "fieldhub-team-role", app_team_roles(team$roles[i]))),
        shiny::p(class = "profile-p", profile$bio),
        app_team_links(profile)))
  }
  members <- if (length(people$members)) shiny::tags$section(class = "fieldhub-team",
    `aria-label` = "Team",
    shiny::h3(class = "fieldhub-about-label", "Team"),
    shiny::div(class = "fieldhub-team-grid",
      lapply(people$members, function(i) {
        profile <- profile_of(i)
        shiny::tags$article(class = "fieldhub-team-profile",
          shiny::div(class = "circular--portrait",
            shiny::tags$img(src = paste0("www/", profile$photo), alt = team$name[i])),
          shiny::tags$header(
            shiny::h3(team$name[i]),
            shiny::p(class = "fieldhub-team-role", app_team_roles(team$roles[i]))),
          shiny::p(class = "profile-p", profile$bio),
          app_team_links(profile))
      })))
  shiny::div(class = "fieldhub-about-main", lead, members)
}

#' Credited people without a portrait, as a compact list
#' @return A section, or \code{NULL} when everyone has a portrait.
#' @noRd
app_team_others <- function(team = fieldhub_team()) {
  people <- app_team_people(team)
  if (!length(people$others)) return(NULL)
  profiles <- app_team_profiles()
  shiny::tags$section(class = "fieldhub-team-others",
    shiny::h3(class = "fieldhub-about-label", "Other contributors"),
    shiny::tags$ul(lapply(people$others, function(i) {
      profile <- profiles[[people$family[i]]]
      name <- people$team$name[i]
      shiny::tags$li(if (!is.null(profile$url)) app_external_link(name, profile$url) else name)
    })))
}
