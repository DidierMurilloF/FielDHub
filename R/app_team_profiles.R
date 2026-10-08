#' Local portraits and existing contributor profiles, keyed by family name
#' @noRd
app_team_profiles <- function() {
  list(
    Murillo = list(photo = "DidierMurillo.jpg",
      bio = paste("I am a statistician and programmer. At NDSU, I work as a statistical consulting",
        "and R/shiny developer as well as supporting breeding teams with the maintenance of databases",
        "and statistical analysis using automated scripts. I was responsible for most of the code",
        "developed and building this shiny app."),
      url = "https://www.linkedin.com/in/didier-murillo-25a7ab113/"),
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

#' Legacy portrait profiles and a compact list of other contributors
#' @noRd
app_team_ui <- function(team = fieldhub_team()) {
  profiles <- app_team_profiles()
  # The app's displayed team is separate from historical package attribution.
  team <- team[team$name != "Jean-Marc Montpetit" & !grepl("(^|, )cph($|, )", team$roles), , drop = FALSE]
  family <- sub("^.* ", "", team$name)
  portraits <- names(Filter(function(profile) !is.null(profile$photo), profiles))
  featured <- match(portraits, family)
  featured <- featured[!is.na(featured)]
  other <- setdiff(seq_len(nrow(team)), featured)
  role_labels <- c(cre = "Maintainer", aut = "Author", ctb = "Contributor", cph = "Copyright holder")
  shiny::tagList(
    shiny::div(class = "fieldhub-team-grid",
    lapply(featured, function(i) {
      profile <- profiles[[family[i]]]
      roles <- strsplit(team$roles[i], ", ", fixed = TRUE)[[1L]]
      labels <- unname(role_labels[roles])
      labels[is.na(labels)] <- roles[is.na(labels)]
      shiny::tags$article(class = "fieldhub-team-profile",
        shiny::div(class = "circular--portrait",
          shiny::tags$img(src = paste0("www/", profile$photo), alt = team$name[i])),
        shiny::tags$header(
          shiny::h3(team$name[i]),
          shiny::p(class = "fieldhub-team-role", paste(labels, collapse = ", "))),
        shiny::p(class = "profile-p", profile$bio),
        shiny::div(class = "fieldhub-team-contact",
          if (!is.null(profile$url)) app_external_link("LinkedIn", profile$url))
      )
    })),
    if (length(other)) shiny::tags$section(class = "fieldhub-team-others",
      shiny::tags$h3("Other contributors"),
      shiny::tags$ul(lapply(other, function(i) {
        profile <- profiles[[family[i]]]
        shiny::tags$li(
          if (!is.null(profile$url)) app_external_link(team$name[i], profile$url) else team$name[i])
      })))
  )
}
