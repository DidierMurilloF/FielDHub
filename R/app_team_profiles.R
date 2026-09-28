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
      url = "https://www.linkedin.com/in/salvador-gezan-54768a1a/"),
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
        "Pipeline Database Managers team and I provide guidelines and support on the statistical analysis.")),
    Walk = list(url = "https://www.linkedin.com/in/tomwalk/")
  )
}

#' Contributor cards derived from Authors@R, with optional local portraits
#' @noRd
app_team_ui <- function(team = fieldhub_team()) {
  profiles <- app_team_profiles()
  role_labels <- c(cre = "Maintainer", aut = "Author", ctb = "Contributor", cph = "Copyright holder")
  shiny::div(class = "grid-container",
    lapply(seq_len(nrow(team)), function(i) {
      profile <- profiles[[sub("^.* ", "", team$name[i])]]
      roles <- strsplit(team$roles[i], ", ", fixed = TRUE)[[1L]]
      labels <- unname(role_labels[roles])
      labels[is.na(labels)] <- roles[is.na(labels)]
      shiny::div(class = "grid-item",
        if (!is.null(profile$photo)) shiny::div(class = "circular--portrait",
          shiny::tags$img(src = paste0("www/", profile$photo), alt = team$name[i])),
        shiny::h3(team$name[i]),
        shiny::p(paste(labels, collapse = ", ")),
        if (!is.null(profile$bio)) shiny::p(class = "profile-p", profile$bio),
        if (nzchar(team$email[i])) shiny::p(shiny::tags$a(team$email[i], href = paste0("mailto:", team$email[i]))),
        if (!is.null(profile$url)) app_external_link("LinkedIn", profile$url)
      )
    })
  )
}
