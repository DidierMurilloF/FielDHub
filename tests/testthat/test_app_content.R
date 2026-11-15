test_that("help topics follow the shared app registry and public API names", {
  registry <- fieldhub_app_registry()
  topics <- fieldhub_help_topics()
  expect_identical(topics$label, vapply(registry, `[[`, character(1), "label"))
  expect_identical(topics$group, vapply(registry, `[[`, character(1), "group"))
  engines <- vapply(registry, `[[`, character(1), "engine")
  expect_identical(topics$engine, engines)
  expect_identical(topics$help, paste0('help("', engines, '", package = "FielDHub")'))
  expect_identical(topics$url, paste0("https://didiermurillof.github.io/FielDHub/reference/", engines, ".html"))
  articles <- ifelse(engines %in% c("CRD", "RCBD"), tolower(engines), engines)
  guides <- paste0("https://didiermurillof.github.io/FielDHub/articles/", articles, ".html")
  guides[engines == "latin_rectangle"] <- topics$url[engines == "latin_rectangle"]
  expect_identical(topics$guide, guides)
})

test_that("Help keeps compact legacy groups and puts optional R commands behind one disclosure", {
  ui <- app_help_ui()
  query <- htmltools::tagQuery(ui)
  expect_equal(query$find(".fieldhub-help-group")$length(), 4L)
  links <- query$find(".fieldhub-help-group a")$selectedTags()
  expect_identical(unname(vapply(links, function(link) link$attribs$href, "")), fieldhub_help_topics()$guide)
  expect_true(all(vapply(links, function(link) identical(link$attribs$rel, "noopener noreferrer"), TRUE)))
  expect_equal(query$find(".fieldhub-help-resources")$length(), 1L)
  expect_equal(query$find(".fieldhub-help-group code")$length(), 0L)
  details <- query$find("details")$selectedTags()
  expect_length(details, 1L)
  expect_null(details[[1L]]$attribs$open)
  expect_equal(query$find("details code")$length(), nrow(fieldhub_help_topics()))
  expect_match(as.character(ui), 'class="fieldhub-footer"', fixed = TRUE)
})

test_that("About retains the six legacy profiles without duplicate credits or the removed profile", {
  ui <- app_about_ui()
  query <- htmltools::tagQuery(ui)
  expect_equal(query$find(".fieldhub-team-profile")$length(), 6L)
  images <- query$find(".fieldhub-team-profile img")$selectedTags()
  profiles <- app_team_profiles()
  portraits <- Filter(function(profile) !is.null(profile$photo), profiles)
  expect_identical(unname(vapply(images, function(image) image$attribs$src, "")),
                   unname(vapply(portraits, function(profile) paste0("www/", profile$photo), "")))
  expect_true(all(nzchar(vapply(images, function(image) image$attribs$alt, ""))))
  html <- as.character(ui)
  expect_match(html, "Thomas Walk", fixed = TRUE)
  expect_false(grepl("Jean-Marc Montpetit|jeanmarc[.]montpetit@videotron[.]ca|Package authors and contributors:", html))
  expect_match(html, 'class="fieldhub-package-info"', fixed = TRUE)
  expect_match(html, 'class="fieldhub-footer"', fixed = TRUE)
  expect_match(html, fieldhub_about_info()$version, fixed = TRUE)
  expect_match(html, fieldhub_about_info()$license, fixed = TRUE)
  expect_false(grepl("mailto:|Copyright:|jeanmarc[.]montpetit@videotron[.]ca", html))
  expect_match(html, "https://www.linkedin.com/in/salvador-a-gezan-54768a1a/", fixed = TRUE)
  expect_match(html, "https://www.linkedin.com/in/richard-horsley-1047301b/", fixed = TRUE)
})

test_that("additional credited people remain visible without introducing extra portrait rows", {
  team <- fieldhub_team()
  team <- rbind(team, data.frame(name = "Another Contributor", roles = "ctb", email = "person@example.org"))
  ui <- shiny::div(app_team_ui(team), app_team_others(team))
  query <- htmltools::tagQuery(ui)
  expect_equal(query$find(".fieldhub-team-profile")$length(), 6L)
  expect_match(as.character(query$find(".fieldhub-team-others")$selectedTags()[[1L]]),
               "Another Contributor", fixed = TRUE)
  # with everyone portrayed there is no "Other contributors" list
  expect_null(app_team_others(team[team$name != "Thomas Walk" & team$name != "Another Contributor", ]))
})

test_that("About leads with the maintainer and keeps package facts beside the team", {
  ui <- app_about_ui()
  query <- htmltools::tagQuery(ui)
  lead <- query$find(".fieldhub-team-lead")$selectedTags()
  expect_length(lead, 1L)
  lead <- as.character(lead[[1L]])
  maintainer <- fieldhub_team()$name[grepl("cre", fieldhub_team()$roles)]
  expect_match(lead, maintainer, fixed = TRUE)
  expect_match(lead, 'src="www/didier-murillo.jpg"', fixed = TRUE)
  expect_match(lead, "genomic and phenomic prediction", fixed = TRUE)
  expect_match(lead, "https://github.com/DidierMurilloF", fixed = TRUE)
  expect_true(file.exists(system.file("app/www/didier-murillo.jpg", package = "FielDHub")))
  # the other five portraits follow in the team list, the maintainer not repeated
  members <- query$find(".fieldhub-team-grid .fieldhub-team-profile")$selectedTags()
  expect_length(members, 5L)
  expect_false(any(grepl(maintainer, vapply(members, as.character, ""), fixed = TRUE)))
  html <- as.character(ui)
  aside <- regmatches(html, regexpr('(?s)<aside class="fieldhub-about-aside".*?</aside>', html, perl = TRUE))
  expect_length(aside, 1L)
  for (text in c(fieldhub_about_info()$version, fieldhub_about_info()$license, fieldhub_about_info()$issues,
                 '<code>citation("FielDHub")</code>', "joss.03122", "Thomas Walk")) {
    expect_match(aside, text, fixed = TRUE)
  }
})

test_that("the About page uses maintained package metadata", {
  info <- fieldhub_about_info()
  description <- utils::packageDescription("FielDHub")
  expect_identical(info$version, description$Version)
  expect_identical(info$authors, description$Author)
  expect_identical(info$license, description$License)
  expect_identical(info$issues, description$BugReports)
})

test_that("welcome and team fragments do not load remote styles or whole HTML documents", {
  for (name in c("home.html", "aboutUs.html")) {
    content <- paste(readLines(system.file("app/www", name, package = "FielDHub"), warn = FALSE), collapse = "\n")
    expect_false(grepl("<(html|head|body)([ >])|<!DOCTYPE", content, ignore.case = TRUE))
    expect_false(grepl("<link[^>]+https?://", content, ignore.case = TRUE))
  }
})

test_that("application styles are scoped to the FielDHub root", {
  for (name in c("style.css", "mobile.css")) {
    css <- paste(readLines(system.file("app/www", name, package = "FielDHub"), warn = FALSE), collapse = "\n")
    css <- gsub("(?s)/\\*.*?\\*/", "", css, perl = TRUE)
    selectors <- regmatches(css, gregexpr("[^{}]+(?=\\{)", css, perl = TRUE))[[1]]
    selectors <- trimws(unlist(strsplit(selectors, ",", fixed = TRUE)))
    selectors <- selectors[!startsWith(selectors, "@media")]
    expect_true(all(startsWith(selectors, "#fieldhub-app") | startsWith(selectors, ".fieldhub-")), info = name)
  }
})

test_that("the app builds Help and About from shared content functions", {
  code <- paste(deparse(body(app_ui)), collapse = " ")
  expect_match(code, "app_help_ui", fixed = TRUE)
  expect_match(code, "app_about_ui", fixed = TRUE)
  expect_match(code, 'id = "fieldhub-app"', fixed = TRUE)
})

test_that("design-page rows stay within the edge-to-edge app container", {
  css <- paste(readLines(system.file("app/www/style.css", package = "FielDHub")), collapse = "\n")
  expect_match(css, "#fieldhub-app .tab-pane > .row {\n  margin-left: 0;\n  margin-right: 0;\n}", fixed = TRUE)
})
