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

test_that("Help lists a guide, reference page and offline R help for every design", {
  ui <- app_help_ui()
  topics <- fieldhub_help_topics()
  query <- htmltools::tagQuery(ui)
  expect_identical(query$find(".fh-guide-group")$length(), length(unique(topics$group)))
  href <- function(selector) {
    unname(vapply(query$find(selector)$selectedTags(), function(link) link$attribs$href, ""))
  }
  expect_identical(href(".fh-guide-name"), topics$guide)
  expect_identical(href(".fh-guide-ref"), topics$url)
  codes <- query$find(".fh-guide-group code")$selectedTags()
  expect_identical(unname(vapply(codes, function(code) as.character(code$children[[1L]]), "")), topics$help)
  links <- query$find("a")$selectedTags()
  external <- Filter(function(link) identical(link$attribs$target, "_blank"), links)
  expect_true(all(vapply(external, function(link) identical(link$attribs$rel, "noopener noreferrer"), TRUE)))
  expect_identical(query$find(".fh-start-card")$length(), 3L)
  html <- as.character(ui)
  expect_false(grepl("New - ", html, fixed = TRUE))
  # no reproduction notes or collapsed offline block any more
  expect_false(grepl("Reproduction code|<details", html))
  expect_match(html, 'src="www/help-banner.jpg"', fixed = TRUE)
  expect_match(html, 'class="fh-footer"', fixed = TRUE)
  expect_match(html, fieldhub_unit()$url, fixed = TRUE)
})

test_that("About leads with the maintainer and gives everyone else a card", {
  ui <- app_about_ui()
  query <- htmltools::tagQuery(ui)
  team <- app_team_people()$team
  maintainer <- team$name[grepl("cre", team$roles)]
  lead <- query$find(".fh-lead")$selectedTags()
  expect_length(lead, 1L)
  lead <- as.character(lead[[1L]])
  expect_match(lead, maintainer, fixed = TRUE)
  expect_match(lead, 'src="www/didier-murillo.jpg"', fixed = TRUE)
  expect_match(lead, "genomic and phenomic prediction", fixed = TRUE)
  expect_match(lead, "https://github.com/DidierMurilloF", fixed = TRUE)
  # one card per other shown person: portraits first, then initials
  cards <- vapply(query$find(".fh-card")$selectedTags(), as.character, "")
  expect_length(cards, nrow(team) - 1L)
  expect_false(any(grepl(maintainer, cards, fixed = TRUE)))
  portraits <- Filter(function(profile) !is.null(profile$photo), app_team_profiles())
  sources <- unname(vapply(query$find(".fh-card img.fh-avatar")$selectedTags(), function(img) img$attribs$src, ""))
  expect_identical(sources, paste0("www/", unname(vapply(portraits, `[[`, "", "photo")))[-1L])
  for (photo in c(sources, "www/didier-murillo.jpg")) {
    expect_true(file.exists(system.file("app", photo, package = "FielDHub")), info = photo)
  }
  walk <- cards[grepl("Thomas Walk", cards, fixed = TRUE)]
  expect_length(walk, 1L)
  expect_match(walk, '<span class="fh-avatar fh-avatar-mono" aria-hidden="true">TW</span>', fixed = TRUE)
  expect_false(grepl("fh-bio-toggle", walk, fixed = TRUE))
  # each bio toggle has its own id and label
  ids <- unlist(regmatches(cards, gregexpr('id="fh-bio-[a-z0-9-]+"', cards)))
  expect_length(ids, length(cards) - 1L)
  expect_false(anyDuplicated(ids) > 0L)
  html <- as.character(ui)
  expect_false(grepl("Jean-Marc Montpetit|jeanmarc[.]montpetit@videotron[.]ca|mailto:|Copyright:", html))
  expect_match(html, "https://www.linkedin.com/in/salvador-a-gezan-54768a1a/", fixed = TRUE)
  expect_match(html, 'class="fh-footer"', fixed = TRUE)
})

test_that("About keeps package facts, citation, the developing unit and photo credits", {
  html <- as.character(app_about_ui())
  aside <- regmatches(html, regexpr('(?s)<aside class="fh-aside".*?</aside>', html, perl = TRUE))
  expect_length(aside, 1L)
  info <- fieldhub_about_info()
  for (text in c(info$version, fieldhub_license_label(info$license), info$issues,
                 '<code>citation("FielDHub")</code>', "joss.03122", fieldhub_unit()$name, fieldhub_unit()$url,
                 'id="photo-credits"', "aerial view of an NDSU breeding trial", "AI-generated")) {
    expect_match(aside, text, fixed = TRUE)
  }
  expect_false(grepl("+ file LICENSE", aside, fixed = TRUE))
  expect_match(html, 'src="www/about-banner.jpg"', fixed = TRUE)
  expect_identical(fieldhub_license_label("MIT + file LICENSE"), "MIT")
  expect_identical(fieldhub_license_label("GPL-3"), "GPL-3")
})

test_that("the people shown stay complete when credits change", {
  team <- fieldhub_team()
  team <- rbind(team, data.frame(name = "Another Contributor", roles = "ctb", email = "person@example.org"))
  html <- as.character(app_team_ui(team))
  expect_match(html, "Another Contributor", fixed = TRUE)
  expect_match(html, ">AC</span>", fixed = TRUE)
  expect_identical(app_team_initials("Ana Heilman"), "AH")
  expect_identical(app_team_initials("Jean Marc Doe"), "JD")
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

test_that("the Welcome page links every design to its navbar tab", {
  registry <- fieldhub_app_registry()
  html <- as.character(app_home_ui(registry))
  links <- regmatches(html, gregexpr('data-fieldhub-tab="[^"]*"', html))[[1]]
  targets <- gsub('^data-fieldhub-tab="|"$', "", links)
  labels <- vapply(registry, `[[`, "", "label")
  # every design once, by the value its navbar tab carries, plus Help and the
  # footer's photo credits on About
  expect_setequal(setdiff(targets, c("Help", "About Us")), htmltools::htmlEscape(labels))
  expect_identical(sum(targets == "Help"), 1L)
  expect_identical(sum(targets == "About Us"), 1L)
  expect_identical(length(targets), length(labels) + 2L)
  expect_match(html, 'data-fieldhub-anchor="photo-credits"', fixed = TRUE)
  # one row per menu, with a thumbnail and its design count; "New - " shows as a badge
  designs <- app_home_designs(registry)
  query <- htmltools::tagQuery(designs)
  groups <- unique(vapply(registry, `[[`, "", "group"))
  expect_identical(query$find(".fh-family")$length(), length(groups))
  thumbs <- unname(vapply(query$find(".fh-family-intro img")$selectedTags(), function(img) img$attribs$src, ""))
  expect_length(thumbs, length(groups))
  expect_identical(query$find(".fh-new")$length(), sum(startsWith(labels, "New - ")))
  expect_false(grepl(">New - ", as.character(designs), fixed = TRUE))
  expect_match(html, paste(length(labels), "experimental designs"), fixed = TRUE)
  # the example trial and teaching section
  expect_match(html, '<table class="fh-book">', fixed = TRUE)
  expect_match(html, '<p class="fh-scale">', fixed = TRUE)
  expect_match(html, "For teaching", fixed = TRUE)
  expect_false(grepl("{{", html, fixed = TRUE))
  images <- unique(c(thumbs, regmatches(html, gregexpr('src="www/[^"]+"', html))[[1]]))
  images <- unique(gsub('^src="|"$', "", images))
  expect_true(all(c("www/welcome-banner.jpg", "www/map-diagonal.png", "www/heatmap-yield.svg") %in% images))
  for (image in images) {
    expect_true(file.exists(system.file("app", image, package = "FielDHub")), info = image)
  }
})

test_that("shipped page images stay small", {
  www <- system.file("app/www", package = "FielDHub")
  images <- c("welcome-banner.jpg", "help-banner.jpg", "about-banner.jpg", "map-diagonal.png",
              "heatmap-yield.svg", paste0("thumb-", c("unreplicated", "prep", "lattice", "other"), ".svg"))
  sizes <- file.size(file.path(www, images))
  expect_false(anyNA(sizes))
  expect_true(all(sizes < 300 * 1024), info = paste(images[sizes >= 300 * 1024], collapse = ", "))
})
