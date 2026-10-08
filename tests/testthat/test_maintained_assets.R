test_that("served fragments get their year and team only at render time", {
  skip_if_not_installed("shiny")
  for (name in c("home.html", "aboutUs.html")) {
    html <- paste(readLines(system.file("app/www", name, package = "FielDHub")), collapse = "\n")
    expect_false(grepl("&copy; [0-9]{4}|Didier Murillo|Salvador Gezan", html))
    expect_false(grepl("style=", html, fixed = TRUE))
    expect_match(html, "{{ footer }}", fixed = TRUE)
  }
  about <- as.character(app_about_ui())
  team <- fieldhub_team()
  visible <- team$name[team$name != "Jean-Marc Montpetit" & !grepl("(^|, )cph($|, )", team$roles)]
  for (name in visible) expect_match(about, name, fixed = TRUE)
  expect_false(grepl("Jean-Marc Montpetit|jeanmarc[.]montpetit@videotron[.]ca", about))
  expect_match(about, format(Sys.Date(), "%Y"), fixed = TRUE)
})

test_that("app code does not write inline styles", {
  functions <- app_functions()
  inline <- names(Filter(function(f) grepl("style[[:space:]]*=", paste(deparse(body(f)), collapse = " ")), functions))
  expect_identical(length(inline), 0L, info = paste(inline, collapse = ", "))
})

test_that("local assets do not request remote resources", {
  root <- system.file("app/www", package = "FielDHub")
  files <- list.files(root, pattern = "[.](html|css|js)$", full.names = TRUE)
  for (file in files) {
    text <- paste(readLines(file, warn = FALSE), collapse = "\n")
    # Outbound hyperlinks and XML namespace identifiers do not fetch assets.
    text <- gsub('<a[^>]*href="https?://[^"]*"[^>]*>', "", text, perl = TRUE)
    namespaces <- c(
      "http://schemas.openxmlformats.org/spreadsheetml/2006/main",
      "http://schemas.openxmlformats.org/officeDocument/2006/relationships",
      "http://schemas.openxmlformats.org/package/2006/relationships",
      "http://schemas.openxmlformats.org/package/2006/content-types",
      "http://www.w3.org/XML/1998/namespace")
    for (namespace in namespaces) text <- gsub(namespace, "", text, fixed = TRUE)
    expect_false(grepl("https?://", text), info = basename(file))
  }
})
