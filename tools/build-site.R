build_site <- function() {
  # Build from the checkout without changing the user's installed R packages.
  root <- normalizePath(".")
  python <- Sys.getenv("FIELDHUB_DOCS_PYTHON", "python3")
  work <- tempfile("fieldhub-site-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  Sys.setenv(R_USER_CACHE_DIR = file.path(work, "cache"))
  lib <- file.path(work, "library")
  dir.create(lib)
  status <- system2(file.path(R.home("bin"), "R"),
                    c("CMD", "INSTALL", "--preclean", "--clean",
                      paste0("--library=", shQuote(lib)), shQuote(root)))
  if (status != 0L) stop("Package installation failed")
  .libPaths(c(lib, .libPaths()))
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  library(FielDHub)
  docs <- file.path(root, ".mkdocs-src")
  unlink(docs, recursive = TRUE)
  dir.create(docs)
  file.copy(list.files("website/static", full.names = TRUE), docs, recursive = TRUE)
  dir.create(file.path(docs, "man/figures"), recursive = TRUE)
  # Preserve the legacy icon URLs with the current branding.
  logos <- c("man/figures/fieldhub-logo.png", "man/figures/icon.gif")
  file.copy(logos, file.path(docs, "man/figures"))
  file.copy(c(logos, "inst/app/www/favicon.ico"), file.path(docs, "assets"))
  render <- function(source, destination) {
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    input <- file.path(dirname(destination), basename(source))
    file.copy(source, input, overwrite = TRUE)
    title <- rmarkdown::yaml_front_matter(input)$title
    if (is.null(title)) {
      headings <- grep("^#{1,6} ", readLines(input, warn = FALSE), value = TRUE)
      title <- if (length(headings)) sub("^#{1,6} ", "", headings[[1]]) else "FielDHub"
    }
    knitr::opts_chunk$set(error = FALSE, fig.retina = 2)
    rmarkdown::render(input,
      output_format = rmarkdown::md_document(
        variant = "gfm+raw_html", fig_retina = 2,
        pandoc_args = c("--wrap=none", "--lua-filter", file.path(root, "website/heading-ids.lua"),
                        "--lua-filter", file.path(root, "website/math.lua"))),
      output_file = basename(destination), envir = new.env(), quiet = TRUE)
    content <- readLines(destination, warn = FALSE)
    writeLines(c("---", yaml::as.yaml(list(title = title,
      version = as.character(packageVersion("FielDHub")), description = paste(title, "— examples and documentation for FielDHub."))),
      "---", content), destination)
    unlink(input)
  }
  render("index.Rmd", file.path(docs, "index.md"))
  dir.create(file.path(docs, "articles"), recursive = TRUE)
  for (source in list.files("vignettes", pattern = "[.]Rmd$", full.names = TRUE, recursive = TRUE)) {
    relative <- sub("^vignettes/", "", source)
    destination <- file.path(docs, "articles", sub("[.]Rmd$", ".md", relative))
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    file.copy("vignettes/references.bib", file.path(dirname(destination), "references.bib"), overwrite = TRUE)
    render(source, destination)
  }
  # pkgdown executes examples and resolves Rd cross-references using the package index.
  pkgdown::build_reference(pkg = pkgdown::as_pkgdown(root, override = list(destination = file.path(work, "reference-site"))),
                           lazy = FALSE, examples = TRUE, devel = FALSE)
  ref_dir <- file.path(work, "reference-site/reference")
  dir.create(file.path(docs, "reference"), showWarnings = FALSE)
  for (source in list.files(ref_dir, pattern = "[.]html$", full.names = TRUE)) {
    html <- xml2::read_html(source)
    main <- xml2::xml_find_first(html, "//main")
    xml2::xml_remove(xml2::xml_find_all(main, ".//a[contains(@class,'anchor')] | .//div[contains(@class,'d-none')]"))
    html_file <- file.path(work, "topic.html")
    writeLines(as.character(main), html_file)
    destination <- file.path(docs, "reference", sub("[.]html$", ".md", basename(source)))
    rmarkdown::pandoc_convert(html_file, from = "html", to = "gfm+raw_html",
                              output = destination, options = c("--wrap=none", "--lua-filter", file.path(root, "website/reference.lua")))
  }
  assets <- list.files(ref_dir, full.names = TRUE)
  assets <- assets[!grepl("[.](html|md)$", assets)]
  file.copy(assets, file.path(docs, "reference"), recursive = TRUE)
  dir.create(file.path(docs, "news"))
  file.copy("NEWS.md", file.path(docs, "news/index.md"))
  for (source in c("CONTRIBUTING.md", "CODE_OF_CONDUCT.md", "LICENSE.md", "RELEASING.md")) {
    file.copy(source, docs)
  }
  description <- read.dcf("DESCRIPTION")[1, ]
  authors <- eval(parse(text = description[["Authors@R"]]))
  roles <- c(aut = "Author", cre = "Maintainer", ctb = "Contributor", cph = "Copyright holder")
  author_lines <- vapply(seq_along(authors), function(i) {
    person <- authors[i]
    name <- paste(c(person$given, person$family), collapse = " ")
    paste0("- **", name, "** — ", paste(unname(roles[person$role]), collapse = ", "))
  }, character(1))
  # The manual citation identifies the version and year of this documentation build.
  year <- format(Sys.Date(), "%Y")
  package_citation <- utils::bibentry(
    bibtype = "Manual", title = paste0("FielDHub: ", description[["Title"]]),
    author = authors[vapply(seq_along(authors), function(i) "aut" %in% authors[i]$role, logical(1))],
    year = year, note = paste("R package version", description[["Version"]]),
    url = "https://cran.r-project.org/package=FielDHub"
  )
  writeLines(c("# Authors and citation", "", author_lines, "",
               "## Package citation", "",
               paste0("Citation for version ", description[["Version"]], " as documented in ", year, "."),
               "", format(package_citation, style = "text"), "",
               "```bibtex", format(package_citation, style = "Bibtex"), "```", "",
               "## JOSS paper", "",
               "Murillo et al. (2021). FielDHub: A Shiny App for Design of Experiments in Life Sciences.",
               "Journal of Open Source Software, 6(61), 3122.",
               "[Read the paper](https://joss.theoj.org/papers/10.21105/joss.03122)."),
             file.path(docs, "authors.md"))
  writeLines(c("# License", readLines("LICENSE")), file.path(docs, "LICENSE-text.md"))
  topics <- getFromNamespace("fieldhub_help_topics", "FielDHub")()
  write.csv(topics, file.path(docs, "topics.csv"), row.names = FALSE)
  writeLines(unique(c(topics$url, topics$guide)), file.path(docs, "app-urls.txt"))
  if (system2(python, c("tools/prepare-site.py")) != 0L) stop("Site preparation failed")
  if (system2(python, c("-m", "mkdocs", "build", "--strict")) != 0L) stop("MkDocs build failed")
  if (system2(python, "tools/check-site.py") != 0L) stop("URL check failed")
  unlink(work, recursive = TRUE)
}

build_site()
