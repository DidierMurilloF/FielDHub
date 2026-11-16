# Run from the repository root with this checkout of FielDHub installed.
build_site <- function() {
  source_dir <- normalizePath("website", mustWork = TRUE)
  docs <- ".mkdocs-src"
  unlink(docs, recursive = TRUE)
  dir.create(docs)
  docs <- normalizePath(docs)
  for (folder in c("articles", "reference", "assets/home")) {
    dir.create(file.path(docs, folder), recursive = TRUE)
  }
  copy <- function(from, to) {
    if (!all(file.copy(from, to, recursive = TRUE))) {
      stop("Could not copy ", paste(from, collapse = ", "))
    }
  }
  copy(list.files(file.path(source_dir, "static"), full.names = TRUE), docs)
  copy(c("man/figures/icon.gif", "inst/app/www/favicon.ico"), file.path(docs, "assets"))

  version <- read.dcf("DESCRIPTION", fields = "Version")[[1L]]
  if (as.character(utils::packageVersion("FielDHub")) != version) {
    stop("Install FielDHub ", version, " from this checkout before rendering.")
  }
  # Each page executes in its own knitting environment.
  library(FielDHub)
  set.seed(42)

  render <- function(input, title, description, metadata = list(), pandoc_args = NULL) {
    input <- normalizePath(input, mustWork = TRUE)
    format <- rmarkdown::md_document(
      variant = "gfm+raw_html", fig_width = 8, fig_height = 5.5,
      pandoc_args = c("--wrap=none", pandoc_args)
    )
    output <- rmarkdown::render(input, output_format = format,
                                envir = new.env(parent = globalenv()), quiet = TRUE)
    content <- readLines(output, warn = FALSE)
    metadata <- c(list(title = title, description = description), metadata)
    yaml <- trimws(yaml::as.yaml(metadata), which = "right")
    writeLines(c("---", yaml, "---", "", content), output)
    unlink(input)
  }

  local_article <- "articles/diagonal_arrangement.html"
  local_reference <- "reference/diagonal_arrangement.md"
  topics <- getFromNamespace("fieldhub_help_topics", "FielDHub")()
  label <- sub("^New - ", "", topics$label)
  links <- ifelse(endsWith(topics$guide, paste0("/", local_article)),
                  sub("\\.html$", ".md", local_article), topics$guide)
  design_table <- c("| Shiny app menu | Design workflow |", "| --- | --- |",
                    sprintf("| %s | [%s](%s) |", topics$group, label, links))

  # Reuse the two executable examples in index.Rmd and give their plots stable names.
  index <- readLines("index.Rmd", warn = FALSE)
  starts <- grep("^### Using the FielDHub function", index)
  ends <- grep("^To see more examples", index)
  if (length(starts) != 2L || length(ends) != 2L || any(ends <= starts)) {
    stop("Could not locate the two worked examples in index.Rmd.")
  }
  example <- function(i, plot_label, link) {
    lines <- index[seq.int(starts[i] + 1L, ends[i] - 1L)]
    figure <- grep("^```\\{r, fig\\.align", lines)
    if (length(figure) != 1L) stop("Expected one plot in each home page example.")
    lines[figure] <- sub("{r,", paste0("{r ", plot_label, ","), lines[figure], fixed = TRUE)
    c(lines, link, "")
  }

  home <- file.path(docs, "index.Rmd")
  setup <- c("```{r, include=FALSE}",
             'knitr::opts_chunk$set(fig.path = "assets/home/", collapse = TRUE, error = FALSE)',
             "```", "")
  writeLines(c(
    setup,
    readLines(file.path(source_dir, "home.Rmd"), warn = FALSE),
    "", design_table, "", "## Worked examples", "",
    "### Diagonal arrangement", "",
    "Allocate 280 genotypes and four checks in a 16 × 20 field split into three experiments.",
    example(1L, "diagonal-map",
            sprintf("[Diagonal arrangement guide](%s) · [Function reference](%s)",
                    sub("\\.html$", ".md", local_article), local_reference)),
    "### Partially replicated design", "",
    "Allocate 138 single-copy entries and 75 entries with two copies in a 16 × 18 field.",
    example(2L, "prep-map", paste0(
      "[Partially replicated design guide](",
      "https://didiermurillof.github.io/FielDHub/articles/partially_replicated.html)")),
    "## Teaching", "",
    "Use the Shiny interface or the R functions to teach randomization, replication, and blocking.",
    "Students can compare field layouts, inspect field books, and repeat seeded designs.",
    "", "## Citation", "",
    paste0("Murillo, D. A., Gezan, S. A., Heilman, A. M., Walk, T. C., Aparicio, J. S., ",
           "& Horsley, R. D. (2021). FielDHub: A Shiny App for Design of Experiments in Life Sciences. ",
           "*Journal of Open Source Software*, 6(61), 3122. [doi:10.21105/joss.03122](https://doi.org/10.21105/joss.03122).")
  ), home)
  render(home, "Design of Experiments in R and Shiny",
         "Design field trials with FielDHub, an open-source R package and Shiny app. Explore 21 experimental designs, field maps, and worked R examples.",
         metadata = list(version = version, hide = "navigation"))

  article <- file.path(docs, "articles", "diagonal_arrangement.Rmd")
  copy(c("vignettes/diagonal_arrangement.Rmd", "vignettes/references.bib"), dirname(article))
  article_source <- readLines(article, warn = FALSE)
  plot_lines <- which(article_source %in% c("plot(single_diag)", "plot(multi_diag)"))
  alts <- c("Single unreplicated field layout with colored check plots on diagonals",
            "Multiple unreplicated experiments shaded by block, with colored checks on diagonals")
  article_source[plot_lines - 1L] <- paste0(
    sub("}$", "", article_source[plot_lines - 1L]), ", fig.alt='", alts, "'}"
  )
  article_source[article_source == "# References"] <- "## References"
  article_source <- gsub("(?<=\\w)\\[@", " [@", article_source, perl = TRUE)
  writeLines(article_source, article)
  render(article, "Unreplicated Diagonal Arrangement Design",
         "Design single and multiple unreplicated field trials with diagonal checks. Follow the FielDHub Shiny and R examples and view the generated field maps.",
         pandoc_args = c("--lua-filter", file.path(source_dir, "heading-ids.lua")))
  unlink(file.path(dirname(article), "references.bib"))
  article_md <- sub("\\.Rmd$", ".md", article)
  cat("\n[diagonal_arrangement() reference](../reference/diagonal_arrangement.md)\n",
      file = article_md, append = TRUE)

  rd <- tools::parse_Rd("man/diagonal_arrangement.Rd")
  tags <- vapply(rd, function(node) attr(node, "Rd_tag"), character(1))
  body <- rd[tags != "\\examples"]
  class(body) <- "Rd"
  html <- tempfile(fileext = ".html")
  on.exit(unlink(html))
  tools::Rd2HTML(body, out = html, no_links = TRUE)
  ref_html <- xml2::read_html(html)
  main <- xml2::xml_find_first(ref_html, "//main")
  xml2::xml_remove(xml2::xml_find_all(main, "./table[1] | ./h2[1]"))
  headings <- xml2::xml_find_all(main, "./h3")
  xml2::xml_name(headings) <- "h2"
  arguments <- xml2::xml_find_first(main, "./h2[.='Arguments']/following-sibling::table[1]")
  if (!inherits(arguments, "xml_missing")) {
    header <- xml2::read_html("<table><thead><tr><th>Argument</th><th>Description</th></tr></thead></table>")
    first_row <- xml2::xml_find_first(arguments, "./tr[1]")
    xml2::xml_add_sibling(first_row, xml2::xml_find_first(header, "//thead"), .where = "before")
    xml2::xml_attr(arguments, "role") <- NULL
  }
  writeLines(as.character(xml2::xml_children(main)), html)
  rmarkdown::pandoc_convert(html, from = "html", to = "gfm",
                            options = "--wrap=none", output = sub("\\.html$", ".md", html))
  ref_body <- sub("\\.html$", ".md", html)
  examples <- tempfile(fileext = ".R")
  on.exit(unlink(c(ref_body, examples)), add = TRUE)
  tools::Rd2ex(rd, out = examples, commentDontrun = TRUE, commentDonttest = TRUE)
  ref <- file.path(docs, "reference", "diagonal_arrangement.Rmd")
  writeLines(c(
    readLines(ref_body, warn = FALSE), "", "## Examples", "",
    "```{r, collapse=TRUE, comment='#>', error=FALSE}",
    readLines(examples, warn = FALSE)[-(1:5)],
    "```", "", "[Diagonal arrangement guide](../articles/diagonal_arrangement.md)"
  ), ref)
  render(ref, "diagonal_arrangement()",
         "Generate unreplicated diagonal field trials in R. See the FielDHub function signature, arguments, return values, reproducibility notes, and examples.")
  ref_md <- sub("\\.Rmd$", ".md", ref)
  content <- readLines(ref_md, warn = FALSE)
  anchors <- c("## Usage" = "ref-usage", "## Author(s)" = "author",
               "## Examples" = "ref-examples")
  matches <- match(content, names(anchors))
  found <- !is.na(matches)
  content[found] <- paste0(content[found], " {#", anchors[matches[found]], "}")
  writeLines(content, ref_md)
}

build_site()
