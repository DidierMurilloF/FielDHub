#' The unit FielDHub is developed at, shown in the footer, Help and About
#' @noRd
fieldhub_unit <- function() {
  list(name = "Agricultural Data Analytics",
       url = "https://www.ndsu.edu/agricultural-data-analytics",
       description = paste("The unit applies AI, statistics and data management tools to help NDSU",
                           "researchers and Extension specialists turn agricultural data into decisions."))
}

#' A footer whose year is supplied at rendering time
#'
#' Shared by the Welcome, Help and About pages. "Photo credits" opens the
#' About tab at its credits (\code{home-links.js}).
#' @noRd
fieldhub_footer <- function(year = format(Sys.Date(), "%Y")) {
  if (length(year) != 1L || is.na(year) || !grepl("^[0-9]{4}$", as.character(year))) {
    fieldhub_abort("Footer year must contain four digits.")
  }
  unit <- fieldhub_unit()
  link <- function(label, url) {
    paste0('<li><a href="', url, '" target="_blank" rel="noopener noreferrer">', label, "</a></li>")
  }
  paste0('<footer class="fh-footer"><div class="fh-inner"><p>&copy; ', year,
    ' <a class="fh-org" href="', unit$url, '" target="_blank" rel="noopener noreferrer">', unit$name,
    "</a>, NDSU</p><ul>",
    link("CRAN", "https://cran.r-project.org/package=FielDHub"),
    link("Source code", "https://github.com/DidierMurilloF/FielDHub"),
    link("FielDHub paper", "https://joss.theoj.org/papers/10.21105/joss.03122"),
    '<li><a href="#" data-fieldhub-tab="About Us" data-fieldhub-anchor="photo-credits">Photo credits</a></li>',
    "</ul></div></footer>")
}

#' Maintained contributor names, roles and contact details
#' @noRd
fieldhub_team <- function(desc = utils::packageDescription("FielDHub")) {
  people <- eval(parse(text = desc[["Authors@R"]]),
                  envir = list(person = utils::person), enclos = baseenv())
  data.frame(
    name = vapply(people, function(person) paste(c(person$given, person$family), collapse = " "), ""),
    roles = vapply(people, function(person) paste(person$role, collapse = ", "), ""),
    email = vapply(people, function(person) paste(person$email, collapse = ", "), ""),
    stringsAsFactors = FALSE
  )
}
