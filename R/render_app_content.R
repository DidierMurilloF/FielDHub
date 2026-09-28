#' A footer whose year is supplied at rendering time
#' @noRd
fieldhub_footer <- function(year = format(Sys.Date(), "%Y")) {
  if (length(year) != 1L || is.na(year) || !grepl("^[0-9]{4}$", as.character(year))) {
    fieldhub_abort("Footer year must contain four digits.")
  }
  paste0('<footer class="fieldhub-footer"><p>&copy; ', year,
    ', <a href="https://sites.google.com/ndsu.edu/plsc-bpdm/home" target="_blank" ',
    'rel="noopener noreferrer"><span class="footer-label">NDSU Big Data Pipeline.</span></a></p></footer>')
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
