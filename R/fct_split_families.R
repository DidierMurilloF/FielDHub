#' Split a population of genotypes randomly into several locations.
#' 
#' @description Split a population of genotypes randomly into several locations, with the
#' aim of having approximatelly the same number of replicates of each genotype, line or
#' treatment per location.
#'
#' @param l Number of locations.
#' @param data Data frame with the entry (ENTRY) and the labels of each treatment (NAME)
#' and number of individuals per family group (FAMILY).
#' 
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Ana Heilman [ctb],
#'         Thomas Walk [ctb], 
#'         Johan Aparicio [ctb], 
#'         Richard Horsley [ctb]
#' 
#' @importFrom stats na.omit 
#'
#' @return A list with two elements.
#' \itemize{
#'   \item \code{rowsEachlist} is a table with a summary of cases.
#'   \item \code{data_locations} is a data frame with the entries for each location
#' }
#' 
#'
#' @examples
#' # Example 1: Split a population of 3000 and 200 families into 8 locations. 
#' # Original dataset is been simulated.
#' set.seed(77)
#' N <- 2000; families <- 100
#' ENTRY <- 1:N
#' NAME <- paste0("SB-", 1:N)
#' FAMILY <- vector(mode = "numeric", length = N)
#' x <- 1:N
#' for (i in x) { FAMILY[i] <- sample(1:families, size = 1, replace = TRUE) }
#' gen.list <- data.frame(list(ENTRY = ENTRY, NAME = NAME, FAMILY = FAMILY))
#' head(gen.list)
#' # Now we are going to use the split_families() function.
#' split_population <- split_families(l = 8, data = gen.list)
#' print(split_population)
#' summary(split_population)
#' head(split_population$data_locations,12)
#'
#' @export
split_families <- function(l = NULL, data = NULL) {
  if(!is.data.frame(data)) {
    stop("\n 'split_families()' requires input data to be a data frame.")
  } 
  if (ncol(data) < 3) {
    stop("\n 'split_families()' requires that data have three columns: ENTRY | NAME | FAMILY.")
  gen.list <- na.omit(data[,1:3])
  } 
  colnames(gen.list) <- c("ENTRY", "NAME", "FAMILY")
  fmlys <- factor(gen.list$FAMILY)
  familyLevels <- levels(fmlys)
  LF <- length(familyLevels)
  locations <- 1:l
  Glist_locations <- vector(mode = "list", length = l)
  v <- matrix(nrow = 0, ncol = 3)
  colnames(v) <- c("ENTRY",  "NAME", "FAMILY")
  for (n in 1:l) {Glist_locations[[n]] <- v}
  a <- vector(mode = "numeric", length = LF)
  sp <- 1
  for (fmly in familyLevels) {
    population <- subset(gen.list, gen.list$FAMILY == fmly)
    sj <- nrow(population)
    if (sj %% l == sj) {
      lOptions <- sample(1:l, size = sj, replace = FALSE)
      k <- 1
      for (w in lOptions) {
        Glist_locations[[w]] <- rbind(Glist_locations[[w]], population[k,])
        k <- k + 1
      }
      warning(paste("Family", fmly, "is not in all locations."))
      a[sp] <- 1
    }else if (sj %% l == 0) {
      lOptions <- 1:l
      reps <- sj / l
      z <- split_vectors(sample(1:sj), len_cuts = rep(reps,l))
      for (w in lOptions) {
        k <- as.vector(z[[w]])
        Glist_locations[[w]] <- rbind(Glist_locations[[w]], population[k,])
      }
      a[sp] <- 2
    }else {
      res <- sj %% l
      reps <- (sj-res) / l
      lOptions <- 1:l
      z <- split_vectors(sample(1:sj), len_cuts = c(rep(reps,l),res))
      for (w in lOptions) {
        k <- as.vector(z[[w]])
        Glist_locations[[w]] <- rbind(Glist_locations[[w]], population[k,])
      }
      lRes <- sample(lOptions, size = res, replace = FALSE)
      k <- as.vector(z[[length(z)]])
      s <- 1
      for (x in lRes) {
        Glist_locations[[x]] <- rbind(Glist_locations[[x]], population[k[s],])
        s <- s + 1
      }
      a[sp] <- 3
    }
    sp <- sp + 1
  }
  rowseach <- vector(mode = "numeric", length = l)
  for(i in locations) { rowseach[i] <- nrow(Glist_locations[[i]]) }
  rowsEachlist <- data.frame(list(Location = paste("Location", 1:l), n = rowseach))
  data_locations <- dplyr::bind_rows(Glist_locations)
  data_locations$LOCATION <- rep(paste("Location", 1:l), rowseach)
  output <- list(rowsEachlist = rowsEachlist, data_locations = data_locations,
                 infoDesign = list(id_design = 17))
  class(output) <- "FielDHub"
  return(invisible(output))
}