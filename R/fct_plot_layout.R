#' Generates field layouts plots
#'
#' @description It generates field layout plots for experimental designs.
#'
#' @param x An FielDHub object.
#' @param optionLayout Type of layout field.
#' @param planter option to order of planter
#' @param l a especific location
#' @param orderReps Order of reps in the field layout
#'
#'
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Ana Heilman [ctb],
#'         Thomas Walk [ctb], 
#'         Johan Aparicio [ctb], 
#'         Richard Horsley [ctb]
#'         
#'
#' @return A list with four elements.
#' \itemize{
#'   \item \code{out_layout} is a the layout plot.
#'   \item \code{fieldBookXY} is a data frame with the field book design.
#'   \item \code{newBooks} is a list with the all field books combinations.
#' }
#'
#' @references
#' Kevin Wright (2020). desplot: Plotting Field Plans for Agricultural Experiments. R package version 1.8.
#' https://CRAN.R-project.org/package=desplot
#'
#' @examples
#' # Example 1: Generates a field layout for an alpha lattice designs with 25 treatments and 4 reps.
#' \dontrun{
#' alpha <- alpha_lattice(t = 20, k = 4, r = 4, l = 1, seed = 101)
#' layoutPlot_a <- plot_layout(x = alpha, optionLayout = 1, planter = "serpentine")
#' layoutPlot_a$plot_layout
#' layoutPlot_a$df
#' }
#'
#' @export
plot_layout <- function(x = NULL, 
                        optionLayout = 1,
                        planter = "serpentine", 
                        l = 1, 
                        orderReps = "vertical_stack_panel") {
  if (class(x) != "FielDHub") stop("x is not a FielDHub class object")
  if (x$infoDesign$id_design %in% c(10, 11, 12, 8, 5, 6)) {
    if (x$infoDesign$id_design %in% c(10, 11, 12, 8)) {
      n_TrtGen <- dplyr::n_distinct(x$fieldBook$ENTRY)
      n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
      if (x$infoDesign$id_design != 9) {
        sizeIblocks <- dplyr::n_distinct(x$fieldBook$UNIT)
      } else {
        sizeIblocks <- dplyr::n_distinct(x$fieldBook$ROW)
      }
      iBlocks <- n_TrtGen/sizeIblocks
      return0 <- plot_iblocks_1(x = x, n_TrtGen = n_TrtGen, n_Reps = n_Reps, 
                              sizeIblocks = sizeIblocks, iBlocks = iBlocks, 
                              optionLayout = optionLayout, planter = planter,
                              l = l, orderReps = orderReps)
    } else if (x$infoDesign$id_design == 5) {
        if (x$infoDesign$typeDesign == "RCBD") {
          wp <- dplyr::n_distinct(x$fieldBook$WHOLE_PLOT)
          sp <- dplyr::n_distinct(x$fieldBook$SUB_PLOT)
          n_TrtGen <- wp * sp
          n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
          sizeIblocks <- sp
          iBlocks <- wp
          return0 <- plot_splitPlots(x = x, n_TrtGen = n_TrtGen, n_Reps = n_Reps, 
                                    sizeIblocks = sizeIblocks, iBlocks = iBlocks, 
                                    optionLayout = optionLayout, planter = planter,
                                    l = l, orderReps = orderReps)
        } else {
          wp <- dplyr::n_distinct(x$fieldBook$WHOLE_PLOT)
          sp <- dplyr::n_distinct(x$fieldBook$SUB_PLOT)
          n_TrtGen <- wp * sp
          n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
          return0 <- plot_CRD(x = x, n_TrtGen = n_TrtGen, 
                              n_Reps = n_Reps, 
                              optionLayout = optionLayout, 
                              planter = planter, l = l)
        }
    } else if (x$infoDesign$id_design == 6) {
        if (x$infoDesign$typeDesign == "RCBD") {
          wp <- dplyr::n_distinct(x$fieldBook$WHOLE_PLOT)
          sp <- dplyr::n_distinct(x$fieldBook$SUB_PLOT)
          ssp <- dplyr::n_distinct(x$fieldBook$SUB_SUB_PLOT)
          n_TrtGen <- dplyr::n_distinct(x$fieldBook$TRT_COMB)
          n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
          sizeIblocks <- ssp
          sizeIblocks <- as.numeric(sizeIblocks)
          iBlocks <- wp * sp
          return0 <- plot_splitPlots(x = x, n_TrtGen = n_TrtGen, n_Reps = n_Reps, 
                                    sizeIblocks = sizeIblocks, iBlocks = iBlocks, 
                                    optionLayout = optionLayout, planter = planter, 
                                    l = l)
        } else {
          wp <- dplyr::n_distinct(x$fieldBook$WHOLE_PLOT)
          sp <- dplyr::n_distinct(x$fieldBook$SUB_PLOT)
          ssp <- dplyr::n_distinct(x$fieldBook$SUB_SUB_PLOT)
          n_TrtGen <- dplyr::n_distinct(x$fieldBook$TRT_COMB)
          n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
          return0 <- plot_CRD(x = x, n_TrtGen = n_TrtGen, 
                              n_Reps = n_Reps, 
                              optionLayout = optionLayout, 
                              planter = planter, l = l)
        }
    }
    return(list(out_layout = return0$p1, 
                out_layoutPlots = return0$p2, 
                fieldBookXY = return0$df, 
                newBooks = return0$newBooks, 
                allSitesFieldbook = return0$allSitesFieldbook))
  } else if (x$infoDesign$id_design %in% c(1, 2, 4)) {
      if (x$infoDesign$id_design == 1) {
        n_TrtGen <- dplyr::n_distinct(x$fieldBook$TREATMENT)
        n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
        return <- plot_CRD(x = x, n_TrtGen = n_TrtGen, n_Reps = n_Reps, 
                           optionLayout = optionLayout, 
                           planter = planter, l = 1)
        return(list(out_layout = return$p1, out_layoutPlots = return$p2, fieldBookXY = return$df, 
                    newBooks = return$newBooks, allSitesFieldbook = return$allSitesFieldbook))
      } else if (x$infoDesign$id_design == 2) {
        n_TrtGen <- dplyr::n_distinct(x$fieldBook$TREATMENT)
        n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
        return <- plot_RCBD(x = x, n_TrtGen = n_TrtGen, 
                            n_Reps = n_Reps, 
                            optionLayout = optionLayout, 
                            planter = planter, 
                            orderReps = orderReps,
                            l = l)
        return(list(out_layout = return$p1, out_layoutPlots = return$p2, fieldBookXY = return$df, 
                    newBooks = return$newBooks, allSitesFieldbook = return$allSitesFieldbook))
      } else if (x$infoDesign$id_design == 4 & x$infoDesign$kind == "RCBD") {
        n_TrtGen <- dplyr::n_distinct(x$fieldBook$TRT_COMB)
        n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
        return <- plot_RCBD(x = x, n_TrtGen = n_TrtGen, n_Reps = n_Reps, optionLayout = optionLayout, 
                           planter = planter, orderReps = orderReps, l = l)
        return(list(out_layout = return$p1, out_layoutPlots = return$p2, fieldBookXY = return$df, 
                    newBooks = return$newBooks, allSitesFieldbook = return$allSitesFieldbook))
      } else if (x$infoDesign$id_design == 4 & x$infoDesign$kind == "CRD") {
        n_TrtGen <- dplyr::n_distinct(x$fieldBook$TRT_COMB)
        n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
        return <- plot_CRD(x = x, n_TrtGen = n_TrtGen, n_Reps = n_Reps, optionLayout = optionLayout, 
                           planter = planter, l = l)
        return(list(out_layout = return$p1, out_layoutPlots = return$p2, fieldBookXY = return$df, 
                    newBooks = return$newBooks, allSitesFieldbook = return$allSitesFieldbook))
      }

  } else if (x$infoDesign$id_design %in% c(3, 7, 9)) { # 7, 9 
    if (x$infoDesign$id_design == 3) {
      rsRep <- dplyr::n_distinct(x$fieldBook$COLUMN)
      csRep <- dplyr::n_distinct(x$fieldBook$ROW)
      dims <- c(rsRep, csRep)
      n_Reps <- dplyr::n_distinct(x$fieldBook$SQUARE)
      return2 <- plot_latinSQ(x = x, dims = dims, n_Reps = n_Reps, 
                              optionLayout = optionLayout, 
                              planter = planter, l = l, 
                              orderReps = orderReps)
    } else if (x$infoDesign$id_design == 7) {
      rsRep <- dplyr::n_distinct(x$fieldBook$HSTRIP)
      csRep <- dplyr::n_distinct(x$fieldBook$VSTRIP)
      dims <- c(rsRep, csRep)
      n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
      return2 <- plot_latinSQ(x = x, dims = dims, 
                              n_Reps = n_Reps, 
                              optionLayout = optionLayout,
                              planter = planter, 
                              l = l, 
                              orderReps = orderReps)
    } else if (x$infoDesign$id_design == 9) {
      rsRep <- dplyr::n_distinct(x$fieldBook$ROW)
      csRep <- dplyr::n_distinct(x$fieldBook$COLUMN)
      dims <- c(rsRep, csRep)
      n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
      return2 <- plot_latinSQ(x = x, dims = dims, 
                              n_Reps = n_Reps, 
                              optionLayout = optionLayout,
                              planter = planter, 
                              l = l, 
                              orderReps = orderReps)
    }
    return(list(out_layout = return2$p1, out_layoutPlots = return2$p2, fieldBookXY = return2$df, 
                newBooks = return2$newBooks, allSitesFieldbook = return2$allSitesFieldbook))
  }
}