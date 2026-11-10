ARCBD_plot_number <- function(plot.number = NULL, 
                              planter = "serpentine", 
                              b = NULL, 
                              name.expt = NULL, 
                              Fillers = NULL,
                              nameEXPT = NULL) {
  validate_planter(planter)
  datos_name <- nameEXPT
  nrows <- b
  ncols <- ncol(datos_name)
  datos_name = as.matrix(datos_name) 
  plot_n_start <- plot.number
  if (!is.null(name.expt)) { 
    Name_expt <- name.expt  
  }else Name_expt = paste0(rep("ARCBD", times = 1), 1)
  for (i in plot_n_start) {
    my_split_plot_nub <- plot_number(
      planter = planter,
      plot_number_start = i,
      layout_names = datos_name,
      expe_names = Name_expt,
      fillers = 0
    )[[1]]
  }

  plot_num1 <- my_split_plot_nub
  if (Fillers > 0) {
    plot_num1[1, filler_columns(nrows, ncols, planter, Fillers)] <- 0
  }
  return(list(plot_num = plot_num1))
}

#' Resolve non-overlapping block-number ranges for each location and rep
#' @noRd
arcbd_block_plot_starts <- function(plotNumber, l, repsExpt, b, plots_per_block,
                                    plotIncrement) {
  if (plotIncrement < plots_per_block) {
    fieldhub_abort("`plotIncrement` must be at least ", plots_per_block,
                   " (the number of plots per block) to avoid overlapping plot numbers.",
                   data = list(argument = "plotIncrement", minimum = plots_per_block))
  }
  starts <- if (length(plotNumber) == l) {
    outer(plotNumber, seq_len(repsExpt) - 1, function(start, rep) {
      start + rep * as.double(b) * plotIncrement
    })
  } else if (length(plotNumber) == repsExpt) {
    matrix(rep(plotNumber, each = l), nrow = l)
  } else {
    matrix(plotNumber, nrow = l, byrow = TRUE)
  }
  span <- (as.double(b) - 1) * plotIncrement + plots_per_block
  if (any(!is.finite(starts)) || any(starts > (2^53 - 1) - (span - 1))) {
    fieldhub_abort("The block plot numbers exceed the range of exactly representable integers. ",
                   "Reduce `plotNumber` or `plotIncrement`.",
                   data = list(argument = "plotNumber"))
  }
  for (location in seq_len(l)) {
    if (any(diff(sort(starts[location, ])) < span)) {
      fieldhub_abort("`plotNumber` ranges must not overlap across reps within each location ",
                     "when `plotIncrement` is used. Supply starts at least ", span, " apart.",
                     data = list(argument = "plotNumber", location = location))
    }
  }
  starts
}

#' Number each actual block in planting order without changing its entries
#' @noRd
arcbd_number_by_block <- function(layout, blocks, start, increment, planter) {
  path <- field_path(nrow(layout), ncol(layout), planter)
  active <- layout[path] != "Filler"
  by_block <- split(which(active), blocks[path][active])
  numbers <- matrix(0, nrow = nrow(layout), ncol = ncol(layout))
  for (block in names(by_block)) {
    cells <- path[by_block[[block]], , drop = FALSE]
    numbers[cells] <- start + (as.numeric(block) - 1) * increment + seq_len(nrow(cells)) - 1
  }
  numbers
}
