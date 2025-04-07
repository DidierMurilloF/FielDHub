#' Generate Vertical Layouts for RCBD Designs
#'
#' @description
#' This function creates a set of vertical layout options for a randomized complete block design (RCBD)
#' using a subset of field book data. It generates a basic vertical grid layout (with a specified number
#' of replicates and treatments per replicate), extended factor-based layouts when the number of treatments
#' has multiple prime factors, and a single-column layout.
#'
#' @param NewBook A data frame containing the field book data for a single location. Typically, this is a subset
#'   of the FielDHub fieldBook.
#' @param plots A numeric or character vector representing the plot identifiers.
#' @param n_TrtGen An integer specifying the total number of treatments.
#' @param n_Reps An integer specifying the number of replicates (blocks).
#' @param planter A character string indicating the plot numbering scheme.
#' @noRd
generate_vertical_layout <- function(NewBook, plots, n_TrtGen, n_Reps, planter) {
  layouts <- list()

  # 1) Basic vertical layout: (n_Reps) rows, (n_TrtGen) columns
  basic_df <- NewBook |>
    dplyr::mutate(
      ROW = rep(1:n_Reps, each = n_TrtGen),
      COLUMN = rep(1:n_TrtGen, times = n_Reps)
    )
  layouts[["basic_vertical"]] <- basic_df

  # 2) Extended vertical factor-based layouts
  pf <- numbers::primeFactors(n_TrtGen)
  if (length(pf) >= 2) {
    factor_combos <- as.data.frame(
      factor_subsets(n_TrtGen, all_factors = TRUE)$comb_factors
    )
    for (i in seq_len(nrow(factor_combos))) {
      s1 <- as.numeric(factor_combos[i, 1])
      s2 <- as.numeric(factor_combos[i, 2])

      df <- NewBook |>
        dplyr::mutate(
          ROW = rep(1:(s1 * n_Reps), each = s2),
          COLUMN = rep(rep(1:s2, times = s1), times = n_Reps)
        )
      nCols <- max(df$COLUMN)

      df$PLOT <- planter_transform(
        plots   = plots,
        planter = planter,
        reps    = n_Reps,
        cols    = nCols,
        units   = NULL
      )
      layouts[[paste0("vertical_ext_", i)]] <- df
    }
  }

  # 3) Single-column layout (all plots in a single column) as the LAST option
  single_column_df <- NewBook |>
    dplyr::mutate(
      ROW = 1:(n_TrtGen * n_Reps),
      COLUMN = 1
    )
  layouts[["single_column"]] <- single_column_df

  return(layouts)
}



#' Generate Horizontal Layouts for RCBD Designs
#'
#' @description
#' This function creates a set of horizontal layout options for a randomized complete block design (RCBD)
#' using a subset of field book data. It produces a basic horizontal grid layout and, when applicable,
#' generates extended factor-based layouts.
#'
#' @param NewBook A data frame containing the field book data for a single location. Typically, this is a subset
#'   of the FielDHub fieldBook.
#' @param plots A numeric or character vector representing the plot identifiers.
#' @param n_TrtGen An integer specifying the total number of treatments.
#' @param n_Reps An integer specifying the number of replicates (blocks).
#' @param planter A character string indicating the plot numbering scheme.
#' @noRd
generate_horizontal_layout <- function(NewBook, plots, n_TrtGen, n_Reps, planter) {
  layouts <- list()

  # B. Basic horizontal grid
  basic_horizontal_df <- NewBook |>
    dplyr::mutate(
      ROW    = rep(1:n_TrtGen, times = n_Reps),
      COLUMN = rep(1:n_Reps, each = n_TrtGen)
    )
  layouts[["basic_horizontal"]] <- basic_horizontal_df

  # C. Extended factor-based layouts
  factor_combos <- as.data.frame(
    factor_subsets(n_TrtGen, all_factors = TRUE)$comb_factors
  )
  if (nrow(factor_combos) > 0) {
    for (i in seq_len(nrow(factor_combos))) {
      s1 <- as.numeric(factor_combos[i, 1])
      s2 <- as.numeric(factor_combos[i, 2])

      # Assign columns per replication
      w <- 1:(s1 * n_Reps)
      u <- seq(1, length(w), by = s1)
      v <- seq(s1, length(w), by = s1)
      z <- unlist(lapply(1:n_Reps, function(j) rep(u[j]:v[j], times = s2)))

      df <- NewBook |>
        dplyr::mutate(
          ROW    = rep(rep(1:s2, each = s1), n_Reps),
          COLUMN = z
        )
      nCols <- max(df$COLUMN)

      df$PLOT <- planter_transform(
        plots   = plots,
        planter = planter,
        reps    = n_Reps,
        cols    = nCols,
        mode    = "Horizontal",
        units   = NULL
      )
      layouts[[paste0("horizontal_ext_", i)]] <- df
    }
  }

  return(layouts)
}




#' Generate and Visualize RCBD Field Layouts
#'
#' @description
#' This function generates various row-column layout options for a randomized complete block design (RCBD) using data from a FielDHub object. It then optionally plots the selected layout with \pkg{desplot}, creating both a field layout plot (showing treatments) and a plot-number layout plot (showing plot IDs).
#'
#' @param x An object containing the RCBD \code{fieldBook} data. Typically a FielDHub object.
#' @param n_TrtGen A numeric value specifying the total number of treatments.
#' @param n_Reps A numeric value specifying the number of replicates (blocks).
#' @param layout An integer indicating which layout option to select from the generated list of possible layouts.
#' @param stacked A character string specifying how plots should be arranged.
#'   Possible values are \code{"vertical"} or \code{"horizontal"}. Defaults to \code{"horizontal"}.
#' @param planter A character string indicating how plots should be numbered
#'   (e.g., \code{"serpentine"} or another style recognized by
#'   \code{\link[FielDHub:planter_transform]{FielDHub:::planter_transform}}).
#' @param l An integer specifying which site (location) to process if multiple locations are present. Defaults to 1.
#'
#' @details
#' Internally, the function generates multiple layout possibilities for each location in the \code{x$fieldBook} data. These layouts can vary based on how treatments and replicates are arranged into rows and columns.
#'
#' \itemize{
#'   \item For \strong{vertical} stacking, the function generates a basic vertical layout plus additional factor-based layouts (if \code{n_TrtGen} has multiple prime factors).
#'   \item For \strong{horizontal} stacking, the function includes a single-column layout, a basic grid layout, and any factor-based layouts.
#' }
#'
#' The user selects the final layout via the \code{layout} argument. The function then (depending on the design type, as indicated by \code{x$infoDesign$id_design}) uses \pkg{desplot} to create two ggplot-based plots:
#' \enumerate{
#'   \item \emph{Field layout plot}: Visualizes how treatments are arranged in the field.
#'   \item \emph{Plot-number layout}: Shows the plot IDs for each experimental unit.
#' }
#'
#' @return
#' A \code{list} containing:
#' \describe{
#'   \item{\code{p1}}{A \pkg{ggplot2} object of the field layout plot (treatments).}
#'   \item{\code{p2}}{A \pkg{ggplot2} object of the plot-number layout (plot IDs).}
#'   \item{\code{df}}{A data frame of the selected layout with columns such as \code{ROW}, \code{COLUMN}, \code{REP}, \code{TREATMENT}, and \code{PLOT}.}
#'   \item{\code{newBooks}}{A list of all possible layout data frames generated for the selected site.}
#'   \item{\code{allSitesFieldbook}}{A combined data frame (if multiple locations exist) of the selected layout for all sites.}
#' }
#' @noRd
plot_RCBD <- function(x = NULL,
                      n_TrtGen = NULL,
                      n_Reps = NULL,
                      layout = 1,
                      stacked = "horizontal",
                      planter = "serpentine",
                      l = 1) {
  site <- l
  locations <- factor(x$fieldBook$LOCATION, levels = unique(x$fieldBook$LOCATION))
  nlocs <- length(locations)

  newBooksLocs <- vector(mode = "list", length = nlocs)
  countLocs <- 1

  # For each location, generate a list of possible layout data frames
  for (locs in levels(locations)) {
    NewBook <- dplyr::filter(x$fieldBook, LOCATION == locs)
    plots <- NewBook$PLOT

    # Generate all layout possibilities (vertical or horizontal)
    if (stacked == "vertical") {
      layout_list <- generate_vertical_layout(NewBook, plots, n_TrtGen, n_Reps, planter)
    } else if (stacked == "horizontal") {
      layout_list <- generate_horizontal_layout(NewBook, plots, n_TrtGen, n_Reps, planter)
    } else {
      stop("Invalid stacking option provided.")
    }

    # Store the list of layouts for this location
    newBooksLocs[[countLocs]] <- layout_list
    countLocs <- countLocs + 1
  }

  ##############################################################################
  # 3. SELECT THE USER-SPECIFIED LAYOUT & PREPARE FOR DESPLOT
  ##############################################################################

  # Check site index
  if (site > nlocs) {
    stop("Provided site index exceeds number of locations.")
  }

  # Layouts available for the chosen site
  site_layouts <- newBooksLocs[[site]]
  # E.g., if site_layouts has 1="single_column", 2="basic_horizontal", 3="horizontal_ext_1", ...

  # Check that requested 'layout' index is valid
  available_layouts <- seq_along(site_layouts)
  if (!layout %in% available_layouts) {
    message(
      "\n Option for layout is not available!\n\n",
      "*********************************************\n",
      "Layout options available for this design are:\n\n",
      paste(available_layouts, collapse = ", "), "\n\n",
      "*********************************************\n"
    )
    return(NULL)
  }

  # Extract the chosen layout
  df <- as.data.frame(site_layouts[[layout]])

  ##############################################################################
  # 4. KEEP YOUR EXISTING DESPLOT LOGIC
  ##############################################################################

  # Example: If id_design == 2 (standard RCBD)
  if (x$infoDesign$id_design == 2) {
    # Combine all sites if needed
    allSites <- vector(mode = "list", length = nlocs)
    for (st in seq_len(nlocs)) {
      site_layouts_st <- newBooksLocs[[st]]
      if (layout <= length(site_layouts_st)) {
        df_st <- as.data.frame(site_layouts_st[[layout]])
        allSites[[st]] <- df_st
      }
    }
    allSitesFieldbook <- dplyr::bind_rows(allSites)
    # Reorder columns (example from your old code)
    allSitesFieldbook <- allSitesFieldbook[, c(1:3, 6, 7, 4:5)]

    # Also reorder df in the same manner
    df <- df[, c(1:3, 6, 7, 4:5)]

    # Prepare for desplot
    rows <- max(as.numeric(df$ROW))
    cols <- max(as.numeric(df$COLUMN))
    ds <- "Randomized Complete Block Design "
    main <- paste0(ds, rows, "X", cols)

    # Plot field layout
    p1 <- desplot::desplot(
      TREATMENT ~ COLUMN + ROW,
      flip       = FALSE,
      out1       = REP,
      out2.gpar  = list(col = "black", lty = 3),
      text       = TREATMENT,
      cex        = 1,
      shorten    = "no",
      data       = df,
      xlab       = "COLUMNS",
      ylab       = "ROWS",
      main       = main,
      show.key   = FALSE,
      gg         = TRUE
    )
    p1 <- add_gg_features(p1)

    # Plot number layout
    df$REP <- as.factor(df$REP)
    p2 <- desplot::desplot(
      REP ~ COLUMN + ROW,
      flip     = FALSE,
      out1     = REP,
      text     = PLOT,
      cex      = 1,
      shorten  = "no",
      data     = df,
      xlab     = "COLUMNS",
      ylab     = "ROWS",
      main     = main,
      show.key = FALSE,
      gg       = TRUE
    )
    p2 <- add_gg_features(p2)
  } else if (x$infoDesign$id_design == 4) {
    if (x$infoDesign$kind == "RCBD") {
      allSites <- vector(mode = "list", length = nlocs)
      for (st in seq_len(nlocs)) {
        newBooksSelected_1 <- newBooksLocs[[st]]
        # CHANGED opt TO layout AND use double brackets:
        if (layout <= length(newBooksSelected_1)) {
          df_1 <- as.data.frame(newBooksSelected_1[[layout]])
          allSites[[st]] <- df_1
        }
      }
      nc <- ncol(df)
      allSitesFieldbook <- dplyr::bind_rows(allSites)
      allSitesFieldbook <- allSitesFieldbook[, c(1:3, (nc - 1), nc, 4:(nc - 2))]
      df <- df[, c(1:3, (nc - 1), nc, 4:(nc - 2))]
      # Plot field layout
      rows <- max(as.numeric(df$ROW))
      cols <- max(as.numeric(df$COLUMN))
      ds <- "Full Factorial Design (RCBD) "
      main <- paste0(ds, rows, "X", cols)
      # Plot field layout
      p1 <- desplot::desplot(TRT_COMB ~ COLUMN + ROW,
        flip = FALSE,
        out1 = REP,
        out2.gpar = list(col = "black", lty = 3),
        text = TRT_COMB, cex = 1, shorten = "no",
        data = df, xlab = "COLUMNS", ylab = "ROWS",
        main = main,
        show.key = FALSE,
        gg = TRUE
      )
      p1 <- add_gg_features(p1)
      # Plot number layout
      df$PLOT <- as.factor(df$PLOT)
      df$REP <- as.factor(df$REP)
      p2 <- desplot::desplot(REP ~ COLUMN + ROW,
        flip = FALSE,
        out1 = REP,
        text = PLOT, cex = 1, shorten = "no",
        data = df, xlab = "COLUMNS", ylab = "ROWS",
        main = main,
        show.key = FALSE,
        gg = TRUE
      )
      p2 <- add_gg_features(p2)
    }
  }

  # Return the plots and data frames as before
  return(list(
    p1                = p1,
    p2                = p2,
    df                = df,
    newBooks          = site_layouts, # or site_layouts[[layout]]
    allSitesFieldbook = allSitesFieldbook
  ))
}
