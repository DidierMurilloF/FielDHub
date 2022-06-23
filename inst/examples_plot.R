library(FielDHub)

# Examples ----------------------------------------------------------------

# Single Unreplicated arrangement  ----------------------------------------

diagonal_single <- diagonal_arrangement(nrows = 20, 
                                        ncols = 15, 
                                        lines = 270, 
                                        checks = 1:4, 
                                        plotNumber = 1,
                                        planter = "serpentine", 
                                        seed = 16)
plot(diagonal_single, l = 1)

# Multiple Diagonal Arrangent ---------------------------------------------

diagonal_multi <- diagonal_arrangement(nrows = 15, 
                                       ncols = 20, 
                                       lines = 270,
                                       checks = 4,
                                       seed = 1, 
                                       l = 1,
                                       blocks = c(70, 100, 100),
                                       kindExpt = "DBUDC")
plot(diagonal_multi)

# Optimized Arrangement ---------------------------------------------------

optim_design <- optimized_arrangement(nrows = 15, 
                                      ncols = 20, 
                                      lines = 270, 
                                      amountChecks = 30, 
                                      checks = 4, 
                                      seed = 1)

plot(optim_design)


# Augmented RCBD ----------------------------------------------------------

augmented <- RCBD_augmented(lines = 121, 
                            checks = 5, 
                            b = 10, 
                            l = 3, 
                            plotNumber = 101, 
                            seed = 1,
                            random = TRUE)
plot(augmented, l = 1)
plot(augmented, l = 2)
plot(augmented, l = 3)
plot(augmented, l = 4)

augmented1 <- RCBD_augmented(lines = 122, 
                             checks = 5, 
                             b = 5, 
                             l = 1, 
                             plotNumber = 101, 
                             seed = 1,
                             random = TRUE, 
                             nrows = 10, 
                             ncols = 15)
plot(augmented1, l = 1)

augmented2 <- RCBD_augmented(lines = 270, 
                             checks = 6, 
                             b = 5, 
                             l = 1, 
                             plotNumber = 101, 
                             seed = 1,
                             random = TRUE, 
                             nrows = 15, 
                             ncols = 20, repsExpt = 2)
plot(augmented2)


#  Partially Replicated Design --------------------------------------------

prep <- partially_replicated(nrows = 15, 
                             ncols = 20, 
                             repGens = c(80, 140),
                             repUnits = c(2, 1), 
                             seed = 1202)
plot(prep)

# Alpha lattice  ----------------------------------------------------------

alpha <- alpha_lattice(t = 42, 
                       k = 6, 
                       r = 3, 
                       l = 2, 
                       plotNumber = c(101, 1001), 
                       seed = 456)
plot(alpha)
plot(alpha, layout = 3, l = 2)
plot(alpha, layout = 2, l = 2, stacked = "horizontal")


# Rectangular lattice -----------------------------------------------------

rectangular <- rectangular_lattice(t = 56, 
                                   k = 7, 
                                   r = 3, 
                                   l = 2, 
                                   plotNumber = c(101, 1001), 
                                   seed = 1452)
plot(rectangular)
plot(rectangular, layout = 2, l = 2)
plot(rectangular, layout = 2, l = 2, stacked = "horizontal")


# Square Lattice ----------------------------------------------------------

square <- square_lattice(t = 49, 
                         k = 7, 
                         r = 3, 
                         l = 2, 
                         plotNumber = c(101, 1001), 
                         seed = 12345)
plot(square)
plot(square, layout = 2, l = 2)
plot(square, layout = 2, l = 2, stacked = "horizontal")


# CRD ---------------------------------------------------------------------

crd <- CRD(t = 18, reps = 5, plotNumber = 101, seed = 123)
plot(crd, layout = 1)
plot(crd, layout = 2)
plot(crd, layout = 3)
# RCBD --------------------------------------------------------------------

rcbd <- RCBD(t = 32, 
             reps = 4, 
             l = 5, 
             plotNumber = 101, 
             continuous = TRUE,
             seed = 4587)
plot(rcbd, l = 1)
plot(rcbd, layout = 2)
plot(rcbd, layout = 3)
plot(rcbd, layout = 4)
plot(rcbd, layout = 5)
plot(rcbd, layout = 6)
plot(rcbd, layout = 7, stacked = "horizontal", l = 2)


# Latin square design -----------------------------------------------------

lsd <- latin_square(t = 5,
                    reps = 2, 
                    plotNumber = 101,
                    seed = 1002,
                    planter = "cartesian")
plot(lsd, layout = 1)


# Factorial design --------------------------------------------------------

fd <- full_factorial(setfactors = c(3,3,2), 
                     reps = 3, 
                     l = 2, 
                     type = 2, 
                     plotNumber = c(1001, 2001))
plot(fd, 3)

# Split Plot Design -------------------------------------------------------

spd <- split_plot(wp = 5, 
                  sp = 3, 
                  reps = 3, 
                  type = 2, 
                  l = 2, 
                  seed = 11145,
                  plotNumber = c(101, 1001))
p <- plot(spd)
head(p$field_book, 10)
p$layout

# Split-Split Plot Design -------------------------------------------------

sspd <- split_split_plot(wp = 2,
                         sp = 2, 
                         ssp = 5, 
                         reps = 3,
                         type = 2, 
                         l = 2, 
                         plotNumber = c(1, 1001))
plot(sspd)

# Strip Plot Design -------------------------------------------------------

strip <- strip_plot(Hplots = 5, 
                    Vplots = 5,
                    b = 3, 
                    l = 3,
                    planter = "cartesian",
                    plotNumber = c(1,1001,2001), 
                    seed = 2356)
plot(strip)

# Incomplete Blocks Design ------------------------------------------------

ibd <- incomplete_blocks(t = 12, 
                         k = 4,
                         r = 3,
                         l = 1, 
                         plotNumber = 1, 
                         seed = 14568)
plot(ibd)
plot(ibd, layout = 2)
plot(ibd, layout = 3)


# Resolvible Row-Column Design --------------------------------------------

row_col <- row_column(t = 36, 
                      nrows = 4,
                      r = 3, 
                      l = 1, 
                      plotNumber = 101, 
                      seed = 1247)
plot(row_col)
plot(row_col, stacked = "horizontal")


