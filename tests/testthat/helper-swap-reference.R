# Reference implementations for the pair-swap optimizer tests: master
# 720449e's per-entry pairs_distance(), kept verbatim (FielDHub's own code) so
# the vectorized version can be checked against it with identical().
swap_ref_pairs_distance <- function(X, dist_method = "euclidean") {
  validate_swap_matrix(X)
  dist_fn <- swap_distance_function(dist_method)

  nr <- nrow(X)
  # NA cells are inactive field positions and must not enter the distance
  # calculations as an artificial replicated treatment.
  tab <- table(as.vector(X))
  dupsI <- as.integer(names(tab)[tab > 1L])
  if (length(dupsI) == 0L) fieldhub_abort("All elements in X appear only once")

  out_list <- vector("list", length(dupsI))
  for (i in seq_along(dupsI)) {
    g <- dupsI[i]
    id <- which(X == g)
    pairs <- utils::combn(id, 2)
    p1 <- pairs[1L, ]
    p2 <- pairs[2L, ]
    rA <- ((p1 - 1L) %% nr) + 1L
    cA <- ((p1 - 1L) %/% nr) + 1L
    rB <- ((p2 - 1L) %% nr) + 1L
    cB <- ((p2 - 1L) %/% nr) + 1L
    distances <- as.numeric(dist_fn(rA, cA, rB, cB))
    out_list[[i]] <- data.frame(
      geno = rep.int(g, length(distances)),
      Pos1 = p1, Pos2 = p2, DIST = distances,
      rA = rA, cA = cA, rB = rB, cB = cB
    )
  }
  plotDist <- do.call(rbind, out_list)
  plotDist <- plotDist[order(plotDist$DIST), ]
  rownames(plotDist) <- NULL
  plotDist
}
