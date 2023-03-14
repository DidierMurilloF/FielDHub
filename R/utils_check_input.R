check_input <- function(design, dataIn) {
  if (design == "sdiag" || design == "mdiag" || design == "optim" || design == "arcbd"
      || design == "prep"|| design == "square" || design == "rect" || design == "alpha"
      || design == "ibd" || design == "rcd") {
    if (ncol(dataIn) >= 2) {
      return(
        all(
          isTRUE(all.equal(dataIn[,1], unique(dataIn[,1]))),
          isTRUE(all.equal(dataIn[,2], unique(dataIn[,2])))
        )
      )
    } else return(NULL)
  } else if (design == "lsd") {
    if (ncol(dataIn) >= 3) { 
      return(
        all(
          isTRUE(all.equal(dataIn[,1],unique(dataIn[,1]))),
          isTRUE(all.equal(dataIn[,2],unique(dataIn[,2]))),
          isTRUE(all.equal(dataIn[,3],unique(dataIn[,3])))
        )
      )
    } else return(NULL)
  } else if (design == "spd") {
    if (ncol(dataIn) >= 2) {
      wp <- as.vector(na.omit(dataIn[,1]))
      sp <- as.vector(na.omit(dataIn[,2]))
      return(
        all(
          isTRUE(all.equal(wp,unique(wp))),
          isTRUE(all.equal(sp,unique(sp)))
        )
      )
    } else return(NULL)
  } else if (design == "sspd") {
    if (ncol(dataIn) >= 3) { 
      wp <- as.vector(na.omit(dataIn[,1]))
      sp <- as.vector(na.omit(dataIn[,2]))
      ssp <- as.vector(na.omit(dataIn[,3]))
      return(
        all(
          isTRUE(all.equal(wp,unique(wp))),
          isTRUE(all.equal(sp,unique(sp))),
          isTRUE(all.equal(ssp,unique(ssp)))
        )
      )
    } else return(NULL)
  } else if (design == "strip") {
    if (ncol(dataIn) >= 2) {
      h <- as.vector(na.omit(dataIn[,1]))
      v <- as.vector(na.omit(dataIn[,2]))
      return(
        all(
          isTRUE(all.equal(h,unique(h))),
          isTRUE(all.equal(v,unique(v)))
        )
      )
    } else return(NULL)
  } else if (design == "crd") {
    if (ncol(dataIn) >= 1) {
      return(isTRUE(all.equal(dataIn[,1],unique(dataIn[,1]))))
    } else return(NULL)
  } else if (design == "factorial") {
    if (ncol(dataIn) >= 2) {
      return(isTRUE(all.equal(dataIn[,2],unique(dataIn[,2]))))
    } else return(NULL)
  } else if (design == "rcbd") {
    if (ncol(dataIn) >= 1) {
      return(isTRUE(all.equal(dataIn[,1],unique(dataIn[,1]))))
    } else return(NULL)
  }
}
