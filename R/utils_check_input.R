check_input <- function(design, dataIn) {
  if (design == "sdiag"||design == "mdiag"||design == "optim"||design == "arcbd"
     ||design == "prep"||design == "square"||design == "rect"||design == "alpha"
     ||design == "spd" ||design == "strip" ||design == "ibd" ||design == "rcd") {
    return(
      all(
        isTRUE(all.equal(dataIn[,1],unique(dataIn[,1]))),
        isTRUE(all.equal(dataIn[,2],unique(dataIn[,2])))
      )
    )
  } else if (design == "lsd"||design == "sspd") {
    return(
      all(
        isTRUE(all.equal(dataIn[,1],unique(dataIn[,1]))),
        isTRUE(all.equal(dataIn[,2],unique(dataIn[,2]))),
        isTRUE(all.equal(dataIn[,3],unique(dataIn[,3])))
      )
    )
  } else if (design == "crd"||design == "rcbd") {
    return(isTRUE(all.equal(dataIn[,1],unique(dataIn[,1]))))
    
  } else if (design == "factorial") {
    return(isTRUE(all.equal(dataIn[,2],unique(dataIn[,2]))))
  }
}