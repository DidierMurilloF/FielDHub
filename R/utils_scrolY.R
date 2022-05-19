scrollY <- function(n_rows){
  scrollY <- "500px"
  if (n_rows >= 15 && n_rows < 30){
    scrollY <- "800px"
  }else if (n_rows >= 30 && n_rows < 40){
    scrollY <- "1200px"
  }else if(n_rows >= 40){
    scrollY <- "1500px"
  }
  return(scrollY)
}