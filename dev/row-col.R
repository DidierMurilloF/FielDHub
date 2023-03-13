library(blocksdesign)
treatments = factor(1:12)
blocks = data.frame(Rows = gl(4,12), Cols = gl(4,3,48))
design(treatments, blocks, searches=200, weighting=0)

blocksdesign::MOLS(2, 3, 7)


blocks(treatments=list(10,5),replicates=list(2,1), blocks = 4)
blocks(treatments=list(3,320,150),replicates=list(8,2,1), blocks = list(4,5))
