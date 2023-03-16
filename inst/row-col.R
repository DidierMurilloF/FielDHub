library(blocksdesign)
treatments = factor(1:12)
blocks = data.frame(Rows = gl(4,12), Cols = gl(4,3,48))
row_col <- design(treatments, blocks, searches=200, weighting=0)
design <- row_col$Design
design$treatments <- as.factor(design$treatments)

library(desplot)
ggdesplot(data = design, treatments ~ Rows + Cols)







