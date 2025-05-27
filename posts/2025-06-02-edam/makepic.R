
library(Rgraphviz)
library(graph)
library(biocViews)
data(biocViewsVocab)
nodes(biocViewsVocab)
ss = subGraph(nodes(biocViewsVocab)[1:20], biocViewsVocab)
graph.par(list(nodes = list(shape = "plaintext", cex = .6)))
gg = layoutGraph(ss, layoutType="dot")
renderGraph(gg)


