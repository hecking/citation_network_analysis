# Fast LiC

This is a faster implementation of different versions of link clustering in Julia (https://julialang.org/).
It runs almost twice as fast as the R implementation but post-processing is much less user friendly.

Required Julia packages can be installed using

```Julia
Pkg.add.(["LightGraphs", "DataFrames", "GraphDataFrameBridge", "CSV", 
	"NamedArrays", "SparseArrays", "Clustering", "Lazy", "GraphIO"])
```



### Example usage:

```Julia
include("atttributed_link_communties.jl")

g = @> readCitationNetwork("../datasets/") getInCore(1)
  res = attributedLinkClustering(g, nothing, getLinkSimilarityNetwork)
  bestCut = filter(row -> row[:pd] == maximum(res.pd), res)

  # get node clusters for link clusters of more than 5 links.
  linkClustersToNodeClusters(
      g, [cl for cl in bestCut.clusters[1] if length(cl) > 5]
    )
```
