using Lazy
using LightGraphs
using MetaGraphs

# Function adapted from JuliaGraphs package so that the in core can be calculated.
function in_core_number(g::AbstractGraph{T}) where T
    has_self_loops(g) && throw(ArgumentError("graph must not have self-loops"))
    degrees = indegree(g)
    #degrees = degree(g)
    vs = sortperm(degrees)
    bin_boundaries = [1]
    curr_degree = 0
    for (i, v) in enumerate(vs)
        if degrees[v] > curr_degree
            append!(bin_boundaries, repeat([i], (degrees[v] - curr_degree)))
            curr_degree = degrees[v]
        end
    end
    vertex_pos = sortperm(vs)
    # initial guesses for core is degree
    core = degrees
    nbrs = [Set(all_neighbors(g, v)) for v in vertices(g)]
    for v in vs
        for u in nbrs[v]
            if core[u] > core[v]
                pop!(nbrs[u], v)
                pos = vertex_pos[u]
                bin_start = bin_boundaries[core[u] + 1]
                vertex_pos[u] = bin_start
                vertex_pos[vs[bin_start]] = pos
                vs[bin_start], vs[pos] = vs[pos], vs[bin_start]
                bin_boundaries[core[u] + 1] += 1
                core[u] -= 1
            end
        end
    end
    return core
end

function readCitationNetwork(path)

    @> path CSV.read(header=["end1", "end2"], delim="\t", types=[String, String]) MetaDiGraph(:end1, :end2)
end

function getInCore(g, k)
    @>> k_core(g, k, corenum=in_core_number(g)) induced_subgraph(g)
end
