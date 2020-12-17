load('../sage/schnyder-toolbox.sage')
from sage.graphs.connectivity import vertex_connectivity

arg = int(sys.argv[1])
g = Flipgraph("graphs/n%s.edges" % arg)

kappa = vertex_connectivity(g.sage_graph(), solver='Gurobi', verbose=9)
print("-----------------------------------------------------------------")
print(">>>>> The vertex connectivity for n = %s is %s" % (arg, kappa))
print("-----------------------------------------------------------------")
