load('../sage/schnyder-toolbox.sage')
from sage.graphs.connectivity import vertex_connectivity

arg = int(sys.argv[1])
g = Flipgraph(f"graphs/n{arg}.edges")

kappa = vertex_connectivity(g.sage_graph(), solver='Gurobi', verbose=9)
print(f"-----------------------------------------------------------------")
print(f">>>>> The vertex connectivity for n = {arg} is {kappa}")
print(f"-----------------------------------------------------------------")
