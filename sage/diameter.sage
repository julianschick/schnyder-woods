load('../sage/schnyder-toolbox.sage')

arg = int(sys.argv[1])
g = Flipgraph("graphs/n%d.edges" % arg)
diameter = g.sage_graph().diameter()

print("-----------------------------------------------------------------")
print(">>>>> The diameter for n = %s is %s" % (arg, diameter))
print("-----------------------------------------------------------------")
