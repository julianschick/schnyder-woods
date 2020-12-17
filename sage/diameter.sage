load('../sage/schnyder-toolbox.sage')

list = []

for i in range(4,9):
    g = Flipgraph("graphs/n%d.edges" % i)

    diam = g.sage_graph().diameter()
    radius = g.sage_graph().radius()

    list.append((i, diam, radius))


print("n;diameter;radius")
for i, diameter, radius in list:
    print("%d;%d;%d" % (i, diameter, radius))




