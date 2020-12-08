load('../sage/schnyder-toolbox.sage')

list = []

for i in range(4,9):
    g = Flipgraph(f"graphs/n{i}.edges")

    diam = g.sage_graph().diameter()
    radius = g.sage_graph().radius()

    list.append((i, diam, radius))


print("n;diameter;radius")
for i, diameter, radius in list:
    print(f"{i};{diameter};{radius}")




