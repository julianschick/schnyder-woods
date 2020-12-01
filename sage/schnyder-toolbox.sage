import numpy as np
from sage.combinat.posets.posets import FinitePoset

class Flipgraph:

	def __init__(self, path, level_path=None):
		edges_file = open(path, "r")
		edges_raw = np.fromfile(edges_file, dtype=np.uint64)
		edges_count = edges_raw[0]
		edges = [(edges_raw[i*2 + 1], edges_raw[i*2 + 2]) for i in range(0, edges_count)]
		self.g =  Graph(edges)

		if level_path:
			self.load_levels(level_path)

	def load_levels(self, path):
		levels_file = open(path, "r")
		self.levels = np.fromfile(levels_file, dtype=np.uint8)

		min_level = min(self.levels)
		max_level = max(self.levels)

		self.levels_partition = []
		level_counter = [0 for i in range(min_level, max_level+1)]

		for level in range(min_level, max_level+1):
			self.levels_partition.append([i for i, x in enumerate(self.levels) if x == level])

	def sage_graph(self):
		return self.g

	def plot(self, iterations=10000):
		self.g.plot(save_pos=True,partition=self.levels_partition, iterations=iterations, vertex_labels=True, vertex_size=400)

	def poset(self):
		if self.levels is None:
			print("Flipgraph must have level information for building the poset")
			return

		arcs = []
		for (v1, v2, weight) in self.g.edges():
			v1, v2 = (v1, v2) if self.levels[v1] < self.levels[v2] else (v2, v1)
			arcs.append((v1,v2))

		digraph = DiGraph(arcs)
		return FinitePoset(digraph)


	def tikz(self):
		if self.levels is None:
			print("Flipgraph must have level information for tikzing.")
			return

		self.plot(iterations=1000000)
		str = ""
		positions = self.g.get_pos()

		min_level = min(self.levels)
		max_level = max(self.levels)

		for v in self.g.vertices():
			color = int(100* (self.levels[v] - min_level) / (max_level - min_level))
			str += f"\\node[flip-node, fill=blue!{color}!red] ({v}) at ({positions.get(v)[0]}, {positions.get(v)[1]}) {{{v}}};\n"

		for (v1, v2, weight) in self.g.edges():
			v1, v2 = (v1, v2) if self.levels[v1] < self.levels[v2] else (v2, v1)

			str += f"\\draw[flip-edge] ({v1}) -- ({v2});\n"

		print(str)