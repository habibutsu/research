import networkx as nx

G = nx.Graph()
G.add_edge(5, 1)
G.add_edge(1, 0)
G.add_edge(0, 7)
G.add_edge(2, 4)
G.add_edge(7, 2)
G.add_edge(0, 6)
G.add_edge(6, 8)
G.add_edge(6, 3)
G.add_edge(1, 9)

for item in nx.number_strongly_connected_components(G):
    print(item.edges())

# for elem in nx.connected_component_subgraphs(G):
#     print(elem.edges())
# Gc = max(nx.connected_component_subgraphs(G), key=len)

# import matplotlib.pyplot as plt
# pos = nx.graphviz_layout(G)
# nx.draw(G, nodecolor='r', edge_color='b')
# plt.draw()  # pyplot draw()
# plt.savefig('sample_graph.png')