from array import array

# Depth First Search



def dfs_(G):
    """
    Implementation based on http://www.ics.uci.edu/~eppstein/PADS/DFS.py
    by D. Eppstein, July 2004.
    """
    pass

def dfs(G):
    """
    Implementation based on "Introduction to Algorithms" - Thomas H. Cormen
    """
    WHITE = 0
    GRAY = 1
    BLACK = 2
    NIL = None

    color = [WHITE for u in G]
    pi = [NIL for u in G]

    def dfs_visit(u):
        color[u] = GRAY
        for v in G[u]:
            if color[v] == WHITE:
                pi[v] = u
                print("u, v = ", u, v)
                dfs_visit(v)
        color[u] = BLACK

    for u in xrange(0, len(G)):
        if color[u] == WHITE:
            dfs_visit(u)



G = [
    [1, 6, 7], #0
    [5, 9, 0], #1
    [7, 4], #2
    [6], #3
    [2], #4
    [1], #5
    [8, 3, 0], #6
    [0, 2], #7
    [6], #8
    [1], #9
]

# G = [
#     [1,4],
#     [0,4,2,3],
#     [1,3],
#     [1,4,2],
#     [3,0,1]
# ]

#import pdb; pdb.set_trace()
#import pudb; pudb.set_trace()
dfs(G)