import collections
import heapq

"""
We view this problem as a DAG, where the value of each node represents the
cost of moving from one of the nodes leading to it. Thus, if we negate all
the values, we can perform Dijkstra's to obtain the path cost that is "most
minimal". When we invert this cost, we get the maximum-cost path. That gives
us our result.
"""

trip = """3
7 4
2 4 6
8 5 9 3"""

tri = """75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"""

def graph (s):
    # convert to list of lists, each list a row in the triangle
    lols = map(lambda x: x.split(), s.split('\n'))
    init = -int(lols[0][0])

    # maps vertices to edges, eg, vertex -> [(vertex, cost to get to vertex)]
    # where vertex is a tuple (row, col)
    g = collections.defaultdict(lambda:[])
    vertices = set()
    for rownum,row in enumerate(lols):
        for colnum,val in enumerate(row):
            # link vertex to element just below
            if (rownum > 0 and colnum < len(row)-1):
                g[(rownum-1, colnum)].append(((rownum,colnum), -int(val)))
            # link vertex to element below and to the right
            if (rownum > 0 and colnum > 0):
                g[(rownum-1, colnum-1)].append(((rownum, colnum), -int(val)))
            vertices.add((rownum,colnum))

    return g, vertices, init

# decrease x's key to some value k
def decreaseKey (q, x, k):
    for (i, (key, val)) in enumerate(q):
        if x == val:
            q[i] = (k, val)
            break

# g maps vertex -> [(vertex, cost to get to vertex)]
# vertices is a list [(rownum,colnum)]
# source is a tuple (rownum,colnum)
def dijkstra (g, vertices, source, init):
    dist = {}
    s = set()
    q = []
    for v in vertices:
        dist[v] = float('inf')
        heapq.heappush(q, (float('inf'), v))
    dist[source] = init
    decreaseKey(q, source, init)

    while len(q) > 0:
        ucost, u = heapq.heappop(q)
        s.add(u)
        for v,vcost in g[u]:
            if dist[v] > dist[u] + vcost:
                dist[v] = dist[u] + vcost

    return dist

def bestPath (vertdict):
    m = 0
    for vert,cost in vertdict.items():
        c = -cost
        if c > m:
            m = c

    return m


if __name__ == '__main__':
    g,vertices,init = graph(trip)
    dists = dijkstra(g, vertices, (0,0), init)
    print bestPath(dists)

    g,vertices,init = graph(tri)
    dists = dijkstra(g, vertices, (0,0), init)
    print bestPath(dists)
