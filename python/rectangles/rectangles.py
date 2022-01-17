import re


def rectangles(ascii_diagram):
    vertices = [
        (r, c)
        for r, row in enumerate(ascii_diagram)
        for c, char in enumerate(row)
        if char == '+'
    ]
    count = 0
    for r in range(len(ascii_diagram) - 1):
        vertices_in_row = [v for v in vertices if v[0] == r]
        count += sum(
            count_using(
                top_left, top_right, vertices, ascii_diagram
            )
            for i, top_left in enumerate(vertices_in_row[:-1])
            for top_right in vertices_in_row[i+1:]
        )
    return count


def count_using(topL, topR, vertices, grid):
    count = 0
    left_vertices = [
        v for v in vertices
        if v[1] == topL[1] and v[0] > topL[0]
    ]
    for bottomL in left_vertices:
        bottomR = [
            v for v in vertices
            if v[1] == topR[1] and v[0] == bottomL[0]
        ]
        if len(bottomR) == 1 and is_complete(
            topL, topR, bottomL, bottomR[0], grid
        ):
            count += 1
    return count


def is_complete(topL, topR, bottomL, bottomR, grid):
    return is_edge(topL, topR, grid) and \
           is_edge(topL, bottomL, grid) and \
           is_edge(topR, bottomR, grid) and \
           is_edge(bottomL, bottomR, grid)


def is_edge(v1, v2, grid):
    if v1[0] == v2[0]:
        # horizontal
        patt = r'^\+[+-]*\+$'
        c1 = min(v1[1], v2[1])
        c2 = max(v1[1], v2[1])
        edge = grid[v1[0]][c1:c2+1]
    else:
        # vertical
        patt = r'^\+[+|]*\+$'
        r1 = min(v1[0], v2[0])
        r2 = max(v1[0], v2[0])
        edge = ''.join(grid[r][v1[1]] for r in range(r1, r2+1))
    return re.match(patt, edge)
