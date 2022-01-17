'''
Algorithm taken from Wikipedia:
https://en.wikipedia.org/wiki/Knapsack_problem

Define m[i,w] to be the maximum value that can be attained
with weight less than or equal to w using items up to i
(first i items).

We can define m[i,w] recursively as follows:

    m[0,w] = 0
    m[i,w] = m[i−1,w] if w_{i} > w (the new item is more than the current weight limit)
    m[i,w] = max (m[i−1,w], m[i−1, w − w_{i}] + v_{i}) if w_{i} ⩽ w
'''

def maximum_value(max_weight, items):
    '''
    // Input:
    // Values (stored in array v)
    // Weights (stored in array w)
    // Number of distinct items (n)
    // Knapsack capacity (W)
    // NOTE: The array "v" and array "w" are assumed to store all
    // relevant values starting at index 1.
    '''
    W = max_weight
    n = len(items)
    w = [None]
    v = [None]
    for item in items:
        w.append(item['weight'])
        v.append(item['value'])

    m = []
    m += [[0] * (W+1)]

    for i in range(1, n+1):
        m += [m[i-1][:]]
        for j in range(W+1):
            if w[i] <= j:
                m[i][j] = max(m[i-1][j], m[i-1][j-w[i]] + v[i])

    return m[n][W]
