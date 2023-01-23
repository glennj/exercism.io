# source: https://github.com/fadado/JBOL/blob/master/fadado.github.io/array/tuple.jq 
# Permutations
# P(n) = n!
#
# example: 
#   [1,2,3] | permutations
# outputs
#   [1,2,3]
#   [1,3,2]
#   [2,1,3]
#   [2,3,1]
#   [3,1,2]
#   [3,2,1]

def permutations: #:: TUPLE => *TUPLE
    def choose: range(0;length);
    #
    if length == 0
    then []
    else
        # pick one and add to what's left permuted
        choose as $i
        | .[$i:$i+1] + (del(.[$i])|permutations)
    end
;

# Combinations
#      Generate all (ordered) combinations of _k_ items from a list of _n_ items.
#      This is "n choose k".
#
# source: https://github.com/glennj/exercism.io/blob/main/elixir/lib/permutations.ex
#
# example: 
#   [1,2,3,4] | combinations(3)
# outputs
#   [1,2,3]
#   [1,2,4]
#   [1,3,4]
#   [2,3,4]

def combinations($k):
    if $k == 0 then [[]]
    elif length == 0 then []
    else
        .[0] as $h      # head
        | .[1:] as $t   # tail
        | ($t | combinations($k - 1) | map([$h] + .)) + ($t | combinations($k))
    end
;

# Partial permutations, Variations
# V(n,k) = n!/(n-k)!
# V(n,1) = n
# V(n,0) = 1
# V(n,n) = P(n)
#
# example
#   [1,2,3,4] | [permutations(3)]
# outputs
#   [ [1,2,3], [1,2,4], [1,3,2], [1,3,4], [1,4,2], [1,4,3],
#     [2,1,3], [2,1,4], [2,3,1], [2,3,4], [2,4,1], [2,4,3],
#     [3,1,2], [3,1,4], [3,2,1], [3,2,4], [3,4,1], [3,4,2],
#     [4,1,2], [4,1,3], [4,2,1], [4,2,3], [4,3,1], [4,3,2]
#   ]
#
def permutations($k): #:: TUPLE|(number) => *TUPLE
    def _perm($k):
        def choose: range(0;length);
        #
        if $k == 1
        then
            .[] | [.] #choose as $i | .[$i:$i+1]
        else
            # choose one and add to what's left permuted
            choose as $i
            | .[$i:$i+1] + (del(.[$i])|_perm($k-1))
        end
    ;
    select(0 <= $k and $k <= length) # not defined for all $k
    | if $k == 0 then [] else _perm($k) end
;


# Derangements
# - no element is in its proper place
# TODO: formula?
# example:
#   [1,2,3,4] | derangements
# outputs
#   [2,1,4,3]
#   [2,3,4,1]
#   [2,4,1,3]
#   [3,1,4,2]
#   [3,4,1,2]
#   [3,4,2,1]
#   [4,1,2,3]
#   [4,3,1,2]
#   [4,3,2,1]

def derangements: #:: TUPLE => *TUPLE
    def choose($i): #:: [a]|(number) => [[number,a]]
        range(0;length) as $j
        | select(.[$j][0] != $i)
        | [$j, .[$j][1]]
    ;
    def _derange($i): #:: [a]|(number) => *[a]
        if length == 0
        then []
        else
            # choose one valid for this "column"
            # and add to what's left after removing this one deranged
            choose($i) as [$j, $x]
            | [$x] + (del(.[$j])|_derange($i+1))
        end
    ;
    select(length >= 2) # no derangements for less than 2 elements
    # . (dot) for _derange has still available enumerated elements
    | [range(0;length) as $i | [$i,.[$i]]]
    | _derange(0)
;
