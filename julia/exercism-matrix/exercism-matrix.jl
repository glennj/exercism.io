const E = [
    0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1 0 0;
    0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0;
    0 1 0 0 0 1 0 0 0 0 0 0 1 0 0 0 1 0;
    0 1 0 0 1 0 1 0 0 0 0 1 0 1 0 0 1 0;
    0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0;
    1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1;
    0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0;
    0 1 0 0 0 0 0 1 0 0 1 0 0 0 0 0 1 0;
    0 1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 1 0;
    0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0;
    0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1 0 0
]

function frown!(E)
    (E[7, 7:12], E[9, 7:12]) = (E[9, 7:12], E[7, 7:12])
    E
end

frown(E) = frown!(copy(E))

function stickerwall(E)
    rs, cs = size(E)
    rows = 2 * rs + 3
    cols = 2 * cs + 3

    wall = zeros(Int, rows, cols)

    # draw the separators
    wall[rs + 2, :] .= 1
    wall[:, cs + 2] .= 1

    # place the happy logos
    wall[1:rs, 1:cs] = wall[rs + 4:rows, cs + 4:cols] = E

    # place the sad logos
    wall[1:rs, cs + 4:cols] = wall[rs + 4:rows, 1:cs] = frown(E)

    wall
end

# Return a dot matrix of the same size, with the dots in each column being the
# number of dots in that column.
function colpixelcount(E)
    E .* sum(E; dims = 1)
end

function render(E)
    render_dot(val) = iszero(val) ? " " : "X"
    join_nl(xs) = join(xs, "\n")

    eachrow(E) .|> (row -> render_dot.(row)) .|> join |> join_nl
end
