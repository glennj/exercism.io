# The first line of input is the starting x, y and angle.
# Any remaining input is the prisms: ID, x, y, and refraction angle.
# The program is expected to print out prism IDs, space separated on one line.

@include "assert"
@include "arrays"
@include "math"


BEGIN {
    arrays::init(line)
    arrays::init(prisms)
}

NR == 1 {
    line["x"] = $1
    line["y"] = $2
    line["a"] = math::deg2rad($3)
}

NR > 1 {
    prisms[$1]["x"] = $2
    prisms[$1]["y"] = $3
    prisms[$1]["a"] = math::deg2rad($4)
}

END {
    arrays::init(ids)

    while (1) {
        # find the ids of the prisms that are in-line with the current line
        arrays::init(inline)
        inline_prisms(inline)

        # find the closest prism of those inline
        nextId = next_prism(inline)

        if (nextId == "") break

        arrays::push(ids, nextId)

        # make the new line from this prism
        line["x"]  = prisms[nextId]["x"]
        line["y"]  = prisms[nextId]["y"]
        line["a"] += prisms[nextId]["a"]
    }

    print arrays::join(ids)
}


# -----------------------------------------------------------
function inline_prisms(ids,   id, dist) {
    for (id in prisms) {
        # determine the perpendicular distance
        dist = (prisms[id]["x"] - line["x"]) * sin(line["a"]) \
             - (prisms[id]["y"] - line["y"]) * cos(line["a"])
        # epsilon value found by trial and error
        if (math::abs(dist) <= 0.0011) {
            arrays::push(ids, id)
        }
    }
}

function next_prism(inline,   nextId, min, id, idx, projection) {
    nextId = ""
    min = 999999999

    for (idx in inline) {
        id = inline[idx]
        projection = (prisms[id]["x"] - line["x"]) * cos(line["a"]) \
                   + (prisms[id]["y"] - line["y"]) * sin(line["a"])
        if (projection > 0 && projection < min) {
            nextId = id
            min = projection
        }
    }

    return nextId
}
