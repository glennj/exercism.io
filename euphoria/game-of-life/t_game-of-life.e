include std/unittest.e

include game-of-life.ex

set_test_verbosity(TEST_SHOW_ALL)

procedure test_empty_matrix()
    sequence input = {}
    sequence expected = {}
    test_equal("empty matrix", expected, tick(input))
end procedure

procedure test_live_cells_with_zero_live_neighbors_die()
    sequence input = {
        {0, 0, 0},
        {0, 1, 0},
        {0, 0, 0}
    }
    sequence expected = {
        {0, 0, 0},
        {0, 0, 0},
        {0, 0, 0}
    }
    test_equal("live cells with zero live neighbors die", expected, tick(input))
end procedure

procedure test_live_cells_with_only_one_live_neighbor_die()
    sequence input = {
        {0, 0, 0},
        {0, 1, 0},
        {0, 1, 0}
    }
    sequence expected = {
        {0, 0, 0},
        {0, 0, 0},
        {0, 0, 0}
    }
    test_equal("live cells with only one live neighbor die", expected, tick(input))
end procedure

procedure test_live_cells_with_two_live_neighbors_stay_alive()
    sequence input = {
        {1, 0, 1},
        {1, 0, 1},
        {1, 0, 1}
    }
    sequence expected = {
        {0, 0, 0},
        {1, 0, 1},
        {0, 0, 0}
    }
    test_equal("live cells with two live neighbors stay alive", expected, tick(input))
end procedure

procedure test_live_cells_with_three_live_neighbors_stay_alive()
    sequence input = {
        {0, 1, 0},
        {1, 0, 0},
        {1, 1, 0}
    }
    sequence expected = {
        {0, 0, 0},
        {1, 0, 0},
        {1, 1, 0}
    }
    test_equal("live cells with three live neighbors stay alive", expected, tick(input))
end procedure

procedure test_dead_cells_with_three_live_neighbors_become_alive()
    sequence input = {
        {1, 1, 0},
        {0, 0, 0},
        {1, 0, 0}
    }
    sequence expected = {
        {0, 0, 0},
        {1, 1, 0},
        {0, 0, 0}
    }
    test_equal("dead cells with three live neighbors become alive", expected, tick(input))
end procedure

procedure test_live_cells_with_four_or_more_neighbors_die()
    sequence input = {
        {1, 1, 1},
        {1, 1, 1},
        {1, 1, 1}
    }
    sequence expected = {
        {1, 0, 1},
        {0, 0, 0},
        {1, 0, 1}
    }
    test_equal("live cells with four or more neighbors die", expected, tick(input))
end procedure

procedure test_bigger_matrix()
    sequence input = {
        {1, 1, 0, 1, 1, 0, 0, 0},
        {1, 0, 1, 1, 0, 0, 0, 0},
        {1, 1, 1, 0, 0, 1, 1, 1},
        {0, 0, 0, 0, 0, 1, 1, 0},
        {1, 0, 0, 0, 1, 1, 0, 0},
        {1, 1, 0, 0, 0, 1, 1, 1},
        {0, 0, 1, 0, 1, 0, 0, 1},
        {1, 0, 0, 0, 0, 0, 1, 1}
    }
    sequence expected = {
        {1, 1, 0, 1, 1, 0, 0, 0},
        {0, 0, 0, 0, 0, 1, 1, 0},
        {1, 0, 1, 1, 1, 1, 0, 1},
        {1, 0, 0, 0, 0, 0, 0, 1},
        {1, 1, 0, 0, 1, 0, 0, 1},
        {1, 1, 0, 1, 0, 0, 0, 1},
        {1, 0, 0, 0, 0, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 1, 1}
    }
    test_equal("bigger matrix", expected, tick(input))
end procedure


test_empty_matrix()
test_live_cells_with_zero_live_neighbors_die()
test_live_cells_with_only_one_live_neighbor_die()
test_live_cells_with_two_live_neighbors_stay_alive()
test_live_cells_with_three_live_neighbors_stay_alive()
test_dead_cells_with_three_live_neighbors_become_alive()
test_live_cells_with_four_or_more_neighbors_die()
test_bigger_matrix()

test_report()
