PREPARATION_TIME_PER_LAYER = 2
EXPECTED_BAKE_TIME = 40

# TODO: define the 'bake_time_remaining()' function
def bake_time_remaining(elapsed_bake_time):
    """Calculate the bake time remaining.

    :param elapsed_bake_time: int baking time already elapsed.
    :return: int remaining bake time derived from 'EXPECTED_BAKE_TIME'.

    Function that takes the actual minutes the lasagna has been in the oven as
    an argument and returns how many minutes the lasagna still needs to bake
    based on the `EXPECTED_BAKE_TIME`.
    """

    return EXPECTED_BAKE_TIME - elapsed_bake_time

def preparation_time_in_minutes(layers):
    """
    TODO Insert docstring here
    """
    return layers * PREPARATION_TIME_PER_LAYER

def elapsed_time_in_minutes(number_of_layers, elapsed_bake_time):
    """
    TODO Insert docstring here
    """
    return preparation_time_in_minutes(number_of_layers) + elapsed_bake_time