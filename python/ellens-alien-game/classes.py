"""
module docstring
"""
# pylint: disable=invalid-name      ## variables "x" and "y"

class Alien:

    """
    class docstring
    """

    total_aliens_created = 0

    def __init__(self, x, y):
        self.x_coordinate = x
        self.y_coordinate = y
        self.health = 3
        Alien.total_aliens_created += 1

    def hit(self):
        """
        method docstring
        """
        self.health -= 1

    def is_alive(self):
        """
        method docstring
        """
        return self.health > 0

    def teleport(self, x, y):
        """
        method docstring
        """
        self.x_coordinate = x
        self.y_coordinate = y

    def collision_detection(self, other):
        """
        method docstring
        """
        pass


def new_aliens_collection(positions):
    """Function taking a list of position tuples, creating one Alien instance per position.

    :param positions: list - A list of tuples of (x, y) coordinates.
    :return: list - A list of Alien objects.
    """
    #aliens = []
    #for (x, y) in positions:
    #    aliens.append(Alien(x,y))
    #
    #return aliens

    #return map(lambda position: Alien(*position), positions)

    return [Alien(x, y) for (x, y) in positions]
