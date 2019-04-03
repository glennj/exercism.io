import unittest

from tournament import tally


# Tests adapted from `problem-specifications//canonical-data.json` @ v1.4.0

class TournamentTest(unittest.TestCase):
    def test_just_the_header_if_no_input(self):
        self.assertEqual(
            tally(''),
            'Team                           | MP |  W |  D |  L |  P'
        )

    def test_a_win_is_three_points_and_a_loss_is_zero_points(self):
        results = 'Allegoric Alaskans;Blithering Badgers;win'
        table = ('Team                           | MP |  W |  D |  L |  P\n'
                 'Allegoric Alaskans             |  1 |  1 |  0 |  0 |  3\n'
                 'Blithering Badgers             |  1 |  0 |  0 |  1 |  0')
        self.assertEqual(tally(results), table)

    def test_a_win_can_also_be_expressed_as_a_loss(self):
        results = 'Blithering Badgers;Allegoric Alaskans;loss'
        table = ('Team                           | MP |  W |  D |  L |  P\n'
                 'Allegoric Alaskans             |  1 |  1 |  0 |  0 |  3\n'
                 'Blithering Badgers             |  1 |  0 |  0 |  1 |  0')
        self.assertEqual(tally(results), table)

    def test_a_different_team_can_win(self):
        results = 'Blithering Badgers;Allegoric Alaskans;win'
        table = ('Team                           | MP |  W |  D |  L |  P\n'
                 'Blithering Badgers             |  1 |  1 |  0 |  0 |  3\n'
                 'Allegoric Alaskans             |  1 |  0 |  0 |  1 |  0')
        self.assertEqual(tally(results), table)

    def test_a_draw_is_one_point_each(self):
        results = 'Allegoric Alaskans;Blithering Badgers;draw'
        table = ('Team                           | MP |  W |  D |  L |  P\n'
                 'Allegoric Alaskans             |  1 |  0 |  1 |  0 |  1\n'
                 'Blithering Badgers             |  1 |  0 |  1 |  0 |  1')
        self.assertEqual(tally(results), table)

    def test_there_can_be_more_than_one_match(self):
        results = ('Allegoric Alaskans;Blithering Badgers;win\n'
                   'Allegoric Alaskans;Blithering Badgers;win')
        table = ('Team                           | MP |  W |  D |  L |  P\n'
                 'Allegoric Alaskans             |  2 |  2 |  0 |  0 |  6\n'
                 'Blithering Badgers             |  2 |  0 |  0 |  2 |  0')
        self.assertEqual(tally(results), table)

    def test_there_can_be_more_than_one_winner(self):
        results = ('Allegoric Alaskans;Blithering Badgers;loss\n'
                   'Allegoric Alaskans;Blithering Badgers;win')
        table = ('Team                           | MP |  W |  D |  L |  P\n'
                 'Allegoric Alaskans             |  2 |  1 |  0 |  1 |  3\n'
                 'Blithering Badgers             |  2 |  1 |  0 |  1 |  3')
        self.assertEqual(tally(results), table)

    def test_there_can_be_more_than_two_teams(self):
        results = ('Allegoric Alaskans;Blithering Badgers;win\n'
                   'Blithering Badgers;Courageous Californians;win\n'
                   'Courageous Californians;Allegoric Alaskans;loss')
        table = ('Team                           | MP |  W |  D |  L |  P\n'
                 'Allegoric Alaskans             |  2 |  2 |  0 |  0 |  6\n'
                 'Blithering Badgers             |  2 |  1 |  0 |  1 |  3\n'
                 'Courageous Californians        |  2 |  0 |  0 |  2 |  0')
        self.assertEqual(tally(results), table)

    def test_typical_input(self):
        results = ('Allegoric Alaskans;Blithering Badgers;win\n'
                   'Devastating Donkeys;Courageous Californians;draw\n'
                   'Devastating Donkeys;Allegoric Alaskans;win\n'
                   'Courageous Californians;Blithering Badgers;loss\n'
                   'Blithering Badgers;Devastating Donkeys;loss\n'
                   'Allegoric Alaskans;Courageous Californians;win')

        table = ('Team                           | MP |  W |  D |  L |  P\n'
                 'Devastating Donkeys            |  3 |  2 |  1 |  0 |  7\n'
                 'Allegoric Alaskans             |  3 |  2 |  0 |  1 |  6\n'
                 'Blithering Badgers             |  3 |  1 |  0 |  2 |  3\n'
                 'Courageous Californians        |  3 |  0 |  1 |  2 |  1')

        self.assertEqual(tally(results), table)

    def test_incomplete_competitionnot_not_all_pairs_have_played(self):
        results = ('Allegoric Alaskans;Blithering Badgers;loss\n'
                   'Devastating Donkeys;Allegoric Alaskans;loss\n'
                   'Courageous Californians;Blithering Badgers;draw\n'
                   'Allegoric Alaskans;Courageous Californians;win')

        table = ('Team                           | MP |  W |  D |  L |  P\n'
                 'Allegoric Alaskans             |  3 |  2 |  0 |  1 |  6\n'
                 'Blithering Badgers             |  2 |  1 |  1 |  0 |  4\n'
                 'Courageous Californians        |  2 |  0 |  1 |  1 |  1\n'
                 'Devastating Donkeys            |  1 |  0 |  0 |  1 |  0')

        self.assertEqual(tally(results), table)

    def test_ties_broken_alphabetically(self):
        results = ('Courageous Californians;Devastating Donkeys;win\n'
                   'Allegoric Alaskans;Blithering Badgers;win\n'
                   'Devastating Donkeys;Allegoric Alaskans;loss\n'
                   'Courageous Californians;Blithering Badgers;win\n'
                   'Blithering Badgers;Devastating Donkeys;draw\n'
                   'Allegoric Alaskans;Courageous Californians;draw')

        table = ('Team                           | MP |  W |  D |  L |  P\n'
                 'Allegoric Alaskans             |  3 |  2 |  1 |  0 |  7\n'
                 'Courageous Californians        |  3 |  2 |  1 |  0 |  7\n'
                 'Blithering Badgers             |  3 |  0 |  1 |  2 |  1\n'
                 'Devastating Donkeys            |  3 |  0 |  1 |  2 |  1')

        self.assertEqual(tally(results), table)


if __name__ == '__main__':
    unittest.main()
