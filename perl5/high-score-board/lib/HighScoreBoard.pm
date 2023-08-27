package HighScoreBoard;

use v5.38;

our %Scores = ();

sub set_player_scores (%new_scores) {
    while (my ($player, $score) = each %new_scores) {
        $Scores{$player} = $score;
    }
}

sub get_player_score ($player) {
    return $Scores{$player};
}

sub increase_player_scores (%additional_scores) {
    while (my ($player, $score) = each %additional_scores) {
        $Scores{$player} += $score;
    }
}

sub sort_players_by_name {
    return sort keys %Scores;
}

sub sort_players_by_score {
    return sort {$Scores{$b} <=> $Scores{$a}} keys %Scores;
}

sub delete_player ($player) {
    delete $Scores{$player};
}
