<?php

declare(strict_types=1);

const FMT = '%-30s | %2s | %2s | %2s | %2s | %2s';

class Tournament
{
    private $_teams = [];

    public function tally(string $scores): string
    {
        $this->_process($scores);

        $cmp = function (Team $a, Team $b): int {
            $res = $a->points() > $b->points()
                || ($a->points() == $b->points() && $a->name < $b->name);
            return $res ? 0 : 1;
        };
        usort($this->_teams, $cmp(...));

        $table = [sprintf(FMT, 'Team', 'MP', 'W', 'D', 'L', 'P'), ...$this->_teams];
        return implode("\n", $table);
    }

    private function _process(string $scores)
    {
        foreach (explode("\n", $scores) as $match) {
            if (!$match) continue;

            [$home, $away, $result] = explode(';', $match, 3);
            if (!($home && $away && $result)) continue;

            foreach ([$home, $away] as $team) {
                if (!array_key_exists($team, $this->_teams)) {
                    $this->_teams[$team] = new Team($team);
                }
            }

            switch ($result) {
            case 'win':
                $this->_teams[$home]->win();
                $this->_teams[$away]->lose();
                break;
            case 'loss':
                $this->_teams[$home]->lose();
                $this->_teams[$away]->win();
                break;
            case 'draw':
                $this->_teams[$home]->draw();
                $this->_teams[$away]->draw();
                break;
            }
        }
    }
}

class Team
{
    public $name;
    public $wins = 0;
    public $losses = 0;
    public $draws = 0;
    public function __construct($name) { $this->name = $name; }
    public function win() { $this->wins++; }
    public function lose() { $this->losses++; }
    public function draw() { $this->draws++; }
    public function points() { return 3 * $this->wins + $this->draws; }
    public function __toString()
    {
        return sprintf(
            FMT, 
            $this->name,
            ($this->wins + $this->draws + $this->losses),
            $this->wins,
            $this->draws,
            $this->losses,
            $this->points()
        );
    }
}
