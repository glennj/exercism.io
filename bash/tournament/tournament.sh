#!/usr/bin/env bash

# an experiment with "object oriented" bash programming

# external tools used: sort

source ./tournament_class.bash
source ./team_class.bash

Tournament new tournament "$@"
tournament process
tournament results
