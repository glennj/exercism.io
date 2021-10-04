#!/usr/bin/env bash

# external tools used: sort

source ./tournament_class.bash
source ./team_class.bash

Tournament new tournament "$@"
tournament process
tournament results
