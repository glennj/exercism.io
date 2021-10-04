#!/usr/bin/env bash

source ./robot_class.bash

Robot new robbie "$1" "$2" "$3"
robbie execute "$4"
robbie position
