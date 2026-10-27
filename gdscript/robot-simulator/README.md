# Robot Simulator

Welcome to Robot Simulator on Exercism's GDScript Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Write a robot simulator.

A robot factory's test facility needs a program to verify robot movements.

The robots have three possible movements:

- turn right
- turn left
- advance

Robots are placed on a hypothetical infinite grid, facing a particular direction (north, east, south, or west) at a set of {x,y} coordinates,
e.g., {3,8}, with coordinates increasing to the north and east.

The robot then receives a number of instructions, at which point the testing facility verifies the robot's new position, and in which direction it is pointing.

- The letter-string "RAALAL" means:
  - Turn right
  - Advance twice
  - Turn left
  - Advance once
  - Turn left yet again
- Say a robot starts at {7, 3} facing north.
  Then running this stream of instructions should leave it at {9, 4} facing west.

## Vector2i

This exercise uses [GDScript's `Vector2i`][vector2i] for the robot's position and direction.
For the direction, see [`Vector2i` constants][vector2i_constants].

[vector2i]: https://docs.godotengine.org/en/stable/classes/class_vector2i.html
[vector2i_constants]: https://docs.godotengine.org/en/stable/classes/class_vector2i.html#constants

## Source

### Created by

- @BNAndras

### Contributed to by

- @pfertyk

### Based on

Inspired by an interview question at a famous company.