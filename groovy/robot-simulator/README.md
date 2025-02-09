# Robot Simulator

Welcome to Robot Simulator on Exercism's Groovy Track.
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

## Properties and fields in Groovy

In this exercise, you might have noticed that the stub defines getters, like `getX()`, but the tests access the value directly as `.x`. This is due to how Groovy defines properties. Let’s break it down:

- A **field** is a member of a class, interface, or trait used to **store data**.
- A **property** is an **externally visible** feature of a class.

Key Points about Groovy Properties:
- Groovy automatically generates getters and setters for non-private fields (or only a getter if the field is marked as `final`).
- Properties can be accessed by name (as in this exercise's test) and will call the getter or setter transparently.
- By convention, Groovy will recognize properties even if there is no backing field, provided there are getters or setters.

In this exercise, we’ve only specified the required getters (and hence properties as per above) to enforce proper implementation. You're free to decide which fields (and their types) are most suitable for your solution.

If you want to explore the topic of fields and properties further, please refer to [Chapter 3.3. Fields and Properties](https://www.groovy-lang.org/objectorientation.html#_fields_and_properties) in the Groovy documentation.

## Source

### Created by

- @BNAndras

### Based on

Inspired by an interview question at a famous company.