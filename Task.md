# Robots

Robbie the robot and his pals deliver presents to houses. Build a program that runs a simulation given a number of robots and a string of their orders, keeps track of the state of the robots, and the presents they’ve delivered.

The robots move in the 4 cardinal directions. They each take turns. On a robot’s turn they will read one instruction from the moves sequence, move in the indicated direction, and attempt to deliver their present.

A robot may deliver a present if, and only if, there are no other robots on the space in which they are ending their turn.

For example, if there are 3 robots: robbie, jane, bob, and a move sequence of ^^VV<> we should get the following moves:

```
robbie ^
jane ^
bob V
robbie V
jane <
bob >
```

A present is delivered when a robot enters a space in the world.

## Requirements
The universe is a discrete grid of houses whose origin is (0, 0) and expands infinitely in every direction.

All robots start at the origin.

The parameters to the simulation are:

  * the number of robots, defaulting to 1
  * the movement sequence, given as a string

The minimal set of operations we want to support are:

  * create a simulation
  * step through one turn
  * query the simulation for the current position of the robots
  * query the simulation for the number of houses with at least n presents (`n where 1 <= n <= max(int)`)
  * query the simulation for the total number of presents delivered
  * run the entire simulation through the full sequence of moves
