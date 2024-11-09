# Crane Container Simulation (CCS)

![Banner (Decorative)](./doc/banner.png)

## Overview

The "Crane Container Simulation" is a terminal application used to aid in the calculation
of heuristics and cost. The goal was to find the shortest possible path to reach the goal
state from the initial state, as per the requirement from the module BS2200 Artificial
Intelligence at the University of Winchester. The Original Hand In Date was Tuesday,
10th of January 2023.

## Contextual Information

The scenario involves four loading bays (A-D), five containers (1-5) and a single crane.

* Each loading bay can have a maximum of three containers.
* The crane can only hold one container at a time.
* The crane takes a maximum of four minutes lifting a container - minus the position of
the container (Bottom (0), Middle (1), Top (2)).
* The crane takes three minutes moving between neighbouring bays.
* The crane takes two minutes dropping a container.

Below shows the initial state of the dock and the goal state that must be reached within
minimum time.

![Initial State](./doc/ccs-context.png)

## Usage by Editing Code

From Line 334, it shows that operations I took to reach the goal state from the initial
state. This program was create to help out with working out heuristics and the total path
cost, manually using the A* algorithm.

```pas
// Increase the Total Cost based on the crane moving from loading bay D to loading bay C.
Inc(TotalCost^, Move(Crane^, LoadingBayD^, LoadingBayC^));
```

## Quick Start

```sh
# Compiling the program.
fpc problem.pas
# Running the program.
./problem
```
