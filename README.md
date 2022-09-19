# Examples for the flume manuscript

For all examples, set the working directory to the root of this repository (e.g., `flume_ms_examples`).

Each example runs through a number of scenarios (where a scenario is a single permutation of whatever model parameters are being varied for the example). Each scenario is run in replicate 8 times. The scenarios are generally stored in a data frame with all of the relevant model parameters specified. In general, running all 8 replicates for a single scenario can be expected to take a few minutes on a modern laptop or similar computer.

### Dispersal/Competition experiment

Directory `ex_kamp_dispersal_comp`.

The `R` directory contains all code. Before running the scenarios you should create a directory called `res`.

Order of operations:

1. `R/run_example.r`
2. ... 