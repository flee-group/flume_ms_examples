# Examples for the flume manuscript

For all examples, set the working directory to the root of this repository (e.g., `flume_ms_examples`).

Each example runs through a number of scenarios (where a scenario is a single permutation of whatever model parameters are being varied for the example). Each scenario is run in replicate 8 times. The scenarios are generally stored in a data frame with all of the relevant model parameters specified. In general, running all 8 replicates for a single scenario can be expected to take a few minutes on a modern laptop or similar computer.

### Dispersal/Competition experiment

Directory `ex_kamp_dispersal_comp`.

The `R` directory contains all code. Before running the scenarios you should create a directory called `res`.

Order of operations:

1. `R/run_example.r`
2. `R/process_results.r` 
3. Other scripts can be run in any order


### Algae N:P Ratio

Directory `ex2_kamp_np_algae`

#### Data description

Samples from the site K03 were dropped, site K03plus was renamed to K03 (Site was moved after initial sampling to K03 plus location).

File `chemistry.rds` contains the water chemistry data. Variables are:

* SRP (soluble reactive phosphorous) in micrograms/L
* NH4, NO3: dissolved ammonium and nitrate, in mg/L
* DIN (dissolved inorganic nitrogen) in mg/L

File `asv.rds` contains abundances of selected (how?) ASVs.