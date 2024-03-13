# R-simmer MAN traffic simulator with latency percentiles

This repository contains scripts for simulating traffic latency in two network topologies **Milano** and **Tokyo** using Google Colab notebooks. The main scripts are:

1. **simulation_MM1_Tokyo.ipynb**
2. **simulation_MM1_Milano.ipynb**

   
The code utilizes Excel files containing topology information with three pages:

* Nodes types and node labeling;
* List of links between nodes (source and destination columns), including distance and capacity for each link;
* List of traffic flows with source and destination, specifying traffic in Gbps.

## Theoretical calculations  
The code calculates total traffic in each link and theoretical delays for the M/M/1 queuing model using the _ighraph_ librabry, considering propagation, transmission, and queuing delays. It estimates the theoretical 99th percentile Hoeffding upper bound as the worst-case scenario.

## Experimental calculations
The next section simulates experiments using DES (Discrete Event Simulator) with the _simmer_ library, considering all traffic flows. It computes the experimental 99th percentile and compares it with several theoretical bounds (Chebisev, Hoeffding, and Markov) for validation.

## Results
The code generates boxplots illustrating:
1. Delays (average delay and Hoeffding upper bound) for various capacities (10 Gbps, 40 Gbps, 100 Gbps, and 400 Gbps) of each link, accounting for transmission and queuing delay.
2. Delays (average delay and Hoeffding upper bound) for different link distances (x0.5, x1, x2, x4) accounting propagetion, transmission, and queuing delay.

The results are reported in the files **Milano_topology_result.xlsx** and **Tokyo_topology_result.xlsx**.





