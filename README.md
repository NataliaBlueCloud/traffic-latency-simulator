# R-simmer MAN traffic simulator with latency percentiles

The MAN Traffic Simulator is a project that aims to analyze the end-to-end delays in a metro network topology, based on the packet size distribution and network load. The code provides two different approaches to calculating the delays: using a simmer simulation with an M/G/1 queueing model, and a theoretical delay calculation.r

This repository contains scripts for simulating traffic latency. 
1. the script **Simulation_3hop_path.ipynb.ipynb** has the short test of simulation M/M/1, M/G/1 queing models and their comparison;
2. The main script is **simulation_MG1.ipynb** for two network topologies **Milano** and **Tokyo** using Google Colab notebooks. 

## Data Source
The average packet size is taken from the Amsterdam Internet Exchange [Ethernet Frame Size Distribution](https://stats.ams-ix.net/sflow/size.html), available online at https://stats.ams-ix.net/sflow/size.html, accessed in July 2023. The packet sizes and their probabilities are as follows:

Packet sizes in Bytes: 64-127, 128-511, 512 - 1023, 1024 - 1513, 1514, more than 1515.

Probabilities: 0.332, 0.054, 0.033, 0.037, 0.346, 0.146, 0.052.

The code also uses an input file with topology information, including links, nodes, and traffic.

Excel files contains topology information with three pages:

* Nodes types and node labeling;
* List of links between nodes (source and destination columns), including distance and capacity for each link;
* List of traffic flows with source and destination, specifying traffic in Gbps.
  
## Prerequisites
To run the code, you will need the following:

R programming language
The following R libraries:
* `simmer`
* `igraph`
* `readxl`
* `dplyr`
* `ggplot2`

## Usage
Open the Google Colab notebook containing the code.
When prompted, enter either "Tokyo" or "Milano" to choose the respective topology.
The code will then load the appropriate input file and perform the simulations and delay calculations.
The results, including plots and delay calculations, will be generated and saved to a report file.
Example usage:
```ruby
Enter 'Tokyo' or 'Milano' to choose the respective topology: Tokyo
```


## Theoretical calculations  
Calculates the end-to-end delays using a theoretical approach.
The code calculates total traffic in each link and theoretical delays for the M/G/1 queuing model using the _ighraph_ librabry, considering propagation, transmission, and queuing delays. 
Calculates the end-to-end delays using a theoretical approach, leveraging the Vysochanskij–Petunin's bound and other mathematical formulas for estimating the theoretical 99th percentile upper bound as the worst-case scenario.

## Experimental calculations
Performs the simulation with an M/G/1 queueing model.
The next section simulates experiments using DES (Discrete Event Simulator) with the `simmer` library, considering all traffic flows. It computes the experimental 99th percentile and compares it with several theoretical bounds (Chebisev, Vysochanskij–Petunin's, and Markov) for validation.

## Results
The code generates boxplots illustrating:
1. Delays (average delay and Vysochanskij–Petunin's upper bound) for various capacities (10 Gbps, 40 Gbps, 100 Gbps, and 400 Gbps) of each link, accounting for propagetion, transmission, and queuing delay.
   
    <img src="/reports/output_data_2.png" alt="Capacities" width="500">
    
2. Delays (average delay and Vysochanskij–Petunin's upper bound) for different link distances (x1, x2, x4, x10) accounting propagetion, transmission, and queuing delay.
   
   <img src="/reports/output_data_1.png" alt="Distances" width="500">
   
The results are reported in the files **Milano_topology_result.xlsx** and **Tokyo_topology_result.xlsx**.





