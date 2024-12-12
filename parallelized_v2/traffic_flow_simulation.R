#!/usr/bin/env Rscript
library(ggplot2)
library(igraph)
library(stringr)
library(tidyverse)
library(readr)
library(SciViews)
library(dplyr)
library(viridis)
library(readxl)
library(openxlsx)
library(simmer)

source("traffic-latency-simulator/func/igraph_functions.R")
source("traffic-latency-simulator/func/general_function_base.R")
source("traffic-latency-simulator/func/simmer_function_base.R")

parallel_simmer_simulation <- function(line, traffic_file, g, PS_size, PS_weights, digits){
	    
  env <- simmer()
  env <- simmer_resources(g, env) # add resources for all nodes of igraph (func is in the file simmer_fictions)
  lambda_simmer <- traffic_file$traffic_ps[line]
  set.seed(42)
  env <- simmer_crosstrajectory_simulation_mg1(env, g, traffic_file$sourceID[line], traffic_file$destinationID[line], lambda_simmer, PS_size = PS_size, PS_weights = PS_weights) #creating cross trajectories with theirs generator  (func is in the file simmer_fictions)
  c(env, traffic_traj_name, delay_queue, delay_prop) := simmer_simulation_mg1(env, traffic_file$sourceID[line], traffic_file$destinationID[line], g, traffic_val=lambda_simmer, log_info = "", PS_size = PS_size, PS_weights = PS_weights) #creation main trajectory with its generator
        
  env %>% #env execution
    run(10000/lambda_simmer) #10000 packets of the main lambda trajectory
        
       
  #########################delay results processing
  all_arrivals_res <- data.frame(env %>%
                                    get_mon_arrivals(per_resource = TRUE) %>%
                                    transform(waiting_time_in_queue = round(end_time - start_time - activity_time)) %>%
                                    transform(spending_time = end_time - start_time))
	  
	  
	  ############################plots for each node (only with queuing delay)
	  ##########computing the bounds and parameters (mean, variance, and etc)
	  path <- shortest_paths(g, traffic_file$sourceID[line], traffic_file$destinationID[line] ,weights = NULL, output = "both", algorithm = c("automatic"))
	  path_size <- length(path[["vpath"]][[1]])
	    n=1
	    for (node in path[["vpath"]][[1]][-path_size])
		      {
			          traffic <- dplyr::filter(all_arrivals_res, paste0("node_", node) == all_arrivals_res$resource)
	        if (length(traffic$spending_time) != 0)
			    {
				          print(node)
		      delay_nodes <- mean(traffic$spending_time)
		            p_99_result <- unname(quantile(traffic$spending_time, probs = 0.99))
		            ####################
		            print(paste("Node - ", node, ":"))
			          bounds_function_node(delay_queue[node], traffic$spending_time, digits, info = paste("Node - ", node, ";","For the traffic flow:from",traffic_file$sourceID[line],"to", traffic_file$destinationID[line]))#plot the results for each node
			          E(g)[get.edge.ids(g, c(path[["vpath"]][[1]][n],path[["vpath"]][[1]][n+1]))]$Queue_Delay_sim <- delay_nodes
				        E(g)[get.edge.ids(g, c(path[["vpath"]][[1]][n],path[["vpath"]][[1]][n+1]))]$Queue_Delay_sim_99p <- p_99_result
				        E(g)[get.edge.ids(g, c(path[["vpath"]][[1]][n],path[["vpath"]][[1]][n+1]))]$Queue_Delay_theor_VP99 <- func_bounds_VP(delay_queue[node], 0.99)
					      
					      print(E(g)[get.edge.ids(g, c(path[["vpath"]][[1]][n],path[["vpath"]][[1]][n+1]))])
					      n=n+1
					            
					          }
		  }
	      
	      ###########################plots for each traffic flow (prop + queuing delays)
	      #########################delay results processing
	      all_arrivals_res <- data.frame(env %>%
					                                        get_mon_arrivals(per_resource = FALSE) %>%
										                                   transform(waiting_time_in_queue = round(end_time - start_time - activity_time)) %>%
														                                      transform(spending_time = end_time - start_time))
	      traffic <- dplyr::filter(all_arrivals_res, grepl(traffic_traj_name , all_arrivals_res$name))
	        exp_delay = sum(delay_prop,delay_queue)
	        
	        bounds_function_1w_path(sum(delay_prop), sum(delay_queue), traffic$spending_time, digits, info = paste("For the traffic flow:from",traffic_file$sourceID[line],"to", traffic_file$destinationID[line]))#plot the results for tr flow
		  #queue_delay_tr_flow <- append(queue_delay_tr_flow, sum(delay_queue))
		  #prop_delay_tr_flow <- append(prop_delay_tr_flow, sum(delay_prop))
		  #delay_tr_flow <- append(delay_tr_flow, mean(traffic$spending_time))
		  #delay_tr_flow_p_99 <- append(delay_tr_flow_p_99, unname(quantile(traffic$spending_time, probs = 0.99)))
		  VP_upbound <- func_bounds_VP(delay_queue, 0.99) + sum(delay_prop)
		  #delay_tr_flow_VP_upbound_p_99_100 <- append(delay_tr_flow_VP_upbound_p_99_100, VP_upbound)
		  #env_w <- wrap(env)
		  
		  return(c(exp_delay, sum(delay_queue), sum(delay_prop), mean(traffic$spending_time), 
			            unname(quantile(traffic$spending_time, probs = 0.99)), VP_upbound))
} 

args <- commandArgs(trailingOnly = TRUE)
filename <- "params"
if (length(args)>0){
 filename<-args[1]
}
print(filename)
load(filename)

res <- parallel_simmer_simulation(line, traffic_file, g, PS_size, PS_weights)

save_as <- paste0(line, "_results")

save(res, file=save_as)
