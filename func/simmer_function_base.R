library(simmer)
library(SciViews)

#########function creates the resources
simmer_resources <- function(graph, env){
  for (i in 1:length(V(graph)))
  {
    env %>%
      add_resource(paste0("node_", i), 1) 
  }
  return(env)
}


#########function calculates the shortest path and creates the trajectory
#simmer_traffic_simulation
simmer_trajectory_simulation <- function(env, from, to, trajectory_name, graph, log_info)
{
  path <- shortest_paths(graph, from, to ,weights = NULL, output = "both", algorithm = c("automatic"))
  path_size <- length(path[["vpath"]][[1]])
  nodes_capacity <- rep(0, length(V(graph)))
  nodes_capacity[path[["vpath"]][[1]][-path_size]] <- E(graph)$Capacity[path[["epath"]][[1]]]
  nodes_prop_delay <- rep(0, length(V(graph)))
  nodes_prop_delay[path[["vpath"]][[1]][-path_size]] <- E(graph)$Prop_Delay[path[["epath"]][[1]]]
  trajectory_name <- lapply(path[["vpath"]][[1]][-path_size], function(i) {
    trajectory() %>%
      seize(paste0("node_", i)) %>%
      set_attribute("capacity", nodes_capacity[i]) %>%
      timeout(function() rexp(1, get_attribute(env, "capacity"))) %>%
      #timeout(function() rexp(1, nodes_capacity[i])) %>%
      release(paste0("node_", i))  %>%
      timeout(function() nodes_prop_delay[i]) #%>%
  }) %>% join()

  delay_prop <- rep(0, length(V(graph)))
  delay_transm <- rep(0, length(V(graph)))
  l=0
  for (i in path[["vpath"]][[1]][-path_size]) { 
    l = l + 1
    print(path[["epath"]][[1]][l])
    delay_transm[i] <- E(graph)$Queue_Delay[path[["epath"]][[1]][l]]
    delay_prop[i] <- E(graph)$Prop_Delay[path[["epath"]][[1]][l]]
  }
    return (list(trajectory_name, delay_transm, delay_prop))
}

simmer_trajectory_simulation_without_prop_delay <- function(env, from, to, trajectory_name, graph, log_info)
{
  path <- shortest_paths(graph, from, to ,weights = NULL, output = "both", algorithm = c("automatic"))
  path_size <- length(path[["vpath"]][[1]])
  nodes_capacity <- rep(0, length(V(graph)))
  nodes_capacity[path[["vpath"]][[1]][-path_size]] <- E(graph)$Capacity[path[["epath"]][[1]]]
  nodes_prop_delay <- rep(0, length(V(graph)))
  nodes_prop_delay[path[["vpath"]][[1]][-path_size]] <- E(graph)$Prop_Delay[path[["epath"]][[1]]]
  trajectory_name <- lapply(path[["vpath"]][[1]][-path_size], function(i) {
    trajectory() %>%
      seize(paste0("node_", i)) %>%
      set_attribute("capacity", nodes_capacity[i]) %>%
      timeout(function() rexp(1, get_attribute(env, "capacity"))) %>%
      #timeout(function() rexp(1, nodes_capacity[i])) %>%
      release(paste0("node_", i))  #%>%
      #timeout(function() nodes_prop_delay[i]) #%>%
  }) %>% join()
  
  delay_prop <- rep(0, length(V(graph)))
  delay_transm <- rep(0, length(V(graph)))
  l=0
  for (i in path[["vpath"]][[1]][-path_size]) { 
    l = l + 1
    print(path[["epath"]][[1]][l])
    delay_transm[i] <- E(graph)$Queue_Delay[path[["epath"]][[1]][l]]
    delay_prop[i] <- E(graph)$Prop_Delay[path[["epath"]][[1]][l]]
  }
  return (list(trajectory_name, delay_transm, delay_prop))
}



#########function generates the traffic 
simmer_generator <- function(env, traffic_trajectory, traffic_name, traffic_val)
{
  env %>%
    add_generator(traffic_name, traffic_trajectory, function() rexp(1, traffic_val))
  return (env)
}

########### generator + trajectory 
simmer_simulation<- function(env, from, to, graph, N, traffic_val, log_info, tr_units = 10^9, bps_ps = TRUE)
{
  name = paste0('traffic_', from, "_", to, "_", log_info)
  if (from != to && traffic_val != 0) { 
    c(traffic, delay_transm, delay_prop) := simmer_trajectory_simulation(env, from, to, trajectory_name = traffic , graph, log_info) #creating trajectory
    assign(name, traffic)
    print(name)
    #print(get(name))
    env<- simmer_generator(env, get(name), name, traffic_val) 
  }
  return(list(env, name, delay_transm, delay_prop))
}

simmer_node_trajectory <- function(i, nodes_capacity) {
  i <- force(i)
  trajectory() %>%
    seize(paste0("node_", i)) %>%
    timeout(function() rexp(1, nodes_capacity[i])) %>%
    release(paste0("node_", i))
}
  
  
#####################creation of cross trajectories on the path if igraph load > lambda/capacity
simmer_crosstrajectory_simulation <- function(env, g, from, to, lambda_simmer)
{
  path <- shortest_paths(g, from, to, weights = NULL, output = "both", algorithm = c("automatic"))

  path_size <- length(path[["vpath"]][[1]])
  nodes_capacity <- rep(0, length(V(g)))
  nodes_capacity[path[["vpath"]][[1]][-path_size]] <- E(g)$Capacity[path[["epath"]][[1]]]

  nodes_load <- rep(0, length(V(g)))
  nodes_load[path[["vpath"]][[1]][-path_size]] <- E(g)$Load[path[["epath"]][[1]]]

  for (i in path[["vpath"]][[1]][-path_size])
  {
    
    load_compare <- lambda_simmer/nodes_capacity[i]
    if (nodes_load[i] > load_compare )
    {
      print(paste0("node_", i))
      print(nodes_capacity[i])
      print(nodes_load[i])
      trajectory_name <- trajectory() %>%
        seize(paste0("node_", i)) %>%
        set_attribute("capacity", nodes_capacity[i]) %>%
        timeout(function() rexp(1, get_attribute(env, "capacity"))) %>%
        #timeout(function() rexp(1, nodes_capacity[i])) %>%
        release(paste0("node_", i)) #%>%

      name <- paste0("traffic_cross_", from,"_", to, "_node_", i,"_")
      assign(name, trajectory_name)
      traffic_val <- nodes_load[i] * nodes_capacity[i] - lambda_simmer
      print(traffic_val)
      env<- simmer_generator(env, get(name), name, traffic_val) 
    }
  }
  return(env)
}


##################plot the bounds for node
bounds_function_node <- function(mu_mean, X, digits = 4, info = "")
{
  a = 0.9
  variance = var(X)
  mu <- mu_mean
  c(upper_bound_Markov, upper_bound_cheb, upper_bound_heoff, upper_bound_VP) := func_bounds_theoretical(mu, variance, a, delay_queue = mu, delay_prop = 0, percentile_of_max = 0.9)
  
  print(paste("simmer E(T) =", mean(X)))
  p_90 <- unname(quantile(X, probs = 0.9))
  p_99 <- unname(quantile(X, probs = 0.99))
  dens <- density(X)
  
  plot(dens,xlim=c(0, upper_bound_Markov + mu), lwd = 2, col = "red", main = info)
  
  abline( v = p_90, col = "red", lty = 3)
  abline( v = p_99, col = "red", lty = 3)
  abline( v = mu, col = "red", lty = 3)
  abline( v = upper_bound_cheb, col = "red", lty = 3)
  abline( v = upper_bound_heoff, col = "red", lty = 3)
  abline( v = upper_bound_Markov, col = "red", lty = 3)
  abline( v = upper_bound_VP, col = "red", lty = 3)
  
  middle_plot = (min(dens$y) + max(dens$y))/2 
  text(x = upper_bound_cheb,y = middle_plot ,paste("Cheb_90 =", round(upper_bound_cheb, digits)),srt=0.2,pos=3,srt=90)
  text(upper_bound_heoff,middle_plot,paste("Hoeff_90 =", round(upper_bound_heoff, digits)),srt=0.2,pos=3,srt=90)
  text(upper_bound_Markov,middle_plot,paste("Mark_90 =",round(upper_bound_Markov, digits)),srt=0.2,pos=3,srt=90)
  text(upper_bound_Markov,middle_plot,paste("Mark_90 =",round(upper_bound_Markov, digits)),srt=0.2,pos=3,srt=90)
  text(upper_bound_VP,middle_plot,paste("VP_90 =", round(upper_bound_VP, digits)),srt=0.2,pos=3,srt=90)
  text(mu,middle_plot,paste("E(T)th =", round(mu, digits)),srt=0.2,pos=3,srt=90)
  text(p_90,middle_plot,paste("p_90 =", round(p_90, digits)),srt=0.2,pos=3,srt=90)
  text(p_99,middle_plot,paste("p_99 =", round(p_99, digits)),srt=0.2,pos=3,srt=90)
  
}


##################plot the bounds
bounds_function_1w_path <- function(delay_prop, delay_queue, X, digits = 4, info = "")
{
  a = 0.99
  variance = var(X)
  mu <- sum(delay_prop, delay_queue)
  c(upper_bound_Markov, upper_bound_cheb, upper_bound_heoff) := func_bounds_theoretical(mu, variance, a, delay_queue, delay_prop, percentile_of_max = 0.9)
    
  print(paste("simmer E(T) =", mean(X)))
  p_90 <- unname(quantile(X, probs = 0.9))
  p_99 <- unname(quantile(X, probs = 0.99))
  dens <- density(X)
  #without t_prop
  #plot(dens,xlim=c(min(dens$x), max(dens$x)+mu), lwd = 2, col = "red", main = info) 
  #with t_prop
  plot(dens,xlim=c(min(dens$x), max(dens$x)), lwd = 2, col = "red", main = info) 
  
  abline( v = p_90, col = "red", lty = 3)
  abline( v = p_99, col = "red", lty = 3)
  abline( v = mu, col = "red", lty = 3)
  #abline( v = upper_bound_cheb, col = "red", lty = 3)
  abline( v = upper_bound_heoff, col = "red", lty = 3)
  abline( v = mean(X), col = "red", lty = 3)
  #abline( v = upper_bound_Markov, col = "red", lty = 3)
  #abline( v = sum(delay_prop), col = "red", lty = 3)
  middle_plot = (min(dens$y) + max(dens$y))/2 
  #text(x = upper_bound_cheb,y = middle_plot ,paste("Cheb_90 =", round(upper_bound_cheb, digits)),srt=0.2,pos=3,srt=90)
  text(upper_bound_heoff,middle_plot,paste("Hoeff_99 =", round(upper_bound_heoff, digits)),srt=0.2,pos=3,srt=90)
  #text(upper_bound_Markov,middle_plot,paste("Mark_90 =",round(upper_bound_Markov, digits)),srt=0.2,pos=3,srt=90)
  text(mu,middle_plot,paste("E(T)th =", round(mu, digits)),srt=0.2,pos=3,srt=90)
  text(p_90,middle_plot,paste("p_90 =", round(p_90, digits)),srt=0.2,pos=3,srt=90)
  text(p_99,middle_plot,paste("p_99 =", round(p_99, digits)),srt=0.2,pos=3,srt=90)
  text(mean(X),middle_plot,paste("E(T)sim =", round(mean(X), digits)),srt=0.2,pos=3,srt=90)
  
  #text(sum(delay_prop), middle_plot, paste("Prop_delay =", round(sum(delay_prop), digits)), srt=0.2, pos=3, srt=90)
  
 
}

############################################g/g/1
simmer_trajectory_simulation_gg1 <- function(env, from, to, trajectory_name, graph, log_info)
{
  path <- shortest_paths(graph, from, to ,weights = NULL, output = "both", algorithm = c("automatic"))
  path_size <- length(path[["vpath"]][[1]])
  nodes_capacity <- rep(0, length(V(graph)))
  nodes_capacity[path[["vpath"]][[1]][-path_size]] <- E(graph)$Capacity[path[["epath"]][[1]]]
  nodes_prop_delay <- rep(0, length(V(graph)))
  nodes_prop_delay[path[["vpath"]][[1]][-path_size]] <- E(graph)$Prop_Delay[path[["epath"]][[1]]]
  trajectory_name <- lapply(path[["vpath"]][[1]][-path_size], function(i) {
    trajectory() %>%
      seize(paste0("node_", i)) %>%
      set_attribute("capacity", nodes_capacity[i]) %>%
      timeout(function() rexp(1, get_attribute(env, "capacity"))) %>%
      #timeout(function() rexp(1, nodes_capacity[i])) %>%
      release(paste0("node_", i))  %>%
      timeout(function() nodes_prop_delay[i]) #%>%
    
  }) %>% join()
  
  delay_prop <- rep(0, length(V(graph)))
  delay_transm <- rep(0, length(V(graph)))
  l=0
  for (i in path[["vpath"]][[1]][-path_size]) { 
    l = l + 1
    print(path[["epath"]][[1]][l])
    delay_transm[i] <- E(graph)$Queue_Delay[path[["epath"]][[1]][l]]
    delay_prop[i] <- E(graph)$Prop_Delay[path[["epath"]][[1]][l]]
  }
  return (list(trajectory_name, delay_transm, delay_prop))
}

############################################m/g/1
#####################creation of cross trajectories on the path if igraph load > lambda/capacity
simmer_crosstrajectory_simulation_mg1 <- function(env, g, from, to, lambda_simmer, PS_size, PS_weights)
{
  path <- shortest_paths(g, from, to, weights = NULL, output = "both", algorithm = c("automatic"))
  N = sum(PS_size*PS_weights)
  path_size <- length(path[["vpath"]][[1]])
  nodes_capacity <- rep(0, length(V(g)))
  nodes_capacity[path[["vpath"]][[1]][-path_size]] <- E(g)$capacityGbps[path[["epath"]][[1]]]
  nodes_capacity_Bps <- nodes_capacity*1e9
  nodes_capacity <- nodes_capacity_Bps/(8*N)
  
  nodes_load <- rep(0, length(V(g)))
  nodes_load[path[["vpath"]][[1]][-path_size]] <- E(g)$Load[path[["epath"]][[1]]]
  
  for (i in path[["vpath"]][[1]][-path_size])
  {
    
    load_compare <- lambda_simmer/(nodes_capacity[i])
    if (nodes_load[i] > load_compare )
    {
      print(paste0("node_", i))
      print(nodes_capacity[i])
      print(nodes_load[i])
      trajectory_name <- trajectory() %>%
        seize(paste0("node_", i)) %>%
        set_attribute("capacity", nodes_capacity_Bps[i]) %>%
        timeout(function() 8*sample(PS_size,size = 1, replace = T, prob = PS_weights)/(get_attribute(env, "capacity"))) %>%
        #timeout(function() rexp(1, get_attribute(env, "capacity"))) %>%
        #timeout(function() rexp(1, nodes_capacity[i])) %>%
        release(paste0("node_", i)) #%>%
      
      name <- paste0("traffic_cross_", from,"_", to, "_node_", i,"_")
      assign(name, trajectory_name)
      traffic_val <- nodes_load[i] * nodes_capacity[i] - lambda_simmer
      print(traffic_val)
      print(lambda_simmer)
      print(nodes_load[i]*nodes_capacity[i])
      env<- simmer_generator(env, get(name), name, traffic_val)
    }
  }
  return(env)
}



########### generator + trajectory 
simmer_simulation_mg1 <- function(env, from, to, graph, traffic_val, log_info, tr_units = 10^9, bps_ps = TRUE, PS_size, PS_weights)
{
  name = paste0('traffic_', from, "_", to, "_", log_info)
  if (from != to && traffic_val != 0) { 
    c(traffic, delay_transm, delay_prop) := simmer_trajectory_simulation_mg1(env, from, to, trajectory_name = traffic , graph, log_info, PS_size = PS_size, PS_weights = PS_weights) #creating trajectory
    assign(name, traffic)
    print(name)
    #print(get(name))
    env<- simmer_generator(env, get(name), name, traffic_val) 
  }
  return(list(env, name, delay_transm, delay_prop))
}

#########function calculates the shortest path and creates the trajectory
#simmer_traffic_simulation
simmer_trajectory_simulation_mg1 <- function(env, from, to, trajectory_name, graph, log_info, PS_size, PS_weights)
{
  path <- shortest_paths(g, from, to, weights = NULL, output = "both", algorithm = c("automatic"))
  N = sum(PS_size*PS_weights)
  path_size <- length(path[["vpath"]][[1]])
  nodes_capacity <- rep(0, length(V(g)))
  nodes_capacity[path[["vpath"]][[1]][-path_size]] <- E(g)$capacityGbps[path[["epath"]][[1]]]
  nodes_capacity_Bps <- nodes_capacity*1e9
  nodes_capacity <- nodes_capacity_Bps/(8*N)
  
  nodes_load <- rep(0, length(V(g)))
  nodes_load[path[["vpath"]][[1]][-path_size]] <- E(g)$Load[path[["epath"]][[1]]]
  
  nodes_prop_delay <- rep(0, length(V(graph)))
  nodes_prop_delay[path[["vpath"]][[1]][-path_size]] <- E(graph)$Prop_Delay[path[["epath"]][[1]]]
  
  trajectory_name <- lapply(path[["vpath"]][[1]][-path_size], function(i) {
    trajectory() %>%
      seize(paste0("node_", i)) %>%
      set_attribute("capacity", nodes_capacity_Bps[i]) %>%
      #set_attribute(function() "packet_size", sample(PS_size, size = 1, replace = T, prob = PS_weights)) %>%
      timeout(function() 8*sample(PS_size, size = 1, replace = T, prob = PS_weights)/(get_attribute(env, "capacity"))) %>%
      #timeout(function() rexp(1, get_attribute(env, "capacity"))) %>%
      #timeout(function() rexp(1, nodes_capacity[i])) %>%
      release(paste0("node_", i))  %>%
      timeout(function() nodes_prop_delay[i]) #%>%
  }) %>% join()
  
  delay_prop <- rep(0, length(V(graph)))
  delay_transm <- rep(0, length(V(graph)))
  l=0
  for (i in path[["vpath"]][[1]][-path_size]) { 
    l = l + 1
    print(path[["epath"]][[1]][l])
    delay_transm[i] <- E(graph)$Queue_Delay[path[["epath"]][[1]][l]]
    delay_prop[i] <- E(graph)$Prop_Delay[path[["epath"]][[1]][l]]
  }
  return (list(trajectory_name, delay_transm, delay_prop))
}
