
#Traffic calc functions
calc_full_traffic_for_matrix <- function(graph, matrix_traffic, N =1250) {
  
  ############log file
  df_log_links <<- data.frame(matrix(ncol = length(E(graph)) + 5, nrow = 0))
  names <- c("source", "destination", "service", "traffic_offered" , "UL/DL")
  colnames(df_log_links) <<- append(names, attributes(E(graph))[["vnames"]], after = length(names))
  df_log_nodes_flows <<- data.frame(matrix(ncol = length(V(graph)) + 5, nrow = 0))
  colnames(df_log_nodes_flows) <<- append(names, attributes(V(graph))[["names"]], after = length(names))
  df_log_nodes_total <<- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(df_log_nodes_total) <<- c("nodes", "service", "traffic_start", "traffic_end", "traffic_bypass")
  
  
  edge_attr(graph = graph, name = 'Traffic', index = E(graph)) <- 0 # create a new attribute
  
  names_vertices <- list('Traffic_Start', 'Traffic_Pass', 'Traffic_End', 'Traffic_Local')
  for (x in names_vertices)
  {
    vertex_attr(graph = graph, name = x, index = V(graph)) <- 0 # create a new attribute
  }
  vertex_attr(graph = g, name = "traffic", index = V(g)) <- 0 # create a new attribute
  
  for (i in V(graph))
  {
    for (j in V(graph))
    {
      if (matrix_traffic[i,j] != 0)
      {
        graph <- calc_traffic_links_for_matrix(graph, source = i, destination = j, traffic_type = matrix_traffic[i,j], traffic_out_attr = 'Traffic', link = "UL")
        graph <- calc_traffic_nodes_for_matrix(graph, source = i, destination = j , traffic_type = matrix_traffic[i,j], start_traffic = 'Traffic_Start', end_traffic = 'Traffic_End', pass_traffic = 'Traffic_Pass',local_traffic = 'Traffic_Local', link = "UL")
      }
    }
  }
  V(graph)$Traffic_Total = V(graph)$Traffic_Start + V(graph)$Traffic_Pass + V(graph)$Traffic_End + V(graph)$Traffic_Local
  #Gbps <- ps
  V(graph)$Traffic_Start_Gbps <- V(graph)$Traffic_Start*8*N/(10^9)
  V(graph)$Traffic_Pass_Gbps <- V(graph)$Traffic_Pass*8*N/(10^9)
  V(graph)$Traffic_End_Gbps <- V(graph)$Traffic_End*8*N/(10^9)
  V(graph)$Traffic_Local_Gbps <- V(graph)$Traffic_Local*8*N/(10^9)
  V(graph)$Traffic_Total_Gbps <- V(graph)$Traffic_Start_Gbps + V(graph)$Traffic_Pass_Gbps + V(graph)$Traffic_End_Gbps + V(graph)$Traffic_Local_Gbps
  
  write_nodes_log_report(graph)
  return(graph)
}


calc_traffic_links_for_matrix <- function(graph, source, destination, traffic_type, traffic_out_attr = 'Traffic_Browser_National', link = "UL", service = "Unknown")
{
  from = source
  to = destination
  
  path <- shortest_paths(graph, from, to,
                         weights = NULL,
                         output = "both",
                         algorithm = c("automatic"))
 
  new_data <- seq(from = 0, to = 0, length = length(E(graph)))
  for (x in path[["epath"]][[1]]) {
    curr_traffic = edge_attr(graph, name=traffic_out_attr, index = E(graph)[x])
    graph <- set_edge_attr(graph, name = traffic_out_attr, index = E(graph)[x], value = curr_traffic + traffic_type)
    new_data[x] <-traffic_type
  }
  new_row <- c(from, to, service, traffic_type, link)
  new_row <- append(new_row, new_data , after = length(new_row))
  .GlobalEnv$df_log_links[nrow(.GlobalEnv$df_log_links) + 1, ] <- new_row
  return(graph)
}

calc_traffic_nodes_for_matrix <- function(graph, source, destination, traffic_type, start_traffic = 'start_traffic', end_traffic = 'end_traffic', pass_traffic = 'pass_traffic', local_traffic, link = "UL", service = "Unknown") {
  from = source
  to = destination
  path <- shortest_paths(graph, from, to,
                         weights = NULL,
                         output = "both",
                         algorithm = c("automatic"))
  n = 0
  new_data <- seq(from = 0, to = 0, length = length(V(graph)))
  for (x in path[["vpath"]][[1]]) {
    if (n == 0) {
      if (from == to) 
      {
        curr_traffic = vertex_attr(graph, name=local_traffic, index = V(graph)[x])
        graph <- set_vertex_attr(graph, name = local_traffic,index = V(graph)[x], value = curr_traffic + traffic_type)}
      else 
      {
        curr_traffic = vertex_attr(graph, name=start_traffic, index = V(graph)[x])
        graph <- set_vertex_attr(graph, name = start_traffic,index = V(graph)[x], value = curr_traffic + traffic_type)}
    }
    else if(n == (length(path[["vpath"]][[1]]) - 1)){
      curr_traffic = vertex_attr(graph, name=end_traffic, index = V(graph)[x])
      graph <- set_vertex_attr(graph, name = end_traffic,index = V(graph)[x], value = curr_traffic + traffic_type)
    }
    else{
      curr_traffic = vertex_attr(graph, name=pass_traffic, index = V(graph)[x])
      graph <- set_vertex_attr(graph, name = pass_traffic,index = V(graph)[x], value = curr_traffic + traffic_type)
    }
    n = n + 1
    new_data[x] <-traffic_type
  }
  new_row <- c(from, to, service, traffic_type, link)
  new_row <- append(new_row, new_data , after = length(new_row))
  
  .GlobalEnv$df_log_nodes_flows[nrow(.GlobalEnv$df_log_nodes_flows) + 1, ] <- new_row
  return(graph)
}


#################function updates the file from input_files , adds load, delays, cost and etc
########################file_old_name <- "input_files/Metro_Haul_topology_full_info_18_05.xlsx"
########################file_name <- "reports/Metro_Haul_topology_full_info_22_05_v3.xlsx"
update_file <- function(g, file_old_name, file_name, delay_tr_flow, exp_delay_igraph, delay_tr_flow_p_99, delay_tr_flow_hoeffd_p_99, sheet_of_nodes = 1)
{
  
  df_sheet_links <- data.frame(sourceID=ends(g, E(g))[,1], destinationID = ends(g, E(g))[,2], capacity_Gbps = E(g)$capacityGbps, capacity_ps = E(g)$Capacity , distance_km = E(g)$Distance,
                               loadPercent = E(g)$Load, traffic_ps = E(g)$Traffic, latencyProp_s = E(g)$Prop_Delay, latencyTransQueuingTheor_s = E(g)$Queue_Delay, latencyTransQueuingSim_s = E(g)$Queue_Delay_sim, latencyTransQueuingSim_p99_s = E(g)$Queue_Delay_sim_99p, 
                               latencyTransQueuingTheor_hoeff99_s = E(g)$Queue_Delay_theor_hoeff99)
  
  df_sheet_nodes <- read_excel(file_old_name, sheet_of_nodes)
  df_sheet_nodes$vCPUs_tr_dependent <- V(g)$CPUs_tr_dependent
  df_sheet_nodes$SSDStorageTB_tr_dependent <- V(g)$TB_tr_dependent
  df_sheet_nodes$CostUnitsDC_tr_dependent <- V(g)$TB_tr_dependent
  df_sheet_nodes$CostUnitsTelco_tr_dependent <- V(g)$'Cost_tr_dependent_Switches+plug'
  df_sheet_nodes$CostTotal_tr_dependent <- V(g)$Cost_tr_dependent
  df_sheet_nodes$Power_consumption_tr_dependent_W <- V(g)$Power_consumption_tr_dependent
  
  
  df_sheet_nodes$vCPUs_basic <- V(g)$CPUs_basic
  df_sheet_nodes$SSDStorageTB_basic <- V(g)$TB_basic
  df_sheet_nodes$CostUnitsDC_basic <- V(g)$TB_basic
  df_sheet_nodes$CostUnitsTelco_basic <- V(g)$'Cost_basic_Switches+plug'
  df_sheet_nodes$CostTotal_basic <- V(g)$Cost_basic
  df_sheet_nodes$Power_consumption_basic_W <- V(g)$Power_consumption_basic
  
  
  
  df_sheet_traffic <- read_excel(file_old_name, sheet = 3)
  df_sheet_traffic$latencyPropTransQueuing_theor_s <- delay_tr_flow
  df_sheet_traffic$latencyPropTransQueuing_sim_s <- exp_delay_igraph
  df_sheet_traffic$latencyPercentile99_theor_s <- delay_tr_flow_p_99
  df_sheet_traffic$latencyPercentile99_sim_s <- delay_tr_flow_hoeffd_p_99
  
  wb <- createWorkbook()
  addWorksheet(wb, sheetName = "Links")
  writeData(wb, sheet = 1, df_sheet_links)
  addWorksheet(wb, sheetName = "Nodes")
  writeData(wb, sheet = 2, df_sheet_nodes)
  addWorksheet(wb, sheetName = "Services")
  writeData(wb, sheet = 3, df_sheet_traffic)
  addWorksheet(wb, sheetName = "EquipmentCost")
  writeData(wb, sheet = 4, read_excel(file_old_name, sheet = 4))
  saveWorkbook(wb, file_name, overwrite = TRUE)
  #safe log file 
  # df_sheet_links <- df_sheet_links[order(df_sheet_links$load, decreasing = TRUE),]
  # writexl::write_xlsx(read_excel(file_old_name, sheet = 4),file_name,sheetName="EquipmentCost",col.names=TRUE,append=TRUE)
  # writexl::write_xlsx(df_sheet_traffic,file_name, sheetName="Services", col.names=TRUE,append=TRUE)
  # writexl::write_xlsx(df_sheet_nodes,file_name, sheetName="Nodes", col.names=TRUE,append=TRUE)
  # writexl::write_xlsx(df_sheet_links,file_name,sheetName="Links", col.names=TRUE,append=TRUE)
  # 
}


