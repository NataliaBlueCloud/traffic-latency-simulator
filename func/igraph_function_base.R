
# Calculation traffic attributes for each link for a specific type of traffic, type of destination nodes: (precise columns in excel file, except local traffic)

calc_traffic_links <- function(graph, node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Reference National CO`, traffic_type, traffic_out_attr = 'Traffic_Browser_National', link = "UL")
{
  from = resource[node]
  to = destination[node]
  print(paste("from=", from,",to=", to))
  path <- shortest_paths(graph, from, to,
                           weights = NULL,
                           output = "both",
                           algorithm = c("automatic"))
  if (grepl('Browser', traffic_out_attr)==1) {service =  "CWB" }
  if (grepl('Direct', traffic_out_attr)==1) {service =  "DCM" }
  if (grepl('Demand', traffic_out_attr)==1) {service =  "CDN" }
  #add_tr_info = paste(from, to, service, traffic_type[node])
  #list_of_atrr <- append(list_of_atrr,add_tr_info,after=length(list_of_atrr))
  #edge_attr(graph = graph, name = add_tr_info, index = E(graph)) <- 0 # create a new attribute
  new_data <- seq(from = 0, to = 0, length = length(E(graph)))
  for (x in path[["epath"]][[1]]) {
    curr_traffic = edge_attr(graph, name=traffic_out_attr, index = E(graph)[x])
    graph <- set_edge_attr(graph, name = traffic_out_attr, index = E(graph)[x], value = curr_traffic + traffic_type[node])
    #graph <- set_edge_attr(graph, name = add_tr_info, index = E(graph)[x], value = traffic_type[node])
    #print(add_tr_info)
    print(traffic_type[node])
    new_data[x] <-traffic_type[node]
  }
  new_row <- c(from, to, service, traffic_type[node], link)
  new_row <- append(new_row, new_data , after = length(new_row))
  .GlobalEnv$df_log_links[nrow(.GlobalEnv$df_log_links) + 1, ] <- new_row
  return(graph)
}

 


#traffic_df_sc1$`CWB term. @ Reference National UL`[3]
#traffic_df_sc1$`CWB term. @ Reference National UL`
#edge_attr(graph = graph1, name = 'Traffic_Browser_National', index = E(graph1)) <- 0 # create a new attribute
#graph1 = calc_traffic_links(graph1, 3, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Reference National CO`, traffic_type = traffic_df_sc1$`CWB term. @ Reference National UL`, traffic_out_attr = 'Traffic_Browser_National')

#Calculation traffic attributes for each node for a specific type of traffic, type of destination nodes: 
#(precise columns in excel file, except local traffic)

calc_traffic_nodes <- function(graph, node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Reference National CO`, traffic_type = `CWB term. @ Reference National UL`, start_traffic = 'start_traffic', end_traffic = 'end_traffic', pass_traffic = 'pass_traffic', link = "UL") {
  from = resource[node]
  to = destination[node]
  path <- shortest_paths(graph, from, to,
                           weights = NULL,
                           output = "both",
                           algorithm = c("automatic"))
  if (grepl('Browser', start_traffic)==1) {service = "CWB" }
  if (grepl('Direct', start_traffic)==1) {service =  "DCM" }
  if (grepl('Demand', start_traffic)==1) {service =  "CDN" }
  n = 0
  new_data <- seq(from = 0, to = 0, length = length(V(graph)))
  for (x in path[["vpath"]][[1]]) {
    if (n == 0) {
      curr_traffic = vertex_attr(graph, name=start_traffic, index = V(graph)[x])
      graph <- set_vertex_attr(graph, name = start_traffic,index = V(graph)[x], value = curr_traffic + traffic_type[node])
      if (from == to) 
      {
        curr_traffic = vertex_attr(graph, name=end_traffic, index = V(graph)[x])
        graph <- set_vertex_attr(graph, name = end_traffic,index = V(graph)[x], value = curr_traffic + traffic_type[node])
      }
    }
    else if(n == (length(path[["vpath"]][[1]]) - 1)){
      curr_traffic = vertex_attr(graph, name=end_traffic, index = V(graph)[x])
      graph <- set_vertex_attr(graph, name = end_traffic,index = V(graph)[x], value = curr_traffic + traffic_type[node])
    }
    else{
      curr_traffic = vertex_attr(graph, name=pass_traffic, index = V(graph)[x])
      graph <- set_vertex_attr(graph, name = pass_traffic,index = V(graph)[x], value = curr_traffic + traffic_type[node])
    }
    n = n + 1
    new_data[x] <-traffic_type[node]
  }
  new_row <- c(from, to, service, traffic_type[node], link)
  new_row <- append(new_row, new_data , after = length(new_row))
  #print(new_row)
  .GlobalEnv$df_log_nodes_flows[nrow(.GlobalEnv$df_log_nodes_flows) + 1, ] <- new_row
  return(graph)
}


#calculation the delays for nodes depending on the destination 
calc_node_delay <- function(graph, node, source, destination, delay_type, delay_type_out)
{
  from = source[node]
  to = destination[node]
  path <- shortest_paths(graph, from, to,
                         weights = NULL,
                         output = "both",
                         algorithm = c("automatic"))
  #print(path[["epath"]][[1]])
  for (link in path[["epath"]][[1]]) 
    {
    link_delay = edge_attr(graph, name = delay_type, index = E(graph)[link])
    curr_delay = vertex_attr(graph, name = delay_type_out, index = V(graph)[node])
    graph <- set_vertex_attr(graph, name = delay_type_out,index = V(graph)[node], value = curr_delay + link_delay)
  }
  return(graph)
}

#function of creating the report for edges and vertices:
write_report <- function(graph, file_type = 0, scenario_number = 1) # file_extension = 0 - Excel, file_extension = 1 - csv
  {
    options(digits.secs=6)              ## switch to subsecond display
    s = format(Sys.time(), "_%Y-%m-%d_%H_%M_%OS")
    s = str_replace_all(s, '[.]', '_')
    
    df1.vs <- as_data_frame(graph,  what="vertices")
    df1.es <- as_data_frame(graph,  what="edges")
    
    if (file_type == 0)
      {
        a = "Reports//edges//Metro_topology_Edges_result"
        c = ".xlsx"
        sheet1 = paste("scenario", scenario_number, sep = "")
        write.xlsx(df1.es, file = paste(a,sheet1,s,c, sep = ""), sheetName = sheet1)
        d = "Reports//vertices//Metro_topology_Vertices_result"
        write.xlsx(df1.vs, file = paste(d,sheet1,s,c, sep = ""), sheetName = sheet1)
      }
    else
      {
        
      }
    
  }



write_nodes_log_report_base <- function(graph = graph, service = "CWB" ){
  new_df <- data.frame(matrix(ncol = 0, nrow = length(V(graph))))
  new_df$nodes <- attributes(V(graph))[["names"]]
  new_df$service <- service
  if (service == "CWB")
  {
    new_df$traffic_start <- V(graph)$Traffic_Browser_Start
    new_df$traffic_end<- V(graph)$Traffic_Browser_End
    new_df$traffic_bypass <- V(graph)$Traffic_Browser_Pass
  }
  if (service == "DCM")
  {
    new_df$traffic_start <- V(graph)$Traffic_Direct_Start
    new_df$traffic_end<- V(graph)$Traffic_Direct_End
    new_df$traffic_bypass <- V(graph)$Traffic_Direct_Pass
  }
  if (service == "CDN")
  {
    new_df$traffic_start <- V(graph)$Traffic_Demand_Start
    new_df$traffic_end<- V(graph)$Traffic_Demand_End
    new_df$traffic_bypass <- V(graph)$Traffic_Demand_Pass
  }
  .GlobalEnv$df_log_nodes_total[nrow(.GlobalEnv$df_log_nodes_total) + V(graph), ] <- new_df
}

write_nodes_log_report <- function(graph = graph){
  df_log_nodes_total <<- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(df_log_nodes_total) <<- c("nodes", "service", "traffic_start", "traffic_end", "traffic_bypass")
  write_nodes_log_report_base(graph = graph, service = "CWB")
  write_nodes_log_report_base(graph = graph, service = "DCM")
  write_nodes_log_report_base(graph = graph, service = "CDN")
}
