

source("functions_base.R")
source("functions_technoeconomic_calc.R")


#Traffic calc functions
calc_full_traffic <- function(graph = graph, traffic_df = traffic_df, Vertices_df = Vertices_df, prefix_base = "") {
  
  df_log_links <<- data.frame(matrix(ncol = length(E(graph)) + 5, nrow = 0))
  names <- c("source", "destination", "service", "traffic_offered" , "UL/DL")
  colnames(df_log_links) <<- append(names, attributes(E(graph))[["vnames"]], after = length(names))
  
  df_log_nodes_flows <<- data.frame(matrix(ncol = length(V(graph)) + 5, nrow = 0))
  colnames(df_log_nodes_flows) <<- append(names, attributes(V(graph))[["names"]], after = length(names))
  
  df_log_nodes_total <<- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(df_log_nodes_total) <<- c("nodes", "service", "traffic_start", "traffic_end", "traffic_bypass")
  
  names_edges <- list(  'Traffic_Browser_Regional' , 'Traffic_Browser_National' , 'Traffic_Browser_other_National' , 'Traffic_Browser_ext' ,
                        'Traffic_Browser' , 'Traffic_Demand_Regional' , 'Traffic_Demand_National' , 'Traffic_Demand_other_National' ,
                        'Traffic_Demand_ext' , 'Traffic_Demand' ,
                        'Traffic_Direct_Regional' , 'Traffic_Direct_National' ,'Traffic_Direct_other_National','Traffic_Direct_ext',
                        'Traffic_Direct','Traffic_Direct_Regional_to_National', 'Traffic_Direct_Twin_Regional','Traffic_Direct_Twin_National')
  
  for (x in names_edges)
  {
    edge_attr(graph = graph, name = x, index = E(graph)) <- 0 # create a new attribute
  }
  names_vertices <- list(  'Traffic_Browser_Start', 'Traffic_Browser_National_Start', 'Traffic_Browser_Regional_Start',
                           'Traffic_Browser_ext_Start', 'Traffic_Browser_oth_National_Start', 'Traffic_Browser_Pass',
                           'Traffic_Browser_National_Pass', 'Traffic_Browser_Regional_Pass', 'Traffic_Browser_ext_Pass',
                           'Traffic_Browser_oth_National_Pass', 'Traffic_Browser_End', 'Traffic_Browser_National_End',
                           'Traffic_Browser_Regional_End', 'Traffic_Browser_ext_End', 'Traffic_Browser_oth_National_End',
                           'Traffic_Demand_Start', 'Traffic_Demand_National_Start', 'Traffic_Demand_Regional_Start',
                           'Traffic_Demand_ext_Start', 'Traffic_Demand_oth_National_Start', 'Traffic_Demand_Pass',
                           'Traffic_Demand_National_Pass', 'Traffic_Demand_Regional_Pass', 'Traffic_Demand_ext_Pass',
                           'Traffic_Demand_oth_National_Pass', 'Traffic_Demand_End', 'Traffic_Demand_National_End',
                           'Traffic_Demand_Regional_End','Traffic_Demand_ext_End','Traffic_Demand_oth_National_End',
                           'Traffic_Direct_Start','Traffic_Direct_National_Start','Traffic_Direct_Regional_Start',
                           'Traffic_Direct_Pass','Traffic_Direct_National_Pass','Traffic_Direct_Regional_Pass',
                           'Traffic_Direct_End', 'Traffic_Direct_National_End','Traffic_Direct_Regional_End',
                           'Traffic_Direct_oth_National_Start','Traffic_Direct_oth_National_Pass','Traffic_Direct_oth_National_End',
                           'Traffic_Demand_ext_Start','Traffic_Demand_ext_Pass','Traffic_Demand_ext_End',
                           'Traffic_Demand', 'Traffic_Direct', 'Traffic_Browser',
                           'Traffic_National_Browser', 'Traffic_National_Direct', 'Traffic_National_Demand', 'Traffic_Nationa',
                           'Traffic_Local_Browser', 'Traffic_Local_Direct', 'Traffic_Local_Demand', 'Traffic_Local', 'Traffic_Total',
                           'Traffic_Regional_Browser', 'Traffic_Regional_Direct', 'Traffic_Regional_Demand', 'Traffic_Regional',
                           'Traffic_oth_National_Browser', 'Traffic_oth_National_Direct', 'Traffic_oth_National_Demand', 'Traffic_oth_National',
                           'Traffic_Direct_ext_Start','Traffic_Direct_ext_Pass','Traffic_Direct_ext_End',
                           'Traffic_ext_Browser', 'Traffic_ext_Direct', 'Traffic_ext_Demand', 'Traffic_ext',
                           'Traffic_End', 'Traffic_Pass', 'Traffic_Start',
                           'Traffic_Direct_Regional_to_National_Start','Traffic_Direct_Regional_to_National_Pass','Traffic_Direct_Regional_to_National_End',
                           'Traffic_Regional_to_National_Direct',
                           'Traffic_Direct_Twin_Regional_Start', 'Traffic_Direct_Twin_Regional_Pass', 'Traffic_Direct_Twin_Regional_End',
                           'Traffic_Direct_Twin_National_Start', 'Traffic_Direct_Twin_National_Pass', 'Traffic_Direct_Twin_National_End',
                           'Traffic_Direct_Twin_National', 'Traffic_Direct_Twin_Regional')

  
  for (x in names_vertices)
  {
    vertex_attr(graph = graph, name = x, index = V(graph)) <- 0 # create a new attribute
  }
  
  if (prefix_base == "")
  {
    Vertices_df$`Others National CO` <- paste("BBB_", Vertices_df$`Reference National CO`, sep = "")
    Vertices_df$`to Gateway` <- paste("GW_",Vertices_df$`Reference National CO`, sep = "")
  }
  else if (prefix_base == "!")
  {
    Vertices_df$`Others National CO` <-  Vertices_df$`Reference National CO`
    Vertices_df$`to Gateway` <- Vertices_df$`Reference National CO`
  }
  else
  {
    Vertices_df$`Others National CO` <- Vertices_df$`Reference National CO` %>% str_replace(prefix_base, "BBB_")
    Vertices_df$`to Gateway` <- Vertices_df$`Reference National CO` %>% str_replace(prefix_base, "GW_")
  }
  for (node in V(graph)) {
    print(node)
    #links
    #CWB
    #to National CO
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Reference National CO`, traffic_type = traffic_df$`CWB term. @ Reference National UL`, traffic_out_attr = 'Traffic_Browser_National', link = "UL")
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Reference National CO`, destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`CWB term. @ Reference National DL`, traffic_out_attr = 'Traffic_Browser_National', link = "DL")
    #to Regional CO
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Reference Regional CO`, traffic_type = traffic_df$`CWB term. @ Reference Regional UL`, traffic_out_attr = 'Traffic_Browser_Regional', link = "UL")
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Reference Regional CO`, destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`CWB term. @ Reference Regional DL`, traffic_out_attr = 'Traffic_Browser_Regional', link = "DL")
    #Others National CO
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Others National CO`, traffic_type = traffic_df$`CWB term. @ others National UL`, traffic_out_attr = 'Traffic_Browser_other_National', link = "UL")
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Others National CO`, destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`CWB term. @ others National DL`, traffic_out_attr = 'Traffic_Browser_other_National', link = "DL")
    #to Gateway
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`to Gateway`, traffic_type = traffic_df$`CWB to Gateway (Ext.) UL`, traffic_out_attr = 'Traffic_Browser_ext', link = "UL")
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`to Gateway`, destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`CWB to Gateway (Ext.) DL`, traffic_out_attr = 'Traffic_Browser_ext', link = "DL")
    #CDN
    #to National CO
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Reference National CO`, traffic_type = traffic_df$`CDN term. @ Reference National UL`, traffic_out_attr = 'Traffic_Demand_National', link = "UL")
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Reference National CO`, destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`CDN term. @ Reference National DL`, traffic_out_attr = 'Traffic_Demand_National', link = "DL")
    #to Regional CO
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Reference Regional CO`, traffic_type = traffic_df$`CDN term. @ Reference Regional UL`, traffic_out_attr = 'Traffic_Demand_Regional', link = "UL")
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Reference Regional CO`, destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`CDN term. @ Reference Regional DL`, traffic_out_attr = 'Traffic_Demand_Regional', link = "DL")
    #Others National CO
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Others National CO`, traffic_type = traffic_df$`CDN term. @ others National UL`, traffic_out_attr = 'Traffic_Demand_other_National', link = "UL")
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Others National CO`, destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`CDN term. @ others National DL`, traffic_out_attr = 'Traffic_Demand_other_National', link = "DL")
    #to Gateway
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`to Gateway`, traffic_type = traffic_df$`CDN to Gateway (Ext.) UL`, traffic_out_attr = 'Traffic_Demand_ext', link = "UL")
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`to Gateway`, destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`CDN to Gateway (Ext.) DL`, traffic_out_attr = 'Traffic_Demand_ext', link = "DL")

    #DCM
    #to National CO
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Reference National CO`, traffic_type = traffic_df$`DCM @ Ref. National CO UL`, traffic_out_attr = 'Traffic_Direct_National', link = "UL")
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Reference National CO`, destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`DCM @ Ref. National CO DL`, traffic_out_attr = 'Traffic_Direct_National', link = "DL")
    #to Regional CO
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Reference Regional CO`, traffic_type = traffic_df$`DCM @ Ref. Regional CO UL`, traffic_out_attr = 'Traffic_Direct_Regional', link = "UL")
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Reference Regional CO`, destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`DCM @ Ref. Regional CO DL`, traffic_out_attr = 'Traffic_Direct_Regional', link = "DL")
    #Others National CO
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Others National CO`, traffic_type = traffic_df$`DCM @ Other National CO UL`, traffic_out_attr = 'Traffic_Direct_other_National', link = "UL")
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Others National CO`, destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`DCM @ Other National CO DL`, traffic_out_attr = 'Traffic_Direct_other_National', link = "DL")
    #to Gateway
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`to Gateway`, traffic_type = traffic_df$`DCM @ National To Gateway (Ext.) UL`, traffic_out_attr = 'Traffic_Direct_ext', link = "UL")
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`to Gateway`, destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`DCM @ National To Gateway (Ext.) DL`, traffic_out_attr = 'Traffic_Direct_ext', link = "DL")
    #Ref. Regional CO to Ref. National CO
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Reference Regional CO`, destination = Vertices_df$`Reference National CO`, traffic_type = traffic_df$`DCM @  Ref. Regional CO to Ref. National CO UL`, traffic_out_attr = 'Traffic_Direct_Regional_to_National', link = "UL")
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Reference National CO`, destination = Vertices_df$`Reference Regional CO`, traffic_type = traffic_df$`DCM @  Ref. Regional CO to Ref. National CO DL`, traffic_out_attr = 'Traffic_Direct_Regional_to_National', link = "DL")
    #Twin Regional CO
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Twin Regional CO`, traffic_type = traffic_df$`DCM @ Twin Regional CO UL`, traffic_out_attr = 'Traffic_Direct_Twin_Regional', link = "UL")
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Twin Regional CO`, destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`DCM @ Twin Regional CO DL`, traffic_out_attr = 'Traffic_Direct_Twin_Regional', link = "DL")
    #Twin National CO
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Twin National CO`, traffic_type = traffic_df$`DCM @ Twin National CO UL`, traffic_out_attr = 'Traffic_Direct_Twin_National', link = "UL")
    graph = calc_traffic_links(graph,node, resource = Vertices_df$`Twin National CO`, destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`DCM @ Twin National CO DL`, traffic_out_attr = 'Traffic_Direct_Twin_National', link = "DL")
    
    
    #nodes
    #CWB
    #to National CO
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Reference National CO` , traffic_type = traffic_df$`CWB term. @ Reference National UL`, start_traffic = 'Traffic_Browser_National_Start', end_traffic = 'Traffic_Browser_National_End', pass_traffic = 'Traffic_Browser_National_Pass', link = "UL")
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Reference National CO` , destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`CWB term. @ Reference National DL`, start_traffic = 'Traffic_Browser_National_Start', end_traffic = 'Traffic_Browser_National_End', pass_traffic = 'Traffic_Browser_National_Pass', link = "DL")
    #to Regional CO
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Reference Regional CO` , traffic_type = traffic_df$`CWB term. @ Reference Regional UL`, start_traffic = 'Traffic_Browser_Regional_Start', end_traffic = 'Traffic_Browser_Regional_End', pass_traffic = 'Traffic_Browser_Regional_Pass', link = "UL")
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Reference Regional CO` , destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`CWB term. @ Reference Regional DL`, start_traffic = 'Traffic_Browser_Regional_Start', end_traffic = 'Traffic_Browser_Regional_End', pass_traffic = 'Traffic_Browser_Regional_Pass', link = "DL")
    #Others National CO
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Others National CO` , traffic_type = traffic_df$`CWB term. @ others National UL`, start_traffic = 'Traffic_Browser_oth_National_Start', end_traffic = 'Traffic_Browser_oth_National_End', pass_traffic = 'Traffic_Browser_oth_National_Pass', link = "UL")
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Others National CO` , destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`CWB term. @ others National DL`, start_traffic = 'Traffic_Browser_oth_National_Start', end_traffic = 'Traffic_Browser_oth_National_End', pass_traffic = 'Traffic_Browser_oth_National_Pass', link = "DL")
    #to Gateway
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`to Gateway` , traffic_type = traffic_df$`CWB to Gateway (Ext.) UL`, start_traffic = 'Traffic_Browser_ext_Start', end_traffic = 'Traffic_Browser_ext_End', pass_traffic = 'Traffic_Browser_ext_Pass', link = "UL")
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`to Gateway` , destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`CWB to Gateway (Ext.) DL`, start_traffic = 'Traffic_Browser_ext_Start', end_traffic = 'Traffic_Browser_ext_End', pass_traffic = 'Traffic_Browser_ext_Pass', link = "DL")
    #total start, pass, end traffic
    
    #DCM
    #to National CO
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Reference National CO` , traffic_type = traffic_df$`DCM @ Ref. National CO UL`, start_traffic = 'Traffic_Direct_National_Start', end_traffic = 'Traffic_Direct_National_End', pass_traffic = 'Traffic_Direct_National_Pass', link = "UL")
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Reference National CO` , destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`DCM @ Ref. National CO DL`, start_traffic = 'Traffic_Direct_National_Start', end_traffic = 'Traffic_Direct_National_End', pass_traffic = 'Traffic_Direct_National_Pass', link = "DL")
    #to Regional CO
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Reference Regional CO` , traffic_type = traffic_df$`DCM @ Ref. Regional CO UL`, start_traffic = 'Traffic_Direct_Regional_Start', end_traffic = 'Traffic_Direct_Regional_End', pass_traffic = 'Traffic_Direct_Regional_Pass', link = "UL")
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Reference Regional CO` , destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`DCM @ Ref. Regional CO DL`, start_traffic = 'Traffic_Direct_Regional_Start', end_traffic = 'Traffic_Direct_Regional_End', pass_traffic = 'Traffic_Direct_Regional_Pass', link = "DL")
    #Others National CO
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Others National CO` , traffic_type = traffic_df$`DCM @ Other National CO UL`, start_traffic = 'Traffic_Direct_oth_National_Start', end_traffic = 'Traffic_Direct_oth_National_End', pass_traffic = 'Traffic_Direct_oth_National_Pass', link = "UL")
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Others National CO` , destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`DCM @ Other National CO DL`, start_traffic = 'Traffic_Direct_oth_National_Start', end_traffic = 'Traffic_Direct_oth_National_End', pass_traffic = 'Traffic_Direct_oth_National_Pass', link = "DL")
    #to Gateway
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`to Gateway` , traffic_type = traffic_df$`DCM @ National To Gateway (Ext.) UL`, start_traffic = 'Traffic_Direct_ext_Start', end_traffic = 'Traffic_Direct_ext_End', pass_traffic = 'Traffic_Direct_ext_Pass', link = "UL")
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`to Gateway` , destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`DCM @ National To Gateway (Ext.) DL`, start_traffic = 'Traffic_Direct_ext_Start', end_traffic = 'Traffic_Direct_ext_End', pass_traffic = 'Traffic_Direct_ext_Pass', link = "DL")
    #Ref. Regional CO to Ref. National CO
    graph = calc_traffic_nodes(graph,node, resource = Vertices_df$`Reference Regional CO`, destination = Vertices_df$`Reference National CO`, traffic_type = traffic_df$`DCM @  Ref. Regional CO to Ref. National CO UL`, start_traffic = 'Traffic_Direct_Regional_to_National_Start', end_traffic = 'Traffic_Direct_Regional_to_National_End', pass_traffic = 'Traffic_Direct_Regional_to_National_Pass', link = "UL")
    graph = calc_traffic_nodes(graph,node, resource = Vertices_df$`Reference National CO`, destination = Vertices_df$`Reference Regional CO`, traffic_type = traffic_df$`DCM @  Ref. Regional CO to Ref. National CO DL`, start_traffic = 'Traffic_Direct_Regional_to_National_Start', end_traffic = 'Traffic_Direct_Regional_to_National_End', pass_traffic = 'Traffic_Direct_Regional_to_National_Pass', link = "DL")
    #Twin Regional CO
    graph = calc_traffic_nodes(graph,node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Twin Regional CO`, traffic_type = traffic_df$`DCM @ Twin Regional CO UL`, start_traffic = 'Traffic_Direct_Twin_Regional_Start', end_traffic = 'Traffic_Direct_Twin_Regional_End', pass_traffic = 'Traffic_Direct_Twin_Regional_Pass', link = "UL")
    graph = calc_traffic_nodes(graph,node, resource = Vertices_df$`Twin Regional CO`, destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`DCM @ Twin Regional CO DL`, start_traffic = 'Traffic_Direct_Twin_Regional_Start', end_traffic = 'Traffic_Direct_Twin_Regional_End', pass_traffic = 'Traffic_Direct_Twin_Regional_Pass', link = "DL")
    #Twin National CO
    graph = calc_traffic_nodes(graph,node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Twin National CO`, traffic_type = traffic_df$`DCM @ Twin National CO UL`, start_traffic = 'Traffic_Direct_Twin_National_Start', end_traffic = 'Traffic_Direct_Twin_National_End', pass_traffic = 'Traffic_Direct_Twin_National_Pass', link = "UL")
    graph = calc_traffic_nodes(graph,node, resource = Vertices_df$`Twin National CO`, destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`DCM @ Twin National CO DL`, start_traffic = 'Traffic_Direct_Twin_National_Start', end_traffic = 'Traffic_Direct_Twin_National_End', pass_traffic = 'Traffic_Direct_Twin_National_Pass', link = "DL")
    
    
    #CDN
    #to National CO
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Reference National CO` , traffic_type = traffic_df$`CDN term. @ Reference National UL`, start_traffic = 'Traffic_Demand_National_Start', end_traffic = 'Traffic_Demand_National_End', pass_traffic = 'Traffic_Demand_National_Pass', link = "UL")
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Reference National CO` , destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`CDN term. @ Reference National DL`, start_traffic = 'Traffic_Demand_National_Start', end_traffic = 'Traffic_Demand_National_End', pass_traffic = 'Traffic_Demand_National_Pass', link = "DL")
    #to Regional CO
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Reference Regional CO` , traffic_type = traffic_df$`CDN term. @ Reference Regional UL`, start_traffic = 'Traffic_Demand_Regional_Start', end_traffic = 'Traffic_Demand_Regional_End', pass_traffic = 'Traffic_Demand_Regional_Pass', link = "UL")
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Reference Regional CO` , destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`CDN term. @ Reference Regional DL`, start_traffic = 'Traffic_Demand_Regional_Start', end_traffic = 'Traffic_Demand_Regional_End', pass_traffic = 'Traffic_Demand_Regional_Pass', link = "DL")
    #Others National CO
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`Others National CO` , traffic_type = traffic_df$`CDN term. @ others National UL`, start_traffic = 'Traffic_Demand_oth_National_Start', end_traffic = 'Traffic_Demand_oth_National_End', pass_traffic = 'Traffic_Demand_oth_National_Pass', link = "UL")
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Others National CO` , destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`CDN term. @ others National DL`, start_traffic = 'Traffic_Demand_oth_National_Start', end_traffic = 'Traffic_Demand_oth_National_End', pass_traffic = 'Traffic_Demand_oth_National_Pass', link = "DL")
    #to Gateway
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`Node Code`, destination = Vertices_df$`to Gateway` , traffic_type = traffic_df$`CDN to Gateway (Ext.) UL`, start_traffic = 'Traffic_Demand_ext_Start', end_traffic ='Traffic_Demand_ext_End', pass_traffic = 'Traffic_Demand_ext_Pass')
    graph <- calc_traffic_nodes(graph, node, resource = Vertices_df$`to Gateway` , destination = Vertices_df$`Node Code`, traffic_type = traffic_df$`CDN to Gateway (Ext.) DL`, start_traffic = 'Traffic_Demand_ext_Start', end_traffic = 'Traffic_Demand_ext_End', pass_traffic = 'Traffic_Demand_ext_Pass')
    #total start, pass, end traffic
    }
  E(graph)$Traffic_Browser = E(graph)$Traffic_Browser_Regional + E(graph)$Traffic_Browser_National + E(graph)$Traffic_Browser_other_National + E(graph)$Traffic_Browser_ext
  E(graph)$Traffic_Demand = E(graph)$Traffic_Demand_Regional + E(graph)$Traffic_Demand_National + E(graph)$Traffic_Demand_other_National + E(graph)$Traffic_Demand_ext
  E(graph)$Traffic_Direct = E(graph)$Traffic_Direct_Regional + E(graph)$Traffic_Direct_National + E(graph)$Traffic_Direct_other_National + E(graph)$Traffic_Direct_ext + E(graph)$Traffic_Direct_Regional_to_National + E(graph)$Traffic_Direct_Twin_Regional + E(graph)$Traffic_Direct_Twin_National
  E(graph)$Total_traffic = E(graph)$Traffic_Browser + E(graph)$Traffic_Demand + E(graph)$Traffic_Direct
  
  V(graph)$Traffic_National_Browser = V(graph)$Traffic_Browser_National_Start + V(graph)$Traffic_Browser_National_Pass + V(graph)$Traffic_Browser_National_End
  V(graph)$Traffic_National_Direct = V(graph)$Traffic_Direct_National_Start + V(graph)$Traffic_Direct_National_Pass + V(graph)$Traffic_Direct_National_End
  V(graph)$Traffic_National_Demand = V(graph)$Traffic_Demand_National_Start + V(graph)$Traffic_Demand_National_Pass + V(graph)$Traffic_Demand_National_End
  V(graph)$Traffic_National = V(graph)$Traffic_National_Browser + V(graph)$Traffic_National_Direct + V(graph)$Traffic_National_Demand

  V(graph)$Traffic_Regional_Browser = V(graph)$Traffic_Browser_Regional_Start + V(graph)$Traffic_Browser_Regional_Pass + V(graph)$Traffic_Browser_Regional_End
  V(graph)$Traffic_Regional_Direct = V(graph)$Traffic_Direct_Regional_Start + V(graph)$Traffic_Direct_Regional_Pass + V(graph)$Traffic_Direct_Regional_End
  V(graph)$Traffic_Regional_Demand = V(graph)$Traffic_Demand_Regional_Start + V(graph)$Traffic_Demand_Regional_Pass + V(graph)$Traffic_Demand_Regional_End
  V(graph)$Traffic_Regional = V(graph)$Traffic_Regional_Browser + V(graph)$Traffic_Regional_Direct + V(graph)$Traffic_Regional_Demand

  V(graph)$Traffic_oth_National_Browser = V(graph)$Traffic_Browser_oth_National_Start + V(graph)$Traffic_Browser_oth_National_Pass + V(graph)$Traffic_Browser_oth_National_End
  V(graph)$Traffic_oth_National_Direct = V(graph)$Traffic_Direct_oth_National_Start + V(graph)$Traffic_Direct_oth_National_Pass + V(graph)$Traffic_Direct_oth_National_End
  V(graph)$Traffic_oth_National_Demand = V(graph)$Traffic_Demand_oth_National_Start + V(graph)$Traffic_Demand_oth_National_Pass + V(graph)$Traffic_Demand_oth_National_End
  V(graph)$Traffic_oth_National = V(graph)$Traffic_oth_National_Browser + V(graph)$Traffic_oth_National_Direct + V(graph)$Traffic_oth_National_Demand

  V(graph)$Traffic_ext_Browser = V(graph)$Traffic_Browser_ext_Start + V(graph)$Traffic_Browser_ext_Pass + V(graph)$Traffic_Browser_ext_End
  V(graph)$Traffic_ext_Direct = V(graph)$Traffic_Direct_ext_Start + V(graph)$Traffic_Direct_ext_Pass + V(graph)$Traffic_Direct_ext_End
  V(graph)$Traffic_ext_Demand = V(graph)$Traffic_Demand_ext_Start + V(graph)$Traffic_Demand_ext_Pass + V(graph)$Traffic_Demand_ext_End
  V(graph)$Traffic_ext = V(graph)$Traffic_ext_Browser + V(graph)$Traffic_ext_Direct + V(graph)$Traffic_ext_Demand
  
  V(graph)$Traffic_Regional_to_National_Direct = V(graph)$Traffic_Direct_Regional_to_National_Start + V(graph)$Traffic_Direct_Regional_to_National_Pass + V(graph)$Traffic_Direct_Regional_to_National_End
  V(graph)$Traffic_Direct_Twin_National = V(graph)$Traffic_Direct_Twin_National_Start + V(graph)$Traffic_Direct_Twin_National_Pass + V(graph)$Traffic_Direct_Twin_National_End
  V(graph)$Traffic_Direct_Twin_Regional = V(graph)$Traffic_Direct_Twin_Regional_Start + V(graph)$Traffic_Direct_Twin_Regional_Pass + V(graph)$Traffic_Direct_Twin_Regional_End
  
  
  V(graph)$Traffic_Browser_Start = V(graph)$Traffic_Browser_National_Start + V(graph)$Traffic_Browser_Regional_Start + V(graph)$Traffic_Browser_ext_Start + V(graph)$Traffic_Browser_oth_National_Start
  V(graph)$Traffic_Browser_Pass = V(graph)$Traffic_Browser_National_Pass + V(graph)$Traffic_Browser_Regional_Pass + V(graph)$Traffic_Browser_ext_Pass + V(graph)$Traffic_Browser_oth_National_Pass
  V(graph)$Traffic_Browser_End = V(graph)$Traffic_Browser_National_End + V(graph)$Traffic_Browser_Regional_End + V(graph)$Traffic_Browser_ext_End + V(graph)$Traffic_Browser_oth_National_End
  V(graph)$Traffic_Direct_Start = V(graph)$Traffic_Direct_National_Start + V(graph)$Traffic_Direct_Regional_Start + V(graph)$Traffic_Direct_ext_Start + V(graph)$Traffic_Direct_oth_National_Start + V(graph)$Traffic_Direct_Regional_to_National_Start + V(graph)$Traffic_Direct_Twin_National_Start
  V(graph)$Traffic_Direct_Pass = V(graph)$Traffic_Direct_National_Pass + V(graph)$Traffic_Direct_Regional_Pass + V(graph)$Traffic_Direct_ext_Pass + V(graph)$Traffic_Direct_oth_National_Pass + V(graph)$Traffic_Direct_Regional_to_National_Pass + V(graph)$Traffic_Direct_Twin_National_Pass
  V(graph)$Traffic_Direct_End = V(graph)$Traffic_Direct_National_End + V(graph)$Traffic_Direct_Regional_End + V(graph)$Traffic_Direct_ext_End + V(graph)$Traffic_Direct_oth_National_End + V(graph)$Traffic_Direct_Regional_to_National_End + V(graph)$Traffic_Direct_Twin_National_End
  V(graph)$Traffic_Demand_Start = V(graph)$Traffic_Demand_National_Start + V(graph)$Traffic_Demand_Regional_Start + V(graph)$Traffic_Demand_ext_Start + V(graph)$Traffic_Demand_oth_National_Start
  V(graph)$Traffic_Demand_Pass = V(graph)$Traffic_Demand_National_Pass + V(graph)$Traffic_Demand_Regional_Pass + V(graph)$Traffic_Demand_ext_Pass + V(graph)$Traffic_Demand_oth_National_Pass
  V(graph)$Traffic_Demand_End = V(graph)$Traffic_Demand_National_End + V(graph)$Traffic_Demand_Regional_End + V(graph)$Traffic_Demand_ext_End + V(graph)$Traffic_Demand_oth_National_End
  V(graph)$Traffic_Demand = V(graph)$Traffic_Demand_Start + V(graph)$Traffic_Demand_Pass + V(graph)$Traffic_Demand_End
  V(graph)$Traffic_Direct = V(graph)$Traffic_Direct_End + V(graph)$Traffic_Direct_Pass + V(graph)$Traffic_Direct_Start
  V(graph)$Traffic_Browser = V(graph)$Traffic_Browser_Start + V(graph)$Traffic_Browser_Pass + V(graph)$Traffic_Browser_End
  V(graph)$Traffic_End = V(graph)$Traffic_Demand_End + V(graph)$Traffic_Direct_End + V(graph)$Traffic_Browser_End
  V(graph)$Traffic_Pass = V(graph)$Traffic_Demand_Pass + V(graph)$Traffic_Direct_Pass + V(graph)$Traffic_Browser_Pass
  V(graph)$Traffic_Start = V(graph)$Traffic_Demand_Start + V(graph)$Traffic_Direct_Start + V(graph)$Traffic_Browser_Start
  
  #Local traffic canculations
  V(graph)$Traffic_Local_Browser = traffic_df$`CWB terminated @ Local UL` + traffic_df$`CWB terminated @ Local DL`
  V(graph)$Traffic_Local_Direct = traffic_df$`DCM @ Local/Regional CO UL` + traffic_df$`DCM @ Local/Regional CO DL` + traffic_df$`DCM @ Local/Regional/National CO UL`+ traffic_df$`DCM @ Local/Regional/National CO DL`
  V(graph)$Traffic_Local_Demand = traffic_df$`CDN terminated @ Local UL` + traffic_df$`CDN terminated @ Local DL`
  V(graph)$Traffic_Local = V(graph)$Traffic_Local_Demand + V(graph)$Traffic_Local_Direct + V(graph)$Traffic_Local_Browser

  V(graph)$Traffic_Total = V(graph)$Traffic_Demand + V(graph)$Traffic_Direct + V(graph)$Traffic_Browser + V(graph)$Traffic_Local
  write_nodes_log_report(graph)
  return(graph)
}




calc_links_capacity <- function(graph, threshold = 0.5, capacity_df = capacity_df) 
  { #threshold - in % (0-1), links_capacity - array with all capacities
  names_edges <- list('Capacity', 'Cost', 'Power_consumption')
  for (x in names_edges)
  {
    edge_attr(graph = graph, name = x, index = E(graph)) <- 0 # create a new attribute
  }
  
  
  for (n in E(graph)) 
  {
    ind = 1
    for (i in capacity_df$Capacity)
    {
      if (i >= (E(graph)[n]$Total_traffic/threshold))
      {
        graph <- set_edge_attr(graph, name = 'Capacity', index = E(graph)[n], value = i)
        graph <- set_edge_attr(graph, name = 'Cost', index = E(graph)[n], value = capacity_df$Cost[ind])
        graph <- set_edge_attr(graph, name = 'Power_consumption', index = E(graph)[n], value = capacity_df$Power_consumption[ind])
        ind %+=% 1
        break
      }
    }  
  }
  return(graph)
}


calc_links_load <- function(graph)
{
  edge_attr(graph = graph, name = 'Load', index = E(graph)) <- 0 # create a new attribute
  E(graph)$Load = E(graph)$Total_traffic/E(graph)$Capacity
  graph <- set_edge_attr(graph, name = "Load",index = E(graph), value = E(graph)$Load)
  return (graph)
}

calc_links_delay_prop <- function(graph)
{
  edge_attr(graph = graph, name = 'Propagation_Delay', index = E(graph)) <- 0 # create a new attribute
  E(graph)$Propagation_Delay = E(graph)$Distance * 5 #[mcs]
  graph <- set_edge_attr(graph, name = 'Propagation_Delay',index = E(graph), value = E(graph)$Propagation_Delay)
  return (graph)
}

#transmission delays in [us]
calc_links_delay_trans <- function(graph, packet_size = 500)
{
  edge_attr(graph = graph, name = 'Transmission_Delay', index = E(graph)) <- 0 # create a new attribute
  E(graph)$Transmission_Delay = (packet_size*8)/E(graph)$Capacity*1/(1 - E(graph)$Load) 
  # (packet_size * 8 bit) / Capacity multilply by 1/(1-load_of_that_link) load is a value between 0 and 1
  E(graph)$Transmission_Delay = E(graph)$Transmission_Delay/1000
  graph <- set_edge_attr(graph, name = 'Transmission_Delay',index = E(graph), value = E(graph)$Transmission_Delay)
  return (graph)
}

calc_links_delays <- function(graph, packet_size = 500) 
{
  graph = calc_links_load(graph)
  graph = calc_links_delay_prop(graph)
  graph = calc_links_delay_trans(graph, packet_size)
  return(graph)
}


calc_nodes_delay_prop <- function(graph, Vertices_df)
{
  names_vertices <- list('Propagation_Delay_Regional', 'Propagation_Delay_National', 'Propagation_Delay_oth_National', 'Propagation_Delay_ext')
  for (x in names_vertices)
  {
    vertex_attr(graph = graph, name = x, index = V(graph)) <- 0 # create a new attribute
  }
  Vertices_df$`Others National CO` <- Vertices_df$`Reference National CO` %>% str_replace("BB", "BBB")
  Vertices_df$`to Gateway` <- Vertices_df$`Reference National CO` %>% str_replace("BB", "GW")
  
  for (node in V(graph)) {
    graph <- calc_node_delay(graph, node, source = Vertices_df$'Node Code', destination = Vertices_df$'Reference Regional CO', 'Propagation_Delay', 'Propagation_Delay_Regional')
    graph <- calc_node_delay(graph, node, source = Vertices_df$'Node Code', destination = Vertices_df$'Reference National CO', 'Propagation_Delay', 'Propagation_Delay_National')
    graph <- calc_node_delay(graph, node, source = Vertices_df$'Node Code', destination = Vertices_df$'Others National CO', 'Propagation_Delay', 'Propagation_Delay_oth_National')
    graph <- calc_node_delay(graph, node, source = Vertices_df$'Node Code', destination = Vertices_df$'to Gateway', 'Propagation_Delay', 'Propagation_Delay_ext')
  }
  return(graph)
}


calc_nodes_delay_trans <- function(graph, Vertices_df)
{
  names_vertices <- list('Transmission_Delay_Regional', 'Transmission_Delay_National', 'Transmission_Delay_oth_National', 'Transmission_Delay_ext')
  for (x in names_vertices)
  {
    vertex_attr(graph = graph, name = x, index = V(graph)) <- 0 # create a new attribute
  }
  Vertices_df$`Others National CO` <- Vertices_df$`Reference National CO` %>% str_replace("BB", "BBB")
  Vertices_df$`to Gateway` <- Vertices_df$`Reference National CO` %>% str_replace("BB", "GW")
  
  for (node in V(graph)) {
    graph <- calc_node_delay(graph, node, source = Vertices_df$'Node Code', destination = Vertices_df$'Reference Regional CO', 'Transmission_Delay', 'Transmission_Delay_Regional')
    graph <- calc_node_delay(graph, node, source = Vertices_df$'Node Code', destination = Vertices_df$'Reference National CO', 'Transmission_Delay', 'Transmission_Delay_National')
    graph <- calc_node_delay(graph, node, source = Vertices_df$'Node Code', destination = Vertices_df$'Others National CO', 'Transmission_Delay', 'Transmission_Delay_oth_National')
    graph <- calc_node_delay(graph, node, source = Vertices_df$'Node Code', destination = Vertices_df$'to Gateway', 'Transmission_Delay', 'Transmission_Delay_ext')
  }
  return(graph)
}


#Calculates the propagation delay for each link [us]
calc_nodes_delay_propagation_average <- function(graph)
{
  x1_arr = V(graph)$Propagation_Delay_National*V(graph)$Traffic_National + V(graph)$Propagation_Delay_Regional*V(graph)$Traffic_Regional + V(graph)$Propagation_Delay_oth_National*V(graph)$Traffic_oth_National + V(graph)$Propagation_Delay_ext*V(graph)$Traffic_ext + 0*V(graph)$Traffic_Local
  x2_arr = V(graph)$Traffic_Total
  for (node in V(graph))
  {
    if (x2_arr[node] == 0) { 
      graph <- set_vertex_attr(graph, name = 'Av_Delay_Prop',index = V(graph)[node], value = 0)}
    else { 
      graph <- set_vertex_attr(graph, name = 'Av_Delay_Prop',index = V(graph)[node], value = x1_arr[node]/x2_arr[node])
      }
  }
  X1 = sum(x1_arr)
  X2 = sum(x2_arr)
  av_delay_prop = X1/X2
  return(list(graph, av_delay_prop))
}

#Calculates the transmission delay for each link [us]
calc_nodes_delay_transmission_average <- function(graph)
{
  x1_arr = V(graph)$Transmission_Delay_National*V(graph)$Traffic_National + V(graph)$Transmission_Delay_Regional*V(graph)$Traffic_Regional + V(graph)$Transmission_Delay_oth_National*V(graph)$Traffic_oth_National + V(graph)$Transmission_Delay_ext*V(graph)$Traffic_ext + 0*V(graph)$Traffic_Local
  x2_arr = V(graph)$Traffic_Total
  for (node in V(graph))
  {
    if (x2_arr[node] == 0) { 
      graph <- set_vertex_attr(graph, name = 'Av_Delay_Trans',index = V(graph)[node], value = 0)}
    else { 
      graph <- set_vertex_attr(graph, name = 'Av_Delay_Trans',index = V(graph)[node], value = x1_arr[node]/x2_arr[node])
    }
  }
  X1 = sum(x1_arr)
  X2 = sum(x2_arr)
  av_delay_trans = X1/X2
  return(list(graph, av_delay_trans))
}



#the function changes the initial traffic values (as multiply on multiply_value) for the TrafficVSCost graph:
traffic_division <- function(traffic_in = traffic_df_sc1, traffic_out = traffic_df_sc1_M10, multiply_value = 10)
{
  
  names = list("CWB terminated @ Local UL", "CWB terminated @ Local DL", 
               "CWB term. @ Reference Regional UL", "CWB term. @ Reference Regional DL",
               "CWB term. @ Reference National UL","CWB term. @ Reference National DL",
               "CWB term. @ others National UL", "CWB term. @ others National DL",
               "CWB to Gateway (Ext.) UL", "CWB to Gateway (Ext.) DL",
               
               "DCM @ Local/Regional CO UL",
               "DCM @ Local/Regional CO DL",
               "DCM @ Local/Regional/National CO UL",
               "DCM @ Local/Regional/National CO DL",
               "DCM @ Ref. Regional CO UL",
               "DCM @ Ref. Regional CO DL",
               "DCM @ Twin Regional CO UL",
               "DCM @ Twin Regional CO DL",
               "DCM @ Other Regional CO UL",
               "DCM @  Other Regional CO DL",
               "DCM @  Ref. Regional CO to Ref. National CO UL",
               "DCM @  Ref. Regional CO to Ref. National CO DL",
               "DCM @ Ref. National CO UL",
               "DCM @ Ref. National CO DL",
               "DCM @ Twin National CO UL",
               "DCM @ Twin National CO DL",
               "DCM @ Other National CO UL",
               "DCM @ Other National CO DL",
               "DCM @ National To Gateway (Ext.) UL",
               "DCM @ National To Gateway (Ext.) DL",
               
               "CDN terminated @ Local UL", "CDN terminated @ Local DL", 
               "CDN term. @ Reference Regional UL", "CDN term. @ Reference Regional DL",
               "CDN term. @ Reference National UL","CDN term. @ Reference National DL",
               "CDN term. @ others National UL", "CDN term. @ others National DL",
               "CDN to Gateway (Ext.) UL", "CDN to Gateway (Ext.) DL")
  for (name in names)
  {
    traffic_out[name] = traffic_in[name] * multiply_value
  }
  return(traffic_out)
}


#Function calculates total traffic, capacity, load, delays, cost and power consumption together:
scenario_calc <- function(Edges_df = Edges_df, Vertices_df = Vertices_df, traffic_df = traffic_df_sc1, capacity_df = capacity_df, prefix_base = "")
{
  graph <- graph_from_data_frame(Edges_df, Vertices_df,  directed = TRUE)
  graph <- calc_full_traffic(graph = graph, traffic_df = traffic_df, Vertices_df = Vertices_df, prefix_base)
  graph <- calc_links_capacity(graph, threshold = 0.5, capacity_df = capacity_df)
  #trans & prop delays are [us]
  graph <- calc_links_delays(graph, packet_size = 500)
  graph <- calc_nodes_delay_prop(graph, Vertices_df)
  graph <- calc_nodes_delay_trans(graph, Vertices_df)
  c(graph, av_delay_prop) := calc_nodes_delay_propagation_average(graph)
  c(graph, av_delay_trans) := calc_nodes_delay_transmission_average(graph)
  
  #Basic DC scenario:
  graph <- calc_resources_basic(graph)
  #Traffic dependent DC scenario:
  graph <- calc_resources_traffic_dependent(graph)
  return(graph)
}



  

