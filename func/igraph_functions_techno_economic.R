

#For AMEN and MCRN differect quantity of switchers and NAS
#Traffic dependent DC scenario:
calc_resources_traffic_dependent <- function(graph, eq_cost_info)
{


  
  # AMEN_Cost_servers = 6*2.6 / 320
  # AMEN_Cost_NAS = 1*3.1  / 320
  # 
  # AMEN_Cost_Switches = (2*0.016 + 36 * 1 + 4*5 + 4*36)/ 320
  # AMEN_Cost_10Gplugg = 2*0.016 / 320
  # AMEN_Cost_10G = 36*1  / 320
  # AMEN_Cost_100G = 4*5 / 320
  # AMEN_Cost_Switch = 4*36 / 320
  # 
  # 
  # MCEN_Cost_servers = 8*3.9 / 1280
  # MCEN_Cost_NAS = 2*3.1 / 1280
  # 
  # MCEN_Cost_Switches = (2*0.016 + 96*1 + 10*5 + 4*96) / 1280
  # MCEN_Cost_10Gplugg = 2*0.016 / 1280
  # MCEN_Cost_10G = 96*1  / 1280
  # MCEN_Cost_100G = 10*5 / 1280
  # MCEN_Cost_Switch = 4*96 / 1280
  # 
  # 
  # AMEN_Power_servers = 6*750 / 320
  # AMEN_Power_NAS = 1*140 / 320
  # AMEN_Power_Switches = (2*1.5 + 36*58 + 4*95 + 300*36) / 320
  # AMEN_Power_10Gplugg = 2*1.5 / 320
  # AMEN_Power_10G = 36*58  / 320
  # AMEN_Power_100G = 4*95 / 320
  # AMEN_Power_Switch = 300*36 / 320
  # 
  # 
  # MCEN_Power_servers = 8*1100 / 1280
  # MCEN_Power_NAS = 2*140 / 1280
  # MCEN_Power_Switches = (2*1.5 + 96*58 + 10*95 + 300*96) / 1280
  # MCEN_Power_10Gplugg = 2*1.5 / 1280
  # MCEN_Power_10G = 96*58  / 1280
  # MCEN_Power_100G = 10*95 / 1280
  # MCEN_Power_Switch = 300*96 / 1280
  
  AMEN_Cost_servers = as.numeric(eq_cost_info$AMEN_Cost_servers[1])
  AMEN_Cost_NAS = as.numeric(eq_cost_info$AMEN_Cost_NAS[1])
  AMEN_Cost_Switches = as.numeric(eq_cost_info$AMEN_Cost_Switches[1])
  AMEN_Cost_10Gplugg = as.numeric(eq_cost_info$AMEN_Cost_10Gplugg[1])
  AMEN_Cost_10G = as.numeric(eq_cost_info$AMEN_Cost_10G[1])
  AMEN_Cost_100G = as.numeric(eq_cost_info$AMEN_Cost_100G[1])
  AMEN_Cost_Switch = as.numeric(eq_cost_info$AMEN_Cost_Switch[1])
  
  
  MCEN_Cost_servers = as.numeric(eq_cost_info$MCEN_Cost_servers[1])
  MCEN_Cost_NAS = as.numeric(eq_cost_info$MCEN_Cost_NAS[1])
  
  MCEN_Cost_Switches = as.numeric(eq_cost_info$MCEN_Cost_Switches[1])
  MCEN_Cost_10Gplugg = as.numeric(eq_cost_info$MCEN_Cost_10Gplugg[1])
  MCEN_Cost_10G = as.numeric(eq_cost_info$MCEN_Power_10G[1])
  MCEN_Cost_100G = as.numeric(eq_cost_info$MCEN_Power_100G[1])
  MCEN_Cost_Switch = as.numeric(eq_cost_info$MCEN_Cost_Switch[1])
  
  
  AMEN_Power_servers = as.numeric(eq_cost_info$AMEN_Power_servers[1])
  AMEN_Power_NAS = as.numeric(eq_cost_info$MCEN_Power_NAS[1])
  AMEN_Power_Switches = as.numeric(eq_cost_info$AMEN_Power_Switches[1])
  AMEN_Power_10Gplugg = as.numeric(eq_cost_info$AMEN_Power_10Gplugg[1])
  AMEN_Power_10G = as.numeric(eq_cost_info$AMEN_Power_10[1])
  AMEN_Power_100G = as.numeric(eq_cost_info$AMEN_Power_100G[1])
  AMEN_Power_Switch = as.numeric(eq_cost_info$AMEN_Power_Switch[1])
  
  
  MCEN_Power_servers = as.numeric(eq_cost_info$MCEN_Power_servers[1])
  MCEN_Power_NAS = as.numeric(eq_cost_info$MCEN_Power_NAS[1])
  MCEN_Power_Switches = as.numeric(eq_cost_info$MCEN_Power_Switches[1])
  MCEN_Power_10Gplugg = as.numeric(eq_cost_info$MCEN_Power_10Gplugg[1])
  MCEN_Power_10G = as.numeric(eq_cost_info$MCEN_Power_10G[1])
  MCEN_Power_100G = as.numeric(eq_cost_info$MCEN_Power_100G[1])
  MCEN_Power_Switch = as.numeric(eq_cost_info$MCEN_Power_Switches[1])
  
  AMEN_Cost_Without = AMEN_Cost_Switches
  AMEN_Power_Without = AMEN_Power_Switches
  AMEN_Cost_With = AMEN_Cost_Switches + AMEN_Cost_NAS + AMEN_Cost_servers
  AMEN_Power_With = AMEN_Power_Switches + AMEN_Power_NAS + AMEN_Power_servers
  
  MCEN_Cost_Without = MCEN_Cost_Switches
  MCEN_Power_Without = MCEN_Power_Switches
  MCEN_Cost_With = MCEN_Cost_Switches + MCEN_Cost_NAS + MCEN_Cost_servers
  MCEN_Power_With =  MCEN_Power_Switches + MCEN_Power_NAS + MCEN_Power_servers
  
  AMEN_CPUs <- as.numeric(eq_cost_info$AMEN_CPUs[1])
  MCEN_CPUs <- as.numeric(eq_cost_info$MCEN_CPUs[1])
  AMEN_TB <- as.numeric(eq_cost_info$AMEN_TB[1])
  MCEN_TB <- as.numeric(eq_cost_info$MCEN_TB[1])
  
  for(node in V(graph))
  {
    if (V(graph)[node]$"Central office type" == "Local CO")
    {
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Cost_Without + V(graph)[node]$Traffic_End * AMEN_Cost_With)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Power_Without + V(graph)[node]$Traffic_End * AMEN_Power_With)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_servers',index = V(graph)[node], value = V(graph)[node]$Traffic_End * AMEN_Cost_servers)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_NAS',index = V(graph)[node], value = V(graph)[node]$Traffic_End * AMEN_Cost_NAS)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_Switches+plug',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Cost_Switches + V(graph)[node]$Traffic_End * AMEN_Cost_Switches)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_10Gplugg',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Cost_10Gplugg + V(graph)[node]$Traffic_End * AMEN_Cost_10Gplugg)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_10G',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Cost_10G + V(graph)[node]$Traffic_End * AMEN_Cost_10G)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_100G',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Cost_100G + V(graph)[node]$Traffic_End * AMEN_Cost_100G)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_Switch',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Cost_Switch + V(graph)[node]$Traffic_End * AMEN_Cost_Switch)
      
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_servers',index = V(graph)[node], value = V(graph)[node]$Traffic_End * AMEN_Power_servers)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_NAS',index = V(graph)[node], value = V(graph)[node]$Traffic_End * AMEN_Power_NAS)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_Switches+plug',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Power_Switches +  V(graph)[node]$Traffic_End * AMEN_Power_Switches)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_10Gplugg',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Power_10Gplugg + V(graph)[node]$Traffic_End * AMEN_Power_10Gplugg)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_10G',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Power_10G + V(graph)[node]$Traffic_End * AMEN_Power_10G)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_100G',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Power_100G + V(graph)[node]$Traffic_End * AMEN_Power_100G)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_Switch',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Power_Switch + V(graph)[node]$Traffic_End * AMEN_Power_Switch)
      
      
      graph <- set_vertex_attr(graph, name = 'CPUs_tr_dependent',index = V(graph)[node], value =  V(graph)[node]$Traffic_End * AMEN_CPUs)
      graph <- set_vertex_attr(graph, name = 'TB_tr_dependent',index = V(graph)[node], value =  V(graph)[node]$Traffic_End * AMEN_TB)
      
      
    }
    else if (V(graph)[node]$"Central office type" == "National CO" || V(graph)[node]$"Central office type" == "Regional CO")
    {
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Cost_Without + V(graph)[node]$Traffic_End * MCEN_Cost_With)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Power_Without + V(graph)[node]$Traffic_End * MCEN_Power_With)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_servers',index = V(graph)[node], value = V(graph)[node]$Traffic_End * MCEN_Cost_servers)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_NAS',index = V(graph)[node], value = V(graph)[node]$Traffic_End * MCEN_Cost_NAS)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_Switches+plug',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Cost_Switches + V(graph)[node]$Traffic_End * MCEN_Cost_Switches)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_10Gplugg',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Cost_10Gplugg + V(graph)[node]$Traffic_End * MCEN_Cost_10Gplugg)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_10G',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Cost_10G + V(graph)[node]$Traffic_End * MCEN_Cost_10G)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_100G',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Cost_100G + V(graph)[node]$Traffic_End * MCEN_Cost_100G)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_Switch',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Cost_Switch + V(graph)[node]$Traffic_End * MCEN_Cost_Switch)
      
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_servers',index = V(graph)[node], value = V(graph)[node]$Traffic_End * MCEN_Power_servers)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_NAS',index = V(graph)[node], value = V(graph)[node]$Traffic_End * MCEN_Power_NAS)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_Switches+plug',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Power_Switches +  V(graph)[node]$Traffic_End * MCEN_Power_Switches)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_10Gplugg',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Power_10Gplugg + V(graph)[node]$Traffic_End * MCEN_Power_10Gplugg)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_10G',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Power_10G + V(graph)[node]$Traffic_End * MCEN_Power_10G)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_100G',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Power_100G + V(graph)[node]$Traffic_End * MCEN_Power_100G)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_Switch',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Power_Switch + V(graph)[node]$Traffic_End * MCEN_Power_Switch)
      graph <- set_vertex_attr(graph, name = 'CPUs_tr_dependent',index = V(graph)[node], value =  V(graph)[node]$Traffic_End * MCEN_CPUs)
      graph <- set_vertex_attr(graph, name = 'TB_tr_dependent',index = V(graph)[node], value =  V(graph)[node]$Traffic_End * MCEN_TB)
      
      
    }
    else
    {
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_servers',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_NAS',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_Switches+plug',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_10Gplugg',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_10G',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_100G',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Cost_tr_dependent_Switch',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_servers',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_NAS',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_Switches+plug',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_10Gplugg',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_10G',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_100G',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_tr_dependent_Switch',index = V(graph)[node], value = 0)
      
      graph <- set_vertex_attr(graph, name = 'CPUs_tr_dependent',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'TB_tr_dependent',index = V(graph)[node], value =  0)
      
    }
  }
  return(graph)
}

#Basic DC scenario:
calc_resources_basic <- function(graph, eq_cost_info)
{
  
  AMEN_Cost_servers = as.numeric(eq_cost_info$AMEN_Cost_servers[3])
  AMEN_Cost_NAS = as.numeric(eq_cost_info$AMEN_Cost_NAS[3])
  AMEN_Cost_Switches = as.numeric(eq_cost_info$AMEN_Cost_Switches[3])
  AMEN_Cost_10Gplugg = as.numeric(eq_cost_info$AMEN_Cost_10Gplugg[3])
  AMEN_Cost_10G = as.numeric(eq_cost_info$AMEN_Cost_10G[3])
  AMEN_Cost_100G = as.numeric(eq_cost_info$AMEN_Cost_100G[3])
  AMEN_Cost_Switch = as.numeric(eq_cost_info$AMEN_Cost_Switch[3])
  
  
  MCEN_Cost_servers = as.numeric(eq_cost_info$MCEN_Cost_servers[3])
  MCEN_Cost_NAS = as.numeric(eq_cost_info$MCEN_Cost_NAS[3])
  
  MCEN_Cost_Switches = as.numeric(eq_cost_info$MCEN_Cost_Switches[3])
  MCEN_Cost_10Gplugg = as.numeric(eq_cost_info$MCEN_Cost_10Gplugg[3])
  MCEN_Cost_10G = as.numeric(eq_cost_info$MCEN_Power_10G[3])
  MCEN_Cost_100G = as.numeric(eq_cost_info$MCEN_Power_100G[3])
  MCEN_Cost_Switch = as.numeric(eq_cost_info$MCEN_Cost_Switch[3])
  
  
  AMEN_Power_servers = as.numeric(eq_cost_info$AMEN_Power_servers[3])
  AMEN_Power_NAS = as.numeric(eq_cost_info$MCEN_Power_NAS[3])
  AMEN_Power_Switches = as.numeric(eq_cost_info$AMEN_Power_Switches[3])
  AMEN_Power_10Gplugg = as.numeric(eq_cost_info$AMEN_Power_10Gplugg[3])
  AMEN_Power_10G = as.numeric(eq_cost_info$AMEN_Power_10[3])
  AMEN_Power_100G = as.numeric(eq_cost_info$AMEN_Power_100G[3])
  AMEN_Power_Switch = as.numeric(eq_cost_info$AMEN_Power_Switch[3])
  
  
  MCEN_Power_servers = as.numeric(eq_cost_info$MCEN_Power_servers[3])
  MCEN_Power_NAS = as.numeric(eq_cost_info$MCEN_Power_NAS[3])
  MCEN_Power_Switches = as.numeric(eq_cost_info$MCEN_Power_Switches[3])
  MCEN_Power_10Gplugg = as.numeric(eq_cost_info$MCEN_Power_10Gplugg[3])
  MCEN_Power_10G = as.numeric(eq_cost_info$MCEN_Power_10G[3])
  MCEN_Power_100G = as.numeric(eq_cost_info$MCEN_Power_100G[3])
  MCEN_Power_Switch = as.numeric(eq_cost_info$MCEN_Power_Switches[3])
  
  AMEN_Cost_Without = AMEN_Cost_Switches
  AMEN_Power_Without = AMEN_Power_Switches
  AMEN_Cost_With = AMEN_Cost_Switches + AMEN_Cost_NAS + AMEN_Cost_servers
  AMEN_Power_With = AMEN_Power_Switches + AMEN_Power_NAS + AMEN_Power_servers
  
  MCEN_Cost_Without = MCEN_Cost_Switches
  MCEN_Power_Without = MCEN_Power_Switches
  MCEN_Cost_With = MCEN_Cost_Switches + MCEN_Cost_NAS + MCEN_Cost_servers
  MCEN_Power_With =  MCEN_Power_Switches + MCEN_Power_NAS + MCEN_Power_servers
  
  AMEN_CPUs <- as.numeric(eq_cost_info$AMEN_CPUs[3])
  MCEN_CPUs <- as.numeric(eq_cost_info$MCEN_CPUs[3])
  AMEN_TB <- as.numeric(eq_cost_info$AMEN_TB[3])
  MCEN_TB <- as.numeric(eq_cost_info$MCEN_TB[3])
  
  # AMEN_Cost_servers = 6*2.6 / 320
  # AMEN_Cost_NAS = 1*3.1  / 320
  # 
  # AMEN_Cost_Switches = (2*0.016 + 36 * 1 + 4*5 + 4*36)/ 320
  # AMEN_Cost_10Gplugg = 2*0.016 / 320
  # AMEN_Cost_10G = 36*1  / 320
  # AMEN_Cost_100G = 4*5 / 320
  # AMEN_Cost_Switch = 4*36 / 320
  # 
  # 
  # MCEN_Cost_servers = 8*3.9 / 1280
  # MCEN_Cost_NAS = 2*3.1 / 1280
  # 
  # MCEN_Cost_Switches = (2*0.016 + 96*1 + 10*5 + 4*96) / 1280
  # MCEN_Cost_10Gplugg = 2*0.016 / 1280
  # MCEN_Cost_10G = 96*1  / 1280
  # MCEN_Cost_100G = 10*5 / 1280
  # MCEN_Cost_Switch = 4*96 / 1280
  # 
  # 
  # AMEN_Power_servers = 6*750 / 320
  # AMEN_Power_NAS = 1*140 / 320
  # AMEN_Power_Switches = (2*1.5 + 36*58 + 4*95 + 300*36) / 320
  # AMEN_Power_10Gplugg = 2*1.5 / 320
  # AMEN_Power_10G = 36*58  / 320
  # AMEN_Power_100G = 4*95 / 320
  # AMEN_Power_Switch = 300*36 / 320
  # 
  # 
  # MCEN_Power_servers = 8*1100 / 1280
  # MCEN_Power_NAS = 2*140 / 1280
  # MCEN_Power_Switches = (2*1.5 + 96*58 + 10*95 + 300*96) / 1280
  # MCEN_Power_10Gplugg = 2*1.5 / 1280
  # MCEN_Power_10G = 96*58  / 1280
  # MCEN_Power_100G = 10*95 / 1280
  # MCEN_Power_Switch = 300*96 / 1280
  # 
  # AMEN_Cost_Without = AMEN_Cost_Switches
  # AMEN_Power_Without = AMEN_Power_Switches
  # AMEN_Cost_With = AMEN_Cost_Switches + AMEN_Cost_NAS + AMEN_Cost_servers
  # AMEN_Power_With = AMEN_Power_Switches + AMEN_Power_NAS + AMEN_Power_servers
  # 
  # MCEN_Cost_Without = MCEN_Cost_Switches
  # MCEN_Power_Without = MCEN_Power_Switches
  # MCEN_Cost_With = MCEN_Cost_Switches + MCEN_Cost_NAS + MCEN_Cost_servers
  # MCEN_Power_With =  MCEN_Power_Switches + MCEN_Power_NAS + MCEN_Power_servers
  # 
  # AMEN_CPUs <- 288*6/320
  # MCEN_CPUs <- 768*8/1280 
  # AMEN_TB <- 40*1/320 
  # MCEN_TB <- 120*2/1280
  # 
  # AMEN_Cost_Without = AMEN_Cost_Switches
  # AMEN_Power_Without = AMEN_Power_Switches
  # AMEN_Cost_With = AMEN_Cost_Switches + AMEN_Cost_NAS + AMEN_Cost_servers
  # AMEN_Power_With = AMEN_Power_Switches + AMEN_Power_NAS + AMEN_Power_servers
  # 
  # MCEN_Cost_Without = MCEN_Cost_Switches
  # MCEN_Power_Without = MCEN_Power_Switches
  # MCEN_Cost_With = MCEN_Cost_Switches + MCEN_Cost_NAS + MCEN_Cost_servers
  # MCEN_Power_With =  MCEN_Power_Switches + MCEN_Power_NAS + MCEN_Power_servers
  
  for(node in V(graph))
  {
    if (V(graph)[node]$"Central office type" == "Local CO")
    {
      graph <- set_vertex_attr(graph, name = 'Cost_basic',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Cost_Without + V(graph)[node]$Traffic_End * AMEN_Cost_With)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Power_Without + V(graph)[node]$Traffic_End * AMEN_Power_With)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_servers',index = V(graph)[node], value = V(graph)[node]$Traffic_End * AMEN_Cost_servers)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_NAS',index = V(graph)[node], value = V(graph)[node]$Traffic_End * AMEN_Cost_NAS)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_Switches+plug',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Cost_Switches + V(graph)[node]$Traffic_End * AMEN_Cost_Switches)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_10Gplugg',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Cost_10Gplugg + V(graph)[node]$Traffic_End * AMEN_Cost_10Gplugg)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_10G',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Cost_10G + V(graph)[node]$Traffic_End * AMEN_Cost_10G)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_100G',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Cost_100G + V(graph)[node]$Traffic_End * AMEN_Cost_100G)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_Switch',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Cost_Switch + V(graph)[node]$Traffic_End * AMEN_Cost_Switch)
      
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_servers',index = V(graph)[node], value = V(graph)[node]$Traffic_End * AMEN_Power_servers)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_NAS',index = V(graph)[node], value = V(graph)[node]$Traffic_End * AMEN_Power_NAS)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_Switches+plug',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Power_Switches +  V(graph)[node]$Traffic_End * AMEN_Power_Switches)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_10Gplugg',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Power_10Gplugg + V(graph)[node]$Traffic_End * AMEN_Power_10Gplugg)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_10G',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Power_10G + V(graph)[node]$Traffic_End * AMEN_Power_10G)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_100G',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Power_100G + V(graph)[node]$Traffic_End * AMEN_Power_100G)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_Switch',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * AMEN_Power_Switch + V(graph)[node]$Traffic_End * AMEN_Power_Switch)
      
      graph <- set_vertex_attr(graph, name = 'CPUs_basic',index = V(graph)[node], value =  V(graph)[node]$Traffic_End * AMEN_CPUs)
      graph <- set_vertex_attr(graph, name = 'TB_basic',index = V(graph)[node], value =  V(graph)[node]$Traffic_End * AMEN_TB)
      
      
    }
    else if (V(graph)[node]$"Central office type" == "National CO" || V(graph)[node]$"Central office type" == "Regional CO")
    {
      graph <- set_vertex_attr(graph, name = 'Cost_basic',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Cost_Without + V(graph)[node]$Traffic_End * MCEN_Cost_With)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Power_Without + V(graph)[node]$Traffic_End * MCEN_Power_With)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_servers',index = V(graph)[node], value = V(graph)[node]$Traffic_End * MCEN_Cost_servers)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_NAS',index = V(graph)[node], value = V(graph)[node]$Traffic_End * MCEN_Cost_NAS)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_Switches+plug',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Cost_Switches + V(graph)[node]$Traffic_End * MCEN_Cost_Switches)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_10Gplugg',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Cost_10Gplugg + V(graph)[node]$Traffic_End * MCEN_Cost_10Gplugg)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_10G',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Cost_10G + V(graph)[node]$Traffic_End * MCEN_Cost_10G)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_100G',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Cost_100G + V(graph)[node]$Traffic_End * MCEN_Cost_100G)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_Switch',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Cost_Switch + V(graph)[node]$Traffic_End * MCEN_Cost_Switch)
      
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_servers',index = V(graph)[node], value = V(graph)[node]$Traffic_End * MCEN_Power_servers)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_NAS',index = V(graph)[node], value = V(graph)[node]$Traffic_End * MCEN_Power_NAS)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_Switches+plug',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Power_Switches +  V(graph)[node]$Traffic_End * MCEN_Power_Switches)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_10Gplugg',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Power_10Gplugg + V(graph)[node]$Traffic_End * MCEN_Power_10Gplugg)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_10G',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Power_10G + V(graph)[node]$Traffic_End * MCEN_Power_10G)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_100G',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Power_100G + V(graph)[node]$Traffic_End * MCEN_Power_100G)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_Switch',index = V(graph)[node], value = V(graph)[node]$Traffic_Pass * MCEN_Power_Switch + V(graph)[node]$Traffic_End * MCEN_Power_Switch)
      graph <- set_vertex_attr(graph, name = 'CPUs_basic',index = V(graph)[node], value =  V(graph)[node]$Traffic_End * MCEN_CPUs)
      graph <- set_vertex_attr(graph, name = 'TB_basic',index = V(graph)[node], value =  V(graph)[node]$Traffic_End * MCEN_TB)
      
      
    }
    else
    {
      graph <- set_vertex_attr(graph, name = 'Cost_basic',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_servers',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_NAS',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_Switches+plug',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_10Gplugg',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_10G',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_100G',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Cost_basic_Switch',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_servers',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_NAS',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_Switches+plug',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_10Gplugg',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_10G',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_100G',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'Power_consumption_basic_Switch',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'CPUs_basic',index = V(graph)[node], value = 0)
      graph <- set_vertex_attr(graph, name = 'TB_basic',index = V(graph)[node], value =  0)
    }
  }
  return(graph)
}

