traffic_simulation_CWB <- function(traffic_df_sc1 = traffic_df_sc1, traffic_df_sc2 = traffic_df_sc2, traffic_df_sc3 = traffic_df_sc3, traffic_parameters = traffic_parameters)
{
  traffic_df_sc1 <- traffic_simulation_CWB_base(traffic_df = traffic_df_sc1, scenario_number = 1, traffic_parameters = traffic_parameters)
  traffic_df_sc2 <- traffic_simulation_CWB_base(traffic_df = traffic_df_sc2, scenario_number = 2, traffic_parameters = traffic_parameters)
  traffic_df_sc3 <- traffic_simulation_CWB_base(traffic_df = traffic_df_sc3, scenario_number = 3, traffic_parameters = traffic_parameters)
  return(list(traffic_df_sc1, traffic_df_sc2, traffic_df_sc3))
}


traffic_simulation_CDN <- function(traffic_df_sc1 = traffic_df_sc1, traffic_df_sc2 = traffic_df_sc2, traffic_df_sc3 = traffic_df_sc3, traffic_parameters = traffic_parameters)
{
  traffic_df_sc1 <- traffic_simulation_CDN_base(traffic_df = traffic_df_sc1, scenario_number = 1, traffic_parameters = traffic_parameters)
  traffic_df_sc2 <- traffic_simulation_CDN_base(traffic_df = traffic_df_sc2, scenario_number = 2, traffic_parameters = traffic_parameters)
  traffic_df_sc3 <- traffic_simulation_CDN_base(traffic_df = traffic_df_sc3, scenario_number = 3, traffic_parameters = traffic_parameters)
  return(list(traffic_df_sc1, traffic_df_sc2, traffic_df_sc3))
}


traffic_simulation_DCM <- function(traffic_df_sc1 = traffic_df_sc1, traffic_df_sc2 = traffic_df_sc2, traffic_df_sc3 = traffic_df_sc3, traffic_parameters = traffic_parameters)
{
  traffic_df_sc1 <- traffic_simulation_DCM_base(traffic_df = traffic_df_sc1, scenario_number = 1, traffic_parameters = traffic_parameters)
  traffic_df_sc2 <- traffic_simulation_DCM_base(traffic_df = traffic_df_sc2, scenario_number = 2, traffic_parameters = traffic_parameters)
  traffic_df_sc3 <- traffic_simulation_DCM_base(traffic_df = traffic_df_sc3, scenario_number = 3, traffic_parameters = traffic_parameters)
  return(list(traffic_df_sc1, traffic_df_sc2, traffic_df_sc3))
}




traffic_simulation_CWB_base <- function(traffic_df = traffic_df, scenario_number = 1, traffic_parameters = traffic_parameters)
{
  #CWB
  #scenario1
  traffic_df$`CWB terminated @ Local UL` <- traffic_parameters$CWB_Local_CO[scenario_number]*(traffic_df$Households*traffic_parameters$CWB_persent_active_users[1]*traffic_parameters$CWB_UL_traffic[1] + 
                                                                                                traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$CWB_persent_active_users[2]*traffic_parameters$CWB_UL_traffic[2] +
                                                                                                traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$CWB_persent_active_users[3]*traffic_parameters$CWB_UL_traffic[3])/1000
  traffic_df$`CWB terminated @ Local DL` <- traffic_parameters$CWB_Local_CO[scenario_number]*(traffic_df$Households*traffic_parameters$CWB_persent_active_users[1]*traffic_parameters$CWB_DL_traffic[1] + 
                                                                                                traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$CWB_persent_active_users[2]*traffic_parameters$CWB_DL_traffic[2] +
                                                                                                traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$CWB_persent_active_users[3]*traffic_parameters$CWB_DL_traffic[3])/1000
  traffic_df$`CWB term. @ Reference Regional UL` <- traffic_parameters$CWB_Regional_CO[scenario_number]*(traffic_df$Households*traffic_parameters$CWB_persent_active_users[1]*traffic_parameters$CWB_UL_traffic[1] + 
                                                                                                           traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$CWB_persent_active_users[2]*traffic_parameters$CWB_UL_traffic[2] +
                                                                                                           traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$CWB_persent_active_users[3]*traffic_parameters$CWB_UL_traffic[3])/1000
  traffic_df$`CWB term. @ Reference Regional DL` <- traffic_parameters$CWB_Regional_CO[scenario_number]*(traffic_df$Households*traffic_parameters$CWB_persent_active_users[1]*traffic_parameters$CWB_DL_traffic[1] + 
                                                                                                           traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$CWB_persent_active_users[2]*traffic_parameters$CWB_DL_traffic[2] +
                                                                                                           traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$CWB_persent_active_users[3]*traffic_parameters$CWB_DL_traffic[3])/1000
  traffic_df$`CWB term. @ Reference National UL` <- traffic_parameters$CWB_National_CO[scenario_number]*(traffic_df$Households*traffic_parameters$CWB_persent_active_users[1]*traffic_parameters$CWB_UL_traffic[1] + 
                                                                                                           traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$CWB_persent_active_users[2]*traffic_parameters$CWB_UL_traffic[2] +
                                                                                                           traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$CWB_persent_active_users[3]*traffic_parameters$CWB_UL_traffic[3])/1000
  traffic_df$`CWB term. @ Reference National DL` <- traffic_parameters$CWB_National_CO[scenario_number]*(traffic_df$Households*traffic_parameters$CWB_persent_active_users[1]*traffic_parameters$CWB_DL_traffic[1] + 
                                                                                                           traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$CWB_persent_active_users[2]*traffic_parameters$CWB_DL_traffic[2] +
                                                                                                           traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$CWB_persent_active_users[3]*traffic_parameters$CWB_DL_traffic[3])/1000
  traffic_df$`CWB term. @ others National UL` <- traffic_parameters$CWB_other_National_CO[scenario_number]*(traffic_df$Households*traffic_parameters$CWB_persent_active_users[1]*traffic_parameters$CWB_UL_traffic[1] + 
                                                                                                              traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$CWB_persent_active_users[2]*traffic_parameters$CWB_UL_traffic[2] +
                                                                                                              traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$CWB_persent_active_users[3]*traffic_parameters$CWB_UL_traffic[3])/1000
  traffic_df$`CWB term. @ others National DL` <- traffic_parameters$CWB_other_National_CO[scenario_number]*(traffic_df$Households*traffic_parameters$CWB_persent_active_users[1]*traffic_parameters$CWB_DL_traffic[1] + 
                                                                                                              traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$CWB_persent_active_users[2]*traffic_parameters$CWB_DL_traffic[2] +
                                                                                                              traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$CWB_persent_active_users[3]*traffic_parameters$CWB_DL_traffic[3])/1000
  traffic_df$`CWB to Gateway (Ext.) UL` <- traffic_parameters$CWB_External[scenario_number]*(traffic_df$Households*traffic_parameters$CWB_persent_active_users[1]*traffic_parameters$CWB_UL_traffic[1] + 
                                                                                               traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$CWB_persent_active_users[2]*traffic_parameters$CWB_UL_traffic[2] +
                                                                                               traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$CWB_persent_active_users[3]*traffic_parameters$CWB_UL_traffic[3])/1000
  traffic_df$`CWB to Gateway (Ext.) DL` <- traffic_parameters$CWB_External[scenario_number]*(traffic_df$Households*traffic_parameters$CWB_persent_active_users[1]*traffic_parameters$CWB_DL_traffic[1] + 
                                                                                               traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$CWB_persent_active_users[2]*traffic_parameters$CWB_DL_traffic[2] +
                                                                                               traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$CWB_persent_active_users[3]*traffic_parameters$CWB_DL_traffic[3])/1000
  return(traffic_df)
}


traffic_simulation_CDN_base <- function( traffic_df = traffic_df, scenario_number = 1, traffic_parameters = traffic_parameters)
{
  traffic_df$`CDN terminated @ Local UL` <- traffic_parameters$CDN_Local_CO[scenario_number]*(traffic_df$Households*traffic_parameters$CDN_persent_active_users[1]*traffic_parameters$CDN_UL_traffic[1] + 
                                                                                                traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$CDN_persent_active_users[2]*traffic_parameters$CDN_UL_traffic[2] +
                                                                                                traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$CDN_persent_active_users[3]*traffic_parameters$CDN_UL_traffic[3])/1000
  traffic_df$`CDN terminated @ Local DL` <- traffic_parameters$CDN_Local_CO[scenario_number]*(traffic_df$Households*traffic_parameters$CDN_persent_active_users[1]*traffic_parameters$CDN_DL_traffic[1] + 
                                                                                                traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$CDN_persent_active_users[2]*traffic_parameters$CDN_DL_traffic[2] +
                                                                                                traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$CDN_persent_active_users[3]*traffic_parameters$CDN_DL_traffic[3])/1000
  traffic_df$`CDN term. @ Reference Regional UL` <- traffic_parameters$CDN_Regional_CO[scenario_number]*(traffic_df$Households*traffic_parameters$CDN_persent_active_users[1]*traffic_parameters$CDN_UL_traffic[1] + 
                                                                                                           traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$CDN_persent_active_users[2]*traffic_parameters$CDN_UL_traffic[2] +
                                                                                                           traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$CDN_persent_active_users[3]*traffic_parameters$CDN_UL_traffic[3])/1000
  traffic_df$`CDN term. @ Reference Regional DL` <- traffic_parameters$CDN_Regional_CO[scenario_number]*(traffic_df$Households*traffic_parameters$CDN_persent_active_users[1]*traffic_parameters$CDN_DL_traffic[1] + 
                                                                                                           traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$CDN_persent_active_users[2]*traffic_parameters$CDN_DL_traffic[2] +
                                                                                                           traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$CDN_persent_active_users[3]*traffic_parameters$CDN_DL_traffic[3])/1000
  traffic_df$`CDN term. @ Reference National UL` <- traffic_parameters$CDN_National_CO[scenario_number]*(traffic_df$Households*traffic_parameters$CDN_persent_active_users[1]*traffic_parameters$CDN_UL_traffic[1] + 
                                                                                                           traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$CDN_persent_active_users[2]*traffic_parameters$CDN_UL_traffic[2] +
                                                                                                           traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$CDN_persent_active_users[3]*traffic_parameters$CDN_UL_traffic[3])/1000
  traffic_df$`CDN term. @ Reference National DL` <- traffic_parameters$CDN_National_CO[scenario_number]*(traffic_df$Households*traffic_parameters$CDN_persent_active_users[1]*traffic_parameters$CDN_DL_traffic[1] + 
                                                                                                           traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$CDN_persent_active_users[2]*traffic_parameters$CDN_DL_traffic[2] +
                                                                                                           traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$CDN_persent_active_users[3]*traffic_parameters$CDN_DL_traffic[3])/1000
  traffic_df$`CDN term. @ others National UL` <- traffic_parameters$CDN_other_National_CO[scenario_number]*(traffic_df$Households*traffic_parameters$CDN_persent_active_users[1]*traffic_parameters$CDN_UL_traffic[1] + 
                                                                                                              traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$CDN_persent_active_users[2]*traffic_parameters$CDN_UL_traffic[2] +
                                                                                                              traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$CDN_persent_active_users[3]*traffic_parameters$CDN_UL_traffic[3])/1000
  traffic_df$`CDN term. @ others National DL` <- traffic_parameters$CDN_other_National_CO[scenario_number]*(traffic_df$Households*traffic_parameters$CDN_persent_active_users[1]*traffic_parameters$CDN_DL_traffic[1] + 
                                                                                                              traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$CDN_persent_active_users[2]*traffic_parameters$CDN_DL_traffic[2] +
                                                                                                              traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$CDN_persent_active_users[3]*traffic_parameters$CDN_DL_traffic[3])/1000
  traffic_df$`CDN to Gateway (Ext.) UL` <- traffic_parameters$CDN_External[scenario_number]*(traffic_df$Households*traffic_parameters$CDN_persent_active_users[1]*traffic_parameters$CDN_UL_traffic[1] + 
                                                                                               traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$CDN_persent_active_users[2]*traffic_parameters$CDN_UL_traffic[2] +
                                                                                               traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$CDN_persent_active_users[3]*traffic_parameters$CDN_UL_traffic[3])/1000
  traffic_df$`CDN to Gateway (Ext.) DL` <- traffic_parameters$CDN_External[scenario_number]*(traffic_df$Households*traffic_parameters$CDN_persent_active_users[1]*traffic_parameters$CDN_DL_traffic[1] + 
                                                                                               traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$CDN_persent_active_users[2]*traffic_parameters$CDN_DL_traffic[2] +
                                                                                               traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$CDN_persent_active_users[3]*traffic_parameters$CDN_DL_traffic[3])/1000
  
  return(traffic_df)
}


traffic_simulation_DCM_base <- function(traffic_df = traffic_df, scenario_number = 1, traffic_parameters = traffic_parameters)
{
  #CND
  traffic_df$`DCM @ Local/Regional CO UL` <- traffic_parameters$DCM_Local_Regional_CO_same_aggr_UL[scenario_number]*(traffic_df$Households*traffic_parameters$DCM_persent_active_users[1]*traffic_parameters$DCM_UL_traffic[1] + 
                                                                                                                       traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$DCM_persent_active_users[2]*traffic_parameters$DCM_UL_traffic[2] +
                                                                                                                       traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$DCM_persent_active_users[3]*traffic_parameters$DCM_UL_traffic[3])/1000
  traffic_df$`DCM @ Local/Regional CO DL` <- traffic_parameters$DCM_Local_Regional_CO_same_aggr_DL[scenario_number]*(traffic_df$Households*traffic_parameters$DCM_persent_active_users[1]*traffic_parameters$DCM_DL_traffic[1] + 
                                                                                                                       traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$DCM_persent_active_users[2]*traffic_parameters$DCM_DL_traffic[2] +
                                                                                                                       traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$DCM_persent_active_users[3]*traffic_parameters$DCM_DL_traffic[3])/1000
  traffic_df$`DCM @ Local/Regional/National CO UL` <- traffic_parameters$DCM_direct_Local_Regional_National_UL[scenario_number]*(traffic_df$Households*traffic_parameters$DCM_persent_active_users[1]*traffic_parameters$DCM_UL_traffic[1] + 
                                                                                                                                   traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$DCM_persent_active_users[2]*traffic_parameters$DCM_UL_traffic[2] +
                                                                                                                                   traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$DCM_persent_active_users[3]*traffic_parameters$DCM_UL_traffic[3])/1000
  traffic_df$`DCM @ Local/Regional/National CO DL` <- traffic_parameters$DCM_direct_Local_Regional_National_UL[scenario_number]*(traffic_df$Households*traffic_parameters$DCM_persent_active_users[1]*traffic_parameters$DCM_DL_traffic[1] + 
                                                                                                                                   traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$DCM_persent_active_users[2]*traffic_parameters$DCM_DL_traffic[2] +
                                                                                                                                   traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$DCM_persent_active_users[3]*traffic_parameters$DCM_DL_traffic[3])/1000
  traffic_df$`DCM @ Ref. Regional CO UL` <- traffic_parameters$DCM_direct_to_Regional_CO_UL[scenario_number]*(traffic_df$Households*traffic_parameters$DCM_persent_active_users[1]*traffic_parameters$DCM_UL_traffic[1] + 
                                                                                                                traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$DCM_persent_active_users[2]*traffic_parameters$DCM_UL_traffic[2] +
                                                                                                                traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$DCM_persent_active_users[3]*traffic_parameters$DCM_UL_traffic[3])/1000
  traffic_df$`DCM @ Ref. Regional CO DL` <- traffic_parameters$DCM_direct_to_Regional_CO_DL[scenario_number]*(traffic_df$Households*traffic_parameters$DCM_persent_active_users[1]*traffic_parameters$DCM_DL_traffic[1] + 
                                                                                                                traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$DCM_persent_active_users[2]*traffic_parameters$DCM_DL_traffic[2] +
                                                                                                                traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$DCM_persent_active_users[3]*traffic_parameters$DCM_DL_traffic[3])/1000
  traffic_df$`DCM @ Twin Regional CO UL` <- traffic_parameters$DCM_Regional_CO_to_Twin_Regional_CO_UL[scenario_number]*(traffic_df$Households*traffic_parameters$DCM_persent_active_users[1]*traffic_parameters$DCM_UL_traffic[1] + 
                                                                                                                          traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$DCM_persent_active_users[2]*traffic_parameters$DCM_UL_traffic[2] +
                                                                                                                          traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$DCM_persent_active_users[3]*traffic_parameters$DCM_UL_traffic[3])/1000
  traffic_df$`DCM @ Twin Regional CO DL` <- traffic_parameters$DCM_Regional_CO_to_Twin_Regional_CO_DL[scenario_number]*(traffic_df$Households*traffic_parameters$DCM_persent_active_users[1]*traffic_parameters$DCM_DL_traffic[1] + 
                                                                                                                          traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$DCM_persent_active_users[2]*traffic_parameters$DCM_DL_traffic[2] +
                                                                                                                          traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$DCM_persent_active_users[3]*traffic_parameters$DCM_DL_traffic[3])/1000
  traffic_df$`DCM @ Other Regional CO UL` <- traffic_parameters$DCM_Regional_CO_to_oth_Regional_CO_UL[scenario_number]*(traffic_df$Households*traffic_parameters$DCM_persent_active_users[1]*traffic_parameters$DCM_UL_traffic[1] + 
                                                                                                                          traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$DCM_persent_active_users[2]*traffic_parameters$DCM_UL_traffic[2] +
                                                                                                                          traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$DCM_persent_active_users[3]*traffic_parameters$DCM_UL_traffic[3])/1000
  traffic_df$`DCM @  Other Regional CO DL` <- traffic_parameters$DCM_Regional_CO_to_oth_Regional_CO_DL[scenario_number]*(traffic_df$Households*traffic_parameters$DCM_persent_active_users[1]*traffic_parameters$DCM_DL_traffic[1] + 
                                                                                                                           traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$DCM_persent_active_users[2]*traffic_parameters$DCM_DL_traffic[2] +
                                                                                                                           traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$DCM_persent_active_users[3]*traffic_parameters$DCM_DL_traffic[3])/1000
  traffic_df$`DCM @  Ref. Regional CO to Ref. National CO UL` <- traffic_parameters$DCM_Regional_CO_to_National_CO_UL[scenario_number]*(traffic_df$Households*traffic_parameters$DCM_persent_active_users[1]*traffic_parameters$DCM_UL_traffic[1] + 
                                                                                                                                          traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$DCM_persent_active_users[2]*traffic_parameters$DCM_UL_traffic[2] +
                                                                                                                                          traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$DCM_persent_active_users[3]*traffic_parameters$DCM_UL_traffic[3])/1000
  traffic_df$`DCM @  Ref. Regional CO to Ref. National CO DL` <- traffic_parameters$DCM_Regional_CO_to_National_CO_DL[scenario_number]*(traffic_df$Households*traffic_parameters$DCM_persent_active_users[1]*traffic_parameters$DCM_DL_traffic[1] + 
                                                                                                                                          traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$DCM_persent_active_users[2]*traffic_parameters$DCM_DL_traffic[2] +
                                                                                                                                          traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$DCM_persent_active_users[3]*traffic_parameters$DCM_DL_traffic[3])/1000
  traffic_df$`DCM @ Ref. National CO UL` <- traffic_parameters$DCM_direct_to_National_CO_UL[scenario_number]*(traffic_df$Households*traffic_parameters$DCM_persent_active_users[1]*traffic_parameters$DCM_UL_traffic[1] + 
                                                                                                                traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$DCM_persent_active_users[2]*traffic_parameters$DCM_UL_traffic[2] +
                                                                                                                traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$DCM_persent_active_users[3]*traffic_parameters$DCM_UL_traffic[3])/1000
  traffic_df$`DCM @ Ref. National CO DL` <- traffic_parameters$DCM_direct_to_National_CO_DL[scenario_number]*(traffic_df$Households*traffic_parameters$DCM_persent_active_users[1]*traffic_parameters$DCM_DL_traffic[1] + 
                                                                                                                traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$DCM_persent_active_users[2]*traffic_parameters$DCM_DL_traffic[2] +
                                                                                                                traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$DCM_persent_active_users[3]*traffic_parameters$DCM_DL_traffic[3])/1000
  traffic_df$`DCM @ Twin National CO UL` <- traffic_parameters$DCM_National_CO_to_Twin_National_CO_UL[scenario_number]*(traffic_df$Households*traffic_parameters$DCM_persent_active_users[1]*traffic_parameters$DCM_UL_traffic[1] + 
                                                                                                                          traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$DCM_persent_active_users[2]*traffic_parameters$DCM_UL_traffic[2] +
                                                                                                                          traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$DCM_persent_active_users[3]*traffic_parameters$DCM_UL_traffic[3])/1000
  traffic_df$`DCM @ Twin National CO DL` <- traffic_parameters$DCM_National_CO_to_Twin_National_CO_DL[scenario_number]*(traffic_df$Households*traffic_parameters$DCM_persent_active_users[1]*traffic_parameters$DCM_DL_traffic[1] + 
                                                                                                                          traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$DCM_persent_active_users[2]*traffic_parameters$DCM_DL_traffic[2] +
                                                                                                                          traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$DCM_persent_active_users[3]*traffic_parameters$DCM_DL_traffic[3])/1000
  traffic_df$`DCM @ Other National CO UL` <- traffic_parameters$DCM_National_CO_to_Oth_National_CO_UL[scenario_number]*(traffic_df$Households*traffic_parameters$DCM_persent_active_users[1]*traffic_parameters$DCM_UL_traffic[1] + 
                                                                                                                          traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$DCM_persent_active_users[2]*traffic_parameters$DCM_UL_traffic[2] +
                                                                                                                          traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$DCM_persent_active_users[3]*traffic_parameters$DCM_UL_traffic[3])/1000
  traffic_df$`DCM @ Other National CO DL` <- traffic_parameters$DCM_National_CO_to_Oth_National_CO_DL[scenario_number]*(traffic_df$Households*traffic_parameters$DCM_persent_active_users[1]*traffic_parameters$DCM_DL_traffic[1] + 
                                                                                                                          traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$DCM_persent_active_users[2]*traffic_parameters$DCM_DL_traffic[2] +
                                                                                                                          traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$DCM_persent_active_users[3]*traffic_parameters$DCM_DL_traffic[3])/1000
  traffic_df$`DCM @ National To Gateway (Ext.) UL` <- traffic_parameters$DCM_National_CO_to_Gateway_UL[scenario_number]*(traffic_df$Households*traffic_parameters$DCM_persent_active_users[1]*traffic_parameters$DCM_UL_traffic[1] + 
                                                                                                                           traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$DCM_persent_active_users[2]*traffic_parameters$DCM_UL_traffic[2] +
                                                                                                                           traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$DCM_persent_active_users[3]*traffic_parameters$DCM_UL_traffic[3])/1000
  traffic_df$`DCM @ National To Gateway (Ext.) DL` <- traffic_parameters$DCM_National_CO_to_Gateway_DL[scenario_number]*(traffic_df$Households*traffic_parameters$DCM_persent_active_users[1]*traffic_parameters$DCM_DL_traffic[1] + 
                                                                                                                           traffic_df$`Macro cells sites`*traffic_parameters$Mobile_active_users[1]*traffic_parameters$DCM_persent_active_users[2]*traffic_parameters$DCM_DL_traffic[2] +
                                                                                                                           traffic_df$`Small cell sites`*traffic_parameters$Mobile_active_users[2]*traffic_parameters$DCM_persent_active_users[3]*traffic_parameters$DCM_DL_traffic[3])/1000
  
  return(traffic_df)
}

