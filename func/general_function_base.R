
':=' <- function(lhs, rhs) {
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL)) 
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL)) 
}


######################### busring calculations 
#C - capacity in [Bps], L - load, n - # burst size U(1,n) in packets
calc_queue_delay_burst <- function(C, L, n = 100, PS_size=c(40,576,1500), PS_weights=c(7/12, 4/12, 1/12)) {
  ES = sum(PS_size*8/C*PS_weights); ES2 = sum((PS_size*8/C)^2*PS_weights)  #arrival times
  CS2 = ES2/ES^2-1
  Lambda_ps <- L/ES
  Lambda_burst <- Lambda_ps*2/(n+1)
  EA = 1/(n*Lambda_burst)*sum(1/c(1:n))
  EA2 =  2/n * 1/(Lambda_burst^2)*sum(1/(c(1:n))^2)
  Var_a = EA2 - EA^2
  CA2 =  Var_a/EA^2 
  #Et <- ES/(1-L)*(CS2 + CA2)/2
  Et <- ES*L/(1-L)*(CS2 + CA2)/2 + ES
  Et[is.na(Et)] <- 0
  return(list(Et, Lambda_ps, Lambda_burst))
}

# ######################### busring calculations old 
# #C - capacity in [Bps], L - load, n - # burst size U(1,n) in packets
# calc_queue_delay_burst <- function(C, L, n = 100, PS_size=c(40,576,1500), PS_weights=c(7/12, 4/12, 1/12)) {
#   ES = sum(PS_size*8/C*PS_weights); ES2 = sum((PS_size*8/C)^2*PS_weights)
#   #arrival times
#   CS2 = ES2/ES^2-1
#   Lambda_ps <- L/ES
#   Lambda_burst <- Lambda_ps*2/(n+1)
#   EA = 1/(n*Lambda_burst)*sum(1/c(1:n))
#   Var_a = 1/(n*Lambda_burst^2)*sum((1/c(1:n))^2)
#   CA2 =  Var_a/EA^2 
#   Et <- ES/(1-L)*(CS2 + CA2)/2
#   Et[is.na(Et)] <- 0
#   return(list(Et, Lambda_ps, Lambda_burst))
# }
# 
# ######################### old v2
# #C - capacity in [B], L - load, n - # burst size U(1,n) in packets
# calc_queue_delay_burst <- function(C, L, n = 100, PS_size=c(40,576,1500), PS_weights=c(7/12, 4/12, 1/12)) {
#   # service times
#   ES = sum(PS_size*8/C*PS_weights); ES2 = sum((PS_size*8/C)^2*PS_weights)
#   #arrival times
#   CS2 = ES2/ES^2-1 # service time coeff of variation 1.58
#   CA2 = n*sum((1/c(1:n))^2) / (sum(1/c(1:n)))^2-1
#   Et <- ES/(1-L)*(CS2 + CA2)/2
#   Lambda_ps <- 1/ES*L
#   Lambda_burst <- Lambda_ps*2/(n+1)
#   return(list(Et, Lambda_ps, Lambda_burst))
# }
# ########################

##############################################Bounds

func_bounds_markov <- function(mu, a)
{
  ####################Markov's inequality
  upper_bound_Markov <- mu/(1-a)
  Prop_Markov <- 1 - mu/upper_bound_Markov
  return(upper_bound_Markov)
}

func_bounds_VP <- function(delay_hops, a)
{
  mu = sum(delay_hops)
  sigma = sqrt(sum(delay_hops^2))
  ###################Vysochanskij–Petunin's bound
  k <- sqrt(4/9/(1-a))
  Prop_VP <- 1 - 4/9/(k^2)
  upper_bound_VP <- k * sigma + mu
  return(upper_bound_VP)
}

func_bounds_chebyshev <- function(mu, variance, a)
{
  sigma2 = sqrt(variance)
  ###################Chebyshev's inequality
  k <- sqrt(1/(1-a))
  Prop_Chebyshev <- 1 - 1/(k)^2
  upper_bound_cheb <- k * sigma2 + mu
  return(upper_bound_cheb)
}

func_bounds_hoeffding <- function(mu, a, delay_queue, delay_prop, percentile_of_max)
{
  ##################Hoeffding's inequality
  min_x <- delay_prop
  max_x <- delay_queue*ln(1/(1-percentile_of_max)) + delay_prop
  delta_of_hops <- (max_x - min_x)^2
  delta_of_hops <- lapply(which(delta_of_hops != 0), function(x) delta_of_hops[x])
  sum_of_delta_hops <- 0
  for (i in delta_of_hops) { sum_of_delta_hops <- sum_of_delta_hops + i }
  alpha <- 1 - a
  delta <- sqrt(ln(alpha)*sum_of_delta_hops/(-2*mu^2))
  upper_bound_heoff <- mu*(1 + delta)
  upper_bound_heoff[is.na(upper_bound_heoff)] <- 0
  return(upper_bound_heoff)
}


func_bounds_theoretical <- function(mu, variance, a, delay_queue, delay_prop, percentile_of_max = 0.9)
{
  
  print(paste("variance=", variance))
  print(paste("sigma2=", sqrt(variance)))
  print(paste("igraph E(T) =", mu))
  ####################Markov's inequality
  upper_bound_Markov <- func_bounds_markov(mu, a)
  
  ###################Chebyshev's inequality
  upper_bound_cheb <- func_bounds_chebyshev(mu, variance, a)
  
  ##################Hoeffding's inequality
  upper_bound_heoff <- func_bounds_hoeffding(mu, a, delay_queue, delay_prop, percentile_of_max = 0.99)
  ###################Vysochanskij–Petunin's bound
  upper_bound_VP <- func_bounds_VP(delay_queue, a) + delay_prop
    
  return(list(upper_bound_Markov, upper_bound_cheb, upper_bound_heoff, upper_bound_VP))
}



read.transposed.xlsx <- function(file,sheetIndex) {
  data <- openxlsx::read.xlsx(file, sheet = sheetIndex, startRow = 2,colNames = FALSE, rowNames = FALSE)
  data <- setNames(data.frame(t(data[ , - 1])), data[ , 1])  # Transpose data
  return(data)            
}
