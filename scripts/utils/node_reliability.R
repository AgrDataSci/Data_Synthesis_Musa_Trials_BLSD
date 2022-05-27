reliability2 <- function(x){
  
  node_win_prob <- exp(x$estimate) / sum(exp(x$estimate)) 
  
  node_rel <- node_win_prob / (node_win_prob[1] + node_win_prob)
  
  x$reliability <- node_rel
  
  node_rel_upper_bound <- (exp(log(node_win_prob) + x$quasiSE) / (node_win_prob[1] + exp(log(node_win_prob) + x$quasiSE)))  
  
  node_rel_lower_bound <- (exp(log(node_win_prob) - x$quasiSE) / (node_win_prob[1] + exp(log(node_win_prob) - x$quasiSE)))  
  
  node_rel_se <-  node_rel - node_rel_lower_bound
  
  #s2$rel_se_upp <- node_2_rel_upper_bound
  
  #s2$rel_se_low <- node_2_rel_lower_bound
  
  x$rel_se <- node_rel_se
  
  # #formulas from summary Plackett-Luce package
  # #https://github.com/hturner/PlackettLuce/blob/master/R/summary.R
  # x$Z <- x$estimate/x$SE
  # 
  # x$p_value <- 2 * pnorm(-abs(x$Z))
  
  return(x)
  
}
