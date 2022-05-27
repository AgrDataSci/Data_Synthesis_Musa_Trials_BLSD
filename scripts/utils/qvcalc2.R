#Code adapted from plot pltree
#Get qvcalc output when the standard qvcalc doesn't work

#object <- musa_plt_01

qvcalc2 <- function(object, log = TRUE, ref = NULL){
  
  # Extract ids from terminal nodes
  node_id <- partykit::nodeids(object, terminal = TRUE)
  
  # get node information
  nodes <- vector(mode = "list", length = length(node_id))
  
  for(i in seq_along(node_id)){
    nodes[[i]] <- object[[node_id[i]]]$node$info$object
  }
  
  # get number of observations in each inner node
  nobs <- vector(mode = "list", length = length(node_id))
  
  for(i in seq_along(node_id)){
    
    nobs[[i]] <- as.integer(object[[ node_id[i] ]]$node$info$nobs) 
    
  }
  
  #get item names
  items <- dimnames(coef(object))[[2]]
  
  coeffs <- lapply(nodes, function(x) {
    psychotools::itempar(x, vcov = FALSE, log = log, ref = ref)
    })
  
  qvSE <- lapply(nodes, function(x){
    
    Z <- x$rankings
    
    Z <- Z[1:length(Z), , as.rankings = F]
    
    rmv <- which(colSums(Z) == 0)
    
    if (length(rmv) > 0) Z <- Z[, -rmv]
    
    Z <- update(x, rankings = Z, weights = freq(Z), start = NULL)
    
    Z <- psychotools::itempar(Z, vcov = TRUE, log = log, ref = ref)
    
    # get estimates from item parameters using qvcalc
    Z <- qvcalc::qvcalc(Z)
    
    # extract data frames with estimates
    Z <- Z$qvframe
    
    #add p-values
    
    #formulas from summary Plackett-Luce package
    #https://github.com/hturner/PlackettLuce/blob/master/R/summary.R
    Z$Z_value <- Z$estimate/Z$SE
    
    Z$p_value <- 2 * pnorm(-abs(Z$Z_value))
    
    Z$items <- rownames(Z)
    
    rownames(Z) <- NULL
    
    
    
    Z <- Z[, c("items", 
               "estimate",
               "SE", 
               "Z_value",
               "p_value",
               "quasiSE",
               "quasiVar")]
    
    })
  
  #x <- vector(mode = "list", length = length(item_par))

  #remove ties if present
  if(0 < sum(grepl(pattern = "tie", x = items))){
    message("Removing ties for plotting")
    items <- items[grep(pattern = "tie", x = items,invert = TRUE)]
  }



  # for(i in seq_along(item_par)){
  # 
  #   xi <- data.frame(estimate = as.vector(item_par[[i]]),
  #                    items = items)
  # 
  #   xi <- merge(xi, qvSE[[i]][, c("items", "SE", "quasiSE", "quasiVar")], 
  #               by = "items", all.x = TRUE)
  # 
  #   x[[i]] <- xi
  # 
  # }

  #item_par <- x
  return(qvSE)
  
}
