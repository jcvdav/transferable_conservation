# List of functions to convert area targets to
# total benefit, and target benefit to area targets

# Calculate area as a function of benefit
area <- function(data, ben) {
  max(data$pct[data$tb <= ben], na.rm = T)
}

benefit <- function(data, area) {
  sum(data$ben[data$pct <= area], na.rm = T)
}














########## OLD FUNCTIONS BELOW ################
###############################################
###############################################
###############################################
###############################################
###############################################
###############################################
###############################################
###############################################







# Define function that finds the top n in each vector
which_max_n <- function(vector, prop, dec) {
  n <- prop * length(vector)
  vector %in% head(sort(vector, decreasing = dec), n)
}

sample_patch <- function(vector, prop) {
  n <- prop * length(vector)
  where <- sample(1:length(vector), size = n, replace = F)
  conserved <- numeric(length = length(vector))
  conserved[where] <- 1
  return(conserved)
}

# Define wrapper that conserves a given proportion
conserve <- function(matrix, proportion, tactic, dec = dec) {
  # browser()
  
  n <- round(proportion * length(matrix), 10)
  
  # How are we conserving?
  if(tactic == "bau"){
    # Call the function to match the max n in each column
    conserved <- apply(X = matrix, MARGIN = 2, FUN = which_max_n, proportion, dec)

  } else if (tactic == "random_c"){
    # browser()
    # Random patch-level conservation
    conserved <- apply(X = matrix, MARGIN = 2, FUN = sample_patch, proportion)

  } else if(tactic == "mkt") {
    # market approach here
    conserved <- matrix %in% head(sort(matrix, decreasing = dec), n)
  }
  
  if(sum(conserved) == length(matrix)){
    fill_1 <- rep(1L, n)
    fill_0 <- rep(0L, length(matrix) - n)
    fill <- c(fill_1, fill_0)
    conserved <- matrix(fill, ncol = ncol(matrix), byrow = T)
  }
  
  
  if(!is.matrix(conserved)){
    conserved <- matrix(conserved, ncol = ncol(matrix))
  }
    
  
  # Return the results
  return(conserved)
}

global_benefits <- function(benefits, costs, proportion, tactic = "bau", dec = TRUE, type){
  # dec = TRUE means that maximum values are retained
  
  if(type == "max_benefits"){
    matrix <- benefits
  } else {
    matrix <- costs
  }
  
  conserved <- conserve(matrix = matrix, proportion = proportion, tactic = tactic, dec = dec)
  global_benefits <- sum(conserved * benefits)
  global_costs <- sum(conserved * costs)
  
  values <- tibble(benefits = global_benefits,
                   costs = global_costs)
  
  return(values)
}










