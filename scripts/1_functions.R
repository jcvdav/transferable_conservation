
# Define function that finds the top n in each vector
which_max_n <- function(vector, n, dec) {
  vector %in% head(sort(vector, decreasing = dec), n)
}

sample_patch <- function(vector, n) {
  vector %in% sample(vector, size = n, replace = F)
}

# Define wrapper that conserves a given proportion
conserve <- function(matrix, proportion, tactic, dec = dec) {
  # browser()
  
  n <- round(proportion * length(matrix), 10)
  
  # How are we conserving?
  if(tactic == "bau"){
    # Call the function to match the max n in each column
    conserved <- apply(X = matrix, MARGIN = 2, FUN = which_max_n, n, dec)
    
  } else if (tactic == "random_c"){
    # Random patch-level conservation
    conserved <- apply(X = matrix, MARGIN = 2, FUN = sample_patch)
    
  } else if(tactic == "mkt") {
    # market approach here
    conserved <- matrix %in% head(sort(matrix, decreasing = dec), n)
  }
  
  
  
  if(sum(conserved) == length(matrix)){
    fill_1 <- rep(1L, n)
    fill_0 <- rep(0L, length(matrix) - n)
    fill <- c(fill_1, fill_0)
    if(length(fill) < length(matrix)){
      c(tactic, proportion, dec, length(fill_1), length(fill_0), (length(matrix) - n)) %>% 
        map(print)
    }
    conserved <- matrix(fill, ncol = ncol(matrix), byrow = T)
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










