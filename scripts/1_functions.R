
# Define function that finds the top n in each vector
which_max_n <- function(vector, proportion) {
  n <- proportion * length(vector)
  vector %in% head(sort(vector, decreasing = T), n)
}

sample_patch <- function(vector, proportion) {
  n <- proportion * length(vector)
  vector %in% sample(vector, size = n, replace = F)
}

top_n <- function(matrix, p)

# Define wrapper that conserves a given proportion
conserve <- function(matrix, proportion, tactic) {
  
  # How are we conserving?
  if(tactic == "bau"){
    
    # Call the function to match the max n in each column
    conserved <- apply(X = matrix, MARGIN = 2, FUN = which_max_n, proportion)
    
  } else if (tactic == "random_c"){
    
    # Random patch-level conservation
    conserved <- apply(X = matrix, MARGIN = 2, FUN = sample_patch, proportion)
    
  } else if (tactic == "random_g") {
    
    # Random global conservation
    n <- proportion * length(matrix)
    conserved <- matrix %in% sample(x = matrix, size = n, replace = F)
    
  } else if(tactic == "mkt") {
    
    # market approach here
    n <- proportion * length(matrix)
    conserved <- matrix %in% head(sort(matrix, decreasing = T), n)
  }
    
  
  # Return the results
  return(conserved)
}

global_benefits <- function(matrix, proportion, tactic = "bau"){
  conserved <- conserve(matrix = matrix, proportion = proportion, tactic = tactic)
  global_benefits <- sum(conserved * matrix)
  
  return(global_benefits)
}










