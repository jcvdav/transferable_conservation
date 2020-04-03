
# Define function that finds the top n in each vector
which_max_n <- function(vector, proportion) {
  n <- proportion * length(vector)
  vector %in% head(sort(vector, decreasing = T), n)
}

# Define wrapper that conserves a given proportion
conserve <- function(matrix, proportion) {
  
  # Call the function to match the max n in each column
  conserved <- apply(X = benefit, MARGIN = 2, FUN = which_max_n, proportion)
  
  # Return the results
  return(conserved)
}

global_benefits <- function(matrix, proportion){
  conserved <- conserve(matrix = benefit, proportion = proportion)
  global_benefits <- sum(conserved * benefit)
  
  return(global_benefits)
}










