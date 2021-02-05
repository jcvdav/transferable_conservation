# List of functions to convert area targets to
# total benefit, and target benefit to area targets

# Calculate area as a function of benefit
area <- function(data, ben) {
  max(data$pct[data$tb <= ben], na.rm = T)
}

# Calculate benefit as a function of area
benefit <- function(data, area) {
  sum(data$benefit[data$pct <= area], na.rm = T)
}

# Assign order of protection for each country
get_country_pathways <- function(data) {
  data %>% 
    mutate(tb = cumsum(benefit),
           pct = (1:nrow(.)) / nrow(.)) %>% 
    select(-type)
}

# Get what the trading price is, for a given desired amount of conservation
get_trading_price <- function(conservation_target, supply_curve) {
  max(supply_curve$mc[supply_curve$tb <= conservation_target], na.rm = T)
}

# Plot conservation supply curve
gg_sc <- function(data, color, h, v) {
  
  
  if(is.null(color)) {
    # Create the plot
    p <- ggplot(data = data,
                mapping = aes(x = tb, y = mc)) +
      geom_line(size = 1) +
      ggtheme_plot() +
      labs(x = "Conservation",
           y = "Marginal costs")
  } else {
    p <- ggplot(data = data,
                mapping = aes(x = tb, y = mc, color = iso3)) +
      geom_line(size = 1) +
      ggtheme_plot() +
      labs(x = "Conservation",
           y = "Marginal costs")
  }
  
  # Add horizontal line for marginal trading price
  if(!is.null(h = )) {
    p <- p +
      geom_hline(yintercept = h, linetype = "dashed")
  }
  
  # Add vertical line for target amount of conservation
  if(!is.null(v)) {
    p <- p +
      geom_vline(xintercept = v, linetype = "dashed")
  }
  
  return(p)
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

######################
my_validate <- function(species_list){
  # browser()
  tmp <- data.frame(input = species_list, stringsAsFactors = FALSE)
  
  results_fishbase <- synonyms(species_list, server = "fishbase") %>%
    select(input = synonym, Status, Species) %>%
    distinct()
  
  results_sealifebase <- synonyms(species_list, server = "sealifebase") %>%
    select(input = synonym, Status, Species) %>%
    distinct()
  
  results <- rbind(results_fishbase, results_sealifebase) %>% 
    drop_na(Species)
  
  accepted <- results %>% 
    filter(Status == "accepted name") %>% 
    select(input, accepted_name = Species)
  
  synonyms <- results %>% 
    filter(Status == "synonym") %>% 
    select(input, synonym = Species)
  
  final <- tmp %>% 
    left_join(accepted, by = "input") %>% 
    left_join(synonyms, by = "input") %>% 
    mutate(Species = ifelse(is.na(accepted_name), synonym, accepted_name),
           Species = ifelse(is.na(Species), input, Species))
  
  return(final$Species)
  
}









