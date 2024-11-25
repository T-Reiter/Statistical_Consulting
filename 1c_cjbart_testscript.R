# Re-Do the simulation of the paper
rm(list = ls())
simdat = function(n = 500, rounds = 5, profiles_per_round = 2){

n = 500
rounds = 5 
profiles_per_round = 2
obs = n*rounds*profiles_per_round

  obs = n*rounds*profiles_per_round
  
  # Get profile attributes
  a1 = sample(c('a','b'), 
              obs, 
              replace = T, 
              prob = c(.5, .5))
  a2 = sample(c('c','d'), 
              obs, 
              replace = T, 
              prob = c(.5, .5))
  a3 = sample(c('e','f'), 
              obs, 
              replace = T, 
              prob = c(.5, .5))
  
  # Get the profile attribute dataframe
  participants_full = rep(1:n, each = rounds*profiles_per_round)
  rounds_full = rep(rep(1:rounds, each = profiles_per_round), n)
  profiles_full = rep(1:profiles_per_round, n*rounds)
  
  # Get subject level covariates
  c1_vec <- rbinom(n, 1, 0.5)          # Binary covariate for each user
  c2_vec <- runif(n, -1, 1)
  
  # Repeat covariates to match the number of observations per user
  c1_full <- rep(c1_vec, each = rounds * profiles_per_round)
  c2_full <- rep(c2_vec, each = rounds * profiles_per_round)
  
  # Combine df
  df = data.frame(participants_full,
                  rounds_full,
                  profiles_full,
                  a1, a2, a3,
                  c1_full, c2_full)
  colnames(df) = c('user_id', 'round_id', 'profile_id', 
                   'a1', 'a2', 'a3',
                   'c1', 'c2')
  
  # Calculate Changes in Utilities
  # df$delta_U1 = mapply(function(c1) 
  #   ifelse(c1 == 1, rnorm(1,1,1), rnorm(1,-1,1)), df$c1)
  # df$delta_U2 = mapply(function(c2) 
  #   rnorm(1, mean = abs(c2 - 0.2), sd = 1), df$c2)
  # df$delta_U3 = rnorm(obs,0,0.5)
  
  
  # Calculate Changes in Utilities per user
  # Generate delta_U1, delta_U2, delta_U3 for each user
  delta_U1_vec <- ifelse(c1_vec == 1, rnorm(n, 1, 1), rnorm(n, -1, 1))
  delta_U2_vec <- rnorm(n, mean = abs(c2_vec - 0.2), sd = 1)
  delta_U3_vec <- rnorm(n, 0, 0.5)
  
  # Repeat delta_U values to match the number of observations per user
  df$delta_U1 <- rep(delta_U1_vec, each = rounds * profiles_per_round)
  df$delta_U2 <- rep(delta_U2_vec, each = rounds * profiles_per_round)
  df$delta_U3 <- rep(delta_U3_vec, each = rounds * profiles_per_round)
  
  # Calculate Utilities
  df$utility_ijk = 
    (df$a1 == 'b')*df$delta_U1 +
    (df$a2 == 'd')*df$delta_U2 + 
    (df$a3 == 'f')*df$delta_U3 + 
    rnorm(n*rounds*profiles_per_round, 0, 0.0005)
  
  # Get the chosen profile per round
  df$y = with(df, ave(utility_ijk, user_id, round_id, # average of data utility grouped by id and round id
                      FUN = function(x) as.integer(x == max(x))))
  # custom fct applied: compare each element to max 
  # if x == max(x) --> x is the max and we get true (1) elso 0
  
  # Get c1 as factor (needed for plotting later on?)
  # df$c1 <- as.factor(df$c1)
  
  return(df)
}








# Fit cjbart
### Fit the extended model
set.seed(89)
# df = simdat(n = 2000, 20, 2)
df = simdat(n = 500,5,2)
vars = c('user_id', 
         # 'round_id', 
         'y', 
         # 'profile_id', 
         'a1', 'a2', 'a3', 'c1', 'c2')
# do we need profile id?
fit = cjbart(data = df[,c(vars)], 
                    Y = "y",
                    # type = "choice",     # forced-choice design
                    id = "user_id",      # respondent id
                    # round = "round_id",  # round of conjoint experiment  
                    # use_round = TRUE,    # include round _id in training (probably needed for RMCE)
                    cores = 4)

# Get IMCEs
het_effects <- IMCE(data = df[,c(vars)], 
                    model = fit, 
                    attribs = c('a1', 'a2', 'a3'),
                    ref_levels = c("a","c","e"),
                    cores = 4)
# warning: number of unique covariate rows does not match number of ids 
# --> duplicates in simulating unique covariates?
nrow(unique(df[,c('c1', 'c2')]))
length(unique(df$user_id))

# Plot the effects
# orders IMCEs by Size (low to high)
# Colors the IMCEs with the color of the respective covariate
het_effects$imce$c1 = as.factor(het_effects$imce$c1)
plot(het_effects) +
  aes(color = as.factor(c1)) +
  facet_wrap(~level, ncol = 2) +
  scale_color_manual(values = c("dodgerblue2","firebrick2")) +
  labs(x = "", color = expression(c[1])) +
  guides(color = guide_legend(title.position="top", title.hjust = 0.5)) +
  theme(legend.position = c(0.75,0.25),
        legend.direction = "horizontal",
        legend.background = element_rect(color = "darkgray"))

cor.test(het_effects$imce$b, as.numeric(het_effects$imce$c1))
cor.test(het_effects$imce$d, as.numeric(het_effects$imce$c1))
cor.test(het_effects$imce$f, as.numeric(het_effects$imce$c1))



plot(het_effects) +
  aes(color = c2) +
  facet_wrap(~level, ncol = 2) +
  scale_color_gradient(low = "dodgerblue2", high = "firebrick2", 
                       guide = guide_colorbar(reverse = TRUE)) +
  labs(x = "", color = expression(c[2])) +
  guides(color = guide_legend(title.position="top", title.hjust = 0.5)) +
  theme(legend.position = c(0.75,0.25),
        legend.direction = "horizontal",
        legend.background = element_rect(color = "darkgray"))

cor.test(het_effects$imce$b, as.numeric(het_effects$imce$c2))
cor.test(het_effects$imce$d, as.numeric(het_effects$imce$c2))
cor.test(het_effects$imce$f, as.numeric(het_effects$imce$c2))












# Run the simulation 100 times (as in paper) and compare results 
cor_results <- data.frame(
  iteration = integer(),
  attrib = character(),
  covar = character(),
  correlation = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Set seed for reproducibility
set.seed(42)


# Loop for 100 iterations
iter = 10


for (i in 1:iter) {
  df <- simdat(n = 500, rounds = 5, profiles_per_round = 2)
  
  # Variables for analysis
  vars <- c('user_id', 'round_id', 
            'y', 
            'a1', 'a2', 'a3', 'c1', 'c2')
  
  # Fit the model using cjbart
  fit <- cjbart(data = df[, vars], 
                Y = "y",
                type = "choice",    
                id = "user_id",     
                round = "round_id",  
                use_round = TRUE,    
                cores = 4)
  
  # Get IMCEs
  het_effects <- IMCE(data = df[, vars], 
                      model = fit, 
                      attribs = c('a1', 'a2', 'a3'),
                      ref_levels = c("a", "c", "e"),
                      cores = 4)
  
  # Perform correlation analysis for each attribute and covariate
  attributes <- c("b", "d", "f")
  covars <- c("c1", "c2")
  
  for (attrib in attributes) {
    for (covar in covars) {
      # Perform correlation
      cor_test <- cor.test(het_effects$imce[[attrib]], 
                           as.numeric(het_effects$imce[[covar]]))
      
      # Store results
      cor_results <- rbind(cor_results, data.frame(
        iteration = i,
        attrib = attrib,
        covar = covar,
        correlation = cor_test$estimate,
        p_value = cor_test$p.value,
        stringsAsFactors = FALSE
      ))
    }
  }
}

# get the summaries 
# Calculate mean and standard deviation for correlations
cor_results %>%
  group_by(covar, attrib) %>%
  summarise(
    mean_correlation = mean(correlation, na.rm = TRUE),
    sd_correlation = sd(correlation, na.rm = TRUE),
    mean_p_value = mean(p_value, na.rm = TRUE),
    .groups = "drop"  # Avoids grouping in the output
  )

