set.seed(seed)

subjects = 500
rounds = 5


# Define individual-level utilities
utilities <- data.frame(id = 1:subjects,
                        c1 = rbinom(subjects, 1, 0.5),
                        c2 = runif(subjects, -1,1)) %>% 
  mutate(X1_1 = ifelse(c1 == 1, 
                       rnorm(subjects, 1, sd = 1),
                       rnorm(subjects, -1, sd = 1)),
         X2_1 = rnorm(subjects, abs(c2-0.2), sd = 1),
         X3_1 = rnorm(subjects, 0,sd = 0.5))

# Use utilities to determine hypothetical conjoint behaviour
conjoint_data <- data.frame(id = rep(1:subjects, each = rounds*2),
                            round = rep(1:rounds, each = 2),
                            profile = 1:2,
                            X1 = rbinom(subjects*rounds*2, 1, 0.5),
                            X2 = rbinom(subjects*rounds*2, 1, 0.5),
                            X3 = rbinom(subjects*rounds*2, 1, 0.5)) %>% 
  left_join(utilities, by = "id") %>% 
  mutate(U = X1*X1_1 + X2*X2_1 + X3*X3_1 + rnorm(subjects*rounds*2, 0,0.0005)) %>% 
  group_by(id, round) %>%
  mutate(Y = ifelse(U == max(U),1,0)) %>% 
  ungroup()

# Mutate data for cleaner presentation
train_data <- conjoint_data %>% select(id, X1, X2, X3, c1, c2, Y) %>% 
  mutate(X1 = ifelse(X1 == 1, "A1: Binary heterogeneity (c1)","a"),
         X2 = ifelse(X2 == 1, "A2: Interval heterogeneity (c2)","c"),
         X3 = ifelse(X3 == 1, "A3: Random heterogeneity","e"))





set.seed(89)

# Function for simulating subject preferences and resulting conjoint data
preference_sim <- function(subjects = 500, rounds = 5, seed = 89) {
  
  set.seed(seed)
  
  # Define individual-level utilities
  utilities <- data.frame(id = 1:subjects,
                          c1 = rbinom(subjects, 1, 0.5),
                          c2 = runif(subjects, -1,1)) %>% 
    mutate(X1_1 = ifelse(c1 == 1, 
                         rnorm(subjects, 1, sd = 1),
                         rnorm(subjects, -1, sd = 1)),
           X2_1 = rnorm(subjects, abs(c2-0.2), sd = 1),
           X3_1 = rnorm(subjects, 0,sd = 0.5))
  
  # Use utilities to determine hypothetical conjoint behaviour
  conjoint_data <- data.frame(id = rep(1:subjects, each = rounds*2),
                              round = rep(1:rounds, each = 2),
                              profile = 1:2,
                              X1 = rbinom(subjects*rounds*2, 1, 0.5),
                              X2 = rbinom(subjects*rounds*2, 1, 0.5),
                              X3 = rbinom(subjects*rounds*2, 1, 0.5)) %>% 
    left_join(utilities, by = "id") %>% 
    mutate(U = X1*X1_1 + X2*X2_1 + X3*X3_1 + rnorm(subjects*rounds*2, 0,0.0005)) %>% 
    group_by(id, round) %>%
    mutate(Y = ifelse(U == max(U),1,0)) %>% 
    ungroup()
  
  # Mutate data for cleaner presentation
  train_data <- conjoint_data %>% select(id, X1, X2, X3, c1, c2, Y) %>% 
    mutate(X1 = ifelse(X1 == 1, "A1: Binary heterogeneity (c1)","a"),
           X2 = ifelse(X2 == 1, "A2: Interval heterogeneity (c2)","c"),
           X3 = ifelse(X3 == 1, "A3: Random heterogeneity","e"))
  
  # Run heterogeneity model
  pref_mod <- cjbart(train_data, Y = "Y", id = "id")
  
  # Generate IMCES
  het_detect <- IMCE(train_data, pref_mod, 
                     attribs = c("X1","X2","X3"), 
                     ref_levels = c("a","c","e"),
                     cores = 8)
  
  return(het_detect)
  
}

# Run simulation example
example_sim <- preference_sim(seed = 89)
example_sim <- preference_sim(seed = 420000)
example_sim <- preference_sim(seed = -13)
## Detected forced-choice outcome
# Calculating IMCEs
# Generate plot
plot(example_sim) +
  aes(color = as.factor(c1)) +
  facet_wrap(~level, ncol = 2) +
  scale_color_manual(values = c("dodgerblue2","firebrick2")) +
  labs(x = "", color = expression(c[1])) +
  guides(color = guide_legend(title.position="top", title.hjust = 0.5)) +
  theme(legend.position = c(0.75,0.25),
        legend.direction = "horizontal",
        legend.background = element_rect(color = "darkgray"))
