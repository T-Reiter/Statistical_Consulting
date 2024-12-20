################################################################################
####### CJBART 
################################################################################

### Load Packages
library(cjbart)
library(haven)
library(tidytext)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot) # standard built-in plot function used in the paper
library(dplyr)
library(tidyr)
library(ggtext)
library(parallel)


### Set WD
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))

### Load Data 
cj_tidy <- readRDS("0_Ukraine_Conjoint_Data/cj_tidy.rds")

# get additional covariate data 
add_covars <- 
  readRDS("0_Ukraine_Conjoint_Data/CJ_Ukraine_NewCovariates.Rds")
# get additional covariate data --> same data different files types?
add_covars2 <- 
  haven::read_dta("0_Ukraine_Conjoint_Data/CJ_Ukraine_NewCovariates.dta")
rm(add_covars2)


### Join the add_covars with the cj_tidy data 
# for this, create c_id (country_id) variable in add_covars
add_covars$country_id <- paste0(add_covars$Country, 
                                "_",
                                add_covars$ID)
# Remove the covars in add_covars that are already present in cj_tidy
which(!colnames(add_covars) %in% colnames(cj_tidy))
add_covars_new <- add_covars[,which(!colnames(add_covars) %in% 
                                      colnames(cj_tidy))]

# join the new covariate data
cj_tidy <- merge(cj_tidy,
                 add_covars_new,
                 by.x = "c_id",
                 by.y = "country_id", 
                 all.x = T) # left join



# Rename Attributes
colnames(cj_tidy)[which(colnames(cj_tidy) %in% c(paste0("attr",1:9)))] <-
  c("Sold_killed_UKR",
    "Sold_killed_RUS",
    "Civ_killed_UKR",
    "Infra_Destr_UKR",
    "Perc_GDP_milit",
    "Perc_GDP_econ",
    "Risk_Nuke",
    "Territ_Cession",
    "Polit_Self_Det_UKR")



#####' *Select variables of interest for the model*
vars_ext = c(
  # General variables
  'y', 'c_id', 
  'task',
  # Attributes
  "Sold_killed_UKR", "Sold_killed_RUS", "Civ_killed_UKR",
  "Infra_Destr_UKR", "Perc_GDP_milit", "Perc_GDP_econ",
  "Risk_Nuke", "Territ_Cession", "Polit_Self_Det_UKR", 
  # Covariates
  'Country', 'age', 'gender', 'leftright3'
  ,"QV_Ukraine", "q3_3", "Q5", "q1617_USA",
  "q1617_RUS", "Q11", "Q15_1", "Q15_7",
  "q10_2", "q10_6", "q10_8", "q10_10"
)


##### Loop Setup
# Detect number of cores for parallelization
no_of_cores = parallel::detectCores()
no_iterations = 11:20 # be careful as the fit objects has ~1GB 

# Set Seeds for the iterations
# seeds = round(runif(20,1,1000),0) # used to determine random seeds
seeds = c(79, 27, 340, 508, 43,
          157, 886, 918, 70, 972,
          373, 960, 999, 826, 984,
          518, 136, 692, 187, 903)



#####' Run the loop

for (iter in no_iterations){
  
  ### Fit cjbart model
  fit_cjbart = cjbart(data = cj_tidy[, vars_ext], 
                      Y = "y",
                      # type = "choice", 
                      id = "c_id", 
                      round = "task", 
                      use_round = TRUE, 
                      seed = seeds[iter],
                      cores = no_of_cores)
  # save model 
  model_name = paste0("1c_robustness_objects/models/fit_cjbart_s",
                      seeds[iter],'.rds')
  saveRDS(fit_cjbart, model_name)
  
  print(paste('# Finished model fit', iter, 'of', length(no_iterations)))
  
  
  ### Get AMCEs
  amces <- AMCE(cj_tidy[, vars_ext], 
                model = fit_cjbart,
                attribs = c("Sold_killed_UKR"
                            , "Sold_killed_RUS"
                            , "Civ_killed_UKR"
                            , "Infra_Destr_UKR"
                            , "Perc_GDP_milit"
                            , "Perc_GDP_econ"
                            , "Risk_Nuke"
                            , "Territ_Cession"
                            , "Polit_Self_Det_UKR"
                ), 
                ref_levels = c('12,500'
                               , '25,000.'
                               , '4,000'
                               , '50B'
                               , '0.1% of GDP'
                               , '0.1% of GDP.'
                               , 'Not present (0%)'
                               , 'None'
                               , 'Full'
                ), 
                alpha = 0.05,
                cores = no_of_cores)
  # save amces 
  amce_name = paste0("1c_robustness_objects/amces/amces_s",seeds[iter],'.rds')
  saveRDS(amces, amce_name)
  
  print(paste('# Finished amces', iter, 'of', length(no_iterations)))
  
  
  ### Get IMCEs
  imces <- IMCE(data = cj_tidy[, vars_ext], 
                keep_omce = TRUE,
                model = fit_cjbart, 
                attribs = c("Sold_killed_UKR"
                            , "Sold_killed_RUS"
                            , "Civ_killed_UKR"
                            , "Infra_Destr_UKR"
                            , "Perc_GDP_milit"
                            , "Perc_GDP_econ"
                            , "Risk_Nuke"
                            , "Territ_Cession"
                            , "Polit_Self_Det_UKR"
                ), 
                ref_levels = c('12,500'
                               , '25,000.'
                               , '4,000'
                               , '50B'
                               , '0.1% of GDP'
                               , '0.1% of GDP.'
                               , 'Not present (0%)'
                               , 'None'
                               , 'Full'
                )
                # ,method = 'parametric'
                , cores = no_of_cores)
  
  # save imces 
  imce_name = paste0("1c_robustness_objects/imces/imces_s",seeds[iter],'.rds')
  saveRDS(imces, imce_name)
  
  print(paste('# Finished imces', iter, 'of', length(no_iterations)))
  
  
  print(paste('### Finished Iteration', iter, 'of', length(no_iterations)))
  
}


###' *Plot AMCE Variability across different Modeling Iterations*
### Compare the AMCEs across the different draws 
# Load AMCEs
amce_files = list.files('1c_robustness_objects/amces/')
rm(amce_df)
for (amce_file in amce_files){
  iter = which(amce_files %in% amce_file)
  path = paste0('1c_robustness_objects/amces/', amce_file)
  amce_temp = readRDS(path)
  amce_df_temp = data.frame(amce_temp$amces$attribute,
                            amce_temp$amces$level,
                            amce_temp$amces$AMCE)
  colnames(amce_df_temp) = c('attribute', 'attribute_level', 
                             paste0('iter_', iter))
  
  if (!exists('amce_df')){
    amce_df = amce_df_temp
  } else {
    amce_df = merge(amce_df, 
                    amce_df_temp,
                    by = c('attribute',
                           'attribute_level'))
  }
}


# Define Attribute name mapping
name_mapping <- c(
  "Civ_killed_UKR"      = "Ukrainian civilian casualties",
  "Sold_killed_UKR"     = "Ukrainian military casualties",
  "Sold_killed_RUS"     = "Russian military casualties",
  "Infra_Destr_UKR"     = "Ukrainian infrastructure loss",
  "Perc_GDP_milit"      = "Military Aid (% GDP)",
  "Perc_GDP_econ"       = "Economic Aid (% GDP)",
  "Risk_Nuke"           = "Nuclear Strike Risk",
  "Territ_Cession"      = "Territorial Concessions",
  "Polit_Self_Det_UKR"  = "Sovereignty"
)

# Define order of attributes
desired_order_attributes <- c(
  "Ukrainian military casualties",
  "Russian military casualties",
  "Ukrainian civilian casualties",
  "Ukrainian infrastructure loss",
  "Military Aid (% GDP)",
  "Economic Aid (% GDP)",
  "Nuclear Strike Risk",
  "Territorial Concessions",
  "Sovereignty"
)

# Define global order attribute levels
global_order <- c(
  "50,000","25,000", 
  "100,000", "50,000.", 
  "16,000","8,000", 
  "200B","100B", 
  "0.3% of GDP","0.2% of GDP", 
  "0.3% of GDP.","0.2% of GDP.", 
  "Moderate (10%)","Low (5%)", 
  "2023 LoC (16%)", "2014 LoC (8%)", "Crimea (4%)",
  "Russian influence", "No EU/NATO"
)

# Reshape the data and set factor orders
amce_long <- amce_df %>%
  pivot_longer(
    cols = starts_with("iter_"),
    names_to = "iteration",
    values_to = "estimate"
  ) %>%
  mutate(
    attribute = recode(attribute, !!!name_mapping),
    attribute = factor(attribute, levels = desired_order_attributes),
    attribute_level = factor(attribute_level, levels = global_order)
  )

# Plot
ggplot(amce_long, aes(x = estimate, y = attribute_level)) +
  geom_boxplot(
    outlier.shape = 'x',
    fill = "lightgray",
    color = "black"
  ) +
  geom_point(
    position = position_jitter(height = 0.2),
    alpha = 0.6,
    size = 2
  ) +
  facet_wrap(~ attribute, scales = "free_y") +
  scale_y_discrete(drop = TRUE) +
  theme_minimal(base_size = 20) +
  theme(
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 22, face = "bold"),
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 20, hjust = 0.5),
    strip.text = element_text(size = 16, face = "bold", hjust = 0),
    strip.background = element_rect(fill = 'gray', colour = 'gray'),
    plot.caption = element_text(size = 16)
  ) +
  labs(x = "AMCE Estimate")

ggsave('Manuscript files/figures/Robustness_AMCEs.png', 
       width = 20, height = 10)


###' *Plot IMCE Variability across 20 Modeling Iterations*
# IMCE variability for single sample users 
# Convert data to long format

# Load AMCEs
imce_files = list.files('1c_robustness_objects/imces/')
rm(imce_df)
for (imce_file in imce_files){
  iter = which(imce_files %in% imce_file)
  path = paste0('1c_robustness_objects/imces/', imce_file)
  imce_temp = readRDS(path)
  imce_df_temp = imce_temp$imce
  
  # Select relevant cols 
  imce_df_temp = unique(imce_df_temp[,1:20]) # unique as we have each IMCE est 
                                             # 4 times (1 per round)
  
  # 
  imce_df_iter = imce_df_temp%>% pivot_longer(
    cols = -c_id,           # All columns except c_id
    names_to = "attribute_level",  # The column to store the original column names
    values_to = paste0("imce_est_iter_", iter)  # The column to store the values
  )
  
  
  if (!exists('imce_df')){
    imce_df = imce_df_iter
  } else {
    imce_df = merge(imce_df, 
                    imce_df_iter,
                    by = c('c_id',
                           'attribute_level'))
  }
}
# --> IMCEs per iteration per person 
# (10011 individuals * 19 levels --> in 20 iterations cols)

# adjust order
global_order <- c(
  "25,000", "50,000",
  "50,000.", "100,000",
  "8,000", "16,000",
  "100B", "200B",
  "0.2% of GDP", "0.3% of GDP",
  "0.2% of GDP.", "0.3% of GDP.",
  "Low (5%)", "Moderate (10%)",
  "Crimea (4%)", "2014 LoC (8%)", "2023 LoC (16%)", 
  "No EU/NATO", "Russian influence"
)


# Include attributes, attribute levels etc. from amce_df 
levels_mappings = unique(amce_long[,c('attribute', 'attribute_level')])
imce_df = merge(levels_mappings,
                imce_df,
                by = 'attribute_level') %>% mutate(
                  attribute_level = factor(attribute_level, levels = rev(global_order)),
                )

random_user <- sample(unique(imce_df$c_id), 1)

# wide to long
user_long <- imce_df %>%
  filter(c_id == random_user) %>%
  pivot_longer(
    cols = starts_with("imce_est_iter_"),
    names_to = "iteration",
    values_to = "estimate"
  )

# plot
ggplot(user_long, aes(x = estimate, y = attribute_level)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitter(height = 0.2), alpha = 0.6) +
  theme_minimal() +
  labs(
    title = paste("IMCE Estimates for User:", random_user),
    x = "IMCE Estimate",
    y = "Attribute Level"
  )



# other ways to plot global variance
# Compute summary stats per user and attribute_level
library(ggridges)
library(data.table) # switch to data.table for better performance
DT <- as.data.table(imce_df)
est_cols <- grep("^imce", names(DT), value = TRUE)

estimate_summaries <- DT[, {
  vals <- unlist(.SD)
  rng <- range(vals)
  .(range_val = rng[2] - rng[1],
    # median_range_val_within = median(vals),
    sd_val = sd(vals))
}, by = .(c_id, attribute, attribute_level), .SDcols = est_cols]

# --> estimate summaries: per individual x IMCE we get a range 
# should have: 10011 * 19 rows (c_id x attribute_level)
# correct!

# Also include min/max, mean, median for the ranges per userXattributelevel
line_data <- estimate_summaries %>%
  group_by(attribute_level) %>%
  summarise(
    mean_range = mean(range_val),
    median_range = median(range_val),
  )

line_data$attribute_level <- factor(line_data$attribute_level)
estimate_summaries$attribute_level <- factor(estimate_summaries$attribute_level, 
                                             levels = levels(line_data$attribute_level))

line_data$attr_pos <- as.numeric(line_data$attribute_level)


# Merge mean AMCEs across iterations into the plot 
DT <- as.data.table(amce_df)
est_cols <- grep("^iter", names(DT), value = TRUE)

mean_amces <- DT[, {
  vals <- unlist(.SD)
  mean_amce <- mean(vals)
}, by = .(attribute_level), .SDcols = est_cols]
colnames(mean_amces) = c('attribute_level', 'mean_amce')


# Ensure the attribute_level factors match between datasets
estimate_summaries$attribute_level <- factor(estimate_summaries$attribute_level)
mean_amces$attribute_level <- factor(mean_amces$attribute_level, 
                                     levels = levels(estimate_summaries$attribute_level))

# Find a max range and select a position to place AMCE text a bit to the right
max_range_val <-10
text_x_pos <- max_range_val * 1.1

library(ggforce)

# Re-factor attribute_level within each attribute so only relevant levels remain
estimate_summaries_faceted <- estimate_summaries %>%
  group_by(attribute) %>%
  mutate(attribute_level = factor(attribute_level, levels = 
                                    unique(attribute_level))) %>%
  ungroup()

# Plot
ggplot(estimate_summaries[,c('attribute', 'attribute_level', 'range_val')], 
       aes(x = range_val*100, y = attribute_level)) +
  geom_density_ridges(scale = 1, rel_min_height = 0.01, alpha = 0.8) +
  
  # Median line red
  geom_segment(data = line_data,
               aes(x = 100*median_range, xend = 100*median_range,
                   y = attr_pos - 0.1, yend = attr_pos + 0.1),
               color = "red", size = 0.5) +
  
  # Add AMCE labels on the right
  geom_text(
    data = mean_amces,
    aes(x = text_x_pos, y = attribute_level, 
        label = paste("AMCE:", round(100*mean_amce, 1), '%')),
    inherit.aes = FALSE,  # Don't use the original aes mapping from the ggplot call
    hjust = 0,            # Align text to the left of the specified x position
    size = 5
  ) +
  
  coord_cartesian(clip = "off", xlim = c(0,12)) + # allow text to appear beyond 
                                                  # the plot region if necessary
  theme_minimal() +
  theme(
    plot.margin = margin(5, 40, 5, 5),  # Increase right margin if needed
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(face='bold', size = 14)
  ) +
  labs(
    title = "",
    x = "Range of within-person IMCE Estimates (in Percentage Points)",
    y = "Attribute Level") 


ggsave('Manuscript files/figures/Robustness_IMCEs.png', 
       width = 20, height = 10)

