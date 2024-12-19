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
library(forcats)


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


#####' *Select variables of interest*
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



#####' *Fit the model*
# function has default seed 99 implemented 
# set.seed(123)
fit_cjbart_ext = cjbart(data = cj_tidy[, vars_ext], 
                        Y = "y",
                        # type = "choice", 
                        id = "c_id", 
                        round = "task", 
                        use_round = TRUE, 
                        seed = 123,
                        cores = 4)

# sNumber provides the seed --> determines randomness in the BART tree growing 
# saveRDS(fit_cjbart_ext, "1c_Model_Objects/2024-11-18_fit_cjbart_ext.rds")
fit_cjbart_ext <- readRDS("1c_Model_Objects/2024-11-18_fit_cjbart_ext.rds")


#' *Fit model with larger (non-default) Burn-In/Draws for*
#' *extended converegence assessment*
fit_cjbart_ext_large = cjbart(data = cj_tidy[, vars_ext], 
                             Y = "y",
                             # type = "choice", 
                             id = "c_id", 
                             round = "task", 
                             use_round = TRUE, 
                             seed = 99,
                             nskip = 2000, # Burn-In instead of 100 (default)
                             ndpost = 5000, # Posterior Draws instead of 1000
                             cores = 4)
# saveRDS(fit_cjbart_ext_large, "1c_Model_Objects/2024-12-17_fit_cjbart_ext_large.rds")
fit_cjbart_ext_large <- readRDS("1c_Model_Objects/2024-12-17_fit_cjbart_ext_large.rds")


#' *Fit model with the custom as-if-non-unix cjbart function*
# Function always uses the if UNIX == FALSE code 
# --> No parallelization & dependence of the external seed that is set 
source('00_Custom_Source/cjbart_custom.R')
set.seed(42)
cfit_cjbart_es42_is123_v1 = cjbart_custom(data = cj_tidy[, vars_ext], 
                             Y = "y",
                             # type = "choice", 
                             id = "c_id", 
                             round = "task", 
                             use_round = TRUE, 
                             seed = 123, # returns warning --> no seed needed
                             cores = 4)
# saveRDS(cfit_cjbart_es42_is123_v1,
#         '1c_Model_Objects/2024-11-18_cfit_cjbart_es42_is123_v1.rds')
# cfit_cjbart_es42_is123_v1 = 
#   readRDS('1c_Model_Objects/2024-11-18_cfit_cjbart_es42_is123_v1.rds')



#####' *Get IMCEs*
### Get IMCEs for the cjbart model
imces_ext <- IMCE(data = cj_tidy[, vars_ext], 
                  keep_omce = TRUE,
                  model = fit_cjbart_ext, 
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
                  ,method = 'rubin'
                  , cores = 2
)
summary.cjbart(imces_ext)

# saveRDS(imces_ext, "1c_Model_Objects/2024-11-18_imces_ext_2.rds")
imces_ext <- readRDS("1c_Model_Objects/2024-11-18_imces_ext.rds")




#####' *Get AMCEs*
amces_ext <- AMCE(cj_tidy[, vars_ext], 
              model = fit_cjbart_ext,
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
              cores = 4)

# saveRDS(amces_ext, "1c_Model_Objects/2024-11-18_amces_ext.rds")
amces_ext <- readRDS("1c_Model_Objects/2024-11-18_amces_ext.rds")


###' *Plot AMCEs* 
# Extract data frames from the results
amces_ext <- readRDS("1c_Model_Objects/2024-11-18_amces_ext.rds")

plot_data <- data.frame(amces_ext$amces$attribute,
                        amces_ext$amces$level,
                        amces_ext$amces$AMCE,
                        amces_ext$amces$AMCE_lower,
                        amces_ext$amces$AMCE_upper)
colnames(plot_data) <- c('Attribute', 'Level', 'AMCE', 'Lower', 'Upper')

# Define reference levels (--> IMCE = 0)
baseline_levels <- data.frame(
  Attribute = c("Sold_killed_UKR", "Sold_killed_RUS", "Civ_killed_UKR",
                "Infra_Destr_UKR", "Perc_GDP_milit", "Perc_GDP_econ",
                "Risk_Nuke", "Territ_Cession", "Polit_Self_Det_UKR"),
  Level = c('12,500', '25,000.', '4,000', '50B', '0.1% of GDP', '0.1% of GDP.',
            'Not present (0%)', 'None', 'Full'),
  AMCE = 0,
  Lower = 0,
  Upper = 0
)

# Add baseline levels to the plot_data
plot_data <- plot_data %>%
  bind_rows(baseline_levels)


# Create the desired order for plotting
desired_orders_df <- data.frame(
  Attribute = c(rep("Sold_killed_UKR", 3),
                rep("Sold_killed_RUS", 3),
                rep("Civ_killed_UKR", 3),
                rep("Infra_Destr_UKR", 3),
                rep("Perc_GDP_milit", 3),
                rep("Perc_GDP_econ", 3),
                rep("Risk_Nuke", 3),
                rep("Territ_Cession", 4),
                rep("Polit_Self_Det_UKR", 3)),
  Level = c('12,500', "25,000", "50,000", 
            '25,000.', "50,000.", "100,000", 
            '4,000', "8,000", "16,000", 
            '50B', "100B", "200B",
            '0.1% of GDP', "0.2% of GDP", "0.3% of GDP", 
            '0.1% of GDP.', "0.2% of GDP.", "0.3% of GDP.", 
            'Not present (0%)', "Low (5%)", "Moderate (10%)", 
            'None', "Crimea (4%)", "2014 LoC (8%)", "2023 LoC (16%)",
            'Full', "No EU/NATO", "Russian influence"),
  global_order = 1:28,
  attribute_order = c(rep(1,3), rep(2,3), rep(3,3), rep(4,3), rep(5,3), 
                      rep(6,3), rep(7,3), rep(8,4), rep(9,3)),
  level_order = c(1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:4, 1:3)
)

# Merge desired order into plot_data
plot_data <- plot_data %>%
  left_join(desired_orders_df, by = c("Attribute", "Level"))

# order the attributes accordingly
plot_data$Attribute = factor(plot_data$Attribute, 
                             levels = unique(plot_data$Attribute[order(
                               plot_data$attribute_order)]))

# Change Labels 
custom_labels <- c(
  "Sold_killed_UKR" = "Ukrainian military casualties",
  "Sold_killed_RUS" = "Russian military casualties",
  "Civ_killed_UKR" = "Ukrainian civilian casualties",
  "Infra_Destr_UKR" = "Ukrainian infrastructure loss",
  "Perc_GDP_milit" = "Military Aid (% GDP)",
  "Perc_GDP_econ" = "Economic Aid (% GDP)",
  "Risk_Nuke" = "Nuclear Strike Risk",
  "Territ_Cession" = "Territorial Concessions",
  "Polit_Self_Det_UKR" = "Sovereignity"
)

plot_data$Level <- factor(plot_data$Level, 
                          levels = rev(plot_data$Level[order(
                            plot_data$global_order)]))


# Plot with baselines included
ggplot(plot_data, aes(x = AMCE, 
                      y = Level,
                      color = Attribute)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), 
                 height = 0.5, 
                 linewidth = 0.85) +
  labs(
    x = "AMCE Estimate",
    y = "",
    title = "") +
  theme_minimal() +
  guides(color = "none") +
  theme(
    axis.text.y = element_text(size = 14.5),
    axis.text.x = element_text(size = 15),
    # axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 15),
    # plot.title = element_text(size = 14, face = "bold"),
    strip.placement = "outside",
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold", 
                              margin = margin(t = 1, b = 1),
                              size = rel(1.2), 
                              hjust = 0),
    panel.background = element_rect(fill = "white")) +
  ggforce::facet_col(facets = "Attribute", 
                     scales = "free_y", 
                     space = "free", 
                     strip.position = c('top'),
                     labeller = labeller(Attribute = custom_labels))

ggsave('Manuscript files/figures/AMCEs_as_cjbart.png', width = 12, height = 8)







#####' *Get Variable Importances*
covars_of_interest_ext <- c("Country", "age", "gender", "leftright3"
                            # additional covariates
                            ,"QV_Ukraine", "q3_3", "Q5", "q1617_USA",
                            "q1617_RUS", "Q11", "Q15_1", "Q15_7",
                            "q10_2", "q10_6", "q10_8", "q10_10")

var_imps_ext <- het_vimp(imces = imces_ext, 
                         covars = covars_of_interest_ext, 
                         cores = 4)

# saveRDS(var_imps_ext, "1c_Model_Objects/2024-11-18_var_imps_ext.rds")
var_imps_ext <- readRDS("1c_Model_Objects/2024-11-18_var_imps_ext.rds")





#####' *Plot Variable Importances*
var_imps_ext <- readRDS("1c_Model_Objects/2024-11-18_var_imps_ext.rds")

### Plot with off-the-shelf function included in package (as in Paper)
# plot(var_imps_ext) # not satisfactory 

#### Custom plot (prettier)
plot_data <- var_imps_ext$results 

# Generate Custom Plot Attribute Labels with Line Breaks
custom_labels <- c("Sold_killed_UKR" = "Sold\nkilled\nUKR", 
                   "Sold_killed_RUS" = "Sold\nkilled\nRUS",
                   "Civ_killed_UKR" = "Civ\nkilled\nUKR", 
                   "Infra_Destr_UKR" = "Infra\nDestr\nUKR", 
                   "Perc_GDP_milit" = "Perc\nGDP\nmilit", 
                   "Perc_GDP_econ" = "Perc\nGDP\necon", 
                   "Risk_Nuke" = "Risk\nNuke", 
                   "Territ_Cession" = "Territ\nCession", 
                   "Polit_Self_Det_UKR" = "Polit\nSD\nUKR")

# Rename Covariates with proper Capitalization 
plot_data[which(plot_data$covar == 'age'), 'covar'] <- "Age"
plot_data[which(plot_data$covar == 'gender'), 'covar'] <- "Gender"
plot_data[which(plot_data$covar == 'leftright3'), 'covar'] <- "Left-Right-3"

# Rename Questionnaire Covariates
plot_data[which(plot_data$covar == 'Q5'), 'covar'] <- "Weapons matter\nfor election 4"  
plot_data[which(plot_data$covar == 'q10_2'), 'covar'] <- "Anti Weapon\ndeliveries 7"
plot_data[which(plot_data$covar == 'q10_6'), 'covar'] <- "General Weapon\nfor defense 7"
plot_data[which(plot_data$covar == 'q10_8'), 'covar'] <- "Weapons if\nciv. casualties  7"
plot_data[which(plot_data$covar == 'q10_10'), 'covar'] <- "Weapons if\n infra. destr. 7"
plot_data[which(plot_data$covar == 'Q11'), 'covar'] <- "Anti Nato 5"  
plot_data[which(plot_data$covar == 'q1617_RUS'), 'covar'] <- "Empathy RUS 7"  
plot_data[which(plot_data$covar == 'q1617_USA'), 'covar'] <- "Anti USA 7"  
plot_data[which(plot_data$covar == 'QV_Ukraine'), 'covar'] <- "Political Agenda:\nWar Ukraine 7"  
plot_data[which(plot_data$covar == 'q3_3'), 'covar'] <- "Scared\nby War 7"
plot_data[which(plot_data$covar == 'Q15_1'), 'covar'] <- "for Peace 01:\nUKR NATO Waiver"
plot_data[which(plot_data$covar == 'Q15_7'), 'covar'] <- "for Peace 01:\nNo Concessions"

# Set desired Attribute order for plotting
desired_orders_df <- data.frame(
  Attribute = c(rep("Sold_killed_UKR", 2),
                rep("Sold_killed_RUS", 2),
                rep("Civ_killed_UKR", 2),
                rep("Infra_Destr_UKR", 2),
                rep("Perc_GDP_milit", 2),
                rep("Perc_GDP_econ", 2),
                rep("Risk_Nuke", 2),
                rep("Territ_Cession", 3),
                rep("Polit_Self_Det_UKR", 2)
                ),
  Level = c("50,000", "25,000.",
            "100,000", "50,000.",
            "16,000", "8,000",
            "200B", "100B",
            "0.3% of GDP", "0.2% of GDP",
            "0.3% of GDP.", "0.2% of GDP.",
            "Moderate (10%)", "Low (5%)",
            "2023 LoC (16%)", "2014 LoC (8%)", "Crimea (4%)",
            "Russian influence", "NO EU/Nato"
            ),
  order_attributes = c(1,1,
                       2,2,
                       3,3,
                       4,4,
                       5,5,
                       6,6,
                       7,7,
                       8,8,8,
                       9,9),
  order_levels = c(1,2,
                   1,2,
                   1,2,
                   1,2,
                   1,2,
                   1,2,
                   1,2,
                   1,2,3,
                   1,2)
)

# Set desired covariate order for plotting
desired_covar_order <- c("Age", "Gender", "Country", "Left-Right-3", 
                         "Weapons matter\nfor election 4", 
                         "Anti Weapon\ndeliveries 7",
                         "General Weapon\nfor defense 7",
                         "Weapons if\nciv. casualties  7",
                         "Weapons if\n infra. destr. 7",
                         "Anti Nato 5",
                         "Empathy RUS 7",
                         "Anti USA 7",
                         "Political Agenda:\nWar Ukraine 7",
                         "Scared\nby War 7",
                         "for Peace 01:\nUKR NATO Waiver",
                         "for Peace 01:\nNo Concessions")


plot_data <- merge(plot_data, 
                   desired_orders_df, 
                   by = c("Attribute", "Level"), 
                   all.x = TRUE)

# Reorder levels within each Attribute
plot_data <- plot_data %>%
  mutate(
    Level_ordered = factor(Level, 
                           levels = unique(Level[order(order_levels)])),
    Attribute = factor(Attribute, 
                       levels = unique(Attribute[order(order_attributes)])), 
    covar = factor(covar, 
                   levels = desired_covar_order)
  )


# Plot 
ggplot(plot_data,
       aes(x = covar,
           y = Level_ordered,
           fill = importance)) +
  facet_grid(Attribute ~ ., 
             space = "free", 
             scales = "free",
             switch = "y",
             labeller = labeller(Attribute = custom_labels)) +
  geom_tile() +
  scale_y_reordered() +
  scale_fill_gradient(low="white", high="firebrick1") +
  labs(x = "Covariates", 
       y = "Attribute Level", 
       fill = "Importance") +
  theme(axis.text.x = element_text(size = 18, 
                                   # face = 'bold',
                                   angle=40, 
                                   vjust = 1, hjust = 1,
                                   lineheight = 0.75),
        axis.text.y = element_text(size = 18, 
                                   # face = 'bold',
                                   vjust = 0.5, hjust = 1),
        axis.title = element_text(size = 18, face = 'bold'),
        strip.placement = "outside",
        strip.background = element_rect(fill = "grey80", 
                                        color = "grey50"),
        strip.text = element_text(color = "black", size = 17),
        panel.background = element_rect(fill = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15)
        )

ggsave('Manuscript files/figures/RF_VarImps_ext.png')






#####' *Fit and plot single decision trees for interpretation*
imces_ext <- readRDS("1c_Model_Objects/2024-11-18_imces_ext.rds")

# custom node function to prefent scientific notation in rpart.plot()
node_fun <- function(x, labs, digits, varlen) {
  # Calculate the proportion of units in the node
  total_units <- fit$frame$n[1]  # Total number of units (= n of root node)
  proportions <- (x$frame$n / total_units) * 100  # Convert to percentage
  # Format the node label with value and proportion
  sprintf("%.4f\n%.0f%%", x$frame$yval, proportions)  # 4 decimals for value, 1 decimal for percentage
}

# custom function that can be used for generating plot titles 
find_attr = function(attribute_level = NULL){
  if (is.null(attribute_level)) {
    print('No attribute level provided.')
  }
  
  attribute_level = as.character(attribute_level)
  attribute = paste0(na.omit(names(cj_tidy)[sapply(cj_tidy, function(col) any(col == attribute_level))]))
  return(attribute)
}

# Rename subject-level covars for ease of interpretation
rename_map <- c(
  "age" = "Age",
  "gender" = "Gender", 
  "Country" = "Country",
  "leftright3" = "Left-Right-3",
  "Q5" = "Weapons matter\nfor election 4",
  "q10_2" = "Anti Weapon\ndeliveries 7",
  "q10_6" = "General Weapon\nfor defense 7",
  "q10_8" = "Weapons if\nciv. casualties  7",
  "q10_10" = "Weapons if infra. destr. 7",
  "Q11" = "Anti Nato 5",
  "q1617_RUS" = "Empathy RUS 7",
  "q1617_USA" = "Anti USA 7",
  "QV_Ukraine" = "Political Agenda:\nWar Ukraine 7",
  "q3_3" = "Scared\nby War 7",
  "Q15_1" = "for Peace 01: UKR NATO Waiver",
  "Q15_7" = "for Peace 01: No Concessions"
)

colnames(imces_ext$imce) <- ifelse(
  colnames(imces_ext$imce) %in% names(rename_map),
  rename_map[colnames(imces_ext$imce)],
  colnames(imces_ext$imce)
)


###' *Single IMCE Prediction Decision Trees for Associations of Interest*
###' 
# IMCE Trees used in Final Presentation / Report 
#' *Civilians_killed_Ukraine (16k vs. 4k)*
fit <- rpart::rpart(formula = `16,000` ~ Country + Age + Gender + 
                      `Left-Right-3` + `for Peace 01: No Concessions`,
                    imces_ext$imce
                    # complexity param, default = 0.01, larger --> less complex 
                    , control = rpart.control(cp = 0.02)  
) 
printcp(fit) 
png("Manuscript files/figures/DecTree_ext_Civ_killed_Ukr_16k.png", width = 800, height = 600)
rpart.plot(fit, node.fun = node_fun, cex = 1.55, tweak = 2, 
           # , main = paste('Target: IMCEs of', find_attr(fit$terms[[2]]), '=',  fit$terms[[2]])
           )
dev.off()


#' *Russian Soldiers Killed (100k vs. 25k)*
# same as obove with extended 
fit <- rpart::rpart(formula = `100,000` ~ Country + Age + Gender + `Left-Right-3` + `for Peace 01: No Concessions`,
                    imces_ext$imce
                    , control = rpart.control(cp = 0.02)  # complexity param, default = 0.01, larger --> less complex 
) 
png("Manuscript files/figures/DecTree_ext_RUS_Sold_killed_100k.png", width = 900, height = 600)
rpart.plot(fit, node.fun = node_fun, cex = 1.1, tweak = 2.65, 
           # , main = paste('Target: IMCEs of', find_attr(fit$terms[[2]]), '=',  fit$terms[[2]])
           )
dev.off()


#' *Risk_Nuke (Moderate (10%) vs. none(0%))*
fit <- rpart::rpart(formula = `Moderate (10%)` ~ Country + Age + Gender + `Left-Right-3` + `for Peace 01: No Concessions`,
                    imces_ext$imce
                    , control = rpart.control(cp = 0.02)  # complexity param, default = 0.01, larger --> less complex 
) 
png("Manuscript files/figures/DecTree_ext_Risk_Nuke_Moderate.png", width = 800, height = 600)
rpart.plot(fit, node.fun = node_fun, cex = 1.1, tweak = 2.8, 
           # , main = paste('Target: IMCEs of', find_attr(fit$terms[[2]]), '=',  fit$terms[[2]])
           )
dev.off()


#' *Territorial_Cession (2014 LoC (8%) vs. None (0%))*
fit <- rpart::rpart(formula = `2014 LoC (8%)` ~ Country + Age + Gender + `Left-Right-3` + `for Peace 01: No Concessions`,
                    imces_ext$imce
                    , control = rpart.control(cp = 0.02)  # complexity param, default = 0.01, larger --> less complex 
) 
png("Manuscript files/figures/DecTree_ext_Territ_Cession_8perc.png", width = 800, height = 600)
rpart.plot(fit, node.fun = node_fun, cex = 1.1, tweak = 2.8, 
           # , main = paste('Target: IMCEs of', find_attr(fit$terms[[2]]), '=',  fit$terms[[2]])
           )
dev.off()





####' *Additional Trees for exploration - Not in Presentation / Report*
# Sanity Check Infstructure Destroyed 100B
fit <- rpart::rpart(formula = `100B` ~ Country + Age + Gender + `Left-Right-3` + 
                      `for Peace 01: No Concessions` + 
                      `Weapons if infra. destr. 7`,
                    imces_ext$imce
                    , control = rpart.control(cp = 0.02)  # complexity param, default = 0.01, larger --> less complex 
) 
rpart.plot(fit, node.fun = node_fun, cex = 1.1, tweak = 2.8, 
           , main = paste('Target: IMCEs of', find_attr(fit$terms[[2]]), '=', fit$terms[[2]])
)

# Ukrainian Soldiers killed (50k)
fit <- rpart::rpart(formula = `50,000` ~ Country + Age + Gender + 
                      `Left-Right-3` + `for Peace 01: No Concessions` + 
                      `Weapons if infra. destr. 7`,
                    imces_ext$imce
                    , control = rpart.control(cp = 0.04)  # complexity param, default = 0.01, larger --> less complex 
) 
rpart.plot(fit, node.fun = node_fun, cex = 0.8, tweak = 2.8, 
           , main = paste('Target: IMCEs of', find_attr(fit$terms[[2]]), '=',  fit$terms[[2]])
)

# Ukrainian Soldiers killed (25k)
fit <- rpart::rpart(formula = `25,000` ~ Country + Age + Gender + 
                      `Left-Right-3` + `for Peace 01: No Concessions` + 
                      `Weapons if infra. destr. 7`,
                    imces_ext$imce
                    , control = rpart.control(cp = 0.04)  # complexity param, default = 0.01, larger --> less complex 
) 
rpart.plot(fit, node.fun = node_fun, cex = 1.1, tweak = 2.8, 
           , main = paste('Target: IMCEs of', find_attr(fit$terms[[2]]), '=',  fit$terms[[2]])
)



# Infrastructure destroyed 200B
fit <- rpart::rpart(formula = `200B` ~ Country + Age + Gender + `Left-Right-3` +
                      `for Peace 01: No Concessions` + 
                      `Weapons if infra. destr. 7`,
                    imces_ext$imce
                    , control = rpart.control(cp = 0.06)  # complexity param, default = 0.01, larger --> less complex 
) 
rpart.plot(fit, node.fun = node_fun, cex = 0.95, tweak = 3, 
           , main = paste('Target: IMCEs of', find_attr(fit$terms[[2]]), '=',  fit$terms[[2]])
)






###' * Additionally Calculate Marginal Means using the cregg package*
library(cregg)
mm = cregg::cj(data = cj_tidy,
              y ~ Sold_killed_UKR + Sold_killed_RUS + Civ_killed_UKR + 
                Infra_Destr_UKR + Perc_GDP_milit + Perc_GDP_econ + 
                Risk_Nuke + Territ_Cession + Polit_Self_Det_UKR,  
              id = ~ c_id, 
              estimate = "mm",
              level_order = "descending") 

# Plot Marginal Means
ggplot(data = mm, 
       aes(x = fct_inorder(level), y = estimate)) +
  theme_bw() +
  ylim(0.4, 0.6) +
  geom_pointrange(aes(ymin = lower, ymax = upper, color = feature), shape=16) + 
  geom_hline(yintercept = 0.5, color = "grey") + 
  coord_flip() +
  facet_wrap(~feature, ncol = 1L, scales = "free_y", 
             strip.position = "top") +
  labs(y="Marginal Means",x="") + 
  theme(strip.text.y.right = element_text(angle = 0), 
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(face = 'bold', size = 12),
        strip.text = element_text(face = "bold", 
                                  margin = margin(t = 2, b = 2),
                                  size = rel(1), 
                                  hjust = 0.01),
        legend.position = "none")

ggsave('Manuscript files/figures/cregg_MarginalMeans.png')


# Country Subgroup MMs 
mm_by_country <- cregg::cj(
  data = cj_tidy,
  formula = y ~ Sold_killed_UKR + Sold_killed_RUS + Civ_killed_UKR + 
    Infra_Destr_UKR + Perc_GDP_milit + Perc_GDP_econ + 
    Risk_Nuke + Territ_Cession + Polit_Self_Det_UKR,
  id = ~ c_id,
  estimate = "mm",
  level_order = "descending",
  by = ~ Country
)

# Plot marginal means by country
ggplot(data = mm_by_country, 
       aes(x = fct_inorder(level), y = estimate, color = Country)) +
  theme_bw() +
  ylim(0.4, 0.61) +
  geom_pointrange(aes(ymin = lower, ymax = upper), 
                  shape = 16,
                  position = position_dodge(width = 0.5)) + 
  geom_hline(yintercept = 0.5, color = "grey") + 
  coord_flip() +
  facet_wrap(~feature, ncol = 1L, scales = "free_y", 
             strip.position = "top") +
  labs(y="Marginal Means", x="", color="Country") + 
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.x = element_text(face = 'bold', size = 11),
        legend.text = element_text(size = 11),
        strip.text = element_text(face = "bold", 
                                  margin = margin(t = 2, b = 2),
                                  size = rel(1), 
                                  hjust = 0.01))

ggsave('Manuscript files/figures/cregg_MarginalMeans_CountrySubgroups.png')



# cregg::AMCEs
amce <- cregg::cj(data = cj_tidy,
                         y ~ Sold_killed_UKR + Sold_killed_RUS + Civ_killed_UKR + 
                           Infra_Destr_UKR + Perc_GDP_milit + Perc_GDP_econ + 
                           Risk_Nuke + Territ_Cession + Polit_Self_Det_UKR,
                         id = ~ c_id,
                         estimate = "amce",
                         level_order = "descending")


### Plot
ggplot(data = amce,
         aes(x = fct_inorder(level), y = estimate)) +
  theme_bw() +
  ylim(-0.15, 0.05) +
  geom_pointrange(aes(ymin = lower, ymax = upper, color = feature), shape=16) +
  geom_hline(yintercept = 0, color = "grey") +
  coord_flip() +
  facet_wrap(~feature, ncol = 1L, scales = "free_y", 
             strip.position = "top") +
  labs(y="Average Marginal Component Effect (AMCE)",
       x="") +
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.x = element_text(face = 'bold', size = 11),
        strip.text = element_text(face = "bold", 
                                   margin = margin(t = 2, b = 2),
                                   size = rel(1), 
                                   hjust = 0.01),
        legend.position = "none")

ggsave('Manuscript files/figures/cregg_AMCEs.png')
