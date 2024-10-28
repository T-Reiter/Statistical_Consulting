################################################################################
####### FACTOR HET 
################################################################################

### Load Packages
library(cjbart)

### Set WD
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))

### Load Data 
cj_tidy <- readRDS("0_Ukraine_Conjoint_Data/cj_tidy.rds")

# get additional covariate data 
add_covars <- 
  readRDS("0_Ukraine_Conjoint_Data/CJ_Ukraine_NewCovariates.Rds")
# get additional covariate data --> same data different files types?
library(haven)
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
# get cols as characters(needed for imces)
# cj_tidy$Sold_killed_UKR <- as.character(cj_tidy$Sold_killed_UKR)
# cj_tidy$Sold_killed_RUS <- as.character(cj_tidy$Sold_killed_RUS)
# cj_tidy$Civ_killed_UKR <- as.character(cj_tidy$Civ_killed_UKR)
# cj_tidy$Infra_Destr_UKR <- as.character(cj_tidy$Infra_Destr_UKR)
# cj_tidy$Perc_GDP_milit <- as.character(cj_tidy$Perc_GDP_milit)
# cj_tidy$Perc_GDP_econ <- as.character(cj_tidy$Perc_GDP_econ)
# cj_tidy$Risk_Nuke <- as.character(cj_tidy$Risk_Nuke)
# cj_tidy$Territ_Cession <- as.character(cj_tidy$Territ_Cession)
# cj_tidy$Polit_Self_Det_UKR <- as.character(cj_tidy$Polit_Self_Det_UKR)



### Fit
fit_cjbart <- cjbart(data = cj_tidy, 
                     Y = "y",
                     type = "choice", 
                     id = "c_id", 
                     round = "task", 
                     use_round = TRUE, 
                     cores = 4)


# saveRDS(fit_cjbart, "1c_Model_Objects/fit_cjbart_add_covars.rds")
fit_cjbart <- readRDS("1c_Model_Objects/fit_cjbart_add_covars.rds")


# Get IMCEs
imces <- IMCE(data = cj_tidy,
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
              , cores = 4
)

saveRDS(imces, "1c_Model_Objects/2024-10-27_imces_all_new_add_covars.rds")
imces_4 <- readRDS("1c_Model_Objects/imces_all_new.rds")
imces <- readRDS("1c_Model_Objects/2024-10-27_imces_all_new_add_covars.rds")


# Get VarImps
covars_of_interest <- c("Country", "age", "gender", "leftright3",
                        # additional covariates
                        "QV_Ukraine", "q3_3", "Q5", "q1617_USA",
                        "q1617_RUS", "Q11", "Q15_1", "Q15_7",
                        "q10_2", "q10_6", "q10_8", "q10_10"
                        )

var_imps <- het_vimp(imces = imces, 
                     covars = covars_of_interest, 
                     cores = 3)

         
# saveRDS(var_imps, "1c_Model_Objects/2024-10-27_var_imps_all_attributes_new_add_covars.rds")
var_imps_4 <- readRDS("1c_Model_Objects/var_imps_all_attributes_new.rds")
var_imps <- readRDS("1c_Model_Objects/2024-10-27_var_imps_all_attributes_new_add_covars.rds")

### Get Plot of VarImps (as in Paper)
# plot(var_imps) # off the shelf-version in package 

# Custom Plot
#### Custom plot
plot_data <- var_imps$results


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


# Rename Covariates with proper Caps 
plot_data[which(plot_data$covar == 'age'), 'covar'] <- "Age"
plot_data[which(plot_data$covar == 'gender'), 'covar'] <- "Gender"
plot_data[which(plot_data$covar == 'leftright3'), 'covar'] <- "Left-Right-3"

desired_orders_df <- data.frame(
  Attribute = c(rep("Civ_killed_UKR", 2),
                rep("Infra_Destr_UKR", 2),
                rep("Perc_GDP_econ", 2),
                rep("Perc_GDP_milit", 2),
                rep("Polit_Self_Det_UKR", 2),
                rep("Risk_Nuke", 2),
                rep("Sold_killed_RUS", 2),
                rep("Sold_killed_UKR", 2),
                rep("Territ_Cession", 3)),
  Level = c("16,000", "8,000",
            "200B", "100B",
            "0.3% of GDP", "0.2% of GDP.",
            "0.3% of GDP", "0.2% of GDP",
            "Russian influence", "NO EU/Nato",
            "Moderate (10%)", "Low (5%)",
            "100,000", "50,000.",
            "25,000", "12,500.",
            "2023 LoC (16%)", "2014 LoC (8%)", "Crimea (4%)"),
  order = c(1,2,
            1,2,
            1,2,
            1,2,
            1,2,
            1,2,
            1,2,
            1,2,
            1,2,3)
)

plot_data <- merge(plot_data, desired_orders_df, by = c("Attribute", "Level"), all.x = TRUE)
library(tidytext)
library(dplyr)
plot_data <- plot_data %>%
  mutate(Level_ordered = reorder_within(Level, -order, Attribute))


library(ggplot2)
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
       y = "Attribute-level", 
       fill = "Importance") +
  theme(axis.text.x = element_text(angle=45, 
                                   vjust = 1, 
                                   hjust = 1),
        strip.placement = "outside",
        strip.background = element_rect(fill = "grey80", 
                                        color = "grey50"),
        strip.text = element_text(color = "black"),
        panel.background = element_rect(fill = "white"))


ggsave('1d_Plots/RF_VarImps_full')


# Q15:
# Wenn dadurch ein Friedensabkommen mit Russland möglich wäre: 
# Sollte die Ukraine eine oder mehrere der 
# unten genannten Zugeständnisse machen?“ 
# Q15_1
# "Auf eine NATO-Mitgliedschaft verzichten“  (binary choice)

# Q15_7:
# Q15_7: "Gar keine Zugeständnisse machen“  (binary choice)


  
# Fit single trees 
library(rpart)
library(rpart.plot) # standard built-in plot


# Single IMCE Prediction Decision Trees
# Num Soldiers Killed Russia (4 Subject Level Covars)
fit <- rpart::rpart(formula = `100,000` ~ Country + age + gender + leftright3,
                    imces_4$imce)
rpart.plot(fit)

fit <- rpart::rpart(formula = `50,000.` ~ Country + age + gender + leftright3,
                    imces_4$imce)
rpart.plot(fit)

# Num Soldiers Killed Russia (Enlarged Covariate Set)
fit <- rpart::rpart(formula = `100,000` ~ 
                      Country + age + gender + leftright3 + 
                      QV_Ukraine + q3_3 + Q5 + q1617_USA + 
                      q1617_RUS + Q11 + Q15_1 + Q15_7 + 
                      q10_2 + q10_6 + q10_8 + q10_10,
                    imces$imce)
rpart.plot(fit)
# Q15_1 == 1 --> Exit Nato, if consequence would be more Russian Soldiers killed
# Most people would say, that if 

fit <- rpart::rpart(formula = `50,000.` ~ 
                      Country + age + gender + leftright3 + 
                      QV_Ukraine + q3_3 + Q5 + q1617_USA + 
                      q1617_RUS + Q11 + Q15_1 + Q15_7 + 
                      q10_2 + q10_6 + q10_8 + q10_10,
                    imces$imce)
rpart.plot(fit)



# Political Self-Determination
fit_2 <- rpart::rpart(formula = `No EU/NATO` ~ Country + age + gender + leftright3,
                    imces$imce)
rpart.plot(fit_2)


# Territorial Cession
fit_3 <- rpart::rpart(formula = `2023 LoC (16%)` ~ Country + age + gender + leftright3,
                      imces$imce)
rpart.plot(fit_3)

fit_4 <- rpart::rpart(formula = `2014 LoC (8%)` ~ Country + age + gender + leftright3,
                      imces$imce)
rpart.plot(fit_4)


# Num Civilians Killed Ukraine
fit <- rpart::rpart(formula = None ~ Country + age + gender + leftright3,
                    imces$imce)
rpart.plot(fit)

