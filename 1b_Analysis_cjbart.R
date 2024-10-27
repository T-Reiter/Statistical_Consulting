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
  readRDS("Ukraine Conjoint Data/CJ_Ukraine_NewCovariates.Rds")
# get additional covariate data --> same data different files types?
library(haven)
add_covars2 <- 
  haven::read_dta("Ukraine Conjoint Data/CJ_Ukraine_NewCovariates.dta")
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


saveRDS(fit_cjbart, "fit_cjbart_add_covars.rds")
fit_cjbart <- readRDS("fit_cjbart_add_covars.rds")


# Get IMCEs
t1 <- Sys.time()
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
              ref_levels = c('25,000'
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
t2 <- Sys.time()

saveRDS(imces, "imces_all_new_add_covars.rds")
# imces <- readRDS("imces_all_new_add_covars.rds")


# Get VarImps
covars_of_interest <- c("Country", "age", "gender", "leftright3",
                        # additional covariates
                        "QV_Ukraine", "q3_3", "Q5", "q1617_USA",
                        "q1617_RUS", "Q11", "Q15_1", "Q15_7",
                        "q10_2", "q10_6", "q10_8", "q10_10"
                        )

var_imps <- het_vimp(imces = imces, 
                     covars = covars_of_interest, 
                     cores = 4)

         
saveRDS(var_imps, "var_imps_all_attributes_new_add_covars.rds")
# var_imps <- readRDS("var_imps_all_attributes_new_add_covars.rds")

# Get Plot of VarImps (as in Paper)
test <- plot(var_imps)

# Change order of the factors --> ToDo

  
# Fit single trees 
library(rpart)
library(rpart.plot) # standard built-in plot


# Single IMCE Prediction Decision Trees
# Num Soldiers Killed Russia
fit <- rpart::rpart(formula = `100,000` ~ Country + age + gender + leftright3,
                    imces$imce)
rpart.plot(fit)

fit <- rpart::rpart(formula = `50,000` ~ Country + age + gender + leftright3,
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







#### Custom plot
plot_data <- x$results


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

# Plot the Var Imps
ggplot2::ggplot(plot_data,
                ggplot2::aes_string(x = "covar",
                                    y = "Level",
                                    fill = "importance")) +
  ggplot2::facet_grid(Attribute ~ ., 
                      space = "free", 
                      scales = "free",
                      switch = "y",
                      labeller = ggplot2::labeller(Attribute = custom_labels)) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_gradient(low="white", high="firebrick1") +
  ggplot2::labs(x = "Covariates", 
                y = "Attribute-level", 
                fill = "Importance") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, 
                                                     vjust = 1, 
                                                     hjust = 1),
                 strip.placement = "outside",
                 strip.background = element_rect(fill = "grey80", 
                                                 color = "grey50"),  # Change the color of the strip background
                 strip.text = element_text(color = "black"
                                           # , face = "bold", # Change the color and style of the text) +
                                           ),
                 panel.background = ggplot2::element_rect(fill = "white")) 

ggsave("var_imps_add_covars.png")




# Reorder Factor levels  tbd 
unique(plot_data$Attribute)

desired_order_1 <- c("8,000", "16,000")
desired_order_2 <- c("100B", "200B")
desired_order_3 <- c("0.2% of GDP.","0.3% of GDP")
desired_order_4 <- c("0.2% of GDP","0.3% of GDP")
desired_order_5 <- c("NO EU/Nato", "Russian influence")
desired_order_6 <- c("Low (5%)", "Moderate (10%)")
desired_order_7 <- c("50,000.", "100,000")
desired_order_8 <- c("12,500.", "25,000")
desired_order_9 <- c("Crimea (4%)", "2014 LoC (8%)", "2023 LoC (16%)")

# Reorder the factor levels in your dataset
plot_data$Civ_killed_UKR <- factor(plot_data$Civ_killed_UKR, 
                                   levels = desired_order_1)
plot_data$Infra_Destr_UKR <- factor(plot_data$Infra_Destr_UKR, 
                                    levels = desired_order_2)
plot_data$Perc_GDP_econ <- factor(plot_data$Perc_GDP_econ, 
                                  levels = desired_order_3)
plot_data$Perc_GDP_milit <- factor(plot_data$Perc_GDP_milit, 
                                   levels = desired_order_4)
plot_data$Polit_Self_Det_UKR <- factor(plot_data$Polit_Self_Det_UKR, 
                                       levels = desired_order_5)
plot_data$Risk_Nuke <- factor(plot_data$Risk_Nuke, 
                              levels = desired_order_6)
plot_data$Sold_killed_RUS <- factor(plot_data$Sold_killed_RUS, 
                                    levels = desired_order_7)
plot_data$Sold_killed_UKR <- factor(plot_data$Sold_killed_UKR, 
                                    levels = desired_order_8)
plot_data$Territ_Cession <- factor(plot_data$Territ_Cession, 
                                   levels = desired_order_9)

