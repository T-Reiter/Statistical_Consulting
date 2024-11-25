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


#####' *Select variables of interest for the 2 different models*
# reduced set of covars 
vars_red = c(
  # General variables
  'y', 'c_id', 'task',
  # Attributes
  "Sold_killed_UKR", "Sold_killed_RUS", "Civ_killed_UKR",
  "Infra_Destr_UKR", "Perc_GDP_milit", "Perc_GDP_econ",
  "Risk_Nuke", "Territ_Cession", "Polit_Self_Det_UKR", 
  # Covariates
  'Country', 'age', 'gender', 'leftright3'
  )

vars_ext = c(
  # General variables
  'y', 'c_id', 
  # 'task',
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



#####' *Fit the models*
### Fit the reduced model
set.seed(13.9238239)
fit_cjbart_red = cjbart(data = cj_tidy[, vars_red], 
                        Y = "y",
                        type = "choice", 
                        id = "c_id", 
                        round = "task", 
                        use_round = TRUE, 
                        cores = 4)

# saveRDS(fit_cjbart_red, "1c_Model_Objects/2024-11-18_fit_cjbart_red_2.rds")
fit_cjbart_red <- readRDS("1c_Model_Objects/2024-11-18_fit_cjbart_red.rds")


### Fit the extended model
set.seed(123)
fit_cjbart_ext6_noround = cjbart(data = cj_tidy[, vars_ext], 
                        Y = "y",
                        # type = "choice", 
                        id = "c_id", 
                        # round = "task", 
                        # use_round = TRUE, 
                        cores = 4)

saveRDS(fit_cjbart_ext6_noround, "1c_Model_Objects/2024-11-18_fit_cjbart_ext6_noround.rds")
fit_cjbart_ext <- readRDS("1c_Model_Objects/2024-11-18_fit_cjbart_ext.rds")


#####' *Get IMCEs*
### Get IMCEs for the reduced model
imces_red <- IMCE(data = cj_tidy[, vars_red], 
              keep_omce = TRUE,
              model = fit_cjbart_red, 
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

# saveRDS(imces_red, "1c_Model_Objects/2024-11-18_imces_red_2.rds")
imces_red <- readRDS("1c_Model_Objects/2024-11-18_imces_red_2.rds")



### Get IMCEs for the extended model
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
                  # ,method = 'parametric'
                  , cores = 4
)

# saveRDS(imces_ext, "1c_Model_Objects/2024-11-18_imces_ext_2.rds")
imces_ext2 <- readRDS("1c_Model_Objects/2024-11-18_imces_ext_2.rds")




#####' *Get Variable Importances*
### For the reduced model
covars_of_interest_red <- c("Country", "age", "gender", "leftright3")

var_imps_red <- het_vimp(imces = imces_red, 
                         covars = covars_of_interest_red, 
                         cores = 4)

saveRDS(var_imps_red, "1c_Model_Objects/2024-11-18_var_imps_red_2.rds")
var_imps_red <- readRDS("1c_Model_Objects/2024-11-18_var_imps_red.rds")


### for the extended model
covars_of_interest_ext <- c("Country", "age", "gender", "leftright3"
                            # additional covariates
                            ,"QV_Ukraine", "q3_3", "Q5", "q1617_USA",
                            "q1617_RUS", "Q11", "Q15_1", "Q15_7",
                            "q10_2", "q10_6", "q10_8", "q10_10")

var_imps_ext <- het_vimp(imces = imces_ext, 
                         covars = covars_of_interest_ext, 
                         cores = 4)

saveRDS(var_imps_ext, "1c_Model_Objects/2024-11-18_var_imps_ext_2.rds")
var_imps_ext <- readRDS("1c_Model_Objects/2024-11-18_var_imps_ext.rds")





#####' *Plot Variable Importances*

### Plot with off-the-shelf function included in package (as in Paper)
# plot(var_imps_red)
# plot(var_imps_ext)

var_imps_ext <- readRDS("1c_Model_Objects/2024-11-18_var_imps_ext.rds")
var_imps_ext_2 <- readRDS("1c_Model_Objects/2024-11-18_var_imps_ext_2.rds")

#### Custom plot (prettier)
# plot_data <- var_imps_red$results # reduced covariate set
plot_data <- var_imps_ext$results # extended covariate set

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

# Rename additional covars 
# Q10_10
# Wenn ein Land militärisch angegriffen wird (ohne eigenes Verschulden), 
# und der Angreifer ihm einen Teil seines Landes wegnimmt, 
# sollte {COUNTRY} dieses Land mit Waffenlieferungen unterstützen.
# 7er Likert: 1 (Stimme überhaupt nicht zu) bis 7 (Stimme vollständig zu)


# Q3_3
# Wie viel Angst macht Ihnen, wenn überhaupt, 
# der Angriff Russlands auf die Ukraine?“ 
# 7er Likert: 1 (überhaupt keine Angst) bis 7 (sehr große Angst)


# Q15:
# Wenn dadurch ein Friedensabkommen mit Russland möglich wäre: 
# Sollte die Ukraine eine oder mehrere der 
# unten genannten Zugeständnisse machen?“ 
# Q15_1
# "Auf eine NATO-Mitgliedschaft verzichten“  (binary choice)

# Q15_7:
# Q15_7: "Gar keine Zugeständnisse machen“  (binary choice)
# plot_data[which(plot_data$covar == 'q10_10'), 'covar'] <- "general_weapon\nas_revenge_7"
# plot_data[which(plot_data$covar == 'q3_3'), 'covar'] <- "scared_by\ninvasion_7"
# plot_data[which(plot_data$covar == 'Q15_1'), 'covar'] <- "ukr_nato_waiver\nfor_peace_binary"
# plot_data[which(plot_data$covar == 'Q15_7'), 'covar'] <- "no_concessions\nfor_peace_binary"




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

plot_data <- merge(plot_data, 
                   desired_orders_df, 
                   by = c("Attribute", "Level"), 
                   all.x = TRUE)

plot_data <- plot_data %>%
  mutate(Level_ordered = reorder_within(Level, -order, Attribute))


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
       y = "Attribute-level", 
       fill = "Importance") +
  theme(axis.text.x = element_text(size = 11, 
                                   face = 'bold',
                                   angle=90, 
                                   vjust = 0.5, 
                                   hjust = 0.5),
        axis.title = element_text(size = 12, face = 'bold'),
        strip.placement = "outside",
        strip.background = element_rect(fill = "grey80", 
                                        color = "grey50"),
        strip.text = element_text(color = "black", size = 11),
        panel.background = element_rect(fill = "white"))


# ggsave('1d_Plots/RF_VarImps_red.png')
# ggsave('1d_Plots/RF_VarImps_ext.png')






#####' *Fit single decision trees for interpretation*
imces_red <- readRDS("1c_Model_Objects/2024-11-18_imces_red.rds")
imces_ext <- readRDS("1c_Model_Objects/2024-11-18_imces_ext.rds")

# custom node function to prefent scientific notation in rpart.plot()
node_fun <- function(x, labs, digits, varlen) {
  # Calculate the proportion of units in the node
  total_units <- sum(x$frame$n)  # Total number of units
  proportions <- (x$frame$n / total_units) * 100  # Convert to percentage
  
  # Format the node label with value and proportion
  sprintf("%.4f\n%.1f%%", x$frame$yval, proportions)  # 4 decimals for value, 1 decimal for percentage
}

# custom function needed for generating plot titles 
find_attr = function(attribute_level = NULL){
  if (is.null(attribute_level)) {
    print('No attribute level provided.')
  }
  
  attribute_level = as.character(attribute_level)
  attribute = paste0(na.omit(names(cj_tidy)[sapply(cj_tidy, function(col) any(col == attribute_level))]))
  return(attribute)
}


### Single IMCE Prediction Decision Trees

# Num Soldiers Killed Russia (4 Subject Level Covars)
# IMCEs of 100,000 RUS Sold. killed (vs. reference: 25,000)
fit <- rpart::rpart(formula = `100,000` ~ Country + age + gender + leftright3,
                    imces_red$imce
                    , control = rpart.control(cp = 0.01)  # complexity param, default = 0.01, larger --> less complex 
                    ) 
printcp(fit) # how many splits with which cp
png("Manuscript files/figures/DecTree_red_RU_Sold_killed_100k.png", width = 800, height = 600)
par(mar = c(0, 0, 0, 0))  # remove margins
rpart.plot(fit, node.fun = node_fun, 
           main = paste('Target: IMCEs of', find_attr(fit$terms[[2]]), '=',  fit$terms[[2]]))
dev.off()


fit <- rpart::rpart(formula = `50,000.` ~ Country + age + gender + leftright3,
                    imces_red$imce
                    , control = rpart.control(cp = 0.02)
                    )
png("Manuscript files/figures/DecTree_red_RU_Sold_killed_50k.png", width = 800, height = 600)
rpart.plot(fit, node.fun = node_fun, 
           main = paste('Target: IMCEs of', find_attr(fit$terms[[2]]), '=',  fit$terms[[2]]))
rpart.plot(fit, node.fun = node_fun)
dev.off()


# Num Soldiers Killed Russia (Enlarged Covariate Set)
fit <- rpart::rpart(formula = `100,000` ~ 
                      Country + age + gender + leftright3 + 
                      QV_Ukraine + q3_3 + Q5 + q1617_USA + 
                      q1617_RUS + Q11 + Q15_1 + Q15_7 + 
                      q10_2 + q10_6 + q10_8 + q10_10,
                    imces_ext$imce)

# Generate the plot
png("Manuscript files/figures/DecTree_red_RU_Sold_killed_100k_ext.png", width = 800, height = 600)
rpart.plot(fit, node.fun = node_fun, 
           main = paste('Target: IMCEs of', find_attr(fit$terms[[2]]), '=',  fit$terms[[2]]))
dev.off()


# Plots New


# Num Soldiers Killed Russia (4 Subject Level Covars)
# IMCEs of 100,000 RUS Sold. killed (vs. reference: 25,000)
fit <- rpart::rpart(formula = `16,000` ~ Country + age + gender + leftright3 + Q15_7,
                    imces_ext$imce
                    , control = rpart.control(cp = 0.04)  # complexity param, default = 0.01, larger --> less complex 
) 
printcp(fit) # how many splits with which cp
png("Manuscript files/figures/DecTree_red_RU_Sold_killed_100k.png", width = 800, height = 600)
par(mar = c(0, 0, 0, 0))  # remove margins
rpart.plot(fit, node.fun = node_fun, 
           main = paste('Target: IMCEs of', find_attr(fit$terms[[2]]), '=',  fit$terms[[2]]))
dev.off()


fit <- rpart::rpart(formula = `Moderate (10%)` ~ Country + age + gender + leftright3 + Q15_7,
                    imces_ext$imce
                    , control = rpart.control(cp = 0.04)  # complexity param, default = 0.01, larger --> less complex 
) 
printcp(fit) # how many splits with which cp
png("Manuscript files/figures/DecTree_red_RU_Sold_killed_100k.png", width = 800, height = 600)
par(mar = c(0, 0, 0, 0))  # remove margins
rpart.plot(fit, node.fun = node_fun, 
           main = paste('Target: IMCEs of', find_attr(fit$terms[[2]]), '=',  fit$terms[[2]]))
dev.off()


fit <- rpart::rpart(formula = `Low (5%)` ~ Country + age + gender + leftright3 + Q15_7,
                    imces_ext$imce
                    , control = rpart.control(cp = 0.04)  # complexity param, default = 0.01, larger --> less complex 
) 
printcp(fit) # how many splits with which cp
png("Manuscript files/figures/DecTree_red_RU_Sold_killed_100k.png", width = 800, height = 600)
par(mar = c(0, 0, 0, 0))  # remove margins
rpart.plot(fit, node.fun = node_fun, 
           main = paste('Target: IMCEs of', find_attr(fit$terms[[2]]), '=',  fit$terms[[2]]))
dev.off()


# sold killed ukr
fit <- rpart::rpart(formula = `2014 LoC (8%)` ~ Country + age + gender + leftright3 + Q15_7,
                    imces_ext$imce
                    , control = rpart.control(cp = 0.02)  # complexity param, default = 0.01, larger --> less complex 
) 
printcp(fit) # how many splits with which cp
png("Manuscript files/figures/DecTree_red_RU_Sold_killed_100k.png", width = 800, height = 600)
par(mar = c(0, 0, 0, 0))  # remove margins
rpart.plot(fit, node.fun = node_fun, 
           main = paste('Target: IMCEs of', find_attr(fit$terms[[2]]), '=',  fit$terms[[2]]))
dev.off()


# sold killed ukr
fit <- rpart::rpart(formula = `100B` ~ Country + age + gender + leftright3 + Q15_7,
                    imces_ext$imce
                    , control = rpart.control(cp = 0.01)  # complexity param, default = 0.01, larger --> less complex 
) 
printcp(fit) # how many splits with which cp
png("Manuscript files/figures/DecTree_red_RU_Sold_killed_100k.png", width = 800, height = 600)
par(mar = c(0, 0, 0, 0))  # remove margins
rpart.plot(fit, node.fun = node_fun, 
           main = paste('Target: IMCEs of', find_attr(fit$terms[[2]]), '=',  fit$terms[[2]]))
dev.off()




# Q15_1 == 1 --> Exit Nato, if consequence would be more Russian Soldiers killed
# Most people would say, that if 
fit <- rpart::rpart(formula = `50,000.` ~ 
                      Country + age + gender + leftright3 + 
                      QV_Ukraine + q3_3 + Q5 + q1617_USA + 
                      q1617_RUS + Q11 + Q15_1 + Q15_7 + 
                      q10_2 + q10_6 + q10_8 + q10_10,
                    imces$imce)
rpart.plot(fit)
png("Manuscript files/figures/DecTree_red_RU_Sold_killed_50k_ext.png", width = 800, height = 600)
rpart.plot(fit, node.fun = node_fun, 
           main = paste('Target: IMCEs of', find_attr(fit$terms[[2]]), '=',  fit$terms[[2]]))
dev.off()



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

