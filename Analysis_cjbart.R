################################################################################
####### FACTOR HET 
################################################################################

### Load Packages
library(cjbart)

### Set WD
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))

### Load Data 
cj_tidy <- readRDS("Ukraine Conjoint Data/cj_tidy.rds")

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


### Fit
fit_cjbart <- cjbart(data = cj_tidy, 
                     Y = "y",
                     type = "choice", 
                     id = "c_id", 
                     round = "task", 
                     use_round = TRUE, 
                     cores = 3)


saveRDS(fit_cjbart, "fit_cjbart.rds")
fit_cjbart <- readRDS("fit_cjbart.rds")


# Get IMCEs
t1 <- Sys.time()
imces <- IMCE(data = cj_tidy,
              keep_omce = TRUE,
              model = fit_cjbart, 
              attribs = c(
                            "Sold_killed_UKR"
                          , "Sold_killed_RUS"
                          , "Civ_killed_UKR"
                          , "Infra_Destr_UKR"
                          , "Perc_GDP_milit"
                          , "Perc_GDP_econ"
                          , "Risk_Nuke"
                          , "Territ_Cession"
                          , "Polit_Self_Det_UKR"
                          ), 
              ref_levels = c(
                               '25,000'
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

saveRDS(imces, "imces_all_new.rds")
imces <- readRDS("imces_all_new.rds")


# Get VarImps
var_imps <- het_vimp(imces = imces, 
                     covars = c("Country", 
                                "age", 
                                "gender", 
                                "leftright3"), 
                     cores = 4)
  
         
saveRDS(var_imps, "var_imps_all_attributes_new.rds")
var_imps <- readRDS("var_imps_all_attributes.rds")

# Get Plot of VarImps (as in Paper)
plot(var_imps)




# Fit single 
library(rpart)
library(rpart.plot)



fit <- rpart::rpart(formula = `100,000` ~ Country + age + gender + leftright3,
                    imces$imce)
rpart.plot(fit)


fit_2 <- rpart::rpart(formula = `No EU/NATO` ~ Country + age + gender + leftright3,
                    imces$imce)
rpart.plot(fit_2)

fit_3 <- rpart::rpart(formula = `2023 LoC (16%)` ~ Country + age + gender + leftright3,
                      imces$imce)
rpart.plot(fit_3)

fit_4 <- rpart::rpart(formula = `2014 LoC (8%)` ~ Country + age + gender + leftright3,
                      imces$imce)
rpart.plot(fit_4)



