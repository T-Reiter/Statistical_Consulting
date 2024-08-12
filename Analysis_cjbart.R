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


### 2-cluster solution
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
imces <- IMCE(data = cj_tidy, 
              model = fit_cjbart, 
              attribs = c('Territ_Cession'), 
              ref_levels = c('None')
              # ,method = 'parametric'
              , cores = 3
)

# Get VarImps
het_vimp(imces, 
         levels = NULL, 
         covars = c("country", 
                    "age", 
                    "gender", 
                    "leftright3"), 
         cores = 3)




