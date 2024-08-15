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
t1 <- Sys.time()
imces <- IMCE(data = cj_tidy,
              keep_omce = TRUE,
              model = fit_cjbart, 
              attribs = c('Territ_Cession','Polit_Self_Det_UKR'), 
              ref_levels = c('None','Full')
              # ,method = 'parametric'
              , cores = 2
)
t2 <- Sys.time()

saveRDS(imces, "imces.rds")

# Get VarImps
var_imps <- het_vimp(imces = imces, 
                     covars = c("Country", 
                                "age", 
                                "gender", 
                                "leftright3"), 
                     cores = 2)
  
         

saveRDS(var_imps, "var_imps.rds")




