################################################################################
####### FACTOR HET 
################################################################################

### Load Packages
library(FactorHet)

### Set WD
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))

### Load Data 
# cj_tidy <- readRDS("Ukraine Conjoint Data/cj_tidy.rds")

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
fit_FH_2 <- FactorHet_mbo(formula = y ~ Sold_killed_UKR + Sold_killed_RUS + 
                                        Civ_killed_UKR + Infra_Destr_UKR +
                                        Perc_GDP_milit + Perc_GDP_econ +
                                        Risk_Nuke + Territ_Cession + 
                                        Polit_Self_Det_UKR,
                                        K = 2,
                                        design = cj_tidy, 
                                        moderator = ~ age + gender + 
                                                      leftright3 + Country, 
                                        group = ~ c_id, 
                                        task = ~ task,
                                        choice_order = ~ concept)
saveRDS(fit_FH_2, "fit_FH_2F.rds")
fit_FH_2 <- readRDS("fit_FH_2F.rds")

FactorHet::marginal_AME(fit_FH_2) # fast
FactorHet::posterior_by_moderators(fit_FH_2) # fast
FactorHet::marginal_AMIE(fit_FH_2) # takes some time

### 3-cluster solution
fit_FH_3 <- FactorHet_mbo(formula = y ~ Sold_killed_UKR + Sold_killed_RUS + 
                                        Civ_killed_UKR + Infra_Destr_UKR +
                                        Perc_GDP_milit + Perc_GDP_econ +
                                        Risk_Nuke + Territ_Cession + 
                                        Polit_Self_Det_UKR,
                                        K = 3,
                                        design = cj_tidy, 
                                        moderator = ~ age + gender + 
                                                      leftright3 + Country, 
                                        group = ~ c_id, 
                                        task = ~ task,
                                        choice_order = ~ concept)
saveRDS(fit_FH_3, "fit_FH_3F.rds")
fit_FH_3 <- readRDS("fit_FH_3F.rds")


FactorHet::marginal_AME(fit_FH_3) # fast
FactorHet::posterior_by_moderators(fit_FH_3) # fast
FactorHet::marginal_AMIE(fit_FH_3) # takes some time
