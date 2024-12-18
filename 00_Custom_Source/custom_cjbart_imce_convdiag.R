#' *Comment TR: Create Functions needed later on*

# Convert character columns to factors with warning
.char_to_fact <- function(data) {
  
  missing_factors <- sapply(data, is.character)
  
  if (sum(missing_factors) > 0) {
    
    missing_vars <- names(missing_factors)[missing_factors]
    
    data[,missing_vars] <- Map(as.factor, subset(data, select = missing_vars))
    
    # message("The following variables were converted to factors: ",
    #         paste0(missing_vars, collapse = ", ")
    # )
    
  }
  
  return(data)
}

# Suppress console output where verbose option not present
.quiet <- function(x) {
  
  # Code from Hadley Wickham
  # See: https://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
  
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
  
}


# Implementation of Rubin's combination rules
.combine <- function(theta, var_theta) {
  
  m <- length(theta)
  
  Q_bar <- (1/m)*sum(theta)
  U_bar <- (1/m)*sum(var_theta)
  
  demean <- (theta-Q_bar)^2
  
  B <- (1/(m-1)) * sum(demean)
  
  Q_bar_var <- U_bar + (1 + (1/m))*B
  Q_bar_se <- sqrt(Q_bar_var)
  
  v_m <- (m-1)*(1+(U_bar/((1+m^-1)*B)))^2
  
  std_err = Q_bar_se
  
  return(std_err)
  
}



#' *Comment TR: IMCE function as used later in the package*
#' Heterogeneous Effects Analysis of Conjoint Results
#'
#' @description \code{IMCE} calculates the individual-level marginal component effects from a BART-estimated conjoint model.
#' @param data A data.frame, containing all attributes, covariates, the outcome and id variables to analyze.
#' @param model A model object, the result of running \code{cjbart()}
#' @param attribs Vector of attribute names for which IMCEs will be predicted
#' @param ref_levels Vector of reference levels, used to calculate marginal effects
#' @param method Character string, setting the variance estimation method to use. When method is "parametric", a typical combined variance estimate is employed; when \code{method = "bayes"}, the 95% posterior interval is calculated; and when \code{method = "rubin"}, combination rules are used to combine the variance analogous to in multiple imputation analysis.
#' @param alpha Number between 0 and 1 -- the significance level used to compute confidence/posterior intervals. When \code{method = "bayes"}, the posterior interval is calculated by taking the alpha/2 and (1-alpha/2) quantiles of the posterior draws. When \code{method = "rubin"}, the confidence interval equals the IMCE +/- \code{qnorm(alpha/2)}. By default, alpha is 0.05 i.e. generating a 95% confidence/posterior interval.
#' @param keep_omce Boolean, indicating whether to keep the OMCE-level results (default = \code{FALSE})
#' @param cores Number of CPU cores used during prediction phase
#' @param skip_checks Boolean, indicating whether to check the structure of the data (default = \code{FALSE}). Only set this to \code{TRUE} if you are confident that the data is structured appropriately
#' @details The OMCE estimates are the result of subtracting the predicted value of each observation under the reference-level category from the predicted value of each observation under the given attribute level.
#' If an attribute has *k* levels, then this will yield *k-1* estimates per observation.
#' The IMCE is the average of the OMCEs for each individual within the data.
#' @return \code{IMCE} returns an object of type "cjbart", a list object.
#' \item{omce}{A data.frame containing the observation-level marginal effects}
#' \item{imce}{A data.frame containing the individual-level marginal effects}
#' \item{imce_upper}{A data.frame containing the upper bound of the IMCE confidence/credible interval}
#' \item{imce_lower}{A data.frame containing the lower bound of the IMCE confidence/credible interval}
#' \item{att_levels}{A vector containing the attribute levels}
#' @seealso [cjbart::cjbart()]
#' @importFrom stats predict
#' @example inst/examples/basic_workflow.R
#' @export

#' *Insert TR: Set Arguments for IMCE dry-run to extract trace plots*
data = cj_tidy[, vars_ext]
keep_omce = TRUE
model = fit_cjbart_ext # standard model  
large = F
# model = fit_cjbart_ext_large 
# large = T
      # 'larger' model for longer Burn-In and More Draws
      # nskip = 2000 (vs. 250), ndraws = 5000 (vs. 1000)
attribs = c("Sold_killed_UKR"
            , "Sold_killed_RUS"
            , "Civ_killed_UKR"
            , "Infra_Destr_UKR"
            , "Perc_GDP_milit"
            , "Perc_GDP_econ"
            , "Risk_Nuke"
            , "Territ_Cession"
            , "Polit_Self_Det_UKR"
)
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
method = 'rubin'
cores = 4
skip_checks = T

#' *Comment TR: Dry Run of IMCE function*
#' *Comment TR: after drilling on the IMCE function*

# IMCE = function(args){

data <- as.data.frame(data)

# Get variables from trained model
Y <- model$Y_col
round <- model$round_col
id <- model$id_col
unq_ids <- unique(data[[id]])
type <- model$type

# Check optional args
if (!(method %in% c("average","bayes","rubin"))) {
  stop("Variance estimation method must be in c('parametric','bayes','rubin'). See ?OMCE for more details.")
}


if (!skip_checks) {
  test_data <- data[,!(names(data) %in% c(attribs, Y, round))]
  
  if (is.data.frame(test_data)) { # Skip if only one variable left (assume it is id)
    
    test_data <- test_data[!duplicated(test_data),]
    
    if(!(id %in% names(test_data))) {
      stop("Could not find id variable in data")
    }
    
    if(nrow(test_data) != length(unique(test_data[[id]]))) {
      warning("Covariates vary within id: if this is not intentional, please check your data")
    }
    
  }
  
  if (!sum(sapply(attribs, function(x) class(data[[x]])) %in% c("character","factor")) == length(attribs)) {
    
    stop("Conjoint attribute columns must be character vectors")
    
  }
  
  rm(test_data)
  gc()
}

# Check attribute-level unique and rename if needed
att_lev_list <- lapply(attribs, function (x) data.frame(Attribute = x, level = unique(data[[x]])))
att_lookup <- do.call("rbind", att_lev_list)

if (length(unique(att_lookup$level)) < nrow(att_lookup)) {
  
  ref_levels <- paste0("<",attribs,">_", ref_levels)
  for (att in attribs) {
    data[[att]] <- paste0("<",att,">_", data[[att]])
  }
  
  warning("Found duplicate level names across attributes: ",
          "[",unique(att_lookup$level[duplicated(att_lookup$level)]),"].",
          "\n*All* levels in data will be renamed to format <attribute>_level, ",
          "e.g. ", ref_levels[1],
          "\n You can use the `att_lookup` table in the return object to compare new and old names")
  
  for (i in 1:length(attribs)) {
    if (!(ref_levels[i] %in% data[[attribs[i]]])) {
      stop("Failed to re-label data. Try manually converting conjoint data so all attribute-level names are unique.")
    }
  }
  
  att_lev_list <- lapply(attribs, function (x) data.frame(Attribute = x, level = unique(data[[x]])))
  att_lookup <- do.call("rbind", att_lev_list)
}

# Add label to lookup (does nothing if no duplicates)
att_lookup$Level <- sub("<.*>_", "", att_lookup$level)


# Frame to store OMCEs
results <- data[,!(names(data) %in% c(attribs, Y))]

# If only attribs and id in data.frame, conditional to correct formatting
if (!is.data.frame(results)) {
  
  if (identical(results,data[[id]])) {
    results <- as.data.frame(results)
    names(results) <- id
  } else {
    stop("Results frame creation failed")
  }
  
}

# Variance and interval frames
var_omce <- data.frame(row.names = 1:nrow(data))
imce_lower <- imce_upper <- data.frame(row.names = 1:length(unq_ids))

# Data frame for predicting outcomes
train_vars <- names(data)[!(names(data) %in% c(id,Y))]
data_predict <- data[,train_vars]

# Vector to store attribute names (for future function calls)
out_levels <- c()

# for (i in 1:length(attribs)) {
  
i=1 # --> Ukr. Soldiers Killed 
# i=9 # --> Sovereignity/Polit. Self. Determination

message("Calculating OMCEs for attribute: ", attribs[i], " [",i,"/",length(attribs),"]")

att_levels <- unique(data[[attribs[i]]][data[[attribs[i]]] != ref_levels[i]])

out_levels <- c(out_levels, as.character(att_levels))

X_pred0 <- data_predict

X_pred0 <- .char_to_fact(X_pred0)

X_pred0[[attribs[i]]] <- factor(ref_levels[i],
                                levels = levels(X_pred0[[attribs[i]]]))
  
  
if (type == "choice") {
  #' *Warnings suppressed in predict function!*
  phat_0 <- # .quiet(
    
    predict(
      
      model,
      newdata = BART::bartModelMatrix(X_pred0),
      mc.cores = cores
    # )
  )$prob.test
} else {
  phat_0 <- .quiet(
    
    predict(
      
      model,
      newdata = BART::bartModelMatrix(X_pred0),
      mc.cores = cores
    )
  )
}
  
# for (att_level in att_levels) {

att_level = att_levels[1] # set level of interest

# use same profiles 
X_pred1 <- X_pred0
# but change the attribute level to the level of interest
X_pred1[[attribs[i]]] <- factor(att_level,
                                levels = levels(X_pred0[[attribs[i]]]))

# Get predictions
if (type == "choice") {
  phat_1 <- # .quiet(
    
    predict(
      
      model,
      newdata = BART::bartModelMatrix(X_pred1),
      mc.cores = cores
      
    # )
  )$prob.test
} else {
  phat_1 <- .quiet(
    
    predict(
      
      model,
      newdata = BART::bartModelMatrix(X_pred1),
      mc.cores = cores
      
    )
  )
}
    
    
## Note, prob.test.mean is equivalent to
# stats::pnorm(colMeans(pred_0$yhat.test))

# Plot and Save Traceplots of the Draws
# dynamically generate plot names
if (large == T){
  suffix = '_large'
} else {
  suffix = ''
}

# Attribute 1: Ukr. Soldiers. Killed (25,000 vs. 12,500)
# phat0
png(paste0("Manuscript files/figures/Traceplots/",
           "Ukr_Sold_killed_phat0_12k_1",
           suffix, ".png"), 
    width = 800, height = 600)
plot(phat_0[,1], cex.lab=1.5, cex.axis = 1.8)
dev.off()
# use BART's geweke diagnostic
# --> calculates a z-score test of equality of means for the first
# frac1 % and the last frac2% of each of the chains
# --> i.e., 80k tests (takes very long)
# --> test 100 random chains using gewekes diag (two-tailed)
set.seed(7)
rand_ind = sample(1:ncol(phat_0),size = 100, replace = F) 
z = gewekediag(phat_0[,rand_ind], frac1 = 0.1, frac2 = 0.5) 
p_vals = pnorm(-1*abs(z$z),0,1) + (1-pnorm(abs(z$z),0,1))
png(paste0("Manuscript files/figures/Geweke_pvals/",
           "Ukr_Sold_killed_phat0_12k",
           suffix, ".png"), 
    width = 800, height = 600)
hist(p_vals, main = '', cex.lab=1.5, cex.axis = 1.8) # cf. sample traceplots --> non convergence for most 
dev.off()


# phat1
png(paste0("Manuscript files/figures/Traceplots/",
           "Ukr_Sold_killed_phat1_25k_1",
           suffix, ".png"), 
    width = 800, height = 600)
plot(phat_1[,1], cex.lab=1.5, cex.axis = 1.8)
dev.off()
# geweke
set.seed(7)
rand_ind = sample(1:ncol(phat_1),size = 100, replace = F) 
z = gewekediag(phat_1[,rand_ind], frac1 = 0.1, frac2 = 0.5) 
p_vals = pnorm(-1*abs(z$z),0,1) + (1-pnorm(abs(z$z),0,1))
png(paste0("Manuscript files/figures/Geweke_pvals/",
           "Ukr_Sold_killed_phat1_25k",
           suffix, ".png"), 
    width = 800, height = 600)
hist(p_vals, main = '', cex.lab=1.5, cex.axis = 1.8) # cf. sample traceplots --> non convergence for most 
dev.off()

# Sample OMCE 'Traceplot'
omce = phat_1 - phat_0
png(paste0("Manuscript files/figures/Traceplots/",
           "Ukr_Sold_killed_omce_25k_1",
           suffix, ".png"), 
    width = 800, height = 600)
plot(omce[,1], cex.lab=1.5, cex.axis = 1.8)
dev.off()
# geweke
set.seed(7)
rand_ind = sample(1:ncol(omce),size = 100, replace = F) 
z = gewekediag(omce[,rand_ind], frac1 = 0.1, frac2 = 0.5) 
p_vals = pnorm(-1*abs(z$z),0,1) + (1-pnorm(abs(z$z),0,1))
png(paste0("Manuscript files/figures/Geweke_pvals/",
           "Ukr_Sold_killed_omce_25k",
           suffix, ".png"), 
    width = 800, height = 600)
hist(p_vals, main = '', cex.lab=1.5, cex.axis = 1.8) # cf. sample traceplots --> non convergence for most 
dev.off()
    
    
    
# Vector to store attribute names (for future function calls)
out_levels <- c()

# for (i in 1:length(attribs)) {
  
# i=1 # --> Ukr. Soldiers Killed 
i=9 # --> Sovereignity/Polit. Self. Determination

message("Calculating OMCEs for attribute: ", attribs[i], " [",i,"/",length(attribs),"]")

att_levels <- unique(data[[attribs[i]]][data[[attribs[i]]] != ref_levels[i]])

out_levels <- c(out_levels, as.character(att_levels))

X_pred0 <- data_predict

X_pred0 <- .char_to_fact(X_pred0)

X_pred0[[attribs[i]]] <- factor(ref_levels[i],
                                levels = levels(X_pred0[[attribs[i]]]))
    
    
if (type == "choice") {
  #' *Warnings suppressed in predict function!*
  phat_0 <- # .quiet(
    
    predict(
      
      model,
      newdata = BART::bartModelMatrix(X_pred0),
      mc.cores = cores
      # )
    )$prob.test
} else {
  phat_0 <- .quiet(
    
    predict(
      
      model,
      newdata = BART::bartModelMatrix(X_pred0),
      mc.cores = cores
    )
  )
}
    
# for (att_level in att_levels) {

att_level = att_levels[1] # compare to the first comparison level
# use same profiles 
X_pred1 <- X_pred0
# but change the attribute level to the level of interest
X_pred1[[attribs[i]]] <- factor(att_level,
                                levels = levels(X_pred0[[attribs[i]]]))

# Get predictions
if (type == "choice") {
  phat_1 <- # .quiet(
    
    predict(
      
      model,
      newdata = BART::bartModelMatrix(X_pred1),
      mc.cores = cores
      
      # )
    )$prob.test
} else {
  phat_1 <- .quiet(
    
    predict(
      
      model,
      newdata = BART::bartModelMatrix(X_pred1),
      mc.cores = cores
      
    )
  )
}
      
      
    
    
    
# Plot and Save Traceplots of the Draws
# dynamically generate plot names
if (large == T){
  suffix = '_large'
} else {
  suffix = ''
}
# Attribute 9: Sovereignity (Full vs. EU/NATO)
# phat0
png(paste0("Manuscript files/figures/Traceplots/",
           "Sovereignity_phat0_Full_80088",
           suffix, ".png"), 
    width = 800, height = 600)
plot(phat_0[,80088], cex.lab=1.5, cex.axis = 1.8)
dev.off()
# use BART's geweke diagnostic
# --> calculates a z-score test of equality of means for the first
# frac1 % and the last frac2% of each of the chains
# --> i.e., 80k tests (takes very long)
# --> test 100 random chains using gewekes diag (two-tailed)
set.seed(7)
rand_ind = sample(1:ncol(phat_0),size = 100, replace = F) 
z = gewekediag(phat_0[,rand_ind], frac1 = 0.1, frac2 = 0.5) 
p_vals = pnorm(-1*abs(z$z),0,1) + (1-pnorm(abs(z$z),0,1))
png(paste0("Manuscript files/figures/Geweke_pvals/",
           "Sovereignity_phat0_Full",
           suffix, ".png"), 
    width = 800, height = 600)
hist(p_vals, main = '', cex.lab=1.5, cex.axis = 1.8) # cf. sample traceplots --> non convergence for most 
dev.off()

# phat1
png(paste0("Manuscript files/figures/Traceplots/",
           "Sovereignity_phat1_EU_NATO_80088",
           suffix, ".png"), 
    width = 800, height = 600)
plot(phat_1[,80088], cex.lab=1.5, cex.axis = 1.8)
dev.off()
# geweke
set.seed(7)
rand_ind = sample(1:ncol(phat_1),size = 100, replace = F) 
z = gewekediag(phat_1[,rand_ind], frac1 = 0.1, frac2 = 0.5) 
p_vals = pnorm(-1*abs(z$z),0,1) + (1-pnorm(abs(z$z),0,1))
png(paste0("Manuscript files/figures/Geweke_pvals/",
           "Sovereignity_phat1_EU_NATO",
           suffix, ".png"), 
    width = 800, height = 600)
hist(p_vals, main = '', cex.lab=1.5, cex.axis = 1.8) # cf. sample traceplots --> non convergence for most 
dev.off()

# Get sample OMCE 'Traceplot'
omce = phat_1 - phat_0
png(paste0("Manuscript files/figures/Traceplots/",
           "Sovereignity_omce_EU_NATO_80088",
           suffix, ".png"), 
    width = 800, height = 600)
plot(omce[,80088], cex.lab=1.5, cex.axis = 1.8)
dev.off()
# geweke
set.seed(7)
rand_ind = sample(1:ncol(omce),size = 100, replace = F) 
z = gewekediag(omce[,rand_ind], frac1 = 0.1, frac2 = 0.5) 
p_vals = pnorm(-1*abs(z$z),0,1) + (1-pnorm(abs(z$z),0,1))
png(paste0("Manuscript files/figures/Geweke_pvals/",
           "Sovereignity_omce_EU_NATO",
           suffix, ".png"), 
    width = 800, height = 600)
hist(p_vals, main = '', cex.lab=1.5, cex.axis = 1.8) # cf. sample traceplots --> non convergence for most 
dev.off()


    
#     # Get OMCE for single attribute-level comparison and store
#     results[[as.character(att_level)]] <- colMeans(phat_1) - colMeans(phat_0)
#     var_omce[[as.character(att_level)]] <- apply(phat_1 - phat_0, 2, stats::var)
#     
#     if (method == "bayes") {
#       
#       # Save interval as vector to make code easier to read.
#       intvl <- c(alpha/2, (1-alpha/2))
#       
#       # Calculate distribution of marginal effects
#       var_z <- phat_1 - phat_0
#       
#       # Calculate IMCE interval at this point to avoid holding many frames in memory
#       imce_ci <- sapply(unq_ids, function(s) stats::quantile(var_z[,data[[id]] == s], intvl))
#       
#       imce_lower[[as.character(att_level)]] <- imce_ci[1,]
#       imce_upper[[as.character(att_level)]] <- imce_ci[2,]
#       
#       rm(var_z, imce_ci)
#       
#     }
#     
#     rm(X_pred1, phat_1)
#     gc()
#     
#   }
#   
# }
# 
# ## IMCE
# 
# message("Calculating IMCEs")
# 
# covars <- results[,!(names(results) %in% c(out_levels))]
# 
# # In case only id is supplied, make sure covariates stored as data.frame
# if (is.data.frame(covars)) {
#   covars <- covars[!duplicated(covars),]
# } else {
#   covars <- data.frame(covars[!duplicated(covars)])
#   names(covars) <- id
# }
# 
# agg_formula <- stats::as.formula(
#   paste0(
#     "cbind(",
#     paste0(paste0("`",out_levels,"`"), collapse = ", "),
#     ") ~ ",
#     id
#   )
# )
# 
# results_imce <- stats::aggregate(agg_formula,
#                                  data = results,
#                                  FUN = mean)
# 
# # Double check nrow now we have covariates recovered
# if (!(nrow(covars) == nrow(results_imce))) {
#   
#   warning("Number of unique covariate rows does not match number of ids -- attempting to merge data, but please check results.")
#   
# }
# 
# results_imce <- merge(results_imce, covars, by = id)
# 
# if (method == "bayes") {
#   
#   # Note bounds are calculated above to prevent having to OMCE draws
#   imce_upper <- as.data.frame(imce_upper)
#   imce_lower <- as.data.frame(imce_lower)
#   
#   imce_lower[[id]] <- imce_upper[[id]] <- unq_ids
#   
# } else if (method == "rubin") {
#   
#   results_var <- sapply(colnames(var_omce), function (x) {
#     sapply(results_imce[[id]], function (y) {
#       
#       .combine(theta = results[results[[id]] == y, x],
#                var_theta = var_omce[results[[id]] == y,x])
#     }
#     )
#   })
#   
#   imce_upper <- sapply(colnames(results_var), function (x) results_imce[[x]] + stats::qnorm(1-(alpha/2))*results_var[,x])
#   imce_lower <- sapply(colnames(results_var), function (x) results_imce[[x]] + stats::qnorm(alpha/2)*results_var[,x])
#   
#   imce_upper <- as.data.frame(imce_upper)
#   imce_lower <- as.data.frame(imce_lower)
#   
#   imce_lower[[id]] <- imce_upper[[id]] <- results_imce[[id]]
#   
# } else if (method == "average") {
#   
#   results_var <- sapply(colnames(var_omce), function (x) {
#     sapply(results_imce[[id]], function (y) {
#       
#       mean(var_omce[results[[id]] == y,x])
#     }
#     )
#   })
#   
#   imce_upper <- sapply(colnames(results_var), function (x) results_imce[[x]] + stats::qnorm(1-(alpha/2))*sqrt(results_var[,x]))
#   imce_lower <- sapply(colnames(results_var), function (x) results_imce[[x]] + stats::qnorm(alpha/2)*sqrt(results_var[,x]))
#   
#   imce_upper <- as.data.frame(imce_upper)
#   imce_lower <- as.data.frame(imce_lower)
#   
#   imce_lower[[id]] <- imce_upper[[id]] <- results_imce[[id]]
# }
# 
# out_obj <- list(imce = results_imce,
#                 imce_lower = imce_lower,
#                 imce_upper = imce_upper,
#                 alpha = alpha,
#                 att_levels = out_levels,
#                 id = id,
#                 att_lookup = att_lookup,
#                 omce = NULL,
#                 imce_var = NULL,
#                 round = NULL)
# 
# # Fill in optional elements
# 
# if (keep_omce) {
#   out_obj$omce <- results
# }
# 
# if (method %in% c("rubin","average")) {
#   out_obj$imce_var = results_var
# }
# 
# if (!is.null(round)) {
#   out_obj$round = round
# }
# 
# class(out_obj) <- "cjbart"
# 
# return(out_obj)



