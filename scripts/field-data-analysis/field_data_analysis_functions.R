#### Functions for model comparision in the tree planting project #### 

# The strategy here is to use a ragged array to hold the fixed-effect variables to be included in each model. This ragged array will hold the fixed effects for every model we want to test. It is created at the beginning for the "field_data_analysis_planting_time.R" and "field_data_analysis_proportion_pine.R" scripts. 

#The function make_formula() can take these plus the response variable and random effects groupings, if any, and make a formula. 

# The function fit_lmer_model() calls make_formula() and fits that model, returning the model object. 

# Make formula for lmer models, given a response and vectors of predictors and groups
make_formula <- function(response, predictors, groups = NULL) {
  if (is.null(response) | is.null(predictors)) {
    print("Missing response or predictors")
    return()
  }
  formula_out <- paste0(response, " ~ ", paste(predictors, collapse = " + "))
  if (!is.null(groups)) {
    for (i in groups) formula_out <- paste0(formula_out, " + (1|", i, ")")
  }
  return(as.formula(formula_out))
}

# Fit lmer model, given a response and vectors of predictors and groups
# Note this is set up so that it works with lapply() over the list of fixed effect vectors -- so the parameter x is a character vector of fixed effects to include in the model. 
fit_lmer_model <- function(x, response, groups = NULL, data){
  if (is.null(response) | is.null(x)) {
    print("Missing response or predictors")
    return()
  }
  mod_form <- make_formula(response, predictors = x, groups)
  m <- lmer(mod_form, data, REML = FALSE, control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
  return(m)
}
  
# testing
# z = make_formula("response", c("pred1", "pred2") , groups ="Fire")
# m = fit_lmer_model("ln.dens.planted", vars_to_test, groups ="Fire", dataset = plot_dhm_for_model)

backwards_eliminate <- function(m, k = 2) {
  # Get the response, fixed effect variables, and random effect variables from the model object
  terms <- all.vars(terms(m))
  response <- terms[1]
  fixef_vector <- terms[2:length(terms)]
  groups <- names(ranef(m))
  # Loop through the fixed effects, dropping one at a time and refitting a reduced model for each reduced set of variables 
  models_drop1 <- list()
  n_fixef <- length(fixef_vector)
  for (i in 1:n_fixef) {
    fixef_drop1 <- fixef_vector[-i]
    models_drop1[[i]] <- fit_lmer_model(x = fixef_drop1, response = response, groups = groups, data = m@frame)
  }
  # Get the AIC of the fitted models and find the one with lowest AIC
  AIC_drop1 <- unlist(lapply(models_drop1, AIC))
  lowest_AIC_variable <- which.min(AIC_drop1)
  lowest_AIC <- AIC(models_drop1[[lowest_AIC_variable]])
  lowest_AIC_model <- models_drop1[[lowest_AIC_variable]]
  
  # Check if the lowest AIC is more than 2 points higher than the AIC of the full model. If so, keep all the variables. If not, drop the variable whose removal leaves the model with the lowest AIC. 
  if (lowest_AIC_model - AIC(m) < 2) {
    print(past0("The least explanatory variable is ", )
  }
}


 
#add_interaction <- function(formula, interaction_to_add) {
#  if (is.null(formula) | is.null(interaction_to_add)) {
#    print("Missing formula or interaction to add")
#    return()
#  }
#  formula_out <- paste0(formula, " + ", interaction_to_add)
#  return(formula_out)
#}


