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

# Check a data frome for missing values and tell which columns have them
check_missing_values <- function(d) {
  missing_count <- apply(d, 2, f <- function(x){return(sum(is.na(x)))})
  return(missing_count)
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

# From an lmer model, extract the response, main effects, interactions, and grouping variables and return a list containing those (the attributes of the list are "response", "main_effects_vector", "interaction_vector", and "groups")
extract_model_terms <- function(m) {
  require(stringr)
  # Get the grouping variables, main effects, and interactions
  terms <- attributes(terms(m))
  response <- all.vars(terms(m))[1]
  model_terms <- terms$term.labels # Get list of variables including main effects and interactions 
  interaction_index <- str_detect(model_terms, ":")
  main_effects_vector <- model_terms[!interaction_index]
  interaction_vector <- model_terms[interaction_index]
    groups <- names(ranef(m))
  # Get the response variable (different for lmer vs glmer)
  model_class <- class(m)[1]
  if (model_class =="lmerModLmerTest") response <- all.vars(terms(m))[1]
    else if (model_class == "glmerMod") response <- rownames(terms$factors)[1]
    
    model_terms <- list(response = response, main_effects_vector = main_effects_vector, interaction_vector = interaction_vector, groups = groups)
  return(model_terms)
}

# Return a list of "drop1" subsets of a vector, so that each subset in the list has one element of the vector sequentially deleted. 
# Optionally can accept the argument fixed, which are variables that won't be removed, and are instead tacked onto every subset. 
vector_drop1 <- function(v, fixed) {
  vector_list_drop1 <- list() 
  for (i in 1:length(v)) vector_list_drop1[[i]] <- c(v[-i], fixed)
  return(vector_list_drop1)
}

return_added_main_effects <- function(m, terms_to_add) { # Function takes an lmer model as input, and returns a list of formulas for models that include each of the variables from terms_to_add, one at a time
  # the argument fixed is a character vector of terms to keep in the model and NOT consider dropping
  model_terms <- extract_model_terms(m) # get info for model formulas
  formulas_add1 <- list() 
  for (i in 1:length(terms_to_add)) { # loop through the main effects and add one at a time
    formulas_add1[[i]] <- make_formula(response = model_terms$response, predictors = c(model_terms$main_effects_vector, terms_to_add[i], model_terms$interaction_vector), groups = model_terms$groups)
  }
  return(formulas_add1 = formulas_add1)
}


return_reduced_main_effects <- function(m, fixed = NULL) { # Function takes an lmer model as input, and returns a list of formulas that are all the formulas for models that are minus one main effect (as in drop1) 
  # the argument fixed is a character vector of terms to keep in the model and NOT consider dropping
  model_terms <- extract_model_terms(m) # get info for model formulas
  if (!is.null(fixed)) {
    index_fixed <- match(fixed, model_terms$main_effects_vector)
    model_terms$main_effects_vector <- model_terms$main_effects_vector[-index_fixed] # remove fixed terms from the set to be tested
  }
  main_effects_drop1 <- vector_drop1(model_terms$main_effects_vector, fixed) # create list of main effects with one dropped sequentially
  formulas_drop1 <- list() 
  for (i in 1:length(model_terms$main_effects_vector)) { # loop through the main effects -- making sure to consider only those in the reduced main effects vector (not the fixed variables that have been appended to it!)
    formulas_drop1[[i]] <- make_formula(response = model_terms$response, predictors = c(main_effects_drop1[[i]], model_terms$interaction_vector), groups = model_terms$groups)
  }
  return(list(formulas_drop1 = formulas_drop1, terms_tested = model_terms$main_effects_vector))
}

# This function takes a current model, m, and tests all nested subsets of the model that sequentially remove one main effect. 
backwards_eliminate <- function(m, fixed = NULL) { ## TO DO: add "fixed" argument for the variables we DON'T want to test for removal, e.g those involved in interactions
  require(lme4)
  # Get the response, fixed effect variables, and random effect variables from the model object
  formulas_drop1 <- return_reduced_main_effects(m, fixed)
  model_list_drop1 <- lapply(formulas_drop1$formulas_drop1, lmer, data = m@frame, REML = FALSE)
  # Get the AIC of the fitted models and find the one with lowest AIC
  AIC_drop1 <- unlist(lapply(model_list_drop1, AIC))
  lowest_AIC_variable <- which.min(AIC_drop1)
  lowest_AIC <- AIC(model_list_drop1[[lowest_AIC_variable]])
  lowest_AIC_model <- model_list_drop1[[lowest_AIC_variable]]
  # Check if the lowest AIC is more than 2 points higher than the AIC of the full model. If so, keep all the variables. If not, drop the variable whose removal leaves the model with the lowest AIC. 
  if ((lowest_AIC - AIC(m)) < 2) {
    variable_to_remove <- formulas_drop1$terms_tested[lowest_AIC_variable]
    print(paste0("The least explanatory variable is ", variable_to_remove, "."))
    return(list(model_list = model_list_drop1, AIC_values <- AIC_drop1))
  }
  else {
    print("Eliminating any main effect increases AIC by >=2 points. All variables should be retained.")
    return(NULL)
  }
}

# This function takes a current model, m, then tests all models that are single-variable additions to the base model. The variables to test are the character vector terms_to_add. The data frame data contains all the variables in the model plus the variables to be tested. 
forward_select <- function(m, data, terms_to_add) {
  formulas_add1 <- return_added_main_effects(m, terms_to_add)
  models_add1 <- lapply(formulas_add1, glmer, data = plot_dhm_pine_std, family = "binomial")
  # Check if the lowest AIC is more than 2 points higher than the AIC of the base model. If so, don't add any variables. If so, add the variable that improves the model AIC the most. 
  AIC_add1 <- unlist(lapply(models_add1, AIC))
  lowest_AIC_variable <- which.min(AIC_add1)
  lowest_AIC <- AIC(models_add1[[lowest_AIC_variable]])
  lowest_AIC_model <- models_add1[[lowest_AIC_variable]]
  
  if ((lowest_AIC - AIC(m)) < 2) {
    variable_to_add <- terms_to_add[lowest_AIC_variable]
    print(paste0("The most explanatory variable is ", variable_to_add, "."))
    return(models_add1[[lowest_AIC_variable]])
  }
}

# Test: 
#forward_select(pines_base_model, terms_to_add = vars_to_test[1:12])
