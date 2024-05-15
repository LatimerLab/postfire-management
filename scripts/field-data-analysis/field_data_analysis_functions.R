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

# From an lmer model, extract the response, main effects, interactions, and grouping variables and return a list containing those (the attributes of the list are "response", "main_effects_vector", "interaction_vector", and "groups")
extract_model_terms <- function(m) {
  require(stringr)
  # Get the response variable, grouping variable, and lists of main effects and interaction terms from the model formula
  terms <- attributes(terms(m))
  response <- all.vars(terms(m))[1]
  model_terms <- terms$term.labels # Get list of variables including main effects and interactions 
  interaction_index <- str_detect(model_terms, ":")
  main_effects_vector <- model_terms[!interaction_index]
  interaction_vector <- model_terms[interaction_index]
  groups <- names(ranef(m))
  model_terms <- list(response = response, main_effects_vector = main_effects_vector, interaction_vector = interaction_vector, groups = groups)
  return(model_terms)
}

# Return a list of "drop1" subsets of a vector, so that each subset in the list has one element of the vector sequentially deleted. 
vector_drop1 <- function(v) {
  vector_list_drop1 <- list() 
  for (i in 1:length(v)) vector_list_drop1[[i]] <- v[-i]
  return(vector_list_drop1)
}

return_reduced_main_effects <- function(m) { # Function takes an lmer model as input, and returns a list of formulas that are all the formulas for models that are minus one main effect (as in drop1) 
  model_terms <- extract_model_terms(m) # get info for model formulas
  main_effects_drop1 <- vector_drop1(model_terms$main_effects_vector) # create list of main effects with one dropped sequentially
  formulas_drop1 <- list() 
  for (i in 1:length(main_effects_drop1)) { 
    formulas_drop1[[i]] <- make_formula(response = model_terms$response, predictors = c(main_effects_drop1[[i]], model_terms$interaction_vector), groups = model_terms$groups)
  }
  return(formulas_drop1)
}

# This function takes a current model, m, and tests all nested subsets of the model that sequentially remove one main effect. 
backwards_eliminate <- function(m) {
  require(lme4)
  # Get the response, fixed effect variables, and random effect variables from the model object
  formulas_drop1 <- return_reduced_main_effects(m)
  model_list_drop1 <- lapply(formulas_drop1, lmer, data = m@frame, REML = FALSE)
  # Get the AIC of the fitted models and find the one with lowest AIC
  AIC_drop1 <- unlist(lapply(model_list_drop1, AIC))
  lowest_AIC_variable <- which.min(AIC_drop1)
  lowest_AIC <- AIC(model_list_drop1[[lowest_AIC_variable]])
  lowest_AIC_model <- model_list_drop1[[lowest_AIC_variable]]
  # Check if the lowest AIC is more than 2 points higher than the AIC of the full model. If so, keep all the variables. If not, drop the variable whose removal leaves the model with the lowest AIC. 
  if ((lowest_AIC - AIC(m) < 2)) {
    variable_to_remove <- attributes(terms(m))$term.labels[lowest_AIC_variable]
    print(paste0("The least explanatory variable is ", variable_to_remove, "."))
    return(lowest_AIC_model)
  }
  else {
    print("Eliminating any main effect increases AIC by >=2 points. All variables should be retained.")
    return(NULL)
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


