#### Functions for model comparision in the tree planting project #### 

# The strategy here is to use a ragged array to hold the fixed-effect variables to be included in each model. This ragged array will hold the fixed effects for every model we want to test. It is created at the beginning for the "field_data_analysis_planting_time.R" and "field_data_analysis_proportion_pine.R" scripts. 

#The function make_formula() can take these plus the response variable and random effects groupings, if any, and make a formula. 

# The function fit_lmer_model() calls make_formula() and fits that model, returning the model object. 

make_formula <- function(response, predictors, groups = NULL) {
  if (is.null(response) | is.null(predictors)) {
    print("Missing response or predictors")
    return()
  }
  formula_out <- paste0(response, " ~ ", paste(predictors, collapse = " + "))
  if (!is.null(groups)) {
    for (i in groups) formula_out <- paste0(formula_out, " + (1|", i, ")")
  }
  return(formula_out)
}

# testing
# z = make_formula("response", c("pred1", "pred2") , groups ="Fire")
 
add_interaction <- function(formula, interaction_to_add) {
  if (is.null(formula) | is.null(interaction_to_add)) {
    print("Missing formula or interaction to add")
    return()
  }
  formula_out <- paste0(formula, " + ", interaction_to_add)
  return(formula_out)
}

# testing
add_interaction(z, "twi:fsplanted")
