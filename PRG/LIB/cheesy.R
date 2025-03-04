### Note: I did not write this function. This function was found here:
# https://osf.io/27d69

#Robust linear mixed modeling (LMM) output functions####
#"cheesy" function for model evaluation with robust standard errors, the function can be used instead of the regular summary() function (it is called cheesy in reference to the used sandwich estimator)
#robust se = robust square root of: squared mean deviation (or variance) (to be found in diagonal of variance-covariance matrix)
cheesy = function(model_norm){#the dynamic part of the function is the model => model_norm is the non-robust model (i.e., the model with normality of residuals problem) assigned with "curly curly" brackets
  #create output and replace se with robust se
  cat("Robust SE \n")#write title for showing new standard errors, they are the square roots of the diagonals of the new variance-covariance matrix (standard errors are square roots of the variances)
  print(cbind(sqrt(diag(clubSandwich::vcovCR({{model_norm}},type="CR2")))))#print the robust se, cbind already gives name, print only does not, therefore colnames (line above) is not needed
  cat(" \n")#new paragraph
  cat("Model Summary \n")#write model summary
  print(parameters::model_parameters({{model_norm}},df_method = "satterthwaite",details=TRUE,robust = TRUE,vcov_estimation = "vcovCR", vcov_type = "CR2"))#print complete summary of model with sandwich estimation
  cat(" \n")#new paragraph
  cat("Model Performance \n")#write model summary
  print(performance::model_performance({{model_norm}}))#print model performance indicators
  return(model_norm)#return puts model in the object to which the function is applied and return stops all code that would follow afterward
}
