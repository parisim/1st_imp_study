### Note: I did not write this function. This function was found here: 
# https://osf.io/27d69

#cheesy function for the likelihood-ratio test (LRT); allows a model comparison with robust se; can be used instead of anova(m1, m2) command
cheesylrt = function(m1, m2){
  print(stats::anova(m1, m2))#print LRT for both model
  m1_ml = update(m1, REML=FALSE)#update to ML estimator, to avoid discrepancy in AIC between anova output (automatically refits to ML) and compare_performance output (does not)
  m2_ml = update(m2, REML=FALSE)#update to ML estimator, to avoid discrepancy in AIC between anova output and compare_performance
  print(performance::compare_performance(m1_ml,m2_ml))#print performance
}


