### Note: I did not write this function. This function was found here: 
# https://osf.io/27d69

#function sig_pwc for printing sign. and non-sign. pairwise comparisonsthi
sig_pwc = function(table, p.name="p.value", sig=TRUE){#p.value is default as name of p value name (p.name), in contrasts, p.name is called p.value
  if(sig==TRUE){
    data.frame(table) %>% #filters statistically significant contrasts and gives them asterisks
      dplyr::filter(get(p.name) < 0.05) %>% #get only those samlle or 
      dplyr::mutate(rounded = round(get(p.name), 3)) %>%#rounds to three digits
      dplyr::mutate(sig = dplyr::if_else(get(p.name)  < 0.001, "***",#create a variable sig for the p.values with .001 get 3 *,...
                                         dplyr::if_else(get(p.name)  < 0.01, "**", 
                                                        dplyr::if_else(get(p.name) < 0.05, "*", ""))))
  }
  else{
    data.frame(table) %>% #gives all non-significant contrasts
      dplyr::filter(get(p.name) >= 0.05) 
  }
}
