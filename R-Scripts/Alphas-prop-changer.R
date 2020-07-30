alpha_prop_changer <- function(alphas_t, prop) {
  alphas_new <- alphas_t
  for (i_prop in 1:length(prop)) {
    alphas_new[,i_prop] <- alphas_t[,i_prop] - 
      qnorm(p = 1 - prop[i_prop], 
            mean = mean(alphas_t[,i_prop]),
            sd = sd(alphas_t[,i_prop]))
  }
  return(alphas_new)
}

