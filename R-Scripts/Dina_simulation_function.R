# Function (based on 'R Data Simulation code for the DINA model' - Kaya, 2015)
dina_data_sim <- function(slip_guess_min, slip_guess_max, cor_skills = .5, 
                          N = 1000, Q) {
  
  # Required packages
  require(MASS)
  
  # parameter from the Q matrix
  items  <- nrow(Q)
  skills <- ncol(Q)
  
  # examinees (students)
  set.seed(42)
  slip_par  <- runif(items, slip_guess_min, slip_guess_max)
  guess_par <- runif(items, slip_guess_min, slip_guess_max)
  
  # alphas from a probit model with a multivariate normal vector with mean zero
  # and correlations between the skills fixed at 0.5 (sd = 1)
  sigma <- matrix(c(1, cor_skills, cor_skills, cor_skills, 
                    cor_skills, 1, cor_skills, cor_skills,
                    cor_skills, cor_skills, 1, cor_skills,
                    cor_skills, cor_skills, cor_skills, 1), skills, skills)
  alpha_t1 <- as.matrix(mvrnorm(N, mu = rep(0, skills), sigma)) # alpha for T1
  alpha_t2 <- as.matrix(mvrnorm(N, mu = rep(0, skills), sigma)) # alpha for T2
  
  # elements of alpha_t are changed to 0 if the value is negative or 1 otherwise
  # creating a dichotomus mastery indicator
  alpha_dico_t1 <- ifelse(alpha_t1 >= 0, 1, 0)
  alpha_dico_t2 <- ifelse(alpha_t2 >= 0, 1, 0)
  
  # item response generation
  # Time 1 ----
  x_s1 <- matrix(1, nrow = N, ncol = items) # Response matrix 'empty' with 1's
  resp_dina1 <- matrix(1, nrow = N, ncol = items)
  
  for (g in 1:nrow(alpha_dico_t1)) {
    for (h in 1:items) {
      for (a in 1:skills) {
        x_s1[g,h] <- x_s1[g,h] * (alpha_dico_t1[g,a]^Q[h,a])
      }
      resp_dina1[g,h] <- (1 - slip_par[h])^x_s1[g,h] * 
        guess_par[h]^(1 - x_s1[g,h])
    }
  }
  
  # generating a random u matrix of random numbers between 0 and 1
  u1 <- matrix(runif(N*items, 0, 1), N, items)
  
  # dichotomizing the response
  resp_dic_dina1 <- ifelse(resp_dina1 >= u1, 1, 0)
  
  # Time 2 ----
  x_s2 <- matrix(1, nrow = N, ncol = items)
  resp_dina2 <- matrix(1, nrow = N, ncol = items)
  
  for (g in 1:nrow(alpha_dico_t2)) {
    for (h in 1:items) {
      for (a in 1:skills) {
        x_s2[g,h] <- x_s2[g,h] * (alpha_dico_t2[g,a]^Q[h,a])
      }
      resp_dina2[g,h] = (1 - slip_par[h]^x_s2[g,h]) * 
        guess_par[h]^(1 - x_s2[g,h])
    }
  }
  
  # generating a random u matrix of random numbers between 0 and 1
  u2 <- matrix(runif(N*items, 0, 1), N, items)
  
  # dichotomizing the response
  resp_dic_dina2 <- ifelse(resp_dina2 >= u2, 1, 0)
  
  # Simulated Data Frame combined
  id <- c(1:N)
  resp_dic_dina <- data.frame(cbind(id, resp_dic_dina1, resp_dic_dina2))
  names(resp_dic_dina) <- c("id", paste0("X", 1:40))
  alphas <- data.frame(cbind(id, alpha_dico_t1, alpha_dico_t2))
  
  # Return list
  return_list <- list(resp_dic_dina[, -1], alphas, slip_par, guess_par)
  names(return_list) <- c("sim_dina", "alphas", "slip_par", "guess_par")
  
  return(return_list)
}