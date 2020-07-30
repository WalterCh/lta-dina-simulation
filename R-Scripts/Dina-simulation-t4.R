# new simulation
library(MASS)
library(CDM)

set.seed(42)

Q <- matrix(c(1,0,0,0,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,
              1,0,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,0,1,1,
              1,1,0,1,0,1,1,0,0,1,1,0,0,0), 
            ncol = 4, nrow = 20, byrow = TRUE)

items  <- nrow(Q)
skills <- ncol(Q)

slip_par  <- runif(items, min = .1, max = .3)
guess_par <- runif(items, min = .1, max = .3)

sigma <- matrix(c(1, .5, .5, .5,
                  .5, 1, .5, .5,
                  .5, .5, 1, .5,
                  .5, .5, .5, 1),
                skills, skills)

# T=1 ---------------------------------------------------------------------

# alphas
alpha_t1 <- as.matrix(mvrnorm(1000, mu = rep(0, skills), sigma)) 

source("./R-Scripts/Alphas-prop-changer.R")

alpha_t1 <- alpha_prop_changer(alphas_t = alpha_t1, 
                               prop = c(.4, .35, .1, .25))
alpha_t1_dico <- ifelse(alpha_t1 >= 0, 1, 0)

# response simulation
x_s1         <- matrix(1, nrow = 1000, ncol = items)
resp_dina_t1 <- matrix(1, nrow = 1000, ncol = items)

for (i_alpha in 1:nrow(alpha_t1_dico)) {
  for (i_item in 1:items) {
    for (i_skill in 1:skills) {
      x_s1[i_alpha,i_item] <-
        x_s1[i_alpha,i_item] * 
        (alpha_t1_dico[i_alpha,i_skill]^Q[i_item,i_skill])
    }
    resp_dina_t1[i_alpha,i_item] <-
      (1 - slip_par[i_item])^x_s1[i_alpha,i_item] *
      guess_par[i_item]^(1 - x_s1[i_alpha,i_item])
  }
}

u1 <- matrix(runif(1000*items, 0, 1), 1000, items)
resp_dina_t1_dico <- ifelse(resp_dina_t1 >= u1, 1, 0)

# T=2 ---------------------------------------------------------------------

# alphas
alpha_t2 <- as.matrix(mvrnorm(1000, mu = rep(0, skills), sigma)) 
alpha_t2 <- alpha_prop_changer(alpha_t2, c(.4, .35, .1, .25))
alpha_t2_dico <- ifelse(alpha_t2 >= 0, 1, 0)


# Patameters retribution --------------------------------------------------

params_dina_t1 <- din(data = resp_dina_t1_dico,
                      q.matrix = Q, rule = "DINA")

data.frame(slip_sim = round(slip_par, 2),
           slip_est = round(params_dina_t1$slip$est, 2),
           slip_se = round(params_dina_t1$slip$se, 2),
           guess_sim = round(guess_par, 2),
           guess_est = round(params_dina_t1$guess$est, 2),
           guess_se = round(params_dina_t1$guess$se, 2))

















