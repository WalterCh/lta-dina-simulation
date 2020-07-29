# new simulation
Q <- matrix(c(1,0,0,0,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,
              1,0,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,0,1,1,
              1,1,0,1,0,1,1,0,0,1,1,0,0,0), 
            ncol = 4, nrow = 20, byrow = TRUE)

items  <- nrow(Q)
skills <- ncol(Q)

# slip & guess parameters
set.seed(42)

slip_par  <- runif(items, min = .1, max = .3)
guess_par <- runif(items, min = .1, max = .3)

# alphas
sigma <- matrix(c(1, .5, .5, .5, 
                  .5, 1, .5, .5,
                  .5, .5, 1, .5,
                  .5, .5, .5, 1), skills, skills)
alpha_t1 <- as.matrix(mvrnorm(1000, mu = rep(0, skills), sigma)) 
alpha_t2 <- as.matrix(mvrnorm(1000, mu = rep(0, skills), sigma)) + 1

# desired alphas proportions
alpha_t1[,1] <- alpha_t1[,1] - qnorm(p = .6, mean = mean(alpha_t1[,1]), 
                                     sd = sd(alpha_t1[,1]))
alpha_t1[,2] <- alpha_t1[,2] - qnorm(p = .65, mean = mean(alpha_t1[,2]), 
                                     sd = sd(alpha_t1[,2]))
alpha_t1[,3] <- alpha_t1[,3] - qnorm(p = .9, mean = mean(alpha_t1[,3]), 
                                     sd = sd(alpha_t1[,3]))
alpha_t1[,4] <- alpha_t1[,4] - qnorm(p = .75, mean = mean(alpha_t1[,4]), 
                                     sd = sd(alpha_t1[,4]))

alpha_t1_dico <- ifelse(alpha_t1 >= 0, 1, 0)
colMeans(alpha_t1_dico)

# dichotomized alphas
alpha_t2_dico <- ifelse(alpha_t2 >= 0, 1, 0)

alphas           <- data.frame(alpha_t1_dico, alpha_t2_dico)
colnames(alphas) <- c(paste(paste("X", 1:4, sep = ""), "T1", sep = "_"),
                      paste(paste("X", 1:4, sep = ""), "T2", sep = "_"))

# response simulation
# T=1
x_s1         <- matrix(1, nrow = 1000, ncol = items)
resp_dina_t1 <- matrix(1, nrow = 1000, ncol = items)

for (i_alpha in 1:nrow(alpha_t1_dico)) {
  for (i_item in 1:items) {
    for (i_skill in skills) {
      x_s1[i_alpha,i_item] <- 
        x_s1[i_alpha,i_item] * (alpha_t1_dico[i_alpha,i_skill]^Q[i_item,i_skill])
    }
    resp_dina_t1[i_alpha,i_item] <- 
      (1 - slip_par[i_item])^x_s1[i_alpha,i_item] * 
      guess_par[i_item]^(1 - x_s1[i_alpha,i_item])
  }
}

u1 <- matrix(runif(1000*items, 0, 1), 1000, items)
resp_dina_t1_dico <- ifelse(resp_dina_t1 >= u1, 1, 0)

# T=2
x_s2         <- matrix(1, nrow = 1000, ncol = items)
resp_dina_t2 <- matrix(1, nrow = 1000, ncol = items)

for (i_alpha in 1:nrow(alpha_t2_dico)) {
  for (i_item in 1:items) {
    for (i_skill in skills) {
      x_s2[i_alpha,i_item] <- 
        x_s2[i_alpha,i_item] * (alpha_t2_dico[i_alpha,i_skill]^Q[i_item,i_skill])
    }
    resp_dina_t2[i_alpha,i_item] <- 
      (1 - slip_par[i_item])^x_s2[i_alpha,i_item] * 
      guess_par[i_item]^(1 - x_s2[i_alpha,i_item])
  }
}

u2 <- matrix(runif(1000*items, 0, 1), 1000, items)
resp_dina_t2_dico <- ifelse(resp_dina_t2 >= u2, 1, 0)

# param sim vs est (T=1)
params_dina_t1 <- din(data = resp_dina_t1_dico,
                      q.matrix = Q)

data.frame(slip_sim = slip_par,
           slip_est = round(params_dina_t1$slip$est, 4),
           guess_sim = guess_par,
           guess_est = round(params_dina_t1$guess$est, 4))

# param sim vs est(T=2)
params_dina_t2 <- din(data = resp_dina_t2_dico,
                      q.matrix = Q)

data.frame(slip_sim = slip_par,
           slip_est = params_dina_t2$slip$est)

# Transition matrix
trans_mat <- alphas %>% 
  as.tbl() %>% 
  mutate(X1_nm = ifelse(X1_T1 == 0 & X1_T2 == 1, 1, 0),
         X1_mm = ifelse(X1_T1 == 1 & X1_T2 == 1, 1, 0),
         X1_mn = ifelse(X1_T1 == 1 & X1_T2 == 0, 1, 0),
         X1_nn = ifelse(X1_T1 == 0 & X1_T2 == 0, 1, 0),
         X2_nm = ifelse(X2_T1 == 0 & X2_T2 == 1, 1, 0),
         X2_mm = ifelse(X2_T1 == 1 & X2_T2 == 1, 1, 0),
         X2_mn = ifelse(X2_T1 == 1 & X2_T2 == 0, 1, 0),
         X2_nn = ifelse(X2_T1 == 0 & X2_T2 == 0, 1, 0),
         X3_nm = ifelse(X3_T1 == 0 & X3_T2 == 1, 1, 0),
         X3_mm = ifelse(X3_T1 == 1 & X3_T2 == 1, 1, 0),
         X3_mn = ifelse(X3_T1 == 1 & X3_T2 == 0, 1, 0),
         X3_nn = ifelse(X3_T1 == 0 & X3_T2 == 0, 1, 0),
         X4_nm = ifelse(X4_T1 == 0 & X4_T2 == 1, 1, 0),
         X4_mm = ifelse(X4_T1 == 1 & X4_T2 == 1, 1, 0),
         X4_mn = ifelse(X4_T1 == 1 & X4_T2 == 0, 1, 0),
         X4_nn = ifelse(X4_T1 == 0 & X4_T2 == 0, 1, 0)) %>% 
  dplyr::select(X1_nm:X4_nn)

matrix(c(mean(trans_mat$X1_nn), mean(trans_mat$X1_mn), 
         mean(trans_mat$X1_nm), mean(trans_mat$X1_mm)), 
       2, 2)
matrix(c(mean(trans_mat$X2_nn), mean(trans_mat$X2_mn), 
         mean(trans_mat$X2_nm), mean(trans_mat$X2_mm)), 
       2, 2)
