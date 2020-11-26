val <- 0
while (val == 0) {
  # new simulation
  library(MASS)
  library(CDM)
  library(dplyr)
  library(ggplot2)
  
  source("./R-Scripts/Custom-ggplot2-theme.R")
  
  set.seed(42)
  
  Q <- matrix(c(1,0,0,0,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,
                0,0,1,0,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
                1,0,1,1,1,1,0,1,0,1,1,0,0,1,1,0,0,0), 
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
  alpha_t2 <- alpha_prop_changer(alpha_t2, c(.45, .52, .15, .33))
  alpha_t2_dico <- ifelse(alpha_t2 >= 0, 1, 0)
  
  # response simulation
  x_s2         <- matrix(1, nrow = 1000, ncol = items)
  resp_dina_t2 <- matrix(1, nrow = 1000, ncol = items)
  
  for (i_alpha in 1:nrow(alpha_t2_dico)) {
    for (i_item in 1:items) {
      for (i_skill in 1:skills) {
        x_s2[i_alpha,i_item] <-
          x_s2[i_alpha,i_item] * 
          (alpha_t2_dico[i_alpha,i_skill]^Q[i_item,i_skill])
      }
      resp_dina_t2[i_alpha,i_item] <-
        (1 - slip_par[i_item])^x_s1[i_alpha,i_item] *
        guess_par[i_item]^(1 - x_s1[i_alpha,i_item])
    }
  }
  
  u2 <- matrix(runif(1000*items, 0, 1), 1000, items)
  resp_dina_t2_dico <- ifelse(resp_dina_t2 >= u2, 1, 0)
  
  
  # T=3 ---------------------------------------------------------------------
  
  # alphas
  alpha_t3 <- as.matrix(mvrnorm(1000, mu = rep(0, skills), sigma)) 
  alpha_t3 <- alpha_prop_changer(alpha_t3, c(.73, .68, .25, .60))
  alpha_t3_dico <- ifelse(alpha_t3 >= 0, 1, 0)
  
  # response simulation
  x_s3         <- matrix(1, nrow = 1000, ncol = items)
  resp_dina_t3 <- matrix(1, nrow = 1000, ncol = items)
  
  for (i_alpha in 1:nrow(alpha_t3_dico)) {
    for (i_item in 1:items) {
      for (i_skill in 1:skills) {
        x_s3[i_alpha,i_item] <-
          x_s3[i_alpha,i_item] * 
          (alpha_t3_dico[i_alpha,i_skill]^Q[i_item,i_skill])
      }
      resp_dina_t3[i_alpha,i_item] <-
        (1 - slip_par[i_item])^x_s1[i_alpha,i_item] *
        guess_par[i_item]^(1 - x_s1[i_alpha,i_item])
    }
  }
  
  u3 <- matrix(runif(1000*items, 0, 1), 1000, items)
  resp_dina_t3_dico <- ifelse(resp_dina_t3 >= u3, 1, 0)
  
  
  # T=4 ---------------------------------------------------------------------
  
  # alphas
  alpha_t4 <- as.matrix(mvrnorm(1000, mu = rep(0, skills), sigma)) 
  alpha_t4 <- alpha_prop_changer(alpha_t4, c(.95, .95, .45, .90))
  alpha_t4_dico <- ifelse(alpha_t4 >= 0, 1, 0)
  
  # response simulation
  x_s4         <- matrix(1, nrow = 1000, ncol = items)
  resp_dina_t4 <- matrix(1, nrow = 1000, ncol = items)
  
  for (i_alpha in 1:nrow(alpha_t4_dico)) {
    for (i_item in 1:items) {
      for (i_skill in 1:skills) {
        x_s4[i_alpha,i_item] <-
          x_s4[i_alpha,i_item] * 
          (alpha_t4_dico[i_alpha,i_skill]^Q[i_item,i_skill])
      }
      resp_dina_t4[i_alpha,i_item] <-
        (1 - slip_par[i_item])^x_s1[i_alpha,i_item] *
        guess_par[i_item]^(1 - x_s1[i_alpha,i_item])
    }
  }
  
  u4 <- matrix(runif(1000*items, 0, 1), 1000, items)
  resp_dina_t4_dico <- ifelse(resp_dina_t4 >= u4, 1, 0)
  
  
  # Sim Data ----------------------------------------------------------------
  
  dina_sim <- list(data.frame(resp_dina_t1_dico, resp_dina_t2_dico,
                              resp_dina_t3_dico, resp_dina_t4_dico),
                   list(alpha_t1_dico, alpha_t2_dico, alpha_t3_dico, 
                        alpha_t4_dico))
  
  names(dina_sim) <- c("data", "alphas")
  colnames(dina_sim$data) <- c(paste(paste("X", 1:20, sep = ""), "T1", 
                                     sep = "-"),
                               paste(paste("X", 1:20, sep = ""), "T2", 
                                     sep = "-"),
                               paste(paste("X", 1:20, sep = ""), "T3", 
                                     sep = "-"),
                               paste(paste("X", 1:20, sep = ""), "T4", 
                                     sep = "-"))
  names(dina_sim$alphas) <- c(paste("alpha_", "t", 1:4, sep = ""))
  val <- 1
}




# Patameters retribution --------------------------------------------------

params_dina_t1 <- din(data = dina_sim$data[,1:20],
                      q.matrix = Q, rule = "DINA")
params_dina_t2 <- din(data = dina_sim$data[,21:40],
                      q.matrix = Q, rule = "DINA")
params_dina_t3 <- din(data = dina_sim$data[,41:60],
                      q.matrix = Q, rule = "DINA")
params_dina_t4 <- din(data = dina_sim$data[,61:80],
                      q.matrix = Q, rule = "DINA")

params_dina_t1_tbl <- data.frame(slip_sim = round(slip_par, 3),
           slip_est = round(params_dina_t1$slip$est, 3),
           slip_se = round(params_dina_t1$slip$se, 3),
           guess_sim = round(guess_par, 3),
           guess_est = round(params_dina_t1$guess$est, 3),
           guess_se = round(params_dina_t1$guess$se, 3))

data.frame(slip_sim = round(slip_par, 3),
           slip_est = round(params_dina_t2$slip$est, 3),
           slip_se = round(params_dina_t2$slip$se, 3),
           guess_sim = round(guess_par, 3),
           guess_est = round(params_dina_t2$guess$est, 3),
           guess_se = round(params_dina_t2$guess$se, 3))

data.frame(slip_sim = round(slip_par, 3),
           slip_est = round(params_dina_t3$slip$est, 3),
           slip_se = round(params_dina_t3$slip$se, 3),
           guess_sim = round(guess_par, 3),
           guess_est = round(params_dina_t3$guess$est, 3),
           guess_se = round(params_dina_t3$guess$se, 3))

data.frame(slip_sim = round(slip_par, 3),
           slip_est = round(params_dina_t4$slip$est, 3),
           slip_se = round(params_dina_t4$slip$se, 3),
           guess_sim = round(guess_par, 3),
           guess_est = round(params_dina_t4$guess$est, 3),
           guess_se = round(params_dina_t4$guess$se, 3))

data.frame(slip_sim = round(slip_par, 3),
           slip_est_t1 = round(params_dina_t1$slip$est, 3),
           slip_est_t2 = round(params_dina_t2$slip$est, 3),
           slip_est_t3 = round(params_dina_t3$slip$est, 3),
           slip_est_t4 = round(params_dina_t4$slip$est, 3),
           guess_sim = round(guess_par, 3),
           guess_est_t1 = round(params_dina_t1$guess$est, 3),
           guess_est_t2 = round(params_dina_t2$guess$est, 3),
           guess_est_t3 = round(params_dina_t3$guess$est, 3),
           guess_est_t4 = round(params_dina_t4$guess$est, 3))



# test <- tibble(slip_sim = round(slip_par, 3),
#                slip_est_t1 = round(params_dina_t1$slip$est, 3),
#                slip_se_t1 = round(params_dina_t1$slip$se, 3))
# test %>% 
#   mutate(slip_est_se = paste(slip_est_t1,"(",slip_se_t1,")", sep = "")) %>% 
#   select(slip_sim, slip_est_se)


# Outputs -----------------------------------------------------------------

# Q-Matrix
write.csv(x = Q, file = "./Tablas/Matriz-Q.csv", row.names = FALSE)

# Parameter estimates for time 1
quali_diag_tbl <- params_dina_t1_tbl %>% 
  mutate("guess (SE)" = paste(slip_est, "(", slip_se, ")", sep = ""),
         "slip (SE)" = paste(guess_est, "(", guess_se, ")", sep = ""),
         "quality diag" = ((1 - slip_est)/slip_est)/
           (guess_est/(1 - guess_est))) %>% 
  select("guess (SE)", "slip (SE)", "quality diag")
write.csv(quali_diag_tbl, "./Tablas/Parametros estimados.csv", 
          row.names = FALSE)

# Frequency and Proportions of Mastery of each skill at each time point
prop_master_tbl <- tibble(
  "Habilidades" = c("Número y Operaciones", "Medición",
                    "Solución de problemas", "Representación"),
  T1 = colMeans(alpha_t1_dico), 
  T2 = colMeans(alpha_t2_dico), 
  T3 = colMeans(alpha_t3_dico), 
  T4 = colMeans(alpha_t4_dico))  
write.csv(prop_master_tbl, 
          "./Tablas/Proporcion de maestria por cada tiempo T.csv",
          row.names = FALSE)

# Proportions of mastery patterns over four skills at each time point
alpha_dico_tbl <- tibble(pattern = c(paste(alpha_t1_dico[,1],
                                           alpha_t1_dico[,2],
                                           alpha_t1_dico[,3],
                                           alpha_t1_dico[,4]),
                                     paste(alpha_t2_dico[,1],
                                           alpha_t2_dico[,2],
                                           alpha_t2_dico[,3],
                                           alpha_t2_dico[,4]),
                                     paste(alpha_t3_dico[,1],
                                           alpha_t3_dico[,2],
                                           alpha_t3_dico[,3],
                                           alpha_t3_dico[,4]),
                                     paste(alpha_t4_dico[,1],
                                           alpha_t4_dico[,2],
                                           alpha_t4_dico[,3],
                                           alpha_t4_dico[,4])),
                         t = rep(1:4, each = 1000))
alpha_dico_pat_tbl <- alpha_dico_tbl %>% 
  group_by(pattern, t) %>% 
  summarise(n = n())

alpha_dico_pat_tbl2 <- tibble(
  pattern = unique(alpha_dico_pat_tbl$pattern),
  T1 = alpha_dico_pat_tbl %>% filter(t == 1) %>% pull(n),
  T2 = alpha_dico_pat_tbl %>% filter(t == 2) %>% pull(n),
  T3 = alpha_dico_pat_tbl %>% filter(t == 3) %>% pull(n),
  T4 = alpha_dico_pat_tbl %>% filter(t == 4) %>% pull(n) %>% append(0, 2))

write.csv(alpha_dico_pat_tbl2, "./Tablas/Proporcion de maestria de patrones de habilidades por cada tiempo.csv", row.names = FALSE)

# plot
prop_master_tbl2 <- tibble(prop = c(prop_master_tbl$T1, prop_master_tbl$T2,
                                    prop_master_tbl$T3, prop_master_tbl$T4),
                           Habilidades = rep(prop_master_tbl$Habilidades, 
                                             times  = 4),
                           t = rep(paste("T", 1:4, sep = ""), each = 4))

prop_master_plot <- ggplot(data = prop_master_tbl2, 
                           aes(x = t, y = prop, fill = Habilidades)) +
  geom_bar(stat = "identity", position = 'dodge') +
  labs(x = element_blank(), y = element_blank(),
       title = "Proporciones de maestría de habilidades en \n cada tiempo T") +
  custom_theme +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

ggsave("./Imagenes/Prop. maestria por cada tiempo T3.pdf", prop_master_plot,
       width = 7, height = 6)
