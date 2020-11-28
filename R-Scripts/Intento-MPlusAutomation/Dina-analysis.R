## LTA-DINA Data Simulation
## Ronald Minchola
## Walter Chanava
## June 10th

# Data Simulation ---------------------------------------------------------

# function in Scripts/data_sim_function.R
source("./R-Scripts/Dina_simulation_function.R")

# Q- matrix
Q <- matrix(c(1,0,0,0,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,
              1,0,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,0,1,1,
              1,1,0,1,0,1,1,0,0,1,1,0,0,0), 
            ncol = 4, nrow = 20, byrow = TRUE)

dina <- dina_data_sim(slip_guess_min = .1, slip_guess_max = .3, Q = Q)

# Packages
library(MplusAutomation)
library(dplyr)
library(ggplot2)
library(extrafont)
library(latex2exp)
library(CDM)

# Preparing data
prepareMplusData(dina$sim_dina, "../Data/lta_dina.dat")

# Model
runModels("C:/Users/Usuario/Dropbox/Dina/mplus", showOutput = TRUE)
output2 <- readModels("C:/Users/Usuario/Dropbox/Dina/mplus/lta_dina.out")

# Savedata analysis
savedata <- as.tbl(output2$savedata)
savedata_prof <- savedata %>%
  select(C1:C8) %>% 
  mutate(C1 = ifelse(C1 == 2, 1, 0), # 2 es dominio y 1 es no dominio
         C2 = ifelse(C2 == 2, 1, 0),
         C3 = ifelse(C3 == 2, 1, 0),
         C4 = ifelse(C4 == 2, 1, 0),
         C5 = ifelse(C5 == 2, 1, 0),
         C6 = ifelse(C6 == 2, 1, 0),
         C7 = ifelse(C7 == 2, 1, 0),
         C8 = ifelse(C8 == 2, 1, 0))

# 1st question slip
# dina$slip_par[1]
# dina$guess_par[1]
# 
# tbl_slip_guess <- tibble(prof1 = dina$alphas[,2], resp1 = dina$sim_dina[,1])
# tbl_slip_guess <- tibble(prof1 = savedata_prof$C1, resp1 = dina$sim_dina[,1])
# tbl_slip_guess <- tbl_slip_guess %>% 
#   mutate(slip = ifelse(prof1 == 1 & resp1 == 0, TRUE, FALSE),
#          guess = ifelse(prof1 == 0 & resp1 == 1, TRUE, FALSE))
# 
# mean(tbl_slip_guess$slip)
# mean(tbl_slip_guess$guess)

# matrix(c(mean(savedata_prof$C1), mean(savedata_prof$C2), 
#          mean(savedata_prof$C3), mean(savedata_prof$C4), 
#          mean(savedata_prof$C5), mean(savedata_prof$C6),
#          mean(savedata_prof$C7), mean(savedata_prof$C8)), nrow = 4, ncol = 2)

# tibble("Habilidades" = c("Número y Operaciones", "Medición", 
#                          "Solución de problemas", "Representación"),
#        p_t1 = c(mean(savedata_prof$C1), mean(savedata_prof$C2), 
#                 mean(savedata_prof$C3), mean(savedata_prof$C4)),
#        pt2 = c(mean(savedata_prof$C5), mean(savedata_prof$C6),
#                mean(savedata_prof$C7), mean(savedata_prof$C8)))

# tabla 4.4
p_transic <- tibble("Habilidades" = c("Número y Operaciones", "Medición", 
                         "Solución de problemas", "Representación"),
                    p_t1 = c(mean(dina$alphas$V2), mean(dina$alphas$V3),
                             mean(dina$alphas$V4), mean(dina$alphas$V5)),
                    p_t2 = c(mean(dina$alphas$V6), mean(dina$alphas$V7),
                             mean(dina$alphas$V8), mean(dina$alphas$V9)))
write.csv(p_transic, "./Tablas/P_Transicion.csv")

# 4.5
Q_tbl <- as.tbl(as.data.frame(Q)) 

prof_comb <- Q_tbl %>% 
  mutate(comb = paste0(V1, V2, V3, V4)) %>% 
  group_by(comb) %>%
  summarise(n = n()) %>% 
  pull(comb)

alphas <- as.tbl(dina$alphas) %>% 
  select(-id)
colnames(alphas) = c(paste0("X", 1:4), paste0("Y", 1:4))

alphas_t1 <- alphas %>% 
  select(X1:X4) %>% 
  mutate(comb = paste0(X1, X2, X3, X4)) %>%  
  group_by(comb) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n))

alphas_t2 <- alphas %>% 
  select(Y1:Y4) %>% 
  mutate(comb = paste0(Y1, Y2, Y3, Y4)) %>%  
  group_by(comb) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n))

write.csv(alphas_t1, "./Tablas/Frec_Prop_T1.csv")
write.csv(alphas_t2, "./Tablas/Frec_Prop_T2.csv")

# 4.6
mean(alphas$X1)
mean(alphas$X2)

alphas_trans <- alphas %>% 
  mutate(X1_nm = ifelse(Y1 == 1 & X1 == 0, 1, 0),
         X1_mm = ifelse(Y1 == 1 & X1 == 1, 1, 0),
         X1_mn = ifelse(Y1 == 0 & X1 == 1, 1, 0),
         X1_nn = ifelse(Y1 == 0 & X1 == 0, 1, 0),
         X2_nm = ifelse(Y2 == 1 & X2 == 0, 1, 0),
         X2_mm = ifelse(Y2 == 1 & X2 == 0, 1, 0),
         X2_mn = ifelse(Y2 == 0 & X2 == 1, 1, 0),
         X2_nn = ifelse(Y2 == 0 & X2 == 0, 1, 0),
         X3_nm = ifelse(Y3 == 1 & X3 == 0, 1, 0),
         X3_mm = ifelse(Y3 == 1 & X3 == 0, 1, 0),
         X3_mn = ifelse(Y3 == 0 & X3 == 1, 1, 0),
         X3_nn = ifelse(Y3 == 0 & X3 == 0, 1, 0),
         X4_nm = ifelse(Y4 == 1 & X4 == 0, 1, 0),
         X4_mm = ifelse(Y4 == 1 & X4 == 0, 1, 0),
         X4_mn = ifelse(Y4 == 0 & X4 == 1, 1, 0),
         X4_nn = ifelse(Y4 == 0 & X4 == 0, 1, 0),) %>% 
  select(X1_nm:X4_nn)

trans_mat <- matrix(colMeans(alphas_trans), nrow = 4, ncol = 4, byrow = TRUE)

tbl_trans <- tibble("Habilidades" = c("Número y Operaciones", "Medición", 
                         "Solución de problemas", "Representación"),
       pm1 = c(mean(alphas$X1), mean(alphas$X2), mean(alphas$X3),
               mean(alphas$X4)),
       pmn = trans_mat[,1],
       pmm = trans_mat[,2],
       pnm = trans_mat[,3],
       pnn = trans_mat[,4])
write.csv(tbl_trans, "./Tablas/4.6 Tabla_prop_trans.csv")

# Plots -------------------------------------------------------------------

loadfonts()
custom_theme <- theme(
  axis.text = element_text(size = 15),
  axis.title = element_text(size = 16),
  legend.text = element_text(size = 15),
  legend.title = element_text(size = 15),
  legend.position = "none",
  panel.background = element_blank(),
  panel.border = element_rect(size = 1, fill = NA),
  plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
  text = element_text(family = "Poppins")
)

tbl_trans_plot <- tibble(
  Habilidades = c(rep(tbl_trans$Habilidades[1], 5),
                  rep(tbl_trans$Habilidades[2], 5),
                  rep(tbl_trans$Habilidades[3], 5),
                  rep(tbl_trans$Habilidades[4], 5)),
  p_trans1 = rep(colnames(tbl_trans)[2:6], times = 4),
  p_trans = c(as.numeric(tbl_trans[1,2:6]), as.numeric(tbl_trans[2,2:6]), 
              as.numeric(tbl_trans[3,2:6]), as.numeric(tbl_trans[4,2:6]))
)

# Transition plot
trans_plot <- ggplot(data = tbl_trans_plot, aes(x = Habilidades, y = p_trans, 
                                  fill = p_trans1)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(labels = c("Pm1","Pmm","Pmn","Pnm","Pnn"),
                    values = RColorBrewer::brewer.pal(5, "Set1")) +
  custom_theme +
  theme(legend.position = 'right', legend.title = element_blank(),
        axis.text = element_text(size = 10)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(y = 'Probabilidad de transición', 
       title="Probabilidades de transicion estimadas para el \n modelo LTA-DINA") +
  coord_flip() +
  scale_y_continuous(limits = c(0, .53))

# Export function of transition plot
ggsave(filename = "../Imagenes/Probabilidades_transicion.pdf", 
       plot = trans_plot, device = "pdf", width = 6, height = 5)

# CDM
dina_new <- dina_data_sim(slip_guess_min = .1, slip_guess_max = .3, Q = Q)

fractions_dina_t1 <- din(data = dina_new$sim_dina[,1:20], q.matrix = Q, 
                         rule = "DINA",
                         guess.min = .1, guess.max = .3, 
                         slip.min = .1, slip.max = .3, 
                         guess.init = dina_new$guess_par, 
                         slip.init = dina_new$slip_par)

fractions_dina_t2 <- din(data = dina_new$sim_dina[,21:40], q.matrix = Q,
                         rule = "DINA",
                         guess.min = .1, guess.max = .3, 
                         slip.min = .1, slip.max = .3, 
                         guess.init = dina_new$guess_par, 
                         slip.init = dina_new$slip_par)

# slip T1 vs T2
data.frame(slip_sim = round(dina_new$slip_par, 4),
           slip_est_T1 = round(fractions_dina_t1$slip$est, 4),
           slip_est_T2 = round(fractions_dina_t2$slip$est, 4))

# guess T1 vs T2
data.frame(guess_sim = round(dina_new$guess_par, 4),
           guess_est_T1 = round(fractions_dina_t1$guess$est, 4),
           guess_est_T2 = round(fractions_dina_t2$guess$est, 4))
