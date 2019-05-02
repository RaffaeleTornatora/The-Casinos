# ------------------------------------------------------------------------------------------------

# Progetto Casino

# Crespi Daniele
# Spedale Stefania
# Tornatora Raffaele

# MODELLAZIONE
# Problema: ottimizzare la distribuzione di macchine delle varie aree dei casinò per aumentare la 
# redditività del casinò stesso.
# I modelli differiscono per la distribuzione delle macchine che è una ponderazione su determinate
# caratteristiche medie delle macchine nelle varie zone dei casinò.
# Come coefficienti per la funzione obiettivo sono utilizzate le GrossRevenuePerMachine medie.
# Come constraint abbiamo il numero massimo di macchine per area, trovate come il numero massimo di
# macchine nell'area nei vari periodi; questa operazione è stata fatta su Excel.
# Altri constraint sono stati il massimo peso delle macchine per area. Alle macchine sono stati 
# quindi associati dei pesi per stimare un peso della "grandezza" della macchina, relativa alla
# distribuzione nel mese corrente.
# Per trovare questi pesi è stato utilizzato il mese con più macchine registrate; e, per ogni tipo
# di macchina, è stato calcolato il peso come il numero di macchine registrate sottratto al numero
# totale di macchine nell'intera area, il peso del tipo di macchina è stato 1 - questo valore.
# Infine i bounds superiori sono la distribuzione delle macchine trovata dalla ponderazione, in 
# quanto questa distribuzione rappresenta il numero massimo di macchine per tipo e zona che il 
# casinò mette a disposizione.

# ------------------------------------------------------------------------------------------------

# Definizione delle librerie

library(lpSolveAPI) 
library(dplyr)
library(tidyr)

# --------------------------------------------------------------------------------------------------

# Preprocessing: definizione dei pesi relativi alla grandezza delle macchine fatto su Excel

# --------------------------------------------------------------------------------------------------

# MODELLO 1
# Distribuzione delle macchine in base alla Forecast media: (FcstUpper - FcstLower)/2

# Casinò Aries

# Inizializzazione del modello
Ares_lp_model_real = make.lp(nrow = 0, ncol = 52)

lp.control(Ares_lp_model_real, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(Ares_lp_model_real, obj = c(8975.40, 5602.83, 6746.22, 7354.60, 1842.03, 6230.37,
                                      11399.81, 21935.82, 11513.60, 7711.75, 5283.02, 6711.26,
                                      1682.16, 6081.41, 11834.47, 26164.27, 32242.69, 16358.69,
                                      9501.64, 14129.19, 2037.98, 5733.73, 11521.24, 34649.75,
                                      8436.67, 9014.61, 9352.43, 9228.80, 1777.22, 6355.82,
                                      12121.54, 32706.49, 31384, 12230.67, 23676.17, 2659, 4301.25,
                                      11725, 45911.17, 8105.96, 8124.48, 7073.68, 10925.70, 2077.87,
                                      6238.11, 11772.81, 22520.28, 2028.62, 7632.55, 14908.27, 4104.06, 
                                      13050.33))

# Definizione dei vincoli
row.add.mode(Ares_lp_model_real, "on") # Inizio a definire i constraint

add.constraint(Ares_lp_model_real,
               xt = c(0.9436, 0.7436, 0.9744, 0.9641, 0.9846, 0.9436, 0.9385, 0.9538, 0.9927, 0.9814,
                      0.9128, 0.6410, 0.9692),
               type = "<", rhs = 179,
               indices = c(1:13))

add.constraint(Ares_lp_model_real,
               xt = c(0.9817, 0.6835, 0.9541, 0.9266, 0.9954, 0.9633, 0.9908, 0.9679, 0.9954, 0.9817,
                      0.9174, 0.6193, 0.9633),
               type = "<", rhs = 200,
               indices = c(14:26))

add.constraint(Ares_lp_model_real,
               xt = c(0.9919, 0.7298, 0.9597, 0.9597, 0.9758, 0.9758, 0.9839, 0.9798, 0.9879, 0.9839,
                      0.9113, 0.5927, 0.9315),
               type = "<", rhs = 228,
               indices = c(27:39))

add.constraint(Ares_lp_model_real,
               xt = c(0.9251, 0.8503, 0.9412, 0.9572, 0.9893, 0.9626, 0.9893, 0.9572, 0.9947, 0.9786,
                      0.8610, 0.6150, 0.9786),
               type = "<", rhs = 173,
               indices = c(40:52))

add.constraint(Ares_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 195, indices = c(1:13))

add.constraint(Ares_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 218, indices = c(14:26))

add.constraint(Ares_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 248, indices = c(27:39))

add.constraint(Ares_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 187, indices = c(40:52))

row.add.mode(Ares_lp_model_real, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(Ares_lp_model_real, lower = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 
                               upper = c(4,8,10,1,4,1,7,1,1,1,12,13,1,3,26,6,5,2,4,6,3,1,1,26,37,3,
                                         5,45,7,10,2,6,3,5,1,1,26,69,9,5,94,6,18,2,12,3,13,1,1,19,139,1))
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
Ares_lp_model_real


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(Ares_lp_model_real)

# Valori ottimali dei coefficienti
values <- get.variables(Ares_lp_model_real)
# Valore ottimale del valore della funzione obiettivo
get.objective(Ares_lp_model_real)

aries_names <- c("0.01-Reel", "0.01-Video", "0.02-Reel", "0.02-Video", "0.05-Reel", "0.05-Video",
                 "0.25-Reel", "0.25.Video", "1-Reel", "1-Video", "2-Reel", "2-Video", "2-VPoker")

zones <- c("Boundary", "Entrance", "Interior", "Restaurant Plaza")

Aries_distr <- data.frame(values[1:13], 
                          values[14:26],
                          values[27:39],
                          values[40:52],
                          row.names = aries_names)

colnames(Aries_distr) <- zones

write.csv(Aries_distr, "Aries_distribution_plays.csv")

# --------------------------------------------------------------------------------------------------

# Casinò Libra

# Inizializzazione del modello
Libra_lp_model_real = make.lp(nrow = 0, ncol = 56)
lp.control(Libra_lp_model_real, sense = "max")
# Definizione dei coefficienti della funzione obiettivo
set.objfn(Libra_lp_model_real, obj = c(7444.06, 7139.47, 3983.67, 7444.06, 6762.48, 2094.29,
                                       20858.36, 11964.48, 4312.50, 5764, 7581.33, 11578.50,
                                       11498, 6894.60, 3731.10, 1214.20, 6583.95, 2211.73, 21699,
                                       11816.11, 17365.29, 6764, 4344.60, 1805.50, 7091.80, 1549,
                                       24810.20, 11288.30, 15305.25, 7158.88, 3851, 1097.83,
                                       5606, 1609.33, 50559.40, 11686.60, 28440.55, 18408.49,
                                       18408.49, 628, 36453.73, 36453.73, 45478.46, 11113, 12605.46,
                                       6444.48, 3939.80, 1300.72, 6496.97, 2302.31, 24112.17,
                                       11491.76, 7920.60, 5468.50, 7920.60, 14626.25))
# Obj è il vettore dei valori; la sua lunghezza deve essere uguale a quella specificata nel parametro
# ncol della funzione make.lp

# Definizione dei vincoli
row.add.mode(Libra_lp_model_real, "on") # Inizio a definire i constraint

add.constraint(Libra_lp_model_real,
               xt = c(0.9348, 0.6792, 0.9565, 0.9811, 0.9245, 0.9814, 0.9623, 0.9434, 0.9434, 0.9623,
                      0.9811, 0.9623, 0.6415, 0.9399),
               type = "<", rhs = 48,
               indices = c(1:14))

add.constraint(Libra_lp_model_real,
               xt = c(0.9464, 0.6964, 0.9643, 0.9821, 0.9286, 0.9821, 0.9821, 0.9821, 0.9643, 0.9751,
                      0.9811, 0.8393, 0.6964, 0.9286),
               type = "<", rhs = 51,
               indices = c(15:28))

add.constraint(Libra_lp_model_real,
               xt = c(0.9231, 0.7436, 0.9847, 0.9231, 0.9231, 0.9744, 0.9744, 0.9744, 0.9487, 0.9751,
                      0.9744, 0.9231, 0.6154, 0.9399),
               type = "<", rhs = 36,
               indices = c(29:42))

add.constraint(Libra_lp_model_real,
               xt = c(0.9348, 0.5854, 0.9565, 0.9878, 0.9634, 0.9878, 0.9878, 0.9756, 0.9878, 0.9878,
                      0.9878, 0.9390, 0.5976, 0.9512),
               type = "<", rhs = 75,
               indices = c(43:56))

add.constraint(Libra_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 53, indices = c(1:14))

add.constraint(Libra_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 56, indices = c(15:28))

add.constraint(Libra_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 39, indices = c(29:42))

add.constraint(Libra_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 82, indices = c(43:56))

row.add.mode(Libra_lp_model_real, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(Libra_lp_model_real, lower = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 
                                upper = c(4,19,0,7,3,2,3,3,1,2,1,18,25,3,6,8,0,3,1,1,0,2,0,1,1,13,
                                          11,2,4,56,0,3,10,1,6,1,8,1,0,7,73,3,4,52,0,0,8,1,3,0,1,
                                          0,0,3,43,3))

# Per stampare il modello creato
Libra_lp_model_real


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(Libra_lp_model_real)

# Valori ottimali dei coefficienti
get.variables(Libra_lp_model_real)
# Valore ottimale del valore della funzione obiettivo
get.objective(Libra_lp_model_real)

values <- get.variables(Libra_lp_model_real)
values <- round(values)
values

libra_names <- c("0.01-Reel", "0.01-Video","0.01 VPoker", "0.02-Reel", "0.02-Video", "0.05-Reel",
                 "0.05-Video", "0.25-Reel", "0.25.Video", "1-Reel", "1-Video", "2-Reel", "2-Video",
                 "2-VPoker")

Libra_distr <- data.frame(values[1:14], 
                          values[15:28],
                          values[29:42],
                          values[43:56],
                          row.names = libra_names)

colnames(Libra_distr) <- zones

write.csv(Libra_distr, "Libra_distribution_plays.csv")


# --------------------------------------------------------------------------------------------------

# MODELLO 2
# Distribuzione delle macchine in base al numero mediano di giocate (PlaysPerMachine)

# Casinò Aries

# Inizializzazione del modello
Ares_lp_model_real = make.lp(nrow = 0, ncol = 52)
 
lp.control(Ares_lp_model_real, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(Ares_lp_model_real, obj = c(8975.40, 5602.83, 6746.22, 7354.60, 1842.03, 6230.37,
                                      11399.81, 21935.82, 11513.60, 7711.75, 5283.02, 6711.26,
                                      1682.16, 6081.41, 11834.47, 26164.27, 32242.69, 16358.69,
                                      9501.64, 14129.19, 2037.98, 5733.73, 11521.24, 34649.75,
                                      8436.67, 9014.61, 9352.43, 9228.80, 1777.22, 6355.82,
                                      12121.54, 32706.49, 31384, 12230.67, 23676.17, 2659, 4301.25,
                                      11725, 45911.17, 8105.96, 8124.48, 7073.68, 10925.70, 2077.87,
                                      6238.11, 11772.81, 22520.28, 2028.62, 7632.55, 14908.27, 4104.06, 
                                      13050.33))

# Definizione dei vincoli
row.add.mode(Ares_lp_model_real, "on") # Inizio a definire i constraint

add.constraint(Ares_lp_model_real,
               xt = c(0.9436, 0.7436, 0.9744, 0.9641, 0.9846, 0.9436, 0.9385, 0.9538, 0.9927, 0.9814,
                      0.9128, 0.6410, 0.9692),
               type = "<", rhs = 179,
               indices = c(1:13))

add.constraint(Ares_lp_model_real,
               xt = c(0.9817, 0.6835, 0.9541, 0.9266, 0.9954, 0.9633, 0.9908, 0.9679, 0.9954, 0.9817,
                      0.9174, 0.6193, 0.9633),
               type = "<", rhs = 200,
               indices = c(14:26))

add.constraint(Ares_lp_model_real,
               xt = c(0.9919, 0.7298, 0.9597, 0.9597, 0.9758, 0.9758, 0.9839, 0.9798, 0.9879, 0.9839,
                      0.9113, 0.5927, 0.9315),
               type = "<", rhs = 228,
               indices = c(27:39))

add.constraint(Ares_lp_model_real,
               xt = c(0.9251, 0.8503, 0.9412, 0.9572, 0.9893, 0.9626, 0.9893, 0.9572, 0.9947, 0.9786,
                      0.8610, 0.6150, 0.9786),
               type = "<", rhs = 173,
               indices = c(40:52))

add.constraint(Ares_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 195, indices = c(1:13))

add.constraint(Ares_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 218, indices = c(14:26))

add.constraint(Ares_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 248, indices = c(27:39))

add.constraint(Ares_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 187, indices = c(40:52))

row.add.mode(Ares_lp_model_real, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(Ares_lp_model_real, lower = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 
                               upper = c(8,31,11,6,4,0,8,1,0,0,25,36,2,4,28,5,5,2,1,6,2,1,0,14,38,2,
                                         1,34,1,5,0,6,1,2,0,1,3,43,4,2,42,3,9,1,6,3,1,1,1,7,63,1))
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
Ares_lp_model_real


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(Ares_lp_model_real)

# Valori ottimali dei coefficienti
values <- get.variables(Ares_lp_model_real)
# Valore ottimale del valore della funzione obiettivo
get.objective(Ares_lp_model_real)

aries_names <- c("0.01-Reel", "0.01-Video", "0.02-Reel", "0.02-Video", "0.05-Reel", "0.05-Video",
                 "0.25-Reel", "0.25.Video", "1-Reel", "1-Video", "2-Reel", "2-Video", "2-VPoker")

zones <- c("Boundary", "Entrance", "Interior", "Restaurant Plaza")

Aries_distr <- data.frame(values[1:13], 
                          values[14:26],
                          values[27:39],
                          values[40:52],
                          row.names = aries_names)

colnames(Aries_distr) <- zones

write.csv(Aries_distr, "Aries_distribution_plays.csv")

# --------------------------------------------------------------------------------------------------

# Casinò Libra

# Inizializzazione del modello
Libra_lp_model_real = make.lp(nrow = 0, ncol = 56)
lp.control(Libra_lp_model_real, sense = "max")
# Definizione dei coefficienti della funzione obiettivo
set.objfn(Libra_lp_model_real, obj = c(7444.06, 7139.47, 3983.67, 7444.06, 6762.48, 2094.29,
                                       20858.36, 11964.48, 4312.50, 5764, 7581.33, 11578.50,
                                       11498, 6894.60, 3731.10, 1214.20, 6583.95, 2211.73, 21699,
                                       11816.11, 17365.29, 6764, 4344.60, 1805.50, 7091.80, 1549,
                                       24810.20, 11288.30, 15305.25, 7158.88, 3851, 1097.83,
                                       5606, 1609.33, 50559.40, 11686.60, 28440.55, 18408.49,
                                       18408.49, 628, 36453.73, 36453.73, 45478.46, 11113, 12605.46,
                                       6444.48, 3939.80, 1300.72, 6496.97, 2302.31, 24112.17,
                                       11491.76, 7920.60, 5468.50, 7920.60, 14626.25))
# Obj è il vettore dei valori; la sua lunghezza deve essere uguale a quella specificata nel parametro
# ncol della funzione make.lp

# Definizione dei vincoli
row.add.mode(Libra_lp_model_real, "on") # Inizio a definire i constraint

add.constraint(Libra_lp_model_real,
               xt = c(0.9348, 0.6792, 0.9565, 0.9811, 0.9245, 0.9814, 0.9623, 0.9434, 0.9434, 0.9623,
                      0.9811, 0.9623, 0.6415, 0.9399),
               type = "<", rhs = 48,
               indices = c(1:14))

add.constraint(Libra_lp_model_real,
               xt = c(0.9464, 0.6964, 0.9643, 0.9821, 0.9286, 0.9821, 0.9821, 0.9821, 0.9643, 0.9751,
                      0.9811, 0.8393, 0.6964, 0.9286),
               type = "<", rhs = 51,
               indices = c(15:28))

add.constraint(Libra_lp_model_real,
               xt = c(0.9231, 0.7436, 0.9847, 0.9231, 0.9231, 0.9744, 0.9744, 0.9744, 0.9487, 0.9751,
                      0.9744, 0.9231, 0.6154, 0.9399),
               type = "<", rhs = 36,
               indices = c(29:42))

add.constraint(Libra_lp_model_real,
               xt = c(0.9348, 0.5854, 0.9565, 0.9878, 0.9634, 0.9878, 0.9878, 0.9756, 0.9878, 0.9878,
                      0.9878, 0.9390, 0.5976, 0.9512),
               type = "<", rhs = 75,
               indices = c(43:56))

add.constraint(Libra_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 53, indices = c(1:14))

add.constraint(Libra_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 56, indices = c(15:28))

add.constraint(Libra_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 39, indices = c(29:42))

add.constraint(Libra_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 82, indices = c(43:56))

row.add.mode(Libra_lp_model_real, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(Libra_lp_model_real, lower = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 
                                upper = c(4,36,0,4,7,1,3,2,6,1,0,21,55,3,4,33,1,8,8,2,5,1,5,1,0,10,
                                          46,5,7,50,0,5,11,2,3,1,6,1,1,15,73,3,4,38,1,5,5,2,8,4,9,
                                          1,1,8,55,5))

# Per stampare il modello creato
Libra_lp_model_real


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(Libra_lp_model_real)

# Valori ottimali dei coefficienti
get.variables(Libra_lp_model_real)
# Valore ottimale del valore della funzione obiettivo
get.objective(Libra_lp_model_real)

values <- get.variables(Libra_lp_model_real)
values <- round(values)
values

libra_names <- c("0.01-Reel", "0.01-Video","0.01 VPoker", "0.02-Reel", "0.02-Video", "0.05-Reel",
                 "0.05-Video", "0.25-Reel", "0.25.Video", "1-Reel", "1-Video", "2-Reel", "2-Video",
                 "2-VPoker")

Libra_distr <- data.frame(values[1:14], 
                          values[15:28],
                          values[29:42],
                          values[43:56],
                          row.names = libra_names)

colnames(Libra_distr) <- zones

write.csv(Libra_distr, "Libra_distribution_plays.csv")

# ---------------------------------------------------------------------------------------------------

# MODELLO 3
# Distribuzione delle macchine in base alle puntate sulle macchine

# Casinò Aries

# Inizializzazione del modello
Ares_lp_model_real = make.lp(nrow = 0, ncol = 52)

lp.control(Ares_lp_model_real, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(Ares_lp_model_real, obj = c(8975.40, 5602.83, 6746.22, 7354.60, 1842.03, 6230.37,
                                      11399.81, 21935.82, 11513.60, 7711.75, 5283.02, 6711.26,
                                      1682.16, 6081.41, 11834.47, 26164.27, 32242.69, 16358.69,
                                      9501.64, 14129.19, 2037.98, 5733.73, 11521.24, 34649.75,
                                      8436.67, 9014.61, 9352.43, 9228.80, 1777.22, 6355.82,
                                      12121.54, 32706.49, 31384, 12230.67, 23676.17, 2659, 4301.25,
                                      11725, 45911.17, 8105.96, 8124.48, 7073.68, 10925.70, 2077.87,
                                      6238.11, 11772.81, 22520.28, 2028.62, 7632.55, 14908.27, 4104.06, 
                                      13050.33))

# Definizione dei vincoli
row.add.mode(Ares_lp_model_real, "on") # Inizio a definire i constraint

add.constraint(Ares_lp_model_real,
               xt = c(0.9436, 0.7436, 0.9744, 0.9641, 0.9846, 0.9436, 0.9385, 0.9538, 0.9927, 0.9814,
                      0.9128, 0.6410, 0.9692),
               type = "<", rhs = 179,
               indices = c(1:13))

add.constraint(Ares_lp_model_real,
               xt = c(0.9817, 0.6835, 0.9541, 0.9266, 0.9954, 0.9633, 0.9908, 0.9679, 0.9954, 0.9817,
                      0.9174, 0.6193, 0.9633),
               type = "<", rhs = 200,
               indices = c(14:26))

add.constraint(Ares_lp_model_real,
               xt = c(0.9919, 0.7298, 0.9597, 0.9597, 0.9758, 0.9758, 0.9839, 0.9798, 0.9879, 0.9839,
                      0.9113, 0.5927, 0.9315),
               type = "<", rhs = 228,
               indices = c(27:39))

add.constraint(Ares_lp_model_real,
               xt = c(0.9251, 0.8503, 0.9412, 0.9572, 0.9893, 0.9626, 0.9893, 0.9572, 0.9947, 0.9786,
                      0.8610, 0.6150, 0.9786),
               type = "<", rhs = 173,
               indices = c(40:52))

add.constraint(Ares_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 195, indices = c(1:13))

add.constraint(Ares_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 218, indices = c(14:26))

add.constraint(Ares_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 248, indices = c(27:39))

add.constraint(Ares_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 187, indices = c(40:52))

row.add.mode(Ares_lp_model_real, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(Ares_lp_model_real, lower = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 
                               upper = c(1,6,1,1,1,3,0,0,0,0,16,78,3,1,31,1,7,1,1,0,7,2,0,16,22,3,
                                         14,78,25,23,9,2,25,2,2,0,8,74,2,3,55,2,6,3,2,0,16,2,0,15,18,3))
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
Ares_lp_model_real


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(Ares_lp_model_real)

# Valori ottimali dei coefficienti
values <- get.variables(Ares_lp_model_real)
# Valore ottimale del valore della funzione obiettivo
get.objective(Ares_lp_model_real)

aries_names <- c("0.01-Reel", "0.01-Video", "0.02-Reel", "0.02-Video", "0.05-Reel", "0.05-Video",
                 "0.25-Reel", "0.25.Video", "1-Reel", "1-Video", "2-Reel", "2-Video", "2-VPoker")

zones <- c("Boundary", "Entrance", "Interior", "Restaurant Plaza")

Aries_distr <- data.frame(values[1:13], 
                          values[14:26],
                          values[27:39],
                          values[40:52],
                          row.names = aries_names)

colnames(Aries_distr) <- zones

write.csv(Aries_distr, "Aries_distribution_puntate.csv")

# ---------------------------------------------------------------------------------------------------

# Casinò Libra

# Inizializzazione del modello
Libra_lp_model_real = make.lp(nrow = 0, ncol = 56)
lp.control(Libra_lp_model_real, sense = "max")
# Definizione dei coefficienti della funzione obiettivo
set.objfn(Libra_lp_model_real, obj = c(7444.06, 7139.47, 3983.67, 7444.06, 6762.48, 2094.29,
                                       20858.36, 11964.48, 4312.50, 5764, 7581.33, 11578.50,
                                       11498, 6894.60, 3731.10, 1214.20, 6583.95, 2211.73, 21699,
                                       11816.11, 17365.29, 6764, 4344.60, 1805.50, 7091.80, 1549,
                                       24810.20, 11288.30, 15305.25, 7158.88, 3851, 1097.83,
                                       5606, 1609.33, 50559.40, 11686.60, 28440.55, 18408.49,
                                       18408.49, 628, 36453.73, 36453.73, 45478.46, 11113, 12605.46,
                                       6444.48, 3939.80, 1300.72, 6496.97, 2302.31, 24112.17,
                                       11491.76, 7920.60, 5468.50, 7920.60, 14626.25))
# Obj è il vettore dei valori; la sua lunghezza deve essere uguale a quella specificata nel parametro
# ncol della funzione make.lp

# Definizione dei vincoli
row.add.mode(Libra_lp_model_real, "on") # Inizio a definire i constraint

add.constraint(Libra_lp_model_real,
               xt = c(0.9348, 0.6792, 0.9565, 0.9811, 0.9245, 0.9814, 0.9623, 0.9434, 0.9434, 0.9623,
                      0.9811, 0.9623, 0.6415, 0.9399),
               type = "<", rhs = 48,
               indices = c(1:14))

add.constraint(Libra_lp_model_real,
               xt = c(0.9464, 0.6964, 0.9643, 0.9821, 0.9286, 0.9821, 0.9821, 0.9821, 0.9643, 0.9751,
                      0.9811, 0.8393, 0.6964, 0.9286),
               type = "<", rhs = 51,
               indices = c(15:28))

add.constraint(Libra_lp_model_real,
               xt = c(0.9231, 0.7436, 0.9847, 0.9231, 0.9231, 0.9744, 0.9744, 0.9744, 0.9487, 0.9751,
                      0.9744, 0.9231, 0.6154, 0.9399),
               type = "<", rhs = 36,
               indices = c(29:42))

add.constraint(Libra_lp_model_real,
               xt = c(0.9348, 0.5854, 0.9565, 0.9878, 0.9634, 0.9878, 0.9878, 0.9756, 0.9878, 0.9878,
                      0.9878, 0.9390, 0.5976, 0.9512),
               type = "<", rhs = 75,
               indices = c(43:56))

add.constraint(Libra_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 53, indices = c(1:14))

add.constraint(Libra_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 56, indices = c(15:28))

add.constraint(Libra_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 39, indices = c(29:42))

add.constraint(Libra_lp_model_real,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 82, indices = c(43:56))

row.add.mode(Libra_lp_model_real, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(Libra_lp_model_real, lower = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 
                                upper = c(0,9,0,11,3,0,2,2,1,3,0,16,93,0,12,23,0,1,0,3,0,1,1,0,0,15,
                                          108,7,2,30,1,3,5,2,23,0,9,0,6,16,78,0,0,60,0,0,10,2,0,1,0,
                                          7,1,17,88,7))

# Per stampare il modello creato
Libra_lp_model_real


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(Libra_lp_model_real)

# Valori ottimali dei coefficienti
get.variables(Libra_lp_model_real)
# Valore ottimale del valore della funzione obiettivo
get.objective(Libra_lp_model_real)

values <- get.variables(Libra_lp_model_real)
values <- round(values)
values

libra_names <- c("0.01-Reel", "0.01-Video","0.01 VPoker", "0.02-Reel", "0.02-Video", "0.05-Reel",
                 "0.05-Video", "0.25-Reel", "0.25.Video", "1-Reel", "1-Video", "2-Reel", "2-Video",
                 "2-VPoker")

Libra_distr <- data.frame(values[1:14], 
                          values[15:28],
                          values[29:42],
                          values[43:56],
                          row.names = libra_names)

colnames(Libra_distr) <- zones

write.csv(Libra_distr, "Libra_distribution_puntate.csv")
