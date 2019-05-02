# ------------------------------------------------------------------------------------------------

# Progetto Casino

# Crespi Daniele
# Spedale Stefania
# Tornatora Raffaele

# MODELLI MENSILI
# Modelli di PL per l'ottimizzazione della distribuzione mensile delle macchine nelle zone dei 
# Casinò con distribuzione delle macchine in base alla Forecast medie delle macchine stesse.
# Questi modelli considerano anche lo spostamento delle macchine da una zona all'altra del casinò.
# I modelli per il mese di settembre sono uguali ai primi modelli spiegati nel codice MODELLAZIONE
# fatta eccezione che le revenue e le forecast considerate si riferiscono al mese di settembre.
# I modelli successivi invece:
# I coefficicenti della funzione obiettivo sono ottenuti sottraendo alla revenue media nel mese 
# corrente quella del mese precedente.
# I constraint per il numero massimo di macchine nelle varie zone sono il numero massimo di 
# macchine che posso aggiungere alla zona del casino, ottenuta come il numero massimo di macchine 
# nella zona sottratto al numero di macchine che ho già nell'area.
# Stessa cosa vale per i constraint che si riferiscono al peso delle macchine.
# Infine i bounds sono:
# quelli inferiori sono il numero massimo di macchine che posso togliere dal casinò, quindi il
# numero di macchine che ho con segno negativo.
# quelli superiori sono invece il numero massimo di macchine che posso aggiungere, quindi il numero
# massimo di macchine che il casinò mette a disposizione sottratto al numero di macchine che ho già
# nella zona.
# Il risultato dato dai modelli è l'incremento di revenue del casinò rispetto al mese precedente,
# se tale valore è positivo, mentre lo sgravo di revenue rispetto sempre al mese precedente, se 
# tale valore è negativo.
# Infine i coefficienti della funzione obiettivo trovati rappresentano l'incremento o il decremento,
# sempre se il valore è rispettivamente positivo o negativo, di macchine di tale tipo nell'area del
# casino.

# ------------------------------------------------------------------------------------------------

# Modelli mensili per lo spostamento delle macchine

# Librerie

library(lpSolveAPI) 
library(dplyr)
library(tidyr)

# -------------------------------------------------------------------------------------------------

# Settembre

# Modelli di ottimizzazione della revenue dei casinò con distribuzione delle macchine in base alla 
# forecast media delle macchine nella zona considerata.

# Casinò Aries

# Inizializzazione del modello
sept_model_aries = make.lp(nrow = 0, ncol = 52)

lp.control(sept_model_aries, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(sept_model_aries, obj = c(9894.29,2011.50,9046.17,927.67,37930.67,1193.90,20294,2586.57,0,0,
                                    8893.39,1846.68,0,2518.75,5946.55,12991.65,5419.23,7517,6212,14860.50,
                                    7751.50,0,0,7378.39,6232.60,0,12881,10636.47,6012.17,11552.71,
                                    12056.78,11439.90,11175.67,0,10830.67,0,8138.10,11854.26,15531.18,
                                    6867.98,22565.35,9057.43,21329,3748.50,39814,5206,34286.69,12343,
                                    27425.50,6913.57,20270.13,3772.50))

# Definizione dei vincoli
row.add.mode(sept_model_aries, "on") # Inizio a definire i constraint

add.constraint(sept_model_aries,
               xt = c(0.9436, 0.7436, 0.9744, 0.9641, 0.9846, 0.9436, 0.9385, 0.9538, 0.9927, 0.9814,
                      0.9128, 0.6410, 0.9692),
               type = "<", rhs = 179,
               indices = c(1:13))

add.constraint(sept_model_aries,
               xt = c(0.9817, 0.6835, 0.9541, 0.9266, 0.9954, 0.9633, 0.9908, 0.9679, 0.9954, 0.9817,
                      0.9174, 0.6193, 0.9633),
               type = "<", rhs = 200,
               indices = c(14:26))

add.constraint(sept_model_aries,
               xt = c(0.9919, 0.7298, 0.9597, 0.9597, 0.9758, 0.9758, 0.9839, 0.9798, 0.9879, 0.9839,
                      0.9113, 0.5927, 0.9315),
               type = "<", rhs = 228,
               indices = c(27:39))

add.constraint(sept_model_aries,
               xt = c(0.9251, 0.8503, 0.9412, 0.9572, 0.9893, 0.9626, 0.9893, 0.9572, 0.9947, 0.9786,
                      0.8610, 0.6150, 0.9786),
               type = "<", rhs = 173,
               indices = c(40:52))

add.constraint(sept_model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 195, indices = c(1:13))

add.constraint(sept_model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 218, indices = c(14:26))

add.constraint(sept_model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 248, indices = c(27:39))

add.constraint(sept_model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 187, indices = c(40:52))

row.add.mode(sept_model_aries, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(sept_model_aries, lower = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 
                             upper = c(8,31,11,6,4,0,8,1,0,0,25,36,2,4,28,5,5,2,1,6,2,1,0,14,38,2,
                                       1,34,1,5,0,6,1,2,0,1,3,43,4,2,42,3,9,1,6,3,1,1,1,7,63,1))
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
sept_model_aries


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(sept_model_aries)

# Valori ottimali dei coefficienti
values <- get.variables(sept_model_aries)
# Valore ottimale del valore della funzione obiettivo
get.objective(sept_model_aries)

aries_names <- c("0.01-Reel", "0.01-Video", "0.02-Reel", "0.02-Video", "0.05-Reel", "0.05-Video",
                 "0.25-Reel", "0.25.Video", "1-Reel", "1-Video", "2-Reel", "2-Video", "2-VPoker")

zones <- c("Boundary", "Entrance", "Interior", "Restaurant Plaza")

Aries_sept_distr <- data.frame(values[1:13], 
                          values[14:26],
                          values[27:39],
                          values[40:52],
                          row.names = aries_names)

colnames(Aries_sept_distr) <- zones

write.csv(Aries_sept_distr, "Aries_sept_distribution.csv")

# -------------------------------------------------------------------------------------------------

# Casinò Libra

# Inizializzazione del modello
Libra_sept_model = make.lp(nrow = 0, ncol = 56)
lp.control(Libra_sept_model, sense = "max")
# Definizione dei coefficienti della funzione obiettivo
set.objfn(Libra_sept_model, obj = c(0,7007.33,10166,6600.83,0,6072.50,18374.75,4562.50,31862,0,
                                    13619.50,6503.25,0,0,8153.67,2130.70,5846,3290.33,6975,0,0,2580,
                                    0,0,6415.87,1915.85,0,6109,0,22004.80,3051.50,18911,5338,0,4281,
                                    38471,0,34892,3244.33,21183.57,0,0,0,11659.83,1747,12953.33,
                                    0,0,1439,0,0,0,1154.75,11662.41,15918.50,0))
# Obj è il vettore dei valori; la sua lunghezza deve essere uguale a quella specificata nel parametro
# ncol della funzione make.lp

# Definizione dei vincoli
row.add.mode(Libra_sept_model, "on") # Inizio a definire i constraint

add.constraint(Libra_sept_model,
               xt = c(0.9348, 0.6792, 0.9565, 0.9811, 0.9245, 0.9814, 0.9623, 0.9434, 0.9434, 0.9623,
                      0.9811, 0.9623, 0.6415, 0.9399),
               type = "<", rhs = 48,
               indices = c(1:14))

add.constraint(Libra_sept_model,
               xt = c(0.9464, 0.6964, 0.9643, 0.9821, 0.9286, 0.9821, 0.9821, 0.9821, 0.9643, 0.9751,
                      0.9811, 0.8393, 0.6964, 0.9286),
               type = "<", rhs = 51,
               indices = c(15:28))

add.constraint(Libra_sept_model,
               xt = c(0.9231, 0.7436, 0.9847, 0.9231, 0.9231, 0.9744, 0.9744, 0.9744, 0.9487, 0.9751,
                      0.9744, 0.9231, 0.6154, 0.9399),
               type = "<", rhs = 36,
               indices = c(29:42))

add.constraint(Libra_sept_model,
               xt = c(0.9348, 0.5854, 0.9565, 0.9878, 0.9634, 0.9878, 0.9878, 0.9756, 0.9878, 0.9878,
                      0.9878, 0.9390, 0.5976, 0.9512),
               type = "<", rhs = 75,
               indices = c(43:56))

add.constraint(Libra_sept_model,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 53, indices = c(1:14))

add.constraint(Libra_sept_model,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 56, indices = c(15:28))

add.constraint(Libra_sept_model,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 39, indices = c(29:42))

add.constraint(Libra_sept_model,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 82, indices = c(43:56))

row.add.mode(Libra_sept_model, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(Libra_sept_model, lower = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 
                             upper = c(4,36,0,4,7,1,3,2,6,1,0,21,55,3,4,33,1,8,8,2,5,1,5,1,0,10,
                                       46,5,7,50,0,5,11,2,3,1,6,1,1,15,73,3,4,38,1,5,5,2,8,4,9,1,1,8,
                                       55,5))

# Per stampare il modello creato
Libra_sept_model


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(Libra_sept_model)

# Valori ottimali dei coefficienti
get.variables(Libra_sept_model)
# Valore ottimale del valore della funzione obiettivo
get.objective(Libra_sept_model)

values <- get.variables(Libra_sept_model)
values <- round(values)
values

libra_names <- c("0.01-Reel", "0.01-Video","0.01 VPoker", "0.02-Reel", "0.02-Video", "0.05-Reel",
                 "0.05-Video", "0.25-Reel", "0.25.Video", "1-Reel", "1-Video", "2-Reel", "2-Video",
                 "2-VPoker")

Libra_sept_distr <- data.frame(values[1:14], 
                          values[15:28],
                          values[29:42],
                          values[43:56],
                          row.names = libra_names)

colnames(Libra_sept_distr) <- zones

write.csv(Libra_sept_distr, "Libra_sept_distribution.csv")

# -------------------------------------------------------------------------------------------------

# Preparazione dei dati per la creazione dei modelli mensili

# Distribuzioni al mese di settembre
distr_aries <- data.frame(c(11,50,5,7,3,11,12,9,0,0,17,70,0),
                          c(4,69,10,16,1,8,2,7,0,0,18,83,0),
                          c(2,67,10,10,6,6,4,0,3,0,22,101,17),
                          c(14,28,11,8,2,7,2,8,1,4,26,72,4))
distr_libra <- data.frame(c(0,36,0,4,0,0,3,0,6,0,0,4,0,0),
                          c(4,29,1,8,8,0,0,1,0,0,0,0,0,5),
                          c(0,37,0,0,0,0,0,1,0,1,0,0,0,0),
                          c(0,14,0,5,0,0,0,0,0,0,0,8,55,0))

# Coefficienti funzione obiettivo
# Aries
c_a_10 <- c(-1950.96,-70.95,1848.16,835.83,50016.33,1951.73,-19228,-2586.57,0,0,-2743.82,342.38,0,
            1115.25,164.12,-6802.44,632.27,5877,-6212,-14860.5,-1014,24543,0,-6491.48,-235.61,0,
            -12881,192.48,-3157.23,-1913.01,-8246.28,-11439.9,-2424.17,12778.38,1064.66,0,-4424.41,-297.08,-15531.18,
            2260.52,-2338.6,-4241.43,14544,-2318.83,-5254.13,7691.5,-15655.02,-12343,56928.75,-3100,2949.37,-3772.5)
c_a_11 <- c(-2858.33,189.64,-6629.33,481.75,-73019,-3145.63,11613,0,0,2659,3040.27,35.33,0,
            -194.71,505.25,2428.12,440.38,-13394,4440.75,3764,-1629.43,-24543,0,10703.26,26.08,0,
            0,1791.96,3828.81,2382.8,484.17,9371,-4510.5,1848.79,-9826.33,0,4181.85,488.46,0,
            4177.5,455.92,1941.17,-14564,26850.33,-7445.99,-12897.5,6983.33,37832,-84354.25,7480.74,4939.56,0)
c_a_12 <- c(57032.6,-216.37,7354.79,-1519.75,-2005.17,342,-12679,0,0,-2659,-1597.45,-260.71,0,
            2880.96,-345.6,707.37,-253.71,0,754.75,211.5,-5108.07,45572,0,-4769.17,96.2,5897.43,
            5469.5,-1317.94,-3926.58,-1735,-4294.67,2607,16316,-1157.67,3076,0,-1381.57,-114.55,0,
            -5548.83,3645.91,-4621.17,4322.2,-25864,3213.12,9599,3718.67,-37832,17945.25,-97.56,-5323.67,0)
c_a_01 <- c(-4007.93,-50.77,-2234.79,-168.5,29518.73,782.98,2882,1701.13,0,0,-1609.19,85.29,2237.33,
            8424.25,553.08,-73.45,-435.14,3415,-307.9,5855.71,0,-45572,0,8887.5,-202.43,-5897.43,
            8294.5,-512.27,4948.39,1795.5,1363,182.25,-20557,-13469.5,583,8886.5,-1896.22,390.61,10253.71,
            -448.79,-3021,55,2187.25,6649.8,44193.5,5558.67,4132,12676,27670,-17945.25,1961.45,-786.33)
c_a_02 <- c(13693.53,-149.86,-6355,2815,-6679.23,322.6,-2882,-950.63,0,0,1974.41,-315.6,-2237.33,
            -8921.5,-814.75,-6257.68,1780.97,-3415,1632.9,-9831.21,6769.17,21066,0,-6895.08,602.58,7521.25,
            -4644.5,828.04,-7705.56,-2059,12492.67,-601.25,4228.67,12028.25,-5683,2730,4441,-904.01,-10253.71,
            912.12,-924.38,5658.63,-60.33,-42817.5,-10154.67,-676.5,23380.33,-7571,0,57.65,-1.11,0)
c_a_03 <- c(-60966.6,166.63,9406,-1883.5,1295.17,1960.34,0,-750.5,0,0,-3054.53,747.89,0,
            3779.72,11.35,8250.6,-905.1,0,-6520.5,0,-6769.17,-21066,0,885.28,-588.99,-638.75,
            -9119.5,-151.19,2680.58,2988.13,-1605.5,-11559,-4228.67,-12028.25,27863.5,-11616.5,-77.92,688,0,
            -4236.5,1566.23,-5497.38,-5578.67,20172.67,14724.8,-10790.5,-31588.5,-20099,61697.5,2029.83,3475.76,0)
c_a_04 <- c(-2130.93,-174.47,-1087.75,1846,-30273.83,-1489.17,6417,2060.38,0,0,2458.52,-703.43,2702.67,
            -4435.92,0.76,-2819.67,-254.3,8085,0,0,0,0,0,-124.72,300.79,-6882.5,
            2559.33,665.95,4024.92,190.12,-8971.67,13050,0,10553.5,-20936,0,1727.84,-245.41,0,
            6763,-309.62,-1146,1229,-23964.67,-12876.92,-2264,-4445.97,0,-5397.25,-816.53,-3486.76,0)
c_a_05 <- c(1940.46,232.73,-9660.25,-1871,23660.33,-1918.75,-6417,-990.88,0,0,2349.35,460.93,-1314.46,
            -513.8,684.37,-2531,356.4,-8085,0,5880,0,0,0,-177.73,269.91,0,
            3574.17,-701.73,-5593,-1989.32,22474.17,-1785.75,15498,-43.33,3214,14672,1141.71,-406.08,0,
            -7350,-507.32,5296.88,2835.25,4660,17761.02,22312,-6162.03,16442,-56300.25,-3416.52,763.29,3713)
c_a_06 <- c(-3630.96,-299.93,9259,401.9,42949,317.75,0,-1069.5,0,0,3137.85,118.75,398.04,
            3284,-670.16,383.1,-244.7,29952.22,0,10185,4883.25,0,0,-4221.97,-261.92,5897.57,
            -361.75,144.19,4974.17,-802.73,-22839.67,-222.55,-3401,1780.83,-4274.5,-14672,-6710.5,-389.32,0,
            -3397,2124.97,-4102.88,-11311.12,-1939.5,-8781.1,-19087.42,11852.21,-16442,19042.25,-5491.63,-2342.99,1113.67)
c_a_07 <- c(3275.33,-427.87,2599.38,-773.4,-24050,3477.75,7282,2491.44,0,0,-10750.3,-415.33,-1786.25,
            -2986.83,180.45,2158.4,-1167.5,-6863.22,0,-7342.25,-4883.25,0,0,4304.6,82.47,2611.43,
            729.75,143.25,1315.83,2494.47,-1640,949.3,-3666,-1677,20821,0,1430.2,597.16,0,
            6080,1451.76,79.31,11749.87,17480.06,-5306.8,-3224.58,-7480.96,0,35571,15227.75,1040.3,-4826.67)
c_a_08 <- c(6128,579.66,6996.34,1098.67,-7021,-1427.25,-7282,-606.87,0,0,8798.47,244.05,0,
            -608.17,51.3,-1963.62,205.5,-10056.67,6200.75,-3037.75,0,37355,4301.25,-4002.97,312.57,-8509,
            -1569.25,-8.12,180.38,-850.67,1942.5,-11991,-2037,1889.14,-2407,0,-1560.08,265.81,18317,
            1152,-2537.79,-4611.81,-14461,-12674.89,103.15,9307.5,-66.25,0,-54613.25,-8381.85,1271.62,0)
# Libra
c_l_10 <- c(0,203.5,-10166,-6600.83,0,-6072.5,-8779.75,690.5,-31862,0,-1424,187.68,0,0,
            -1756,486.8,1958,-3290.33,-6975,0,0,-2580,0,0,1522.13,812.72,0,-6109,
            0,-1940.97,-580.5,-760,-5338,0,-4281,-38471,0,9794,1477.17,9568.76,0,7489,
            0,123.37,-1747,-12953.33,0,11094.5,-1439,0,246,0,907.25,-215.99,-15918.5,0)
c_l_11 <- c(0,-1100.03,0,5953,0,8248,4866.5,-5253,43463.5,0,-2952,116.3,0,0,
            -6397.67,-428.69,-270.33,704,0,0,0,0,0,0,-1509.42,-499.51,0,6755,
            2689,1332.01,-2471,1527,0,0,0,19634,0,-21081,-1814.5,-8582.55,0,-7489,
            0,1406.43,0,11615.75,0,-11094.5,824,0,-246,0,-165.83,-1086.36,0,0)
c_l_12 <- c(0,812.45,0,-521.17,0,-8248,-5044.5,0,-16363,0,2036.58,-173.93,0,0,
            0,-626.39,-1230.67,2384.5,0,0,0,321,0,0,-606.66,-630.93,0,-6755,
            -2689,-2914.13,3778,10083,4787,20235,0,19297,0,8403,-2907,6414.92,0,0,
            0,-2042.21,0,1311.25,0,0,1289,0,0,0,-579.67,234.07,13334,11578.5)
c_l_01 <- c(0,-379.57,0,-5431.83,0,0,12877,0,-8089.5,0,-2874.58,-95.71,0,0,
            0,763.77,-6303,-2581.5,0,0,0,1606,0,0,-61.42,1262.27,0,4428,
            0,1281.69,985,-12684,-4787,-20235,5208,42043,0,48227,4079.84,-6737.37,0,0,
            0,1092.14,1213,-2605.75,0,9668,-2113,0,0,0,-1316.5,1281.66,-13334,-11578.5)
c_l_02 <- c(0,-111.62,15434,4704,0,0,-11010,0,2492.5,0,6661.34,-57.81,0,0,
            0,-809.52,0,1194.83,0,0,0,-1927,0,0,1979.83,1117.27,0,-4428,
            0,3918.66,-4763,4880,0,0,-5208,-80794,0,-34292,783.16,2292.5,0,8428.5,
            0,366.21,-1213,2485.75,2229,2929,484,13629,0,0,915,1284.96,0,0)
c_l_03 <- c(0,376.21,-15434,3370,0,0,5127,0,3186,0,-15066.84,-408.06,0,0,
            0,87.33,0,1066.67,0,0,5613,0,0,0,-5701,1167.64,6229.5,0,
            4986.33,-4243.06,3247,2868,0,32156,0,79458,0,14517,-1340,343.1,0,-8428.5,
            0,-138.59,1345,11568,0,-1960,0,1281,1010,0,-806.5,-490.77,0,0)
c_l_04 <- c(0,-136.77,0,-2068.5,0,0,-6511,0,-24689.5,0,12926,213.57,0,0,
            0,-456,0,-745,0,0,-5613,0,0,0,-5701,1167.64,-6229.5,0,
            -710.66,2580.52,348.5,-947,3703,-5977,0,-79458,0,-38767,-3523,5146.36,0,6826.5,
            0,-298.73,-1345,11568,0,-1960,0,1281,1010,0,-806.5,-490.77,0,0)
c_l_05 <- c(0,-377.6,0,443,0,7423,4637,0,11456.5,0,2393,280.59,0,0,
            6630.33,904.36,0,538.2,0,0,0,0,0,0,6550,-548.16,0,0,
            -4275.67,-3775.57,1283.5,-23878,-3703,-26179,3431,73956,0,-21693,4423,-7783.83,0,-6826.5,
            0,-442.95,0,590,0,-9712,0,1889,-1010,11113,-215.16,-514.93,0,0)
c_l_06 <- c(0,305.8,0,1962,0,-7423,-14537,6145,14352.5,0,-1699.44,-565.21,0,0,
            1586.34,-439.07,7299,112.8,0,0,7427,0,0,0,88.72,-176.48,0,0,
            0,1094.85,-4879,27204,2873,25246,-3431,-27657,0,75278,-4423,380.4,0,0,
            0,444.46,104,-12158,1382,14209,288,-12755,0,-11113,1492.66,845.08,0,0)
c_l_07 <- c(0,291.98,0,-1010.67,0,7643,29157,1362,14727.5,0,-13619.56,228.53,0,4312.5,
            -1917.67,1327.21,-812,-2674.5,6553,0,481,0,0,0,-165.72,-466,4707.5,0,
            0,5519.59,0,-9206,-2873,-25246,3097,-46299,0,-33553,3377.84,1440.89,0,0,
            0,-108.29,-104,11883,-1382,-2184,-288,0,0,0,-2071,134.19,0,0)
c_l_08 <- c(0,172.38,8894,-649.83,0,-7643,-29157,-7507,-1563,0,12937.5,439.05,0,-4312.5,
            -6299,-380.68,-6487,3829,-6553,1549,-220.5,0,0,0,-6473,-105.85,-4707.5,0,
            0,-3877.39,0,-17998,5022,0,141,43101,0,-1987,1031.99,3237.85,0,0,
            0,-256.47,1662,-592.5,0,-2886,0,11598,0,0,1169,-348.98,0,0)

# Constraint
# Aries
p_a_boundary <- c(0.9436, 0.7436, 0.9744, 0.9641, 0.9846, 0.9436, 0.9385, 0.9538, 0.9927, 0.9814,
                  0.9128, 0.6410, 0.9692)
p_a_entrance <- c(0.9817, 0.6835, 0.9541, 0.9266, 0.9954, 0.9633, 0.9908, 0.9679, 0.9954, 0.9817,
                  0.9174, 0.6193, 0.9633)
p_a_interior <- c(0.9919, 0.7298, 0.9597, 0.9597, 0.9758, 0.9758, 0.9839, 0.9798, 0.9879, 0.9839,
                  0.9113, 0.5927, 0.9315)
p_a_restaurant <- c(0.9251, 0.8503, 0.9412, 0.9572, 0.9893, 0.9626, 0.9893, 0.9572, 0.9947, 0.9786,
                    0.8610, 0.6150, 0.9786)

mm_a_boundary <- 195
mm_a_entrance <- 218
mm_a_interior <- 248
mm_a_restaurant <- 187
mmp_a_boundary <- 179
mmp_a_entrance <- 200
mmp_a_interior <- 228
mmp_a_restaurant <- 176

medp_a_boundary <- 0.9187
medp_a_entrance <- 0.9185
medp_a_interior <- 0.9203
medp_a_restaurant <- 0.9231

# Libra
p_l_boundary <- c(0.9348, 0.6792, 0.9565, 0.9811, 0.9245, 0.9814, 0.9623, 0.9434, 0.9434, 0.9623,
                  0.9811, 0.9623, 0.6415, 0.9399)
p_l_entrance <- c(0.9464, 0.6964, 0.9643, 0.9821, 0.9286, 0.9821, 0.9821, 0.9821, 0.9643, 0.9751,
                  0.9811, 0.8393, 0.6964, 0.9286)
p_l_interior <- c(0.9231, 0.7436, 0.9847, 0.9231, 0.9231, 0.9744, 0.9744, 0.9744, 0.9487, 0.9751,
                  0.9744, 0.9231, 0.6154, 0.9399)
p_l_restaurant <- c(0.9348, 0.5854, 0.9565, 0.9878, 0.9634, 0.9878, 0.9878, 0.9756, 0.9878, 0.9878,
                    0.9878, 0.9390, 0.5976, 0.9512)

mm_l_boundary <- 53
mm_l_entrance <- 56
mm_l_interior <- 39
mm_l_restaurant <- 82
mmp_l_boundary <- 48
mmp_l_entrance <- 51
mmp_l_interior <- 36
mmp_l_restaurant <- 75

medp_l_boundary <- 0.9138
medp_l_entrance <- 0.9178
medp_l_interior <- 0.9115
medp_l_restaurant <- 0.9165

# Bounds

nmm_aries <- c(16,173,29,33,10,23,20,22,2,3,62,258,14)
nmm_libra <- c(18,119,1,13,22,4,11,6,9,4,2,41,152,11)

# Print results constants

aries_names <- c("0.01-Reel", "0.01-Video", "0.02-Reel", "0.02-Video", "0.05-Reel", "0.05-Video",
                 "0.25-Reel", "0.25.Video", "1-Reel", "1-Video", "2-Reel", "2-Video", "2-VPoker")
libra_names <- c("0.01-Reel", "0.01-Video","0.01-VPoker", "0.02-Reel", "0.02-Video", "0.05-Reel", 
                 "0.05-Video", "0.25-Reel", "0.25.Video", "1-Reel", "1-Video", "2-Reel", "2-Video",
                 "2-VPoker")

zones <- c("Boundary", "Entrance", "Interior", "Restaurant Plaza")

# -------------------------------------------------------------------------------------------------

# Aries

# -------------------------------------------------------------------------------------------------

# Ottobre

n <- 0
m <- 0
i <- 1

# Constraint

m_a_boundary <- 0
for (n in 1:13) {
  m_a_boundary <- m_a_boundary + distr_aries[n,1]
}
m_a_entrance <- 0
for (n in 1:13) {
  m_a_entrance <- m_a_entrance + distr_aries[n,2]
}
m_a_interior <- 0
for (n in 1:13) {
  m_a_interior <- m_a_interior + distr_aries[n,3]
}
m_a_restaurant <- 0
for (n in 1:13) {
  m_a_restaurant <- m_a_restaurant + distr_aries[n,4]
}

mmp_a_boundary <- m_a_boundary * medp_a_boundary
mmp_a_entrance <- m_a_entrance * medp_a_entrance
mmp_a_interior <- m_a_interior * medp_a_interior
mmp_a_restaurant <- m_a_restaurant * medp_a_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    low_bounds[i] <- -(distr_aries[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    high_bounds[i] <- nmm_aries[m] - distr_aries[m,n] 
    i <- i + 1
  }
}

# Inizializzazione del modello
model_aries = make.lp(nrow = 0, ncol = 52)

lp.control(model_aries, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_aries, obj = c_a_10)

# Definizione dei vincoli
row.add.mode(model_aries, "on") # Inizio a definire i constraint

add.constraint(model_aries, xt = p_a_boundary, type = "<", mmp_a_boundary, 
               indices = c(1:13))
add.constraint(model_aries, xt = p_a_entrance, type = "<", mmp_a_entrance, 
               indices = c(14:26))
add.constraint(model_aries, xt = p_a_interior, type = "<", mmp_a_interior,
               indices = c(27:39))
add.constraint(model_aries, xt = p_a_restaurant, type = "<", mmp_a_restaurant,
               indices = c(40:52))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_boundary - m_a_boundary),
               indices = c(1:13))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_entrance - m_a_entrance), 
               indices = c(14:26))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_interior - m_a_interior), 
               indices = c(27:39))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_restaurant - m_a_restaurant), 
               indices = c(40:52))

row.add.mode(model_aries, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_aries, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_aries


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_aries)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_aries)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_aries)

res_distr <- data.frame(res_coef[1:13], 
                        res_coef[14:26],
                        res_coef[27:39],
                        res_coef[40:52],
                        row.names = aries_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:13) {
    if ((res_distr[m,n] < 0) && (c_a_10[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_a_10[i])
      i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:13) {
    distr_aries[m,n] <- distr_aries[m,n] + res_distr[m,n]
    distr_aries[m,n] <- round(distr_aries[m,n])
  }
}

res_obj
write.csv(distr_aries, "Aries_oct_distribution.csv")

# ------------------------------------------------------------------------------------------------

# Novembre

# ------------------------------------------------------------------------------------------------

n <- 0
m <- 0
i <- 1

# Constraint

m_a_boundary <- 0
for (n in 1:13) {
  m_a_boundary <- m_a_boundary + distr_aries[n,1]
}
m_a_entrance <- 0
for (n in 1:13) {
  m_a_entrance <- m_a_entrance + distr_aries[n,2]
}
m_a_interior <- 0
for (n in 1:13) {
  m_a_interior <- m_a_interior + distr_aries[n,3]
}
m_a_restaurant <- 0
for (n in 1:13) {
  m_a_restaurant <- m_a_restaurant + distr_aries[n,4]
}

mmp_a_boundary <- m_a_boundary * medp_a_boundary
mmp_a_entrance <- m_a_entrance * medp_a_entrance
mmp_a_interior <- m_a_interior * medp_a_interior
mmp_a_restaurant <- m_a_restaurant * medp_a_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    low_bounds[i] <- -(distr_aries[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    high_bounds[i] <- nmm_aries[m] - distr_aries[m,n]
    i <- i + 1
  }
}

# Inizializzazione del modello
model_aries = make.lp(nrow = 0, ncol = 52)

lp.control(model_aries, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_aries, obj = c_a_11)

# Definizione dei vincoli
row.add.mode(model_aries, "on") # Inizio a definire i constraint

add.constraint(model_aries, xt = p_a_boundary, type = "<", mmp_a_boundary, 
               indices = c(1:13))
add.constraint(model_aries, xt = p_a_entrance, type = "<", mmp_a_entrance, 
               indices = c(14:26))
add.constraint(model_aries, xt = p_a_interior, type = "<", mmp_a_interior,
               indices = c(27:39))
add.constraint(model_aries, xt = p_a_restaurant, type = "<", mmp_a_restaurant,
               indices = c(40:52))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_boundary - m_a_boundary),
               indices = c(1:13))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_entrance - m_a_entrance), 
               indices = c(14:26))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_interior - m_a_interior), 
               indices = c(27:39))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_restaurant - m_a_restaurant), 
               indices = c(40:52))

row.add.mode(model_aries, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_aries, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_aries


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_aries)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_aries)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_aries)

res_distr <- data.frame(res_coef[1:13], 
                        res_coef[14:26],
                        res_coef[27:39],
                        res_coef[40:52],
                        row.names = aries_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:13) {
    if ((res_distr[m,n] < 0) && (c_a_11[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_a_11[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:13) {
    distr_aries[m,n] <- distr_aries[m,n] + res_distr[m,n]
    distr_aries[m,n] <- round(distr_aries[m,n])
  }
}

res_obj
write.csv(res_distr, "Aries_nov_distribution.csv")

# -------------------------------------------------------------------------------------------------

# Dicembre

# -------------------------------------------------------------------------------------------------

n <- 0
m <- 0
i <- 1

# Constraint

m_a_boundary <- 0
for (n in 1:13) {
  m_a_boundary <- m_a_boundary + distr_aries[n,1]
}
m_a_entrance <- 0
for (n in 1:13) {
  m_a_entrance <- m_a_entrance + distr_aries[n,2]
}
m_a_interior <- 0
for (n in 1:13) {
  m_a_interior <- m_a_interior + distr_aries[n,3]
}
m_a_restaurant <- 0
for (n in 1:13) {
  m_a_restaurant <- m_a_restaurant + distr_aries[n,4]
}

mmp_a_boundary <- m_a_boundary * medp_a_boundary
mmp_a_entrance <- m_a_entrance * medp_a_entrance
mmp_a_interior <- m_a_interior * medp_a_interior
mmp_a_restaurant <- m_a_restaurant * medp_a_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    low_bounds[i] <- -(distr_aries[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    high_bounds[i] <- nmm_aries[m] - distr_aries[m,n]
    i <- i + 1
  }
}

# Inizializzazione del modello
model_aries = make.lp(nrow = 0, ncol = 52)

lp.control(model_aries, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_aries, obj = c_a_12)

# Definizione dei vincoli
row.add.mode(model_aries, "on") # Inizio a definire i constraint

add.constraint(model_aries, xt = p_a_boundary, type = "<", mmp_a_boundary, 
               indices = c(1:13))
add.constraint(model_aries, xt = p_a_entrance, type = "<", mmp_a_entrance, 
               indices = c(14:26))
add.constraint(model_aries, xt = p_a_interior, type = "<", mmp_a_interior,
               indices = c(27:39))
add.constraint(model_aries, xt = p_a_restaurant, type = "<", mmp_a_restaurant,
               indices = c(40:52))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_boundary - m_a_boundary),
               indices = c(1:13))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_entrance - m_a_entrance), 
               indices = c(14:26))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_interior - m_a_interior), 
               indices = c(27:39))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_restaurant - m_a_restaurant), 
               indices = c(40:52))

row.add.mode(model_aries, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_aries, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_aries


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_aries)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_aries)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_aries)

res_distr <- data.frame(res_coef[1:13], 
                        res_coef[14:26],
                        res_coef[27:39],
                        res_coef[40:52],
                        row.names = aries_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:13) {
    if ((res_distr[m,n] < 0) && (c_a_12[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_a_12[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:13) {
    distr_aries[m,n] <- distr_aries[m,n] + res_distr[m,n]
    distr_aries[m,n] <- round(distr_aries[m,n])
  }
}

res_obj
write.csv(res_distr, "Aries_dic_distribution.csv")

# -------------------------------------------------------------------------------------------------

# Gennaio

# -------------------------------------------------------------------------------------------------

n <- 0
m <- 0
i <- 1

# Constraint

m_a_boundary <- 0
for (n in 1:13) {
  m_a_boundary <- m_a_boundary + distr_aries[n,1]
}
m_a_entrance <- 0
for (n in 1:13) {
  m_a_entrance <- m_a_entrance + distr_aries[n,2]
}
m_a_interior <- 0
for (n in 1:13) {
  m_a_interior <- m_a_interior + distr_aries[n,3]
}
m_a_restaurant <- 0
for (n in 1:13) {
  m_a_restaurant <- m_a_restaurant + distr_aries[n,4]
}

mmp_a_boundary <- m_a_boundary * medp_a_boundary
mmp_a_entrance <- m_a_entrance * medp_a_entrance
mmp_a_interior <- m_a_interior * medp_a_interior
mmp_a_restaurant <- m_a_restaurant * medp_a_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    low_bounds[i] <- -(distr_aries[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    high_bounds[i] <- nmm_aries[m] - distr_aries[m,n]
    i <- i + 1
  }
}

# Inizializzazione del modello
model_aries = make.lp(nrow = 0, ncol = 52)

lp.control(model_aries, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_aries, obj = c_a_01)

# Definizione dei vincoli
row.add.mode(model_aries, "on") # Inizio a definire i constraint

add.constraint(model_aries, xt = p_a_boundary, type = "<", mmp_a_boundary, 
               indices = c(1:13))
add.constraint(model_aries, xt = p_a_entrance, type = "<", mmp_a_entrance, 
               indices = c(14:26))
add.constraint(model_aries, xt = p_a_interior, type = "<", mmp_a_interior,
               indices = c(27:39))
add.constraint(model_aries, xt = p_a_restaurant, type = "<", mmp_a_restaurant,
               indices = c(40:52))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_boundary - m_a_boundary),
               indices = c(1:13))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_entrance - m_a_entrance), 
               indices = c(14:26))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_interior - m_a_interior), 
               indices = c(27:39))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_restaurant - m_a_restaurant), 
               indices = c(40:52))

row.add.mode(model_aries, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_aries, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_aries


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_aries)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_aries)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_aries)

res_distr <- data.frame(res_coef[1:13], 
                        res_coef[14:26],
                        res_coef[27:39],
                        res_coef[40:52],
                        row.names = aries_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:13) {
    if ((res_distr[m,n] < 0) && (c_a_01[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_a_01[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:13) {
    distr_aries[m,n] <- distr_aries[m,n] + res_distr[m,n]
    distr_aries[m,n] <- round(distr_aries[m,n])
  }
}

res_obj
write.csv(res_distr, "Aries_gen_distribution.csv")

# -------------------------------------------------------------------------------------------------

# Febbraio

# -------------------------------------------------------------------------------------------------

n <- 0
m <- 0
i <- 1

# Constraint

m_a_boundary <- 0
for (n in 1:13) {
  m_a_boundary <- m_a_boundary + distr_aries[n,1]
}
m_a_entrance <- 0
for (n in 1:13) {
  m_a_entrance <- m_a_entrance + distr_aries[n,2]
}
m_a_interior <- 0
for (n in 1:13) {
  m_a_interior <- m_a_interior + distr_aries[n,3]
}
m_a_restaurant <- 0
for (n in 1:13) {
  m_a_restaurant <- m_a_restaurant + distr_aries[n,4]
}

mmp_a_boundary <- m_a_boundary * medp_a_boundary
mmp_a_entrance <- m_a_entrance * medp_a_entrance
mmp_a_interior <- m_a_interior * medp_a_interior
mmp_a_restaurant <- m_a_restaurant * medp_a_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    low_bounds[i] <- -(distr_aries[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    high_bounds[i] <- nmm_aries[m] - distr_aries[m,n]
    i <- i + 1
  }
}

# Inizializzazione del modello
model_aries = make.lp(nrow = 0, ncol = 52)

lp.control(model_aries, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_aries, obj = c_a_02)

# Definizione dei vincoli
row.add.mode(model_aries, "on") # Inizio a definire i constraint

add.constraint(model_aries, xt = p_a_boundary, type = "<", mmp_a_boundary, 
               indices = c(1:13))
add.constraint(model_aries, xt = p_a_entrance, type = "<", mmp_a_entrance, 
               indices = c(14:26))
add.constraint(model_aries, xt = p_a_interior, type = "<", mmp_a_interior,
               indices = c(27:39))
add.constraint(model_aries, xt = p_a_restaurant, type = "<", mmp_a_restaurant,
               indices = c(40:52))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_boundary - m_a_boundary),
               indices = c(1:13))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_entrance - m_a_entrance), 
               indices = c(14:26))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_interior - m_a_interior), 
               indices = c(27:39))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_restaurant - m_a_restaurant), 
               indices = c(40:52))

row.add.mode(model_aries, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_aries, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_aries


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_aries)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_aries)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_aries)

res_distr <- data.frame(res_coef[1:13], 
                        res_coef[14:26],
                        res_coef[27:39],
                        res_coef[40:52],
                        row.names = aries_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:13) {
    if ((res_distr[m,n] < 0) && (c_a_02[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_a_02[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:13) {
    distr_aries[m,n] <- distr_aries[m,n] + res_distr[m,n]
    distr_aries[m,n] <- round(distr_aries[m,n])
  }
}

res_obj
write.csv(res_distr, "Aries_feb_distribution.csv")

# -------------------------------------------------------------------------------------------------

# Marzo

# -------------------------------------------------------------------------------------------------

n <- 0
m <- 0
i <- 1

# Constraint

m_a_boundary <- 0
for (n in 1:13) {
  m_a_boundary <- m_a_boundary + distr_aries[n,1]
}
m_a_entrance <- 0
for (n in 1:13) {
  m_a_entrance <- m_a_entrance + distr_aries[n,2]
}
m_a_interior <- 0
for (n in 1:13) {
  m_a_interior <- m_a_interior + distr_aries[n,3]
}
m_a_restaurant <- 0
for (n in 1:13) {
  m_a_restaurant <- m_a_restaurant + distr_aries[n,4]
}

mmp_a_boundary <- m_a_boundary * medp_a_boundary
mmp_a_entrance <- m_a_entrance * medp_a_entrance
mmp_a_interior <- m_a_interior * medp_a_interior
mmp_a_restaurant <- m_a_restaurant * medp_a_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    low_bounds[i] <- -(distr_aries[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    high_bounds[i] <- nmm_aries[m] - distr_aries[m,n]
    i <- i + 1
  }
}

# Inizializzazione del modello
model_aries = make.lp(nrow = 0, ncol = 52)

lp.control(model_aries, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_aries, obj = c_a_03)

# Definizione dei vincoli
row.add.mode(model_aries, "on") # Inizio a definire i constraint

add.constraint(model_aries, xt = p_a_boundary, type = "<", mmp_a_boundary, 
               indices = c(1:13))
add.constraint(model_aries, xt = p_a_entrance, type = "<", mmp_a_entrance, 
               indices = c(14:26))
add.constraint(model_aries, xt = p_a_interior, type = "<", mmp_a_interior,
               indices = c(27:39))
add.constraint(model_aries, xt = p_a_restaurant, type = "<", mmp_a_restaurant,
               indices = c(40:52))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_boundary - m_a_boundary),
               indices = c(1:13))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_entrance - m_a_entrance), 
               indices = c(14:26))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_interior - m_a_interior), 
               indices = c(27:39))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_restaurant - m_a_restaurant), 
               indices = c(40:52))

row.add.mode(model_aries, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_aries, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_aries


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_aries)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_aries)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_aries)

res_distr <- data.frame(res_coef[1:13], 
                        res_coef[14:26],
                        res_coef[27:39],
                        res_coef[40:52],
                        row.names = aries_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:13) {
    if ((res_distr[m,n] < 0) && (c_a_03[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_a_03[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:13) {
    distr_aries[m,n] <- distr_aries[m,n] + res_distr[m,n]
    distr_aries[m,n] <- round(distr_aries[m,n])
  }
}

res_obj
write.csv(res_distr, "Aries_mar_distribution.csv")

# -------------------------------------------------------------------------------------------------

# Aprile

# -------------------------------------------------------------------------------------------------

n <- 0
m <- 0
i <- 1

# Constraint

m_a_boundary <- 0
for (n in 1:13) {
  m_a_boundary <- m_a_boundary + distr_aries[n,1]
}
m_a_entrance <- 0
for (n in 1:13) {
  m_a_entrance <- m_a_entrance + distr_aries[n,2]
}
m_a_interior <- 0
for (n in 1:13) {
  m_a_interior <- m_a_interior + distr_aries[n,3]
}
m_a_restaurant <- 0
for (n in 1:13) {
  m_a_restaurant <- m_a_restaurant + distr_aries[n,4]
}

mmp_a_boundary <- m_a_boundary * medp_a_boundary
mmp_a_entrance <- m_a_entrance * medp_a_entrance
mmp_a_interior <- m_a_interior * medp_a_interior
mmp_a_restaurant <- m_a_restaurant * medp_a_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    low_bounds[i] <- -(distr_aries[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    high_bounds[i] <- nmm_aries[m] - distr_aries[m,n]
    i <- i + 1
  }
}

# Inizializzazione del modello
model_aries = make.lp(nrow = 0, ncol = 52)

lp.control(model_aries, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_aries, obj = c_a_04)

# Definizione dei vincoli
row.add.mode(model_aries, "on") # Inizio a definire i constraint

add.constraint(model_aries, xt = p_a_boundary, type = "<", mmp_a_boundary, 
               indices = c(1:13))
add.constraint(model_aries, xt = p_a_entrance, type = "<", mmp_a_entrance, 
               indices = c(14:26))
add.constraint(model_aries, xt = p_a_interior, type = "<", mmp_a_interior,
               indices = c(27:39))
add.constraint(model_aries, xt = p_a_restaurant, type = "<", mmp_a_restaurant,
               indices = c(40:52))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_boundary - m_a_boundary),
               indices = c(1:13))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_entrance - m_a_entrance), 
               indices = c(14:26))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_interior - m_a_interior), 
               indices = c(27:39))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_restaurant - m_a_restaurant), 
               indices = c(40:52))

row.add.mode(model_aries, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_aries, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_aries


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_aries)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_aries)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_aries)

res_distr <- data.frame(res_coef[1:13], 
                        res_coef[14:26],
                        res_coef[27:39],
                        res_coef[40:52],
                        row.names = aries_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:13) {
    if ((res_distr[m,n] < 0) && (c_a_04[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_a_04[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:13) {
    distr_aries[m,n] <- distr_aries[m,n] + res_distr[m,n]
    distr_aries[m,n] <- round(distr_aries[m,n])
  }
}

res_obj
write.csv(res_distr, "Aries_apr_distribution.csv")

# -------------------------------------------------------------------------------------------------

# Maggio

# -------------------------------------------------------------------------------------------------

n <- 0
m <- 0
i <- 1

# Constraint

m_a_boundary <- 0
for (n in 1:13) {
  m_a_boundary <- m_a_boundary + distr_aries[n,1]
}
m_a_entrance <- 0
for (n in 1:13) {
  m_a_entrance <- m_a_entrance + distr_aries[n,2]
}
m_a_interior <- 0
for (n in 1:13) {
  m_a_interior <- m_a_interior + distr_aries[n,3]
}
m_a_restaurant <- 0
for (n in 1:13) {
  m_a_restaurant <- m_a_restaurant + distr_aries[n,4]
}

mmp_a_boundary <- m_a_boundary * medp_a_boundary
mmp_a_entrance <- m_a_entrance * medp_a_entrance
mmp_a_interior <- m_a_interior * medp_a_interior
mmp_a_restaurant <- m_a_restaurant * medp_a_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    low_bounds[i] <- -(distr_aries[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    high_bounds[i] <- nmm_aries[m] - distr_aries[m,n]
    i <- i + 1
  }
}

# Inizializzazione del modello
model_aries = make.lp(nrow = 0, ncol = 52)

lp.control(model_aries, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_aries, obj = c_a_05)

# Definizione dei vincoli
row.add.mode(model_aries, "on") # Inizio a definire i constraint

add.constraint(model_aries, xt = p_a_boundary, type = "<", mmp_a_boundary, 
               indices = c(1:13))
add.constraint(model_aries, xt = p_a_entrance, type = "<", mmp_a_entrance, 
               indices = c(14:26))
add.constraint(model_aries, xt = p_a_interior, type = "<", mmp_a_interior,
               indices = c(27:39))
add.constraint(model_aries, xt = p_a_restaurant, type = "<", mmp_a_restaurant,
               indices = c(40:52))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_boundary - m_a_boundary),
               indices = c(1:13))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_entrance - m_a_entrance), 
               indices = c(14:26))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_interior - m_a_interior), 
               indices = c(27:39))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_restaurant - m_a_restaurant), 
               indices = c(40:52))

row.add.mode(model_aries, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_aries, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_aries


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_aries)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_aries)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_aries)

res_distr <- data.frame(res_coef[1:13], 
                        res_coef[14:26],
                        res_coef[27:39],
                        res_coef[40:52],
                        row.names = aries_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:13) {
    if ((res_distr[m,n] < 0) && (c_a_05[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_a_05[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:13) {
    distr_aries[m,n] <- distr_aries[m,n] + res_distr[m,n]
    distr_aries[m,n] <- round(distr_aries[m,n])
  }
}

res_obj
write.csv(res_distr, "Aries_mag_distribution.csv")

# -------------------------------------------------------------------------------------------------

# Giugno

# -------------------------------------------------------------------------------------------------

n <- 0
m <- 0
i <- 1

# Constraint

m_a_boundary <- 0
for (n in 1:13) {
  m_a_boundary <- m_a_boundary + distr_aries[n,1]
}
m_a_entrance <- 0
for (n in 1:13) {
  m_a_entrance <- m_a_entrance + distr_aries[n,2]
}
m_a_interior <- 0
for (n in 1:13) {
  m_a_interior <- m_a_interior + distr_aries[n,3]
}
m_a_restaurant <- 0
for (n in 1:13) {
  m_a_restaurant <- m_a_restaurant + distr_aries[n,4]
}

mmp_a_boundary <- m_a_boundary * medp_a_boundary
mmp_a_entrance <- m_a_entrance * medp_a_entrance
mmp_a_interior <- m_a_interior * medp_a_interior
mmp_a_restaurant <- m_a_restaurant * medp_a_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    low_bounds[i] <- -(distr_aries[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    high_bounds[i] <- nmm_aries[m] - distr_aries[m,n]
    i <- i + 1
  }
}

# Inizializzazione del modello
model_aries = make.lp(nrow = 0, ncol = 52)

lp.control(model_aries, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_aries, obj = c_a_06)

# Definizione dei vincoli
row.add.mode(model_aries, "on") # Inizio a definire i constraint

add.constraint(model_aries, xt = p_a_boundary, type = "<", mmp_a_boundary, 
               indices = c(1:13))
add.constraint(model_aries, xt = p_a_entrance, type = "<", mmp_a_entrance, 
               indices = c(14:26))
add.constraint(model_aries, xt = p_a_interior, type = "<", mmp_a_interior,
               indices = c(27:39))
add.constraint(model_aries, xt = p_a_restaurant, type = "<", mmp_a_restaurant,
               indices = c(40:52))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 195, indices = c(1:13))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 218, indices = c(14:26))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 248, indices = c(27:39))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = 187, indices = c(40:52))

row.add.mode(model_aries, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_aries, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_aries


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_aries)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_aries)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_aries)

res_distr <- data.frame(res_coef[1:13], 
                        res_coef[14:26],
                        res_coef[27:39],
                        res_coef[40:52],
                        row.names = aries_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:13) {
    if ((res_distr[m,n] < 0) && (c_a_06[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_a_06[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:13) {
    distr_aries[m,n] <- distr_aries[m,n] + res_distr[m,n]
    distr_aries[m,n] <- round(distr_aries[m,n])
  }
}

res_obj
write.csv(res_distr, "Aries_giu_distribution.csv")

# -------------------------------------------------------------------------------------------------

# Luglio

# -------------------------------------------------------------------------------------------------

n <- 0
m <- 0
i <- 1

# Constraint

m_a_boundary <- 0
for (n in 1:13) {
  m_a_boundary <- m_a_boundary + distr_aries[n,1]
}
m_a_entrance <- 0
for (n in 1:13) {
  m_a_entrance <- m_a_entrance + distr_aries[n,2]
}
m_a_interior <- 0
for (n in 1:13) {
  m_a_interior <- m_a_interior + distr_aries[n,3]
}
m_a_restaurant <- 0
for (n in 1:13) {
  m_a_restaurant <- m_a_restaurant + distr_aries[n,4]
}

mmp_a_boundary <- m_a_boundary * medp_a_boundary
mmp_a_entrance <- m_a_entrance * medp_a_entrance
mmp_a_interior <- m_a_interior * medp_a_interior
mmp_a_restaurant <- m_a_restaurant * medp_a_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    low_bounds[i] <- -(distr_aries[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    high_bounds[i] <- nmm_aries[m] - distr_aries[m,n]
    i <- i + 1
  }
}

# Inizializzazione del modello
model_aries = make.lp(nrow = 0, ncol = 52)

lp.control(model_aries, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_aries, obj = c_a_07)

# Definizione dei vincoli
row.add.mode(model_aries, "on") # Inizio a definire i constraint

add.constraint(model_aries, xt = p_a_boundary, type = "<", mmp_a_boundary, 
               indices = c(1:13))
add.constraint(model_aries, xt = p_a_entrance, type = "<", mmp_a_entrance, 
               indices = c(14:26))
add.constraint(model_aries, xt = p_a_interior, type = "<", mmp_a_interior,
               indices = c(27:39))
add.constraint(model_aries, xt = p_a_restaurant, type = "<", mmp_a_restaurant,
               indices = c(40:52))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_boundary - m_a_boundary),
               indices = c(1:13))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_entrance - m_a_entrance), 
               indices = c(14:26))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_interior - m_a_interior), 
               indices = c(27:39))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_restaurant - m_a_restaurant), 
               indices = c(40:52))
row.add.mode(model_aries, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_aries, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_aries


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_aries)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_aries)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_aries)

res_distr <- data.frame(res_coef[1:13], 
                        res_coef[14:26],
                        res_coef[27:39],
                        res_coef[40:52],
                        row.names = aries_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:13) {
    if ((res_distr[m,n] < 0) && (c_a_07[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_a_07[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:13) {
    distr_aries[m,n] <- distr_aries[m,n] + res_distr[m,n]
    distr_aries[m,n] <- round(distr_aries[m,n])
  }
}

res_obj
write.csv(res_distr, "Aries_lug_distribution.csv")

# -------------------------------------------------------------------------------------------------

# Agosto

# -------------------------------------------------------------------------------------------------

n <- 0
m <- 0
i <- 1

# Constraint

m_a_boundary <- 0
for (n in 1:13) {
  m_a_boundary <- m_a_boundary + distr_aries[n,1]
}
m_a_entrance <- 0
for (n in 1:13) {
  m_a_entrance <- m_a_entrance + distr_aries[n,2]
}
m_a_interior <- 0
for (n in 1:13) {
  m_a_interior <- m_a_interior + distr_aries[n,3]
}
m_a_restaurant <- 0
for (n in 1:13) {
  m_a_restaurant <- m_a_restaurant + distr_aries[n,4]
}

mmp_a_boundary <- m_a_boundary * medp_a_boundary
mmp_a_entrance <- m_a_entrance * medp_a_entrance
mmp_a_interior <- m_a_interior * medp_a_interior
mmp_a_restaurant <- m_a_restaurant * medp_a_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    low_bounds[i] <- -(distr_aries[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:13) {
    high_bounds[i] <- nmm_aries[m] - distr_aries[m,n]
    i <- i + 1
  }
}

# Inizializzazione del modello
model_aries = make.lp(nrow = 0, ncol = 52)

lp.control(model_aries, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_aries, obj = c_a_08)

# Definizione dei vincoli
row.add.mode(model_aries, "on") # Inizio a definire i constraint

add.constraint(model_aries, xt = p_a_boundary, type = "<", mmp_a_boundary, 
               indices = c(1:13))
add.constraint(model_aries, xt = p_a_entrance, type = "<", mmp_a_entrance, 
               indices = c(14:26))
add.constraint(model_aries, xt = p_a_interior, type = "<", mmp_a_interior,
               indices = c(27:39))
add.constraint(model_aries, xt = p_a_restaurant, type = "<", mmp_a_restaurant,
               indices = c(40:52))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_boundary - m_a_boundary),
               indices = c(1:13))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_entrance - m_a_entrance), 
               indices = c(14:26))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_interior - m_a_interior), 
               indices = c(27:39))

add.constraint(model_aries,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_a_restaurant - m_a_restaurant), 
               indices = c(40:52))

row.add.mode(model_aries, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_aries, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_aries


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_aries)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_aries)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_aries)

res_distr <- data.frame(res_coef[1:13], 
                        res_coef[14:26],
                        res_coef[27:39],
                        res_coef[40:52],
                        row.names = aries_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:13) {
    if ((res_distr[m,n] < 0) && (c_a_08[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_a_08[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:13) {
    distr_aries[m,n] <- distr_aries[m,n] + res_distr[m,n]
    distr_aries[m,n] <- round(distr_aries[m,n])
  }
}

res_obj
write.csv(res_distr, "Aries_ago_distribution.csv")


# -------------------------------------------------------------------------------------------------

# Libra

# -------------------------------------------------------------------------------------------------

# Ottobre

n <- 0
m <- 0
i <- 1

# Constraint

m_l_boundary <- 0
for (n in 1:14) {
  m_l_boundary <- m_l_boundary + distr_libra[n,1]
}
m_l_entrance <- 0
for (n in 1:14) {
  m_l_entrance <- m_l_entrance + distr_libra[n,2]
}
m_l_interior <- 0
for (n in 1:14) {
  m_l_interior <- m_l_interior + distr_libra[n,3]
}
m_l_restaurant <- 0
for (n in 1:14) {
  m_l_restaurant <- m_l_restaurant + distr_libra[n,4]
}

mmp_l_boundary <- m_l_boundary * medp_l_boundary
mmp_l_entrance <- m_l_entrance * medp_l_entrance
mmp_l_interior <- m_l_interior * medp_l_interior
mmp_l_restaurant <- m_l_restaurant * medp_l_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    low_bounds[i] <- -(distr_libra[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    high_bounds[i] <- nmm_libra[m] - distr_libra[m,n] 
    i <- i + 1
  }
}

# Inizializzazione del modello
model_libra = make.lp(nrow = 0, ncol = 56)

lp.control(model_libra, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_libra, obj = c_l_10)

# Definizione dei vincoli
row.add.mode(model_libra, "on") # Inizio a definire i constraint

add.constraint(model_libra, xt = p_l_boundary, type = "<", rhs = mmp_l_boundary, 
               indices = c(1:14))
add.constraint(model_libra, xt = p_l_entrance, type = "<", rhs = mmp_l_entrance, 
               indices = c(15:28))
add.constraint(model_libra, xt = p_l_interior, type = "<", rhs = mmp_l_interior,
               indices = c(29:42))
add.constraint(model_libra, xt = p_l_restaurant, type = "<", rhs = mmp_l_restaurant,
               indices = c(43:56))

add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_boundary - m_l_boundary), 
               indices = c(1:14))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_entrance - m_l_entrance), 
               indices = c(15:28))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_interior - m_l_interior), 
               indices = c(29:42))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_restaurant - m_l_restaurant), 
               indices = c(43:56))

row.add.mode(model_libra, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_libra, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_libra


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_libra)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_libra)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_libra)

res_distr <- data.frame(res_coef[1:14], 
                        res_coef[15:28],
                        res_coef[29:42],
                        res_coef[43:56],
                        row.names = libra_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:14) {
    if ((res_distr[m,n] < 0) && (c_l_10[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_l_10[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:14) {
    distr_libra[m,n] <- distr_libra[m,n] + res_distr[m,n] 
  }
}

res_obj
write.csv(res_distr, "Libra_ott_distribution.csv")

# -------------------------------------------------------------------------------------------------

# Novembre

# -------------------------------------------------------------------------------------------------

n <- 0
m <- 0
i <- 1

# Constraint

m_l_boundary <- 0
for (n in 1:14) {
  m_l_boundary <- m_l_boundary + distr_libra[n,1]
}
m_l_entrance <- 0
for (n in 1:14) {
  m_l_entrance <- m_l_entrance + distr_libra[n,2]
}
m_l_interior <- 0
for (n in 1:14) {
  m_l_interior <- m_l_interior + distr_libra[n,3]
}
m_l_restaurant <- 0
for (n in 1:14) {
  m_l_restaurant <- m_l_restaurant + distr_libra[n,4]
}

mmp_l_boundary <- m_l_boundary * medp_l_boundary
mmp_l_entrance <- m_l_entrance * medp_l_entrance
mmp_l_interior <- m_l_interior * medp_l_interior
mmp_l_restaurant <- m_l_restaurant * medp_l_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    low_bounds[i] <- -(distr_libra[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    high_bounds[i] <- nmm_libra[m] - distr_libra[m,n] 
    i <- i + 1
  }
}

# Inizializzazione del modello
model_libra = make.lp(nrow = 0, ncol = 56)

lp.control(model_libra, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_libra, obj = c_l_11)

# Definizione dei vincoli
row.add.mode(model_libra, "on") # Inizio a definire i constraint

add.constraint(model_libra, xt = p_l_boundary, type = "<", rhs = mmp_l_boundary, 
               indices = c(1:14))
add.constraint(model_libra, xt = p_l_entrance, type = "<", rhs = mmp_l_entrance, 
               indices = c(15:28))
add.constraint(model_libra, xt = p_l_interior, type = "<", rhs = mmp_l_interior,
               indices = c(29:42))
add.constraint(model_libra, xt = p_l_restaurant, type = "<", rhs = mmp_l_restaurant,
               indices = c(43:56))

add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_boundary - m_l_boundary), 
               indices = c(1:14))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_entrance - m_l_entrance), 
               indices = c(15:28))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_interior - m_l_interior), 
               indices = c(29:42))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_restaurant - m_l_restaurant), 
               indices = c(43:56))

row.add.mode(model_libra, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_libra, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_libra


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_libra)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_libra)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_libra)

res_distr <- data.frame(res_coef[1:14], 
                        res_coef[15:28],
                        res_coef[29:42],
                        res_coef[43:56],
                        row.names = libra_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:14) {
    if ((res_distr[m,n] < 0) && (c_l_11[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_l_11[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:14) {
    distr_libra[m,n] <- distr_libra[m,n] + res_distr[m,n] 
  }
}

res_obj
write.csv(res_distr, "Libra_nov_distribution.csv")

# -------------------------------------------------------------------------------------------------

# Dicembre

# -------------------------------------------------------------------------------------------------

n <- 0
m <- 0
i <- 1

# Constraint

m_l_boundary <- 0
for (n in 1:14) {
  m_l_boundary <- m_l_boundary + distr_libra[n,1]
}
m_l_entrance <- 0
for (n in 1:14) {
  m_l_entrance <- m_l_entrance + distr_libra[n,2]
}
m_l_interior <- 0
for (n in 1:14) {
  m_l_interior <- m_l_interior + distr_libra[n,3]
}
m_l_restaurant <- 0
for (n in 1:14) {
  m_l_restaurant <- m_l_restaurant + distr_libra[n,4]
}

mmp_l_boundary <- m_l_boundary * medp_l_boundary
mmp_l_entrance <- m_l_entrance * medp_l_entrance
mmp_l_interior <- m_l_interior * medp_l_interior
mmp_l_restaurant <- m_l_restaurant * medp_l_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    low_bounds[i] <- -(distr_libra[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    high_bounds[i] <- nmm_libra[m] - distr_libra[m,n] 
    i <- i + 1
  }
}

# Inizializzazione del modello
model_libra = make.lp(nrow = 0, ncol = 56)

lp.control(model_libra, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_libra, obj = c_l_12)

# Definizione dei vincoli
row.add.mode(model_libra, "on") # Inizio a definire i constraint

add.constraint(model_libra, xt = p_l_boundary, type = "<", rhs = mmp_l_boundary, 
               indices = c(1:14))
add.constraint(model_libra, xt = p_l_entrance, type = "<", rhs = mmp_l_entrance, 
               indices = c(15:28))
add.constraint(model_libra, xt = p_l_interior, type = "<", rhs = mmp_l_interior,
               indices = c(29:42))
add.constraint(model_libra, xt = p_l_restaurant, type = "<", rhs = mmp_l_restaurant,
               indices = c(43:56))

add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_boundary - m_l_boundary), 
               indices = c(1:14))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_entrance - m_l_entrance), 
               indices = c(15:28))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_interior - m_l_interior), 
               indices = c(29:42))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_restaurant - m_l_restaurant), 
               indices = c(43:56))

row.add.mode(model_libra, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_libra, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_libra


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_libra)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_libra)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_libra)

res_distr <- data.frame(res_coef[1:14], 
                        res_coef[15:28],
                        res_coef[29:42],
                        res_coef[43:56],
                        row.names = libra_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:14) {
    if ((res_distr[m,n] < 0) && (c_l_12[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_l_12[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:14) {
    distr_libra[m,n] <- distr_libra[m,n] + res_distr[m,n] 
  }
}

res_obj
write.csv(res_distr, "Libra_dic_distribution.csv")

# -------------------------------------------------------------------------------------------------

# Gennaio

# -------------------------------------------------------------------------------------------------

n <- 0
m <- 0
i <- 1

# Constraint

m_l_boundary <- 0
for (n in 1:14) {
  m_l_boundary <- m_l_boundary + distr_libra[n,1]
}
m_l_entrance <- 0
for (n in 1:14) {
  m_l_entrance <- m_l_entrance + distr_libra[n,2]
}
m_l_interior <- 0
for (n in 1:14) {
  m_l_interior <- m_l_interior + distr_libra[n,3]
}
m_l_restaurant <- 0
for (n in 1:14) {
  m_l_restaurant <- m_l_restaurant + distr_libra[n,4]
}

mmp_l_boundary <- m_l_boundary * medp_l_boundary
mmp_l_entrance <- m_l_entrance * medp_l_entrance
mmp_l_interior <- m_l_interior * medp_l_interior
mmp_l_restaurant <- m_l_restaurant * medp_l_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    low_bounds[i] <- -(distr_libra[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    high_bounds[i] <- nmm_libra[m] - distr_libra[m,n] 
    i <- i + 1
  }
}

# Inizializzazione del modello
model_libra = make.lp(nrow = 0, ncol = 56)

lp.control(model_libra, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_libra, obj = c_l_01)

# Definizione dei vincoli
row.add.mode(model_libra, "on") # Inizio a definire i constraint

add.constraint(model_libra, xt = p_l_boundary, type = "<", rhs = mmp_l_boundary, 
               indices = c(1:14))
add.constraint(model_libra, xt = p_l_entrance, type = "<", rhs = mmp_l_entrance, 
               indices = c(15:28))
add.constraint(model_libra, xt = p_l_interior, type = "<", rhs = mmp_l_interior,
               indices = c(29:42))
add.constraint(model_libra, xt = p_l_restaurant, type = "<", rhs = mmp_l_restaurant,
               indices = c(43:56))

add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_boundary - m_l_boundary), 
               indices = c(1:14))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_entrance - m_l_entrance), 
               indices = c(15:28))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_interior - m_l_interior), 
               indices = c(29:42))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_restaurant - m_l_restaurant), 
               indices = c(43:56))

row.add.mode(model_libra, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_libra, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_libra


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_libra)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_libra)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_libra)

res_distr <- data.frame(res_coef[1:14], 
                        res_coef[15:28],
                        res_coef[29:42],
                        res_coef[43:56],
                        row.names = libra_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:14) {
    if ((res_distr[m,n] < 0) && (c_l_01[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_l_01[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:14) {
    distr_libra[m,n] <- distr_libra[m,n] + res_distr[m,n] 
  }
}

res_obj
write.csv(res_distr, "Libra_gen_distribution.csv")

# -------------------------------------------------------------------------------------------------

# Febbraio

# -------------------------------------------------------------------------------------------------

n <- 0
m <- 0
i <- 1

# Constraint

m_l_boundary <- 0
for (n in 1:14) {
  m_l_boundary <- m_l_boundary + distr_libra[n,1]
}
m_l_entrance <- 0
for (n in 1:14) {
  m_l_entrance <- m_l_entrance + distr_libra[n,2]
}
m_l_interior <- 0
for (n in 1:14) {
  m_l_interior <- m_l_interior + distr_libra[n,3]
}
m_l_restaurant <- 0
for (n in 1:14) {
  m_l_restaurant <- m_l_restaurant + distr_libra[n,4]
}

mmp_l_boundary <- m_l_boundary * medp_l_boundary
mmp_l_entrance <- m_l_entrance * medp_l_entrance
mmp_l_interior <- m_l_interior * medp_l_interior
mmp_l_restaurant <- m_l_restaurant * medp_l_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    low_bounds[i] <- -(distr_libra[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    high_bounds[i] <- nmm_libra[m] - distr_libra[m,n] 
    i <- i + 1
  }
}

# Inizializzazione del modello
model_libra = make.lp(nrow = 0, ncol = 56)

lp.control(model_libra, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_libra, obj = c_l_02)

# Definizione dei vincoli
row.add.mode(model_libra, "on") # Inizio a definire i constraint

add.constraint(model_libra, xt = p_l_boundary, type = "<", rhs = mmp_l_boundary, 
               indices = c(1:14))
add.constraint(model_libra, xt = p_l_entrance, type = "<", rhs = mmp_l_entrance, 
               indices = c(15:28))
add.constraint(model_libra, xt = p_l_interior, type = "<", rhs = mmp_l_interior,
               indices = c(29:42))
add.constraint(model_libra, xt = p_l_restaurant, type = "<", rhs = mmp_l_restaurant,
               indices = c(43:56))

add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_boundary - m_l_boundary), 
               indices = c(1:14))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_entrance - m_l_entrance), 
               indices = c(15:28))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_interior - m_l_interior), 
               indices = c(29:42))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_restaurant - m_l_restaurant), 
               indices = c(43:56))

row.add.mode(model_libra, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_libra, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_libra


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_libra)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_libra)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_libra)

res_distr <- data.frame(res_coef[1:14], 
                        res_coef[15:28],
                        res_coef[29:42],
                        res_coef[43:56],
                        row.names = libra_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:14) {
    if ((res_distr[m,n] < 0) && (c_l_02[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_l_02[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:14) {
    distr_libra[m,n] <- distr_libra[m,n] + res_distr[m,n] 
  }
}

res_obj
write.csv(res_distr, "Libra_feb_distribution.csv")

# -------------------------------------------------------------------------------------------------

# Marzo

# -------------------------------------------------------------------------------------------------

n <- 0
m <- 0
i <- 1

# Constraint

m_l_boundary <- 0
for (n in 1:14) {
  m_l_boundary <- m_l_boundary + distr_libra[n,1]
}
m_l_entrance <- 0
for (n in 1:14) {
  m_l_entrance <- m_l_entrance + distr_libra[n,2]
}
m_l_interior <- 0
for (n in 1:14) {
  m_l_interior <- m_l_interior + distr_libra[n,3]
}
m_l_restaurant <- 0
for (n in 1:14) {
  m_l_restaurant <- m_l_restaurant + distr_libra[n,4]
}

mmp_l_boundary <- m_l_boundary * medp_l_boundary
mmp_l_entrance <- m_l_entrance * medp_l_entrance
mmp_l_interior <- m_l_interior * medp_l_interior
mmp_l_restaurant <- m_l_restaurant * medp_l_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    low_bounds[i] <- -(distr_libra[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    high_bounds[i] <- nmm_libra[m] - distr_libra[m,n] 
    i <- i + 1
  }
}

# Inizializzazione del modello
model_libra = make.lp(nrow = 0, ncol = 56)

lp.control(model_libra, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_libra, obj = c_l_03)

# Definizione dei vincoli
row.add.mode(model_libra, "on") # Inizio a definire i constraint

add.constraint(model_libra, xt = p_l_boundary, type = "<", rhs = mmp_l_boundary, 
               indices = c(1:14))
add.constraint(model_libra, xt = p_l_entrance, type = "<", rhs = mmp_l_entrance, 
               indices = c(15:28))
add.constraint(model_libra, xt = p_l_interior, type = "<", rhs = mmp_l_interior,
               indices = c(29:42))
add.constraint(model_libra, xt = p_l_restaurant, type = "<", rhs = mmp_l_restaurant,
               indices = c(43:56))

add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_boundary - m_l_boundary), 
               indices = c(1:14))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_entrance - m_l_entrance), 
               indices = c(15:28))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_interior - m_l_interior), 
               indices = c(29:42))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_restaurant - m_l_restaurant), 
               indices = c(43:56))

row.add.mode(model_libra, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_libra, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_libra


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_libra)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_libra)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_libra)

res_distr <- data.frame(res_coef[1:14], 
                        res_coef[15:28],
                        res_coef[29:42],
                        res_coef[43:56],
                        row.names = libra_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:14) {
    if ((res_distr[m,n] < 0) && (c_l_03[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_l_03[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:14) {
    distr_libra[m,n] <- distr_libra[m,n] + res_distr[m,n] 
  }
}

res_obj
write.csv(res_distr, "Libra_mar_distribution.csv")
# -------------------------------------------------------------------------------------------------

# Aprile

# -------------------------------------------------------------------------------------------------

n <- 0
m <- 0
i <- 1

# Constraint

m_l_boundary <- 0
for (n in 1:14) {
  m_l_boundary <- m_l_boundary + distr_libra[n,1]
}
m_l_entrance <- 0
for (n in 1:14) {
  m_l_entrance <- m_l_entrance + distr_libra[n,2]
}
m_l_interior <- 0
for (n in 1:14) {
  m_l_interior <- m_l_interior + distr_libra[n,3]
}
m_l_restaurant <- 0
for (n in 1:14) {
  m_l_restaurant <- m_l_restaurant + distr_libra[n,4]
}

mmp_l_boundary <- m_l_boundary * medp_l_boundary
mmp_l_entrance <- m_l_entrance * medp_l_entrance
mmp_l_interior <- m_l_interior * medp_l_interior
mmp_l_restaurant <- m_l_restaurant * medp_l_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    low_bounds[i] <- -(distr_libra[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    high_bounds[i] <- nmm_libra[m] - distr_libra[m,n] 
    i <- i + 1
  }
}

# Inizializzazione del modello
model_libra = make.lp(nrow = 0, ncol = 56)

lp.control(model_libra, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_libra, obj = c_l_04)

# Definizione dei vincoli
row.add.mode(model_libra, "on") # Inizio a definire i constraint

add.constraint(model_libra, xt = p_l_boundary, type = "<", rhs = mmp_l_boundary, 
               indices = c(1:14))
add.constraint(model_libra, xt = p_l_entrance, type = "<", rhs = mmp_l_entrance, 
               indices = c(15:28))
add.constraint(model_libra, xt = p_l_interior, type = "<", rhs = mmp_l_interior,
               indices = c(29:42))
add.constraint(model_libra, xt = p_l_restaurant, type = "<", rhs = mmp_l_restaurant,
               indices = c(43:56))

add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_boundary - m_l_boundary), 
               indices = c(1:14))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_entrance - m_l_entrance), 
               indices = c(15:28))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_interior - m_l_interior), 
               indices = c(29:42))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_restaurant - m_l_restaurant), 
               indices = c(43:56))

row.add.mode(model_libra, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_libra, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_libra


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_libra)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_libra)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_libra)

res_distr <- data.frame(res_coef[1:14], 
                        res_coef[15:28],
                        res_coef[29:42],
                        res_coef[43:56],
                        row.names = libra_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:14) {
    if ((res_distr[m,n] < 0) && (c_l_04[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_l_04[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:14) {
    distr_libra[m,n] <- distr_libra[m,n] + res_distr[m,n] 
  }
}

res_obj
write.csv(res_distr, "Libra_apr_distribution.csv")

# -------------------------------------------------------------------------------------------------

# Maggio

# -------------------------------------------------------------------------------------------------

n <- 0
m <- 0
i <- 1

# Constraint

m_l_boundary <- 0
for (n in 1:14) {
  m_l_boundary <- m_l_boundary + distr_libra[n,1]
}
m_l_entrance <- 0
for (n in 1:14) {
  m_l_entrance <- m_l_entrance + distr_libra[n,2]
}
m_l_interior <- 0
for (n in 1:14) {
  m_l_interior <- m_l_interior + distr_libra[n,3]
}
m_l_restaurant <- 0
for (n in 1:14) {
  m_l_restaurant <- m_l_restaurant + distr_libra[n,4]
}

mmp_l_boundary <- m_l_boundary * medp_l_boundary
mmp_l_entrance <- m_l_entrance * medp_l_entrance
mmp_l_interior <- m_l_interior * medp_l_interior
mmp_l_restaurant <- m_l_restaurant * medp_l_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    low_bounds[i] <- -(distr_libra[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    high_bounds[i] <- nmm_libra[m] - distr_libra[m,n] 
    i <- i + 1
  }
}

# Inizializzazione del modello
model_libra = make.lp(nrow = 0, ncol = 56)

lp.control(model_libra, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_libra, obj = c_l_05)

# Definizione dei vincoli
row.add.mode(model_libra, "on") # Inizio a definire i constraint

add.constraint(model_libra, xt = p_l_boundary, type = "<", rhs = mmp_l_boundary, 
               indices = c(1:14))
add.constraint(model_libra, xt = p_l_entrance, type = "<", rhs = mmp_l_entrance, 
               indices = c(15:28))
add.constraint(model_libra, xt = p_l_interior, type = "<", rhs = mmp_l_interior,
               indices = c(29:42))
add.constraint(model_libra, xt = p_l_restaurant, type = "<", rhs = mmp_l_restaurant,
               indices = c(43:56))

add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_boundary - m_l_boundary), 
               indices = c(1:14))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_entrance - m_l_entrance), 
               indices = c(15:28))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_interior - m_l_interior), 
               indices = c(29:42))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_restaurant - m_l_restaurant), 
               indices = c(43:56))

row.add.mode(model_libra, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_libra, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_libra


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_libra)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_libra)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_libra)

res_distr <- data.frame(res_coef[1:14], 
                        res_coef[15:28],
                        res_coef[29:42],
                        res_coef[43:56],
                        row.names = libra_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:14) {
    if ((res_distr[m,n] < 0) && (c_l_05[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_l_05[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:14) {
    distr_libra[m,n] <- distr_libra[m,n] + res_distr[m,n] 
  }
}

res_obj
write.csv(res_distr, "Libra_mag_distribution.csv")

# -------------------------------------------------------------------------------------------------

# Giugno

# -------------------------------------------------------------------------------------------------

n <- 0
m <- 0
i <- 1

# Constraint

m_l_boundary <- 0
for (n in 1:14) {
  m_l_boundary <- m_l_boundary + distr_libra[n,1]
}
m_l_entrance <- 0
for (n in 1:14) {
  m_l_entrance <- m_l_entrance + distr_libra[n,2]
}
m_l_interior <- 0
for (n in 1:14) {
  m_l_interior <- m_l_interior + distr_libra[n,3]
}
m_l_restaurant <- 0
for (n in 1:14) {
  m_l_restaurant <- m_l_restaurant + distr_libra[n,4]
}

mmp_l_boundary <- m_l_boundary * medp_l_boundary
mmp_l_entrance <- m_l_entrance * medp_l_entrance
mmp_l_interior <- m_l_interior * medp_l_interior
mmp_l_restaurant <- m_l_restaurant * medp_l_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    low_bounds[i] <- -(distr_libra[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    high_bounds[i] <- nmm_libra[m] - distr_libra[m,n] 
    i <- i + 1
  }
}

# Inizializzazione del modello
model_libra = make.lp(nrow = 0, ncol = 56)

lp.control(model_libra, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_libra, obj = c_l_06)

# Definizione dei vincoli
row.add.mode(model_libra, "on") # Inizio a definire i constraint

add.constraint(model_libra, xt = p_l_boundary, type = "<", rhs = mmp_l_boundary, 
               indices = c(1:14))
add.constraint(model_libra, xt = p_l_entrance, type = "<", rhs = mmp_l_entrance, 
               indices = c(15:28))
add.constraint(model_libra, xt = p_l_interior, type = "<", rhs = mmp_l_interior,
               indices = c(29:42))
add.constraint(model_libra, xt = p_l_restaurant, type = "<", rhs = mmp_l_restaurant,
               indices = c(43:56))

add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_boundary - m_l_boundary), 
               indices = c(1:14))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_entrance - m_l_entrance), 
               indices = c(15:28))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_interior - m_l_interior), 
               indices = c(29:42))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_restaurant - m_l_restaurant), 
               indices = c(43:56))

row.add.mode(model_libra, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_libra, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_libra


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_libra)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_libra)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_libra)

res_distr <- data.frame(res_coef[1:14], 
                        res_coef[15:28],
                        res_coef[29:42],
                        res_coef[43:56],
                        row.names = libra_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:14) {
    if ((res_distr[m,n] < 0) && (c_l_06[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_l_06[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:14) {
    distr_libra[m,n] <- distr_libra[m,n] + res_distr[m,n] 
  }
}

res_obj
write.csv(res_distr, "Libra_giu_distribution.csv")

# -------------------------------------------------------------------------------------------------

# Luglio

# -------------------------------------------------------------------------------------------------

n <- 0
m <- 0
i <- 1

# Constraint

m_l_boundary <- 0
for (n in 1:14) {
  m_l_boundary <- m_l_boundary + distr_libra[n,1]
}
m_l_entrance <- 0
for (n in 1:14) {
  m_l_entrance <- m_l_entrance + distr_libra[n,2]
}
m_l_interior <- 0
for (n in 1:14) {
  m_l_interior <- m_l_interior + distr_libra[n,3]
}
m_l_restaurant <- 0
for (n in 1:14) {
  m_l_restaurant <- m_l_restaurant + distr_libra[n,4]
}

mmp_l_boundary <- m_l_boundary * medp_l_boundary
mmp_l_entrance <- m_l_entrance * medp_l_entrance
mmp_l_interior <- m_l_interior * medp_l_interior
mmp_l_restaurant <- m_l_restaurant * medp_l_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    low_bounds[i] <- -(distr_libra[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    high_bounds[i] <- nmm_libra[m] - distr_libra[m,n] 
    i <- i + 1
  }
}

# Inizializzazione del modello
model_libra = make.lp(nrow = 0, ncol = 56)

lp.control(model_libra, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_libra, obj = c_l_07)

# Definizione dei vincoli
row.add.mode(model_libra, "on") # Inizio a definire i constraint

add.constraint(model_libra, xt = p_l_boundary, type = "<", rhs = mmp_l_boundary, 
               indices = c(1:14))
add.constraint(model_libra, xt = p_l_entrance, type = "<", rhs = mmp_l_entrance, 
               indices = c(15:28))
add.constraint(model_libra, xt = p_l_interior, type = "<", rhs = mmp_l_interior,
               indices = c(29:42))
add.constraint(model_libra, xt = p_l_restaurant, type = "<", rhs = mmp_l_restaurant,
               indices = c(43:56))

add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_boundary - m_l_boundary), 
               indices = c(1:14))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_entrance - m_l_entrance), 
               indices = c(15:28))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_interior - m_l_interior), 
               indices = c(29:42))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_restaurant - m_l_restaurant), 
               indices = c(43:56))

row.add.mode(model_libra, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_libra, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_libra


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_libra)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_libra)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_libra)

res_distr <- data.frame(res_coef[1:14], 
                        res_coef[15:28],
                        res_coef[29:42],
                        res_coef[43:56],
                        row.names = libra_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:14) {
    if ((res_distr[m,n] < 0) && (c_l_07[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_l_07[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:14) {
    distr_libra[m,n] <- distr_libra[m,n] + res_distr[m,n] 
  }
}

res_obj
write.csv(res_distr, "Libra_lug_distribution.csv")

# -------------------------------------------------------------------------------------------------

# Agosto

# -------------------------------------------------------------------------------------------------

n <- 0
m <- 0
i <- 1

# Constraint

m_l_boundary <- 0
for (n in 1:14) {
  m_l_boundary <- m_l_boundary + distr_libra[n,1]
}
m_l_entrance <- 0
for (n in 1:14) {
  m_l_entrance <- m_l_entrance + distr_libra[n,2]
}
m_l_interior <- 0
for (n in 1:14) {
  m_l_interior <- m_l_interior + distr_libra[n,3]
}
m_l_restaurant <- 0
for (n in 1:14) {
  m_l_restaurant <- m_l_restaurant + distr_libra[n,4]
}

new_mmp_l_boundary <- (mm_l_boundary * m_l_boundary)/mmp_l_boundary
new_mmp_l_entrance <- (mm_l_entrance * m_l_entrance)/mmp_l_entrance
new_mmp_l_interior <- (mm_l_interior * m_l_interior)/mmp_l_interior
new_mmp_l_restaurant <- (mm_l_restaurant * m_l_restaurant)/mmp_l_restaurant

# Bounds

low_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    low_bounds[i] <- -(distr_libra[m,n])
    i <- i + 1
  }
}

i <- 1

high_bounds <- c()
for (n in 1:4) {
  for (m in 1:14) {
    high_bounds[i] <- nmm_libra[m] - distr_libra[m,n] 
    i <- i + 1
  }
}

# Inizializzazione del modello
model_libra = make.lp(nrow = 0, ncol = 56)

lp.control(model_libra, sense = "max")

# Definizione dei coefficienti della funzione obiettivo
set.objfn(model_libra, obj = c_l_08)

# Definizione dei vincoli
row.add.mode(model_libra, "on") # Inizio a definire i constraint

add.constraint(model_libra, xt = p_l_boundary, type = "<", rhs = mmp_l_boundary, 
               indices = c(1:14))
add.constraint(model_libra, xt = p_l_entrance, type = "<", rhs = mmp_l_entrance, 
               indices = c(15:28))
add.constraint(model_libra, xt = p_l_interior, type = "<", rhs = mmp_l_interior,
               indices = c(29:42))
add.constraint(model_libra, xt = p_l_restaurant, type = "<", rhs = mmp_l_restaurant,
               indices = c(43:56))

add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_boundary - m_l_boundary), 
               indices = c(1:14))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_entrance - m_l_entrance), 
               indices = c(15:28))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_interior - m_l_interior), 
               indices = c(29:42))
add.constraint(model_libra,
               xt = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), type = "<=", rhs = (mm_l_restaurant - m_l_restaurant), 
               indices = c(43:56))

row.add.mode(model_libra, "off") # Finisco di definire i constraint

# Definizione dei limiti relativi alle variabili
set.bounds(model_libra, lower = low_bounds, upper = high_bounds)
# lower: limiti inferiori
# upper: limiti superiori
# Se si vuole mettere un limite infinito inserire un numero grande, es.99999999999

# Per stampare il modello creato
model_libra


# Risoluzione del problema e analisi dei risultati

# Risolvo il problema attraverso il metodo del Simplesso
solve(model_libra)

# Valori ottimali dei coefficienti
res_coef <- get.variables(model_libra)
res_coef <- round(res_coef)
# Valore ottimale del valore della funzione obiettivo
obj <- get.objective(model_libra)

res_distr <- data.frame(res_coef[1:14], 
                        res_coef[15:28],
                        res_coef[29:42],
                        res_coef[43:56],
                        row.names = libra_names)

colnames(res_distr) <- zones

i <- 1
diff_obj <- 0
for (n in 1:4) {
  for (m in 1:14) {
    if ((res_distr[m,n] < 0) && (c_l_08[i] < 0))
      diff_obj <- diff_obj + (res_distr[m,n] * c_l_08[i])
    i <- i + 1
  }
}
res_obj <- obj - diff_obj
res_obj <- round(res_obj)

for (n in 1:4) {
  for (m in 1:14) {
    distr_libra[m,n] <- distr_libra[m,n] + res_distr[m,n] 
  }
}

res_obj
write.csv(res_distr, "Libra_ago_distribution.csv")