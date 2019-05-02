# ------------------------------------------------------------------------------------------------

# Progetto Casino

# Crespi Daniele
# Spedale Stefania
# Tornatora Raffaele

# Import dei dati, esplorazione e preprocessing

# ------------------------------------------------------------------------------------------------

# Import dei dati
data <- readxl::read_xls(path = "Dataset/Lucky_Duck_Entertainment_revenue_2013.xls")

# Preprocessing

# Questo caso presenta nella colonna diff_w_lower un .; si elimina il caso 
data <- data[-c(1764),]

# Creazione dei dataset aggregati per le analisi: 

# Struttura del primo dataset da creare:
# Chiave 1  Chiave 2  Valori
# con:
# Chiave 1: Denomination - Machine Type
# Chiave 2: Casinò - Section
# Valori: SUM(NoMachines) e MAX finale sui periodi
#         AVG(GrossRevenuePerMachine)
#         MED(Plays)

# Struttura del secondo dataset per le analisi:
# Chiave 1  Chiave 2  Chiave 3  Valori
# con:
# Chiave 1: Denomination - Machine Type
# Chiave 2: Casinò - Section
# Chiave 3: Month
# Valori: SUM(NoMachines) e MAX finale sui periodi
#         AVG(GrossRevenuePerMachine)
#         MED(Plays)

# Creazione chiave 1:
data$Denomination <- as.character(data$Denomination) # Trasformazione della Denomination in stringa
data$key_1 <- data$MachineType
# Creazione della chiave concatenando i valori di Denomination e Machine_Type in una nuova colonna
i <- 0
for (i in 1:4148) {
  den <- data[i,4]
  type <- data[i,5]
  data[i,18] <- paste(den, type, sep = " - ")
}
data$Denomination <- as.numeric(data$Denomination) # Trasformazione della Denomination a numerica

# Creazione chiave 2:
data$key_2 <- data$Casino
i <- 0
for (i in 1:4148) {
  cas <- data[i,2]
  sec <- data[i,3]
  data[i,19] <- paste(cas, sec, sep = " - ")
}

# Salvataggio di queste due nuove chiavi in un nuovo dataframe

# Creazione di un dataset clone, da ora si lavora su questo dataset
data_k <- data
# Rimozione delle colonne utilizzate per creare le chiavi dal dataset
data_k <- data_k[,-c(2:5)]
# Riordinamento delle colonne
data_k <- data_k[c(14,15,1,2,3,4,5,6,7,8,9,10,11,12,13)]

write.csv(data_k, "Lucky_Duck_with_keys.csv")

# Creazione e salvataggio del primo dataset:
library(dplyr)

data_grouped <- group_by(data_k, key_1, key_2)
data_grouped

data_grouped_def <- summarise(data_grouped, 
                          MachinesNumber = mean(NoMachines),
                          GrossRevenue = mean(GrossRevenuePerMachine),
                          Revenue = (mean(NoMachines) * mean(GrossRevenuePerMachine)), 
                          Forecast = mean((FcstLower + FcstUpper)/2),
                          Plays = median(PlaysPerMachine))
write.csv(data_grouped_def, "Lucky_Duck_grouped.csv")

# Creazione e salvataggio del secondo dataset:
data_grouped_2 <- group_by(data_k, key_1, key_2, Month)

data_grouped_def_2 <- summarise(data_grouped_2, 
                                NumberMachines = sum(NoMachines),
                                GrossRevenue = mean(GrossRevenuePerMachine),
                                Revenue = (mean(NoMachines) * mean(GrossRevenuePerMachine)), 
                                Forecast = mean((FcstLower + FcstUpper)/2),
                                Plays = median(PlaysPerMachine))
write.csv(data_grouped_def_2, "Lucky_Duck_grouped_2.csv")
