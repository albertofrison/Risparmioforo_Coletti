################################################################################
# This is the R implementation of the Risparmioforo Python project from Prof. Paolo Coletti
# YT Channel: https://www.youtube.com/@PaoloColetti
# https://colab.research.google.com/drive/1hQXxDTKuccacVeHPs79fDPVw2HjOqHgB?usp=sharing#scrollTo=pe5BeLMCLaIV
# The code works but I have not yet tested it (as it I have just translate it from ChatGPT)
# Made with ♥︎ by Alberto Frison
# Created on February 2023
################################################################################

################################################################################
#
# LOAD LIBRARIES AND CLEANING ENVIRONMENT
#
################################################################################
library(tidyverse)
library(anytime)

rm (list = ls())


lista <- list(
  "MSCI USA" = "https://raw.githubusercontent.com/paolocole/Stock-Indexes-Historical-Data/main/NET/EUR/LARGE_AND_MID_CAP/COUNTRIES/MSCI%20USA.csv",
  "MSCI Europe" = "https://raw.githubusercontent.com/paolocole/Stock-Indexes-Historical-Data/main/NET/EUR/LARGE_AND_MID_CAP/REGIONS/MSCI%20EUROPE.csv",
  "MSCI Emerging Asia" = "https://raw.githubusercontent.com/paolocole/Stock-Indexes-Historical-Data/main/NET/EUR/LARGE_AND_MID_CAP/REGIONS/MSCI%20EM%20ASIA.csv",
  "MSCI World" = "https://raw.githubusercontent.com/paolocole/Stock-Indexes-Historical-Data/main/NET/EUR/LARGE_AND_MID_CAP/REGIONS/MSCI%20WORLD.csv",
  "Gold Spot" = "https://raw.githubusercontent.com/AxelFooley/Market-Indexes-Historical-Data/main/Gold-Spot.csv"
)

dati <- list()

for (nome in names(lista)) {
  dato <- read.csv(lista[[nome]])
  dato$Rendimento <- (dato[,2] / lag(dato[,2], 1)-1)
  print(summary(dato$Rendimento))
  dati[[nome]] <- dato
}


#-------------------------------------------------------------------------------
# CORRELATION MATRIX 
correlazione <- matrix (nrow = 5, ncol = 5, dimnames = list (names(lista),names(lista))) 

for (nome_1 in names(lista)) {
  for (nome_2 in names(lista)) {
    
    data_inizio <- max (anydate(dati[[nome_1]]$Date[1]), anydate(dati[[nome_2]]$Date[1]))
    data_fine <- min (anydate(dati[[nome_1]]$Date[nrow(dati[[nome_1]])]), anydate(dati[[nome_2]]$Date[nrow(dati[[nome_2]])]))
    
    indice_data_inizio_ar1 <-  which (anydate(dati[[nome_1]]$Date) == data_inizio) + 1
    indice_data_fine_ar1 <-  which (anydate(dati[[nome_1]]$Date) == data_fine)
    
    indice_data_inizio_ar2 <-  which (anydate(dati[[nome_2]]$Date) == data_inizio)+1
    indice_data_fine_ar2 <-  which (anydate(dati[[nome_2]]$Date) == data_fine)
    
    correlazione[nome_1,nome_2] <- round (cor (dati[[nome_1]]$Rendimento[indice_data_inizio_ar1:indice_data_fine_ar1],
                                        dati[[nome_2]]$Rendimento[indice_data_inizio_ar2:indice_data_fine_ar2]), 3)
  
  }  
}
correlazione

#-------------------------------------------------------------------------------
# some plots
indice <- "MSCI World"

as.data.frame (dati[indice]) %>%
  ggplot (aes (x = anydate(MSCI.World.Date), y = MSCI.World.WORLD))+
  geom_line()

indice <- "MSCI Emerging Asia"

as.data.frame (dati[indice]) %>%
  ggplot (aes (x = anydate(MSCI.Emerging.Asia.Date), y = MSCI.Emerging.Asia.EM.ASIA))+
  geom_line()

#-------------------------------------------------------------------------------
# let's get some statistical information from the data
indice <- "MSCI World"

mean <- mean(dati[[indice]]$Rendimento, na.rm = TRUE)
std_dev <- sd (dati[[indice]]$Rendimento, na.rm = TRUE)

?sd

# looks normal? not really
hist(dati[[indice]]$Rendimento, 50)
hist (rnorm (10000, mean, std_dev), 50)

?hist
# THE two distributions do not loook alike to me

# why not use the sample function to extract randomly one monthly yeild from the vector?
rendimenti <- dati[[indice]]$Rendimento[2:length(dati[[indice]]$Rendimento)]

# looks the same as the original yield array
hist (sample (rendimenti, 1000000, replace = TRUE))


#-------------------------------------------------------------------------------
# Calcola i Versamenti cumulati
sim_versamenti <- function() {
  versamenti <- 0
  aumento_risparmio_mensile <- (1 + aumento_risparmio) ^ (1/12) - 1
  
  for (i in 1:mesi) {
    versamenti <- versamenti + risparmio
    risparmio <- risparmio * (1 + aumento_risparmio_mensile)
    }
  
  return (versamenti)
}

#-------------------------------------------------------------------------------
# Calcola valore a x anni pre-tax
sim_valore <- function() {
  valore <- 0
  aumento_risparmio_mensile <- (1 + aumento_risparmio) ^ (1/12) - 1

  for (i in 1:mesi) {
    # valore <- valore * (1 + rnorm (1, mean, std_dev)) + risparmio * ifelse (i %% 12 == 0, 1 - 0.002, 1)  # imposta di bollo pagata a dicembre di ogni anno
    valore <- valore * (1 + sample (rendimenti, 1)) + risparmio * ifelse (i %% 12 == 0, 1 - 0.002, 1)  # imposta di bollo pagata a dicembre di ogni anno
    risparmio <- risparmio * (1 + aumento_risparmio_mensile)
    }
  return (valore)
}
#-------------------------------------------------------------------------------
# an effective simulation
indice <- "MSCI World"
anni <- 10
mesi <- anni * 12
risparmio <- 1000 # risparmio mensile
aumento_risparmio <- 0.03 # 3% annuale - nel calcolo viene mensilizzato
num_simulazioni <- 10000 # almeno 10.000 simulazioni per avere una minima valenza

simulazioni <- array(num_simulazioni)
simulazioni  <- replicate(num_simulazioni, sim_valore ())

versamenti_effettuati <- sim_versamenti()

versamenti_effettuati
summary(simulazioni)

ggplot (data = as.data.frame(simulazioni), aes (x = simulazioni)) +
  geom_histogram(binwidth = 500, color = "red", fill = "white", ) +
  geom_vline(xintercept = versamenti_effettuati, color = "blue") +
  labs (title = "Simulazione Valore Investimento", subtitle = paste ("Investimento di", risparmio, "€ al mese per", anni,"anni sull'indice", indice, "-", num_simulazioni, "simulazioni Monte Carlo"), x = "Valore Nominale Investimento")
  

#-------------------------------------------------------------------------------
# risparmio <- 1000
# aumento_risparmio <- 0.03 
# 
# aliquota <- 0.26
# bollo <- 0.002
# anni <- c(10, 20, 30, 40, 50)
# 
# rendimenti_mensili <- tail(dati[[indice]][,2] / lag(dati[[indice]][,2]) - 1, -1)
# aumento_risparmio_mensile <- (1 + aumento_risparmio)^(1/12) - 1
# mese <- 0
# quanti_mesi <- anni[length(anni)] * 12
# calcolo <- matrix(0, nrow = quanti_mesi, ncol = length(rendimenti_mensili))
# calcolo[1,] <- 0
# versamenti <- rep(0, quanti_mesi)
# versamenti[1] <- risparmio
# calcolo[1,] <- versamenti[1]
# 
# for (mese in 2:quanti_mesi) {
#   versamenti[mese] <- versamenti[mese - 1] * (1 + aumento_risparmio_mensile)
# }
# 
# for (mese in 2:quanti_mesi) {
#   versamenti[mese] <- versamenti[mese] - (calcolo[mese - 1,] * bollo * (mese %% 12 == 0))
#   for (simulazione in 1:length(rendimenti_mensili)) {
#     quale_simulazione <- ((simulazione + mese - 1) %% length(rendimenti_mensili)) + 1
#     calcolo[mese, simulazione] <- versamenti[mese] + calcolo[mese - 1, simulazione] * (1 + rendimenti_mensili[quale_simulazione])
#   }
# }
# 
# for (anno in anni) {
#   cat(anno, "anni: ")
#   c <- calcolo[anno * 12, ]
#   v <- sum(versamenti[1:(anno * 12)])
#   tassa <- (c - v) * aliquota
#   netto <- c - tassa
#   cat("versato", round(v / 1000), " - ", "min", round(min(netto) / 1000), " media", round(mean(netto) / 1000), " max", round(max(netto) / 1000), "\n")
#   hist(netto / 1000, breaks = length(netto) / 15, main = paste(anno, "anni"), xlab = "Netto (in migliaia di euro)", col = "lightblue", border = "black")
#   abline(v = v / 1000, col = "red", lty = 2, label = "versato")
#   axis(1, at = seq(0, max(netto / 1000), by = 500), las = 2)
#   grid()
#   box()
#   print(netto)
# }



risparmio <- 1000
aumento_risparmio <- 0.03
indice <- "MSCI World"
aliquota <- 0.26
bollo <- 0.002
anni <- c(10, 20, 30, 40, 50)
inflazione_stima <- "reale riscalata"  # fissa, reale, reale riscalata, lognormale
inflazione_stimata <- 0.03

rendimenti_mensili <- tail(dati[[indice]][,2] / lag(dati[[indice]][,2]) - 1, -1)
aumento_risparmio_mensile <- (1 + aumento_risparmio)^(1/12) - 1
mese <- 0
quanti_mesi <- anni[length(anni)] * 12
calcolo <- matrix(0, nrow = quanti_mesi, ncol = length(rendimenti_mensili))
calcolo[1,] <- 0
versamenti <- rep(0, quanti_mesi)
versamenti[1] <- risparmio



for (mese in 2:quanti_mesi) {
  versamenti[mese] <- versamenti[mese - 1] * (1 + aumento_risparmio_mensile)
}

for (mese in 2:quanti_mesi) {
  versamenti[mese] <- versamenti[mese] - (calcolo[mese - 1,] * bollo * (mese %% 12 == 0))
  for (simulazione in 1:length(rendimenti_mensili)) {
    quale_simulazione <- ((simulazione + mese - 1) %% length(rendimenti_mensili)) + 1
    calcolo[mese, simulazione] <- versamenti[mese] + calcolo[mese - 1, simulazione] * (1 + rendimenti_mensili[quale_simulazione])
  }
}

quante_simulazioni <- 100
inflazione_reale <- c(2.3, 3.4, 1.3, 2.8, -0.4, 2.3, 2.1, 4.7, 7.5, 5.9, 4.6, 2.3, 3.7, 1.4, 2.6, 5.0, 4.8, 5.7, 10.8, 19.1, 17.0, 16.8, 17.0, 12.1, 14.8,
                      21.2, 17.8, 16.5, 14.7, 10.8, 9.2, 5.8, 4.8, 5.0, 6.3, 6.5, 6.2, 5.3, 4.7, 4.1, 5.3, 4.0, 2.0, 2.0, 1.7, 2.5, 2.7, 2.5, 2.7, 2.2, 1.9,
                      2.1, 1.8, 3.3, 0.8, 1.5, 2.7, 3.0, 1.2, 0.2, 0.1, -0.1, 1.2, 1.2, 0.6, -0.2, 1.9, 8.1, 8.7) / 100

if (inflazione_stima == "fissa") {
  inflazione <- matrix(rep(inflazione_stimata, anni[length(anni)] * length(rendimenti_mensili) * quante_simulazioni), nrow = anni[length(anni)], ncol = length(rendimenti_mensili) * quante_simulazioni, byrow = TRUE)
} else if (inflazione_stima == "reale") {
  inflazione <- array(inflazione_reale[sample(1:length(inflazione_reale), anni[length(anni)] * length(rendimenti_mensili) * quante_simulazioni, replace = TRUE)], dim = c(anni[length(anni)], length(rendimenti_mensili), quante_simulazioni))
} else if (inflazione_stima == "reale riscalata") {
  inflazione <- array(inflazione_reale * (inflazione_stimata / mean(inflazione_reale))[sample(1:length(inflazione_reale), anni[length(anni)] * length(rendimenti_mensili) * quante_simulazioni, replace = TRUE)], dim = c(anni[length(anni)], length(rendimenti_mensili), quante_simulazioni))
} else if (inflazione_stima == "lognormale") {
  mu <- log(inflazione_stimata)
  sigma <- log((1 + sqrt(1 + 4 * var(inflazione_reale) / exp(2 * mu))) / 2)
  mu <- log(inflazione_stimata) - sigma^2 / 2
  sigma <- log((1 + sqrt(1 + 4 * var(inflazione_reale) / exp(2 * mu))) / 2)
  mu <- log(inflazione_stimata) - sigma^2 / 2
  inflazione <- array(rlnorm(length = anni[length(anni)] * length(rendimenti_mensili) * quante_simulazioni, meanlog = mu, sdlog = sigma), dim = c(anni[length(anni)], length(rendimenti_mensili), quante_simulazioni))
} else {
  stop("Ciccio, guarda che non so come gestire l'inflazione!")
}

cat("Media:", mean(inflazione), "Dev st:", sd(inflazione), "\n")
cat(dim(inflazione), "\n")

for (anno in anni) {
  cat(anno, "anni: ")
  c <- calcolo[anno * 12, ]
  v <- sum(versamenti[1:(anno * 12)])
  tassa <- (c - v) * aliquota
  netto <- c - tassa
  
  inflazione_fino_anno <- array(1, dim = c(length(rendimenti_mensili), quante_simulazioni))
  for (i in 1:anno) {
    inflazione_fino_anno <- inflazione_fino_anno * (1 + inflazione[i, , , drop = FALSE])
  }
  
  matriciona <- netto / inflazione_fino_anno
  netto <- as.numeric(matriciona)
  
  v <- rep(0, length(inflazione))
  inflazione_finora <- rep(1, length(inflazione))
  for (a in 1:anno) {
    inflazione_mensile <- (inflazione[a, , , drop = FALSE] + 1)^(1/12)
    for (m in 1:12) {
      inflazione_finora <- inflazione_finora * inflazione_mensile
      v <- v + versamenti[m + (a - 1) * 12] / inflazione_finora
    }
  }
  
  cat("versato medio", round(mean(v) / 1000), " - ", "min", round(min(netto) / 1000), " media", round(mean(netto) / 1000), " max", round(max(netto) / 1000), "\n")
  if (inflazione_stima == "fissa") {
    hist(netto, breaks = length(netto) / 1500, main = paste(anno, "anni"), xlab = "Netto (in migliaia di euro)", col = "lightblue", border = "black")
    abline(v = v[1] / 1000, col = "red", lty = 2, label = "versato")
  } else {
    hist(netto, breaks = length(netto) / 150, main = paste(anno, "anni"), xlab = "Netto (in migliaia di euro)", col = "lightblue", border = "black")
    hist(v, breaks = length(v) / 150, col = 'red', alpha = 0.4)
  }
  axis(1, at = seq(0, max(netto) / 1000, by = 500), las = 2)
  grid()
  box()
  print(netto)
}

