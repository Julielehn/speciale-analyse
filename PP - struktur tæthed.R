library(ggplot2)
install.packages("viridis")  # Install
library("viridis")           # Load


#######################################################################
##### Indl�s data

setwd("C:/Users/Julie Sander Lehnert/OneDrive/Dokumenter/Skole/UNI/Speciale/Behandlet data/Brugbar/Modellering")

list.files()

#Loader fil som indenholder habitatkode, for hvert plot
mix.data <- read.csv("Strukturdata modellering - behandlet.csv",
                     header = T,
                     sep = ";")

names(mix.data)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####### Boxplot forberedlse #########
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

table(mix.data$Habitatskode)

# 0: Omdrift/kulturpr�get
# 2320: Revling-indlandsklit
# 2330: Gr�s-indlandsklit
# 3130: s�er/vandhuller m. amfibiske planter 
# 4010: V�d hede
# 4030: T�r hede
# 5130: Enekrat
# 6230: Surt overdrev inkl. gr�shede
# 6410: Tidvis v�d eng
# 7140: H�nges�k
# 7150: T�rvelavning
# 7200: Fattigk�r
# 7230: Rigk�r
# 9100: 25% krond�kke af n�letr�er
# 9190: Stilkeegekrat
# 91D0: Skovbevokset t�rvemose

# Nogle af habitatstyperne er kun repr�senteret ved et plot.
#Disse fjernes eller l�gges sammen. 
# 0: omdrift, 3130 7150: T�rvelavning, 91D0: Skovbevokset t�rvemose. Da de kun
# findes i s� lav grad siger de ikke ret meget om omr�det alligevel. 
# 2320 og 2330 l�gges sammen. 7200 og 7230 l�gges sammen. 


mix.data <- subset(mix.data, Habitatskode != "0" &
                           Habitatskode != "7150" &
                           Habitatskode != "91D0" &
                           Habitatskode != "3130" &
                     Habitatskode != "7200")

mix.data$Habitatskode[mix.data$Habitatskode == 2320] <- "2320/2330"
mix.data$Habitatskode[mix.data$Habitatskode == 2330] <- "2320/2330"

habitatsnavne <- c("Revling-indlandsklit", 
                   "V�d hede", 
                   "T�r hede",
                   "Eneb�rkrat",
                   "Surt overdrev inkl. gr�shede",
                   "Tidvis v�d eng",
                   "H�nges�k",
                   "Fattig/rigk�r",
                   "Andre skovtyper",
                   "Stilkeegekrat")


# Basic box plot

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Boxplot - vegetationsh�jde ######
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

names(mix.data)

p.box.vegetation <- ggplot(mix.data, aes(Habitatskode, Vegetationsh�jde.total)) +
  geom_boxplot(aes(fill = Habitatskode)) + 
  labs(y = "Vegetationsh�jde (cm)",
       x = "Habitatstype") +
  theme_bw() +
  scale_fill_viridis(option = "mako", discrete = T, labels = habitatsnavne) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  

p.box.vegetation



