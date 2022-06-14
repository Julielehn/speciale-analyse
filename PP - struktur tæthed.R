library(ggplot2)
install.packages("viridis")  # Install
library("viridis")           # Load


#######################################################################
##### Indlæs data

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

# 0: Omdrift/kulturpræget
# 2320: Revling-indlandsklit
# 2330: Græs-indlandsklit
# 3130: søer/vandhuller m. amfibiske planter 
# 4010: Våd hede
# 4030: Tør hede
# 5130: Enekrat
# 6230: Surt overdrev inkl. græshede
# 6410: Tidvis våd eng
# 7140: Hængesæk
# 7150: Tørvelavning
# 7200: Fattigkær
# 7230: Rigkær
# 9100: 25% krondække af nåletræer
# 9190: Stilkeegekrat
# 91D0: Skovbevokset tørvemose

# Nogle af habitatstyperne er kun repræsenteret ved et plot.
#Disse fjernes eller lægges sammen. 
# 0: omdrift, 3130 7150: Tørvelavning, 91D0: Skovbevokset tørvemose. Da de kun
# findes i så lav grad siger de ikke ret meget om området alligevel. 
# 2320 og 2330 lægges sammen. 7200 og 7230 lægges sammen. 


mix.data <- subset(mix.data, Habitatskode != "0" &
                           Habitatskode != "7150" &
                           Habitatskode != "91D0" &
                           Habitatskode != "3130" &
                     Habitatskode != "7200")

mix.data$Habitatskode[mix.data$Habitatskode == 2320] <- "2320/2330"
mix.data$Habitatskode[mix.data$Habitatskode == 2330] <- "2320/2330"

habitatsnavne <- c("Revling-indlandsklit", 
                   "Våd hede", 
                   "Tør hede",
                   "Enebærkrat",
                   "Surt overdrev inkl. græshede",
                   "Tidvis våd eng",
                   "Hængesæk",
                   "Fattig/rigkær",
                   "Andre skovtyper",
                   "Stilkeegekrat")


# Basic box plot

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Boxplot - vegetationshøjde ######
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

names(mix.data)

p.box.vegetation <- ggplot(mix.data, aes(Habitatskode, Vegetationshøjde.total)) +
  geom_boxplot(aes(fill = Habitatskode)) + 
  labs(y = "Vegetationshøjde (cm)",
       x = "Habitatstype") +
  theme_bw() +
  scale_fill_viridis(option = "mako", discrete = T, labels = habitatsnavne) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  

p.box.vegetation



