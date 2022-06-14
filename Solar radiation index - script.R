#solar radiation index

setwd("C:/Users/Julie Sander Lehnert/OneDrive/Dokumenter/Skole/UNI/Speciale/Behandlet data/Brugbar")

list.files()

#Loader fil som indenholder habitatkode, for hvert plot
mix.data <- read_excel("Mix data - Behandlet.xlsx")
mix.data <- data.frame(mix.data)

######################

# Subsetter, så det kun er de kolloner, som skal bruges til at udregne
# solar radiation index, som er tilbage

names(mix.data)
head(mix.data)

mix.data <- mix.data[,c("Plot",
                        "GPS.Y",
                        "Retning",
                        "Hældning"
                        )]

####################################
#Formel for solar radiation fra Keating 2017

# Der beskrives det at retningen skal konverteres: ny.retning = 180 - retning

mix.data$Ny.retning <- 180-mix.data$Retning

mix.data$SIR <- cos(mix.data$GPS.Y) * cos(mix.data$Hældning) +
  sin(mix.data$GPS.Y) * sin(mix.data$Hældning) * cos(mix.data$Ny.retning)

SIR.data <- mix.data[, c("Plot",
                         "SIR")]

SIR.data

write.csv(SIR.data, "Solar radiation index - beregnet.csv")

