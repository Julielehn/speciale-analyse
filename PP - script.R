library(readxl)
library(tidyverse)
library(multcompView)
library(dplyr)
library(ggplot2)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######## Load data ##############
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## loader Pin-Point data

setwd("C:/Users/Julie Sander Lehnert/OneDrive/Dokumenter/Skole/UNI/Speciale/Behandlet data/Brugbar/Pin point - Indtasning")

list.files()
file_list <- list.files() #Opretter en list navne p� de filer, som jeg �nsker at loade

length(file_list) # check om der er der rigtige antal filer

raa.filer <- data.frame() # Opretter en tom dataframe til PP data


for (i in 1:length(file_list)){  #Loop som loader alle filerne ned i samme data frame
  temp_data <- read_excel(file_list[i], sheet = 2, range = cell_cols("A:G")) #each file will be read in, specify which columns you need read in to avoid any errors
  names(temp_data) <- c("Plot", "Fund", "Tilstede.PP", "Antal.pins", "Tilstede.5m",
                        "Dansk.navn", "Videnskabeligt.navn")
  raa.filer <- rbind(raa.filer, temp_data) #for each iteration, bind the new data to the building dataset
}
PP.data <- data.frame(raa.filer) ## PP data laves om til dataframe

length(unique(PP.data$Plot))

## Loader data om v�rtsplanter og nektar/pollenplanter
setwd("C:/Users/Julie Sander Lehnert/OneDrive/Dokumenter/Skole/UNI/Speciale/Behandlet data/Brugbar")

v�rt.nektar.data <- data.frame(read_excel("Artliste - V�rtsplante + nektar.xlsx"))


## Loader habitatstype data
setwd("C:/Users/Julie Sander Lehnert/OneDrive/Dokumenter/Skole/UNI/Speciale/Behandlet data/Brugbar")

mix.data <- read_excel("Mix data - Behandlet.xlsx") #Loader fil som indenholder habitatkode, for hvert plot

habitat.data <- mix.data[,c("Plot", "Habitatskode")] #Ny dataframe, som kun indeholder plot og habitatkode


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Oprydning og sammens�tning af data #######
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## omrydning af PP data
#PP.data <- subset(PP.data, Fund == 1) ## Subset, hvor der kun beholder alle observationer, hvor fund = 1

PP.data$Plot[PP.data$Plot == "TRUE"] <- 1 ## ved loading er plot 1 blevet til TRUE. Dette �ndres tilbage


## Oprydning af V�rtsplante og nektarplante data
v�rt.nektar.data[is.na(v�rt.nektar.data)] = 0 # Alle de arter, som ikke har nogle v�rdier, f�r v�rdien 0

names(v�rt.nektar.data)
v�rt.nektar.data <- v�rt.nektar.data[,c("Danske.arter",
                                        "V�rtsplante", 
                                        "Nektar" )] # subsetter til kun �nskede kolloner

names(v�rt.nektar.data) <- c("Dansk.navn", 
                             "V�rtsplante", 
                             "Nektar") #Omd�ber koloner, s� de stemmer overens med andre datafiler

## Merger alt data
PP.data <- merge(PP.data, 
                 v�rt.nektar.data, 
                 by = "Dansk.navn")


length(unique(PP.data$Plot))# Alle plots er der stadig



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###### Ny variabel - T�thed af planterarter #######
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Variablen t�thed tilf�jes. T�theden bestemmes p� baggrund af en PinPointramme
#og 5 meter cirkel. Det vil sige at der er 3 forskellige positioner. 
# 1: Antal pins ramt i PinPoint. (0 til 25)
# 2: Findes i PinPoint men ikke ramt (1 eller 0)
# 3: Findes i 5 meter cirkelen (1 eller 0)
# i Datas�ttet vil hver observation kun kun tage en positiv v�rdi i en af de 3 kategorier
# For at kunne medregne observationerne for PP med ikke ramt og 5 meter cirklen skaleres 
# data, s� der hypotetisk er 26 punkter i rammen, hvor det 26'ende punkt repre�senerer
# fund i PP eller i 5 meter cirklen
# T�thedsvariablen tilf�jes med en ifelse statement
PP.data$T�thed <- ifelse(PP.data$Antal.pins != 0, 
                      PP.data$Antal.pins + 1,
                      PP.data$Tilstede.PP + PP.data$Tilstede.5m)


## Den relative t�thed i forhold til de 26 mulige posistioner dannes som variabel
PP.data$Procent.t�thed <- PP.data$T�thed/26


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###### V�rtsplante og nektar datas�t ######
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## V�rtsplante datas�t

## Danner en dataframe med alle de observationer indeholde en funden v�rtsplante
V�rtsplante.df <- subset(PP.data, V�rtsplante == 1)

length(unique(V�rtsplante.df$Dansk.navn)) ## 27 arter ud af 210 er v�rtsplanter

## For at v�re sikker p�, at vi bibeholder alle plot, selvom de ikke indeholder 
## v�rtsplanter.Dannes en ny dataframe, som indeholder
## alle plot-ID's
Plot.df <- data.frame(Plot = unique(PP.data$Plot))

## Plot-dataframe merges med v�rtsplante dataframes. Husk at bruge kommadoen
## all.x, s� alle plot-ID beholdes, selvom de ikke indeholder arter, som er 
## v�rtsplanter
V�rtsplante.df <- merge(Plot.df, V�rtsplante.df, by ="Plot", all.x = TRUE)

length(unique(V�rtsplante.df$Plot)) # Alle plots er der stadig

# Plots som ikke har nogle v�rtsplanter, og derfor indeholder v�rdien NA
# f�r v�rdien 0
V�rtsplante.df[is.na(V�rtsplante.df)] = 0

V�rtsplante.df <- merge(V�rtsplante.df,
                 habitat.data,
                 by = "Plot")
V�rtsplante.df

# Der beholdes de kolloner, som skal bruges til dannelse af boxplot
V�rtsplante.df <- V�rtsplante.df[, c("Plot", 
                                     "Dansk.navn",
                                     "Procent.t�thed",
                                     "Habitatskode")]



#### Nektar/pollen datas�t
## Danner en dataframe med alle de fundne planter, som er Nektar/pollen planter
nektar.df <- subset(PP.data, Nektar == 1)

length(unique(nektar.df$Dansk.navn)) ## 29 arter ud af 210 er nektar/pollensplanter

## For at v�re sikker p�, at vi bibeholder alle plot, selvom de ikke indeholder 
## v�rtsplanter.Dannes en ny dataframe, som indeholder
## alle plot-ID's
Plot.df <- data.frame(Plot = unique(PP.data$Plot))

## Plot-dataframe merges med v�rtsplante dataframes. Husk at bruge kommadoen
## all.x, s� alle plot-ID beholdes, selvom de ikke indeholder arter, som er 
## v�rtsplanter
nektar.df <- merge(Plot.df, nektar.df, by ="Plot", all.x = TRUE)

length(unique(nektar.df$Plot)) # Alle plots er der stadig

# Plots som ikke har nogle v�rtsplanter, og derfor indeholder v�rdien NA
# f�r v�rdien 0
nektar.df[is.na(nektar.df)] = 0

nektar.df <- merge(nektar.df,
                        habitat.data,
                        by = "Plot")

# Der beholdes de kolloner, som skal bruges til dannelse af boxplot
nektar.df <- nektar.df[, c("Plot",
                           "Procent.t�thed",
                           "Dansk.navn",
                           "Habitatskode")]

table(nektar.df$Habitatskode)
#################################
## subset for habitatstyper

v�rt.2320 <- subset(V�rtsplante.df, Habitatskode == "2320")
unique(v�rt.2320$Plot) # 7 plots

v�rt.4010 <- subset(V�rtsplante.df, Habitatskode == "4010")
length(unique(v�rt.4010$Plot)) # 23 plots

v�rt.4030 <- subset(V�rtsplante.df, Habitatskode == "4030")
length(unique(v�rt.4030$Plot)) # 29 plots

v�rt.5130 <- subset(V�rtsplante.df, Habitatskode == "5130")
length(unique(v�rt.5130$Plot)) # 8 plots

v�rt.6230 <- subset(V�rtsplante.df, Habitatskode == "6230")
length(unique(v�rt.6230$Plot)) # 13 plots

v�rt.6410 <- subset(V�rtsplante.df, Habitatskode == "6410")
length(unique(v�rt.6410$Plot)) # 2 plots

v�rt.7140 <- subset(V�rtsplante.df, Habitatskode == "7140")
length(unique(v�rt.7140$Plot)) # 3 plots

v�rt.7230 <- subset(V�rtsplante.df, Habitatskode == "7230")
length(unique(v�rt.7230$Plot)) # 5 plots

v�rt.9100 <- subset(V�rtsplante.df, Habitatskode == "9100")
length(unique(v�rt.9100$Plot)) # 10 plots

v�rt.9190 <- subset(V�rtsplante.df, Habitatskode == "9190")
length(unique(v�rt.9190$Plot)) # 8 plots


nektar.2320 <- subset(nektar.df, Habitatskode == "2320")

nektar.4010 <- subset(nektar.df, Habitatskode == "4010")

nektar.4030 <- subset(nektar.df, Habitatskode == "4030")

nektar.5130 <- subset(nektar.df, Habitatskode == "5130")

nektar.6230 <- subset(nektar.df, Habitatskode == "6230")

nektar.6410 <- subset(nektar.df, Habitatskode == "6410")

nektar.7140 <- subset(nektar.df, Habitatskode == "7140")

nektar.7230 <- subset(nektar.df, Habitatskode == "7230")

nektar.9100 <- subset(nektar.df, Habitatskode == "9100")

nektar.9190 <- subset(nektar.df, Habitatskode == "9190")


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####### Boxplot - V�rtsplante #######
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##V�rtsplanter i 2320
# der dannes en dataframe, som inderholder en 2+" for de arter, som er tilstede
# og en placering for dette "+" i plottet
tk <- group_by(v�rt.2320, Dansk.navn) %>%
  summarise(Mean = mean(Procent.t�thed),
            Quant = quantile(Procent.t�thed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

#udskriving af boxplot
box.v�rt.2320 <- ggplot(v�rt.2320, 
       aes(Dansk.navn, Procent.t�thed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis d�kning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Revling-indlandsklit (2320)") + 
  annotate("text", x=25, y=0.90, label= "n = 7") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -1, hjust = -1, size = 3)

box.v�rt.2320

## v�rtsplante i 4010
tk <- group_by(v�rt.4010, Dansk.navn) %>%
  summarise(Mean = mean(Procent.t�thed),
            Quant = quantile(Procent.t�thed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk


box.v�rt.4010 <- ggplot(v�rt.4010, 
                        aes(Dansk.navn, Procent.t�thed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis d�kning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("V�d hede (4010)") +
  annotate("text", x = 25, y= 0.95, label = "n = 23") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -1, hjust = -1, size = 3)

box.v�rt.4010

## v�rtsplante i 4030
tk <- group_by(v�rt.4030, Dansk.navn) %>%
  summarise(Mean = mean(Procent.t�thed),
            Quant = quantile(Procent.t�thed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.v�rt.4030 <- ggplot(v�rt.4030, 
                        aes(Dansk.navn, Procent.t�thed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis d�kning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("T�r hede (4030)") + 
  annotate("text", x = 25, y = 0.95, label = "n = 29") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -1, hjust = -1, size = 3)

box.v�rt.4030

## v�rtsplante i 5130
tk <- group_by(v�rt.5130, Dansk.navn) %>%
  summarise(Mean = mean(Procent.t�thed),
            Quant = quantile(Procent.t�thed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.v�rt.5130 <- ggplot(v�rt.5130, 
                        aes(Dansk.navn, Procent.t�thed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis d�kning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Eneb�rkrat (5130)") +
  annotate("text", x=25, y=0.95, label= "n = 8") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -1, hjust = -1, size = 3)

box.v�rt.5130

## v�rtsplante i 6230
tk <- group_by(v�rt.6230, Dansk.navn) %>%
  summarise(Mean = mean(Procent.t�thed),
            Quant = quantile(Procent.t�thed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.v�rt.6230 <- ggplot(v�rt.6230, 
                        aes(Dansk.navn, Procent.t�thed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis d�kning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Surt overdrev (6230)") +
  annotate("text", x=25, y=0.95, label= "n = 13") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.v�rt.6230

## v�rtsplante i 6410
tk <- group_by(v�rt.6410, Dansk.navn) %>%
  summarise(Mean = mean(Procent.t�thed),
            Quant = quantile(Procent.t�thed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.v�rt.6410 <- ggplot(v�rt.6410, 
                        aes(Dansk.navn, Procent.t�thed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis d�kning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Tidvis v�d eng (6410)") +
  annotate("text", x=25, y=0.95, label= "n = 2") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.v�rt.6410

## v�rtsplante i 7140
tk <- group_by(v�rt.7140, Dansk.navn) %>%
  summarise(Mean = mean(Procent.t�thed),
            Quant = quantile(Procent.t�thed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.v�rt.7140 <- ggplot(v�rt.7140, 
                        aes(Dansk.navn, Procent.t�thed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis d�kning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("H�nges�k (7140)") +
  annotate("text", x=25, y=0.95, label= "n = 3") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.v�rt.7140

## v�rtsplante i 7230
tk <- group_by(v�rt.7230, Dansk.navn) %>%
  summarise(Mean = mean(Procent.t�thed),
            Quant = quantile(Procent.t�thed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.v�rt.7230 <- ggplot(v�rt.7230, 
                        aes(Dansk.navn, Procent.t�thed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis d�kning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Rigk�r (7230)") +
  annotate("text", x=25, y=0.95, label= "n = 5") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.v�rt.7230

## v�rtsplante i 9100
tk <- group_by(v�rt.9100, Dansk.navn) %>%
  summarise(Mean = mean(Procent.t�thed),
            Quant = quantile(Procent.t�thed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.v�rt.9100 <- ggplot(v�rt.9100, 
                        aes(Dansk.navn, Procent.t�thed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis d�kning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Andre skovtyper (9100)") +
  annotate("text", x=25, y=0.95, label= "n = 10") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.v�rt.9100

## v�rtsplante i 9190
tk <- group_by(v�rt.9190, Dansk.navn) %>%
  summarise(Mean = mean(Procent.t�thed),
            Quant = quantile(Procent.t�thed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.v�rt.9190 <- ggplot(v�rt.9190, 
                        aes(Dansk.navn, Procent.t�thed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis d�kning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Stilkeegekrat (9190)") +
  annotate("text", x=25, y=0.95, label= "n = 8") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.v�rt.9190


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Boxplot - nektar/pollen #######
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


## nektar/pollenplanter i 2320
tk <- group_by(nektar.2320, Dansk.navn) %>%
  summarise(Mean = mean(Procent.t�thed),
            Quant = quantile(Procent.t�thed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.nektar.2320 <- ggplot(nektar.2320, 
                        aes(Dansk.navn, Procent.t�thed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis d�kning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Revling-indlandsklit (2320)") +
  annotate("text", x=28, y=0.95, label= "n = 7") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.nektar.2320

## nektar/pollenplanter i 4010
tk <- group_by(nektar.4010, Dansk.navn) %>%
  summarise(Mean = mean(Procent.t�thed),
            Quant = quantile(Procent.t�thed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.nektar.4010 <- ggplot(nektar.4010, 
                          aes(Dansk.navn, Procent.t�thed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis d�kning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("V�d hede (4010)") +
  annotate("text", x=28, y=0.95, label= "n = 23") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.nektar.4010

## nektar/pollenplanter i 4030
tk <- group_by(nektar.4030, Dansk.navn) %>%
  summarise(Mean = mean(Procent.t�thed),
            Quant = quantile(Procent.t�thed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.nektar.4030 <- ggplot(nektar.4030, 
                          aes(Dansk.navn, Procent.t�thed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis d�kning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("T�r hede (4030)") +
  annotate("text", x=28, y=0.95, label= "n = 29") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.nektar.4030

## nektar/pollenplanter i 5130
tk <- group_by(nektar.5130, Dansk.navn) %>%
  summarise(Mean = mean(Procent.t�thed),
            Quant = quantile(Procent.t�thed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.nektar.5130 <- ggplot(nektar.5130, 
                          aes(Dansk.navn, Procent.t�thed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis d�kning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Eneb�rkrat (5130)") +
  annotate("text", x=28, y=0.95, label= "n = 8") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.nektar.5130

## nektar/pollenplanter i 6230
tk <- group_by(nektar.6230, Dansk.navn) %>%
  summarise(Mean = mean(Procent.t�thed),
            Quant = quantile(Procent.t�thed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.nektar.6230 <- ggplot(nektar.6230, 
                          aes(Dansk.navn, Procent.t�thed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis d�kning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Surt overdrev (6230)") +
  annotate("text", x=28, y=0.95, label= "n = 13") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.nektar.6230

## nektar/pollenplanter i 6410
tk <- group_by(nektar.6410, Dansk.navn) %>%
  summarise(Mean = mean(Procent.t�thed),
            Quant = quantile(Procent.t�thed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.nektar.6410 <- ggplot(nektar.6410, 
                          aes(Dansk.navn, Procent.t�thed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis d�kning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Tidvis v�d eng (6410)") +
  annotate("text", x=28, y=0.95, label= "n = 2") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.nektar.6410

## nektar/pollenplanter i 7140
tk <- group_by(nektar.7140, Dansk.navn) %>%
  summarise(Mean = mean(Procent.t�thed),
            Quant = quantile(Procent.t�thed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.nektar.7140 <- ggplot(nektar.7140, 
                          aes(Dansk.navn, Procent.t�thed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis d�kning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("H�nges�k (7140)") +
  annotate("text", x=28, y=0.95, label= "n = 3") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.nektar.7140

## nektar/pollenplanter i 7230
tk <- group_by(nektar.7230, Dansk.navn) %>%
  summarise(Mean = mean(Procent.t�thed),
            Quant = quantile(Procent.t�thed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.nektar.7230 <- ggplot(nektar.7230, 
                          aes(Dansk.navn, Procent.t�thed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis d�kning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Rigk�r (7230)") +
  annotate("text", x=28, y=0.95, label= "n = 5") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.nektar.7230

## nektar/pollenplanter i 9100
tk <- group_by(nektar.9100, Dansk.navn) %>%
  summarise(Mean = mean(Procent.t�thed),
            Quant = quantile(Procent.t�thed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.nektar.9100 <- ggplot(nektar.9100, 
                          aes(Dansk.navn, Procent.t�thed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis d�kning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Andet skov (9100)") +
  annotate("text", x=28, y=0.95, label= "n = 10") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.nektar.9100

## nektar/pollenplanter i 9190
tk <- group_by(nektar.9190, Dansk.navn) %>%
  summarise(Mean = mean(Procent.t�thed),
            Quant = quantile(Procent.t�thed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.nektar.9190 <- ggplot(nektar.9190, 
                          aes(Dansk.navn, Procent.t�thed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis d�kning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Stilkegekrat (9190)") +
  annotate("text", x=28, y=0.95, label= "n = 8") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.nektar.9190


########## Scriptet er slut ###############
