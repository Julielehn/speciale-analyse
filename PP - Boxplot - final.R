## Scriptet er skrevet af Julie Sander Lehnert i perioden september 2021 til
## januar 2023

## Anvendte pakker:
library(plyr)
library(vegan)
library(ggstatsplot)


################################################################################

## Dette script bruges til at lave plots over forekomsten og tætheden af
## ressourcer-arter for rødlistede insekter, som er tilstede i hvert 
## habitatstype i området, som er undersøgt. I dette filfælde Stråsø plantage.
## Yderligere fortages statistisk analyse i form af ......, for at undersøge 
## om der skulle, være en statistisk forskel i tilstedeværelsen af 
## ressource-planter i mellem de forskellige habitatstyper

################################################################################

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######### Data indlæses ###########
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setwd("C:/Users/Julie Sander Lehnert/OneDrive/Dokumenter/Skole/UNI/Speciale/Behandlet data/Endelig udgave")
list.files()

## Dette datasæt indeholder Pin-point data fra i alt 114 plots.
## Der er i alt 7 variable:
## Plotnavn, Fund/ikke fund (1/0), tilstede i PP-ramme (1/0), Antal punkter i PP-ramme (1-25), 
## tilstede i 5-meter-cirkel, Plantens danske navn, plantens videnskabelige navn
PP.data <- read.csv("PP.data.behandlet.rå.csv",
                    header = T,
                    sep = ",")


## Datasættet indeholder et register over alle de plantearter, som er fundet
## i løbet af feltarbejdet. Og om disse arter er værtsplante eller nektar/pollen-
## plante for nogle af de rødslistede insekter, som er registeret i feltarbejds-
## området. 
## Datasættet indeholder 4 variable:
## Dansk navn, Vækstform, Værtsplante (1/0), og Nektar/pollenplante (1/0)
vært.nektar.data <- read.csv("Vært.nektar.plante.data.behandlet.rå.csv",
                             header = T,
                             sep = ",")

## Datasættet indeholder et register over de 114 plots undersøgt i feltarbejdet
## og den habitatstype, som findes på de enkelte plots.
## Datasættet indeholder 2 variable:
## Plot og Habitatstype
habitat.data <- read.csv("Habitatstyper.plot.behandlet.rå.csv",
                         header = T,
                         sep = ",")


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###### Ny variabel - Tæthed af planterarter #######
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Variablen tæthed tilføjes til PP-data. Tætheden af de enkelte plantearter
## bestemmes på baggrund af observationer fra PinPoint-rammen og
## og 5 meter cirkel. Det vil sige at der er 3 forskellige positioner. 
## 1: Antal pins ramt i PinPoint. (0 til 25)
## 2: Findes i PinPoint men ikke ramt (1 eller 0)
## 3: Findes i 5 meter cirkelen (1 eller 0)
## I Datasættet vil hver observation kun kun tage en værdi i en af de 3
## kategorier. For at kunne medregne observationerne for PP med ikke ramt og
## 5 meter cirklen skaleres data, så der hypotetisk er 26 punkter i rammen, 
## hvor det 26'ende punkt repreæsenerer fund i PP eller i 5 meter cirklen

## Tæthedsvariablen tilføjes med en ifelse statement
PP.data$Tæthed <- ifelse(PP.data$Antal.pins != 0, 
                         PP.data$Antal.pins + 1,
                         PP.data$Tilstede.PP + PP.data$Tilstede.5m)


## Den relative tæthed i forhold til de 26 mulige posistioner dannes som variabel
PP.data$Procent.tæthed <- PP.data$Tæthed/26

## Datasættet udskrives til senere brug
write.csv(PP.data, "C:/Users/Julie Sander Lehnert/OneDrive/Dokumenter/Skole/UNI/Speciale/Behandlet data/Endelig udgave/PP.data.tæthed.behandlet.rå.csv")


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Tilføjelse af ressource-forhold ###
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## For at kunne lave plots over forekomsten af ressourceplanter i de enkelte
## plots tilføjes data over ressourceforhold for de enkelte plantearter.

PP.data <- merge(PP.data,
              vært.nektar.data,
              by = "Dansk.navn")



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###### ny dataframes - antal ressourceplante pr. plot ########
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Der laves en ny dataframe, hvor det kun er ressourceplanter, som fremgår for 
## hvor plot. HVilket betyder, at alle de plantearter, som er fundet i 
## feltarbejdet, men ikke er værtsplanter for nogle af de rødlistede arter 
## fjernes. Derefter kan antallet af ressourceplanter fundet pr. plot findes. 

## Danner en dataframe med alle de observationer indeholde en funden værtsplante
Værtsplante.df <- subset(PP.data, Værtsplante == 1)

length(unique(Værtsplante.df$Dansk.navn)) ## 27 arter ud af 210 er værtsplanter

## Danner en dataframe med alle de fundne planter, som er Nektar/pollen planter
nektar.df <- subset(PP.data, Nektar == 1)

length(unique(nektar.df$Dansk.navn)) ## 29 arter ud af 210 er nektar/pollensplanter

## De to dataframes merges
ressource.df <- rbind(Værtsplante.df, nektar.df)

ressource.df

## Da nogle af plantearterne både er værts- og nektarplanter fjernes alle dubletter
ressource.df <- unique(ressource.df)

## For at være sikker på, at vi bibeholder alle plot, selvom de ikke indeholder 
## værtsplanter.Dannes en ny dataframe, som indeholder
## alle plot-ID's
Plot.df <- data.frame(Plot = unique(PP.data$Plot))

## Plot-dataframe merges med værtsplante dataframes. Husk at bruge kommadoen
## all.x, så alle plot-ID beholdes, selvom de ikke indeholder arter, som er 
## værtsplanter
ressource.df <- merge(Plot.df, ressource.df, by ="Plot", all.x = TRUE)

length(unique(ressource.df$Plot)) # Alle plots er der stadig

# Plots som ikke har nogle værtsplanter, og derfor indeholder værdien NA
# får værdien 0
ressource.df[is.na(ressource.df)] = 0

## For at finde antallet af ressourceplanter pr. plot laves et for-loop, 
## som tæller antallet at ressourceplanter fundet på hvert plot

## liste over plot
plot.list <- unique(PP.data$Plot)

## tom dataframe, som skal indeholde optællingerne
ressource.antal.df <- data.frame()

## for-loop som laver optællingerne, og tilføjer dem til dataframen lavet til
## formålet
for (i in plot.list){
  sub <- subset(ressource.df, Plot == i)
  df <- data.frame(Plot = i,
                   Antal.arter = nrow(sub))
  ressource.antal.df <- rbind(ressource.antal.df, df)
  
}
# Tjek at den nye dataframe er blevet fyldt
ressource.antal.df



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Ny variabel - gens, tæthed af ressorceplanter pr. plot #####
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Der laves en ny dataframe, hvor den gennemsnitlige tæthed af 
## ressourceplanterne for hvert plot fremgår.
## For at finde tætheden af ressourceplanter pr. plot laves et for-loop, 
## som tæller regner den gennesmnitlige tæthed at ressourceplanter fundet for
## hvert plot

## liste over plot
plot.list <- unique(PP.data$Plot)

## tom dataframe, som skal indeholde optællingerne
ressource.tæthed.df <- data.frame()

## for-loop som laver optællingerne, og tilføjer dem til dataframen lavet til
## formålet
for (i in plot.list){
  sub <- subset(ressource.df, Plot == i)
  df <- data.frame(Plot = i,
                   PP.tæthed = mean(sub$Tæthed),
                   PP.procent.tæthed = mean(sub$Procent.tæthed))
  ressource.tæthed.df <- rbind(ressource.tæthed.df, df)
  
}

# Tjek at den nye dataframe er blevet fyldt
ressource.tæthed.df



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Ny variabel - Diversitet af ressourceplanter pr. plot ####
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Der laves en ny dataframe, som indeholder diversiteten af ressourceplanter
## for hvert plot. Diversiteten skal ses som en sammensætning af rigdommen 
## (antallet) af arter og tætheden af arterne, som et fælles værdi. Diversiteten
## udregnes i dette tilfælde, som et Shannons diversitetsindex

## For at kunne udregne shannon diversitets index, skal der først laves en matrix
## som indeholder Plotnavn, artsnavn på arterne, og tætheden af planterne
## Derfor laves først et subset af disse variable
ressource.sub <- ressource.df[,c("Plot", "Dansk.navn", "Tæthed")]

## Matrixen laves
ressource.matrix <- reshape(ressource.sub, 
                        idvar = "Plot", 
                        timevar = "Dansk.navn", 
                        direction = "wide")

## Kollonenavne ændres
names(ressource.matrix) <- sub('^Tæthed.', '', names(ressource.matrix))

## Alle NA-værdier ændres til 0
ressource.matrix[is.na(ressource.matrix)] <- 0


### Udregning af shannon diversity
shannon <- ddply(ressource.matrix, ~Plot, function(x) {
  data.frame(SHANNON=diversity(x[-1], index="shannon"))})

shannon


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Plot antallet af ressourcearter pr. habitatstype  ###############
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Der plottes antallet af ressource planter pr. plot for hver habitatstyper.
## Dette gøres her i et simplet boxplot for at få en umildbard ide om hvordan
## det ser ud.

## Habitatskode data tilføjes det datasættet med antallet af ressourceplanter
## pr plot.
ressource.antal.df <- merge(habitat.data,
                              ressource.antal.df)

## Antallet af ressourcearter i hvert hver habitatstype plottes i et boxplot
boxplot(Antal.arter ~ Habitatskode,
        data = ressource.antal.df)

#### Nogle af habitatstyperne er kun repræsenteret ved et plot.
## Disse fjernes eller lægges sammen. 
## Habitatstyperne, 0: omdrift, 3130: å-banke 7150: Tørvelavning, 7200: fattigkær
## og 91D0: Skovbevokset tørvemose, fjernes, da de kun er repræsenteret ved
## 1 plot, og derfor i lav grad siger noget om planteressourcerne i området. 
## 2320 og 2330 lægges sammen.

## Der er umildbart disse habitatsttyper fundet under feltarbejdet. 
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

ressource.antal.df <-subset(ressource.antal.df, Habitatskode != "0" &
                           Habitatskode != "7150" &
                           Habitatskode != "91D0" &
                           Habitatskode != "3130" &
                           Habitatskode != "7200")

ressource.antal.df$Habitatskode[ressource.antal.df$Habitatskode == 2320] <- "2320/2330"
ressource.antal.df$Habitatskode[ressource.antal.df$Habitatskode == 2330] <- "2320/2330"

## Boxplottet plottes igen med de "nye" habitatstyper
boxplot(Antal.arter ~ Habitatskode,
        data = ressource.antal.df)


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Statistisk test - Antal ressourcearter #########
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Der udføres en statistisk test, for at undersøge om det er en statistisk
## forskel mellem antallet af ressourcearter, som er tilstede i de enkelte
## habitatstyper. Først undersøges selve datagrundlaget, for at finde den
## den bedste statistiske test. 

## Tjek om data er normalfordelt, da det har indeflydelse på valg af test
shapiro.test(ressource.antal.df$Antal.arter[ressource.antal.df$Habitatskode == "4010"])
shapiro.test(ressource.antal.df$Antal.arter[ressource.antal.df$Habitatskode == "9190"])
shapiro.test(ressource.antal.df$Antal.arter[ressource.antal.df$Habitatskode == "4030"])
shapiro.test(ressource.antal.df$Antal.arter[ressource.antal.df$Habitatskode == "7140"])
shapiro.test(ressource.antal.df$Antal.arter[ressource.antal.df$Habitatskode == "9100"])
shapiro.test(ressource.antal.df$Antal.arter[ressource.antal.df$Habitatskode == "5130"])
shapiro.test(ressource.antal.df$Antal.arter[ressource.antal.df$Habitatskode == "6230"])
shapiro.test(ressource.antal.df$Antal.arter[ressource.antal.df$Habitatskode == "6410"])
shapiro.test(ressource.antal.df$Antal.arter[ressource.antal.df$Habitatskode == "7230"])
shapiro.test(ressource.antal.df$Antal.arter[ressource.antal.df$Habitatskode == "2320/2330"])

## Datagrundlaget er ikke ikke normal fordelte. Derfor udføres en Kruskal-wallis
## test, for at finde ud af, om der er en sigifikant forskel i antallet af 
## ressourcerarter mellem de forskellige habitater. Dette efterfølges af en Dunn
## test, for at finde ud af, hvilke specifikke habitater har en sigifikant 
## forskelligt antal ressourcearter i forhold til de andre habitatstyper

## Kruskal-wallis test
kruskal <- kruskal.test(Antal.arter ~ Habitatskode,
                        data = ressource.antal.df)
kruskal

## Dunn-test i form af boxplot med relevant data
ggbetweenstats(data = ressource.antal.df,
               x = Habitatskode,
               y = Antal.arter,
               type = "np",
               xlab = "Habitatstyper",
               ylab = "Antal ressourcearter")


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Plot tætheden af ressourcearter pr. habitatstype  ###############
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Der plottes tætheden af ressource planter pr. plot for hver habitatstyper.
## Dette gøres her i et simplet boxplot for at få en umildbard ide om hvordan
## det ser ud.

## Habitatskode data tilføjes det datasættet med tætheden af ressourceplanter
## pr plot.
ressource.tæthed.df <- merge(habitat.data,
                            ressource.tæthed.df)

## Antallet af ressourcearter i hvert hver habitatstype plottes i et boxplot
names(ressource.tæthed.df)
boxplot(PP.tæthed ~ Habitatskode,
        data = ressource.tæthed.df)

#### Nogle af habitatstyperne er kun repræsenteret ved et plot.
## Disse fjernes eller lægges sammen. 
## Habitatstyperne, 0: omdrift, 3130: å-banke 7150: Tørvelavning, 7200: fattigkær
## og 91D0: Skovbevokset tørvemose, fjernes, da de kun er repræsenteret ved
## 1 plot, og derfor i lav grad siger noget om planteressourcerne i området. 
## 2320 og 2330 lægges sammen.

## Der er umildbart disse habitatsttyper fundet under feltarbejdet. 
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

ressource.tæthed.df <-subset(ressource.tæthed.df, Habitatskode != "0" &
                              Habitatskode != "7150" &
                              Habitatskode != "91D0" &
                              Habitatskode != "3130" &
                              Habitatskode != "7200")

ressource.tæthed.df$Habitatskode[ressource.tæthed.df$Habitatskode == 2320] <- "2320/2330"
ressource.tæthed.df$Habitatskode[ressource.tæthed.df$Habitatskode == 2330] <- "2320/2330"

## Boxplottet plottes igen med de "nye" habitatstyper
boxplot(PP.tæthed ~ Habitatskode,
        data = ressource.tæthed.df)



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###### Statistisk test - Tæthed ressourcearter #####
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Der udføres en statistisk test, for at undersøge om det er en statistisk
## forskel mellem tætheden af ressourcearter, som er tilstede i de enkelte
## habitatstyper. Først undersøges selve datagrundlaget, for at finde den
## den bedste statistiske test. 

## Tjek om data er normalfordelt, da det har indeflydelse på valg af test
shapiro.test(ressource.tæthed.df$PP.tæthed[ressource.tæthed.df$Habitatskode == "4010"])
shapiro.test(ressource.tæthed.df$PP.tæthed[ressource.tæthed.df$Habitatskode == "9190"])
shapiro.test(ressource.tæthed.df$PP.tæthed[ressource.tæthed.df$Habitatskode == "4030"])
shapiro.test(ressource.tæthed.df$PP.tæthed[ressource.tæthed.df$Habitatskode == "7140"])
shapiro.test(ressource.tæthed.df$PP.tæthed[ressource.tæthed.df$Habitatskode == "9100"])
shapiro.test(ressource.tæthed.df$PP.tæthed[ressource.tæthed.df$Habitatskode == "5130"])
shapiro.test(ressource.tæthed.df$PP.tæthed[ressource.tæthed.df$Habitatskode == "6230"])
shapiro.test(ressource.tæthed.df$PP.tæthed[ressource.tæthed.df$Habitatskode == "6410"])
shapiro.test(ressource.tæthed.df$PP.tæthed[ressource.tæthed.df$Habitatskode == "7230"])
shapiro.test(ressource.tæthed.df$PP.tæthed[ressource.tæthed.df$Habitatskode == "2320/2330"])

## Hele datagrundlaget er ikke ikke normal fordelte. Derfor udføres en Kruskal-wallis
## test, for at finde ud af, om der er en sigifikant forskel i tætheden af 
## ressourcerarter mellem de forskellige habitater. Dette efterfølges af en Dunn
## test, for at finde ud af, hvilke specifikke habitater har en sigifikant 
## forskelligt tæthed af ressourcearter i forhold til de andre habitatstyper

## Kruskal-wallis test
kruskal <- kruskal.test(PP.tæthed ~ Habitatskode,
                        data = ressource.tæthed.df)

kruskal
## Der er en sigifikant forskel mellem tætheden af ressourcearter i habitatstyperne.
## Det vides dog ikke hvilke habitatstyper, som er signifikant forskellige. 

## Dunn-test i form af boxplot med relevant data
ggbetweenstats(data = ressource.tæthed.df,
               x = Habitatskode,
               y = PP.tæthed,
               type = "np",
               xlab = "Habitatstyper",
               ylab = "Gennemsnitligt tæthed af ressourcearter \n (Antal punkter i Pin-point-rammen)")



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Plot diversiteten af ressourcearter pr. habitatstype  ###############
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Der plottes diversiteten af ressourceplanter pr. plot for hver habitatstyper.
## Dette gøres her i et simplet boxplot for at få en umildbard ide om hvordan
## det ser ud.

## Habitatskode data tilføjes det datasættet med diversiteten af ressourceplanter
## pr plot.
ressource.diversitet.df <- merge(habitat.data,
                             shannon)

## Antallet af ressourcearter i hvert hver habitatstype plottes i et boxplot
names(ressource.diversitet.df)
boxplot(SHANNON ~ Habitatskode,
        data = ressource.diversitet.df)

#### Nogle af habitatstyperne er kun repræsenteret ved et plot.
## Disse fjernes eller lægges sammen. 
## Habitatstyperne, 0: omdrift, 3130: å-banke 7150: Tørvelavning, 7200: fattigkær
## og 91D0: Skovbevokset tørvemose, fjernes, da de kun er repræsenteret ved
## 1 plot, og derfor i lav grad siger noget om planteressourcerne i området. 
## 2320 og 2330 lægges sammen.

## Der er umildbart disse habitatsttyper fundet under feltarbejdet. 
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

ressource.diversitet.df <-subset(ressource.diversitet.df, Habitatskode != "0" &
                               Habitatskode != "7150" &
                               Habitatskode != "91D0" &
                               Habitatskode != "3130" &
                               Habitatskode != "7200")

ressource.diversitet.df$Habitatskode[ressource.diversitet.df$Habitatskode == 2320] <- "2320/2330"
ressource.diversitet.df$Habitatskode[ressource.diversitet.df$Habitatskode == 2330] <- "2320/2330"

## Boxplottet plottes igen med de "nye" habitatstyper
boxplot(SHANNON ~ Habitatskode,
        data = ressource.diversitet.df)


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###### Statistisk test - Diversitet ressourcearter #####
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Der udføres en statistisk test, for at undersøge om det er en statistisk
## forskel mellem tætheden af ressourcearter, som er tilstede i de enkelte
## habitatstyper. Først undersøges selve datagrundlaget, for at finde den
## den bedste statistiske test. 

## Tjek om data er normalfordelt, da det har indeflydelse på valg af test
shapiro.test(ressource.diversitet.df$SHANNON[ressource.diversitet.df$Habitatskode == "4010"])
shapiro.test(ressource.diversitet.df$SHANNON[ressource.diversitet.df$Habitatskode == "9190"])
shapiro.test(ressource.diversitet.df$SHANNON[ressource.diversitet.df$Habitatskode == "4030"])
shapiro.test(ressource.diversitet.df$SHANNON[ressource.diversitet.df$Habitatskode == "7140"])
shapiro.test(ressource.diversitet.df$SHANNON[ressource.diversitet.df$Habitatskode == "9100"])
shapiro.test(ressource.diversitet.df$SHANNON[ressource.diversitet.df$Habitatskode == "5130"])
shapiro.test(ressource.diversitet.df$SHANNON[ressource.diversitet.df$Habitatskode == "6230"])
shapiro.test(ressource.diversitet.df$SHANNON[ressource.diversitet.df$Habitatskode == "6410"])
shapiro.test(ressource.diversitet.df$SHANNON[ressource.diversitet.df$Habitatskode == "7230"])
shapiro.test(ressource.diversitet.df$SHANNON[ressource.diversitet.df$Habitatskode == "2320/2330"])

## Hele datagrundlaget er ikke ikke normal fordelte. Derfor udføres en Kruskal-wallis
## test, for at finde ud af, om der er en sigifikant forskel i diversiteten af 
## ressourcerarter mellem de forskellige habitater. Dette efterfølges af en Dunn
## test, for at finde ud af, hvilke specifikke habitater har en sigifikant 
## forskelligt diversitet af ressourcearter i forhold til de andre habitatstyper

## Kruskal-wallis test
kruskal <- kruskal.test(SHANNON ~ Habitatskode,
                        data = ressource.diversitet.df)

kruskal

## Der er en sigifikant forskel mellem diversiteten af ressourcearter i habitatstyperne.
## Det vides dog ikke hvilke habitatstyper, som er signifikant forskellige. 

## Dunn-test i form af boxplot med relevant data
ggbetweenstats(data = ressource.diversitet.df,
               x = Habitatskode,
               y = SHANNON,
               type = "np",
               xlab = "Habitatstyper",
               ylab = "Diversitet af ressourceplanter \n (Shannon index)")

################################################################################
## Denne dele af scriptet er ikke brugt i rapporten!!
## Denne del af scriptet bruges til at danne boxplots, der direkte viser
## hvilke arter, som er tilstede  på i hvert habitatstype og tætheden af dem. 
################################################################################

## Der laves subset af datagrundlaget for hver habitatstype - på baggrund af 
## seperat værtsplantedatasæt og nektar/pollen-datasæt.

vært.2320 <- subset(Værtsplante.df, Habitatskode == "2320")
unique(vært.2320$Plot) # 7 plots

vært.4010 <- subset(Værtsplante.df, Habitatskode == "4010")
length(unique(vært.4010$Plot)) # 23 plots

vært.4030 <- subset(Værtsplante.df, Habitatskode == "4030")
length(unique(vært.4030$Plot)) # 29 plots

vært.5130 <- subset(Værtsplante.df, Habitatskode == "5130")
length(unique(vært.5130$Plot)) # 8 plots

vært.6230 <- subset(Værtsplante.df, Habitatskode == "6230")
length(unique(vært.6230$Plot)) # 13 plots

vært.6410 <- subset(Værtsplante.df, Habitatskode == "6410")
length(unique(vært.6410$Plot)) # 2 plots

vært.7140 <- subset(Værtsplante.df, Habitatskode == "7140")
length(unique(vært.7140$Plot)) # 3 plots

vært.7230 <- subset(Værtsplante.df, Habitatskode == "7230")
length(unique(vært.7230$Plot)) # 5 plots

vært.9100 <- subset(Værtsplante.df, Habitatskode == "9100")
length(unique(vært.9100$Plot)) # 10 plots

vært.9190 <- subset(Værtsplante.df, Habitatskode == "9190")
length(unique(vært.9190$Plot)) # 8 plots


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
####### Boxplot - Værtsplante #######
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##Boxplot for værtsplanter i 2320
# der dannes en dataframe, som inderholder et "+" for de arter, som er tilstede
# og en placering for dette "+" i plottet
tk <- group_by(vært.2320, Dansk.navn) %>%
  summarise(Mean = mean(Procent.tæthed),
            Quant = quantile(Procent.tæthed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

#Danner boxplot
box.vært.2320 <- ggplot(vært.2320, 
                        aes(Dansk.navn, Procent.tæthed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis dækning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Revling-indlandsklit (2320)") + 
  annotate("text", x=25, y=0.90, label= "n = 7") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -1, hjust = -1, size = 3)

box.vært.2320

## værtsplante i 4010
tk <- group_by(vært.4010, Dansk.navn) %>%
  summarise(Mean = mean(Procent.tæthed),
            Quant = quantile(Procent.tæthed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.vært.4010 <- ggplot(vært.4010, 
                        aes(Dansk.navn, Procent.tæthed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis dækning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Våd hede (4010)") +
  annotate("text", x = 25, y= 0.95, label = "n = 23") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -1, hjust = -1, size = 3)

box.vært.4010

## værtsplante i 4030
tk <- group_by(vært.4030, Dansk.navn) %>%
  summarise(Mean = mean(Procent.tæthed),
            Quant = quantile(Procent.tæthed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.vært.4030 <- ggplot(vært.4030, 
                        aes(Dansk.navn, Procent.tæthed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis dækning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Tør hede (4030)") + 
  annotate("text", x = 25, y = 0.95, label = "n = 29") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -1, hjust = -1, size = 3)

box.vært.4030

## værtsplante i 5130
tk <- group_by(vært.5130, Dansk.navn) %>%
  summarise(Mean = mean(Procent.tæthed),
            Quant = quantile(Procent.tæthed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.vært.5130 <- ggplot(vært.5130, 
                        aes(Dansk.navn, Procent.tæthed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis dækning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Enebærkrat (5130)") +
  annotate("text", x=25, y=0.95, label= "n = 8") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -1, hjust = -1, size = 3)

box.vært.5130

## værtsplante i 6230
tk <- group_by(vært.6230, Dansk.navn) %>%
  summarise(Mean = mean(Procent.tæthed),
            Quant = quantile(Procent.tæthed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.vært.6230 <- ggplot(vært.6230, 
                        aes(Dansk.navn, Procent.tæthed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis dækning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Surt overdrev (6230)") +
  annotate("text", x=25, y=0.95, label= "n = 13") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.vært.6230

## værtsplante i 6410
tk <- group_by(vært.6410, Dansk.navn) %>%
  summarise(Mean = mean(Procent.tæthed),
            Quant = quantile(Procent.tæthed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.vært.6410 <- ggplot(vært.6410, 
                        aes(Dansk.navn, Procent.tæthed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis dækning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Tidvis våd eng (6410)") +
  annotate("text", x=25, y=0.95, label= "n = 2") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.vært.6410

## værtsplante i 7140
tk <- group_by(vært.7140, Dansk.navn) %>%
  summarise(Mean = mean(Procent.tæthed),
            Quant = quantile(Procent.tæthed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.vært.7140 <- ggplot(vært.7140, 
                        aes(Dansk.navn, Procent.tæthed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis dækning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Hængesæk (7140)") +
  annotate("text", x=25, y=0.95, label= "n = 3") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.vært.7140

## værtsplante i 7230
tk <- group_by(vært.7230, Dansk.navn) %>%
  summarise(Mean = mean(Procent.tæthed),
            Quant = quantile(Procent.tæthed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.vært.7230 <- ggplot(vært.7230, 
                        aes(Dansk.navn, Procent.tæthed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis dækning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Rigkær (7230)") +
  annotate("text", x=25, y=0.95, label= "n = 5") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.vært.7230

## værtsplante i 9100
tk <- group_by(vært.9100, Dansk.navn) %>%
  summarise(Mean = mean(Procent.tæthed),
            Quant = quantile(Procent.tæthed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.vært.9100 <- ggplot(vært.9100, 
                        aes(Dansk.navn, Procent.tæthed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis dækning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Andre skovtyper (9100)") +
  annotate("text", x=25, y=0.95, label= "n = 10") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.vært.9100

## værtsplante i 9190
tk <- group_by(vært.9190, Dansk.navn) %>%
  summarise(Mean = mean(Procent.tæthed),
            Quant = quantile(Procent.tæthed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.vært.9190 <- ggplot(vært.9190, 
                        aes(Dansk.navn, Procent.tæthed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis dækning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Stilkeegekrat (9190)") +
  annotate("text", x=25, y=0.95, label= "n = 8") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.vært.9190


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Boxplot - nektar/pollen #######
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


## nektar/pollenplanter i 2320
tk <- group_by(nektar.2320, Dansk.navn) %>%
  summarise(Mean = mean(Procent.tæthed),
            Quant = quantile(Procent.tæthed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.nektar.2320 <- ggplot(nektar.2320, 
                          aes(Dansk.navn, Procent.tæthed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis dækning (%)") +
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
  summarise(Mean = mean(Procent.tæthed),
            Quant = quantile(Procent.tæthed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.nektar.4010 <- ggplot(nektar.4010, 
                          aes(Dansk.navn, Procent.tæthed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis dækning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Våd hede (4010)") +
  annotate("text", x=28, y=0.95, label= "n = 23") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.nektar.4010

## nektar/pollenplanter i 4030
tk <- group_by(nektar.4030, Dansk.navn) %>%
  summarise(Mean = mean(Procent.tæthed),
            Quant = quantile(Procent.tæthed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.nektar.4030 <- ggplot(nektar.4030, 
                          aes(Dansk.navn, Procent.tæthed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis dækning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Tør hede (4030)") +
  annotate("text", x=28, y=0.95, label= "n = 29") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.nektar.4030

## nektar/pollenplanter i 5130
tk <- group_by(nektar.5130, Dansk.navn) %>%
  summarise(Mean = mean(Procent.tæthed),
            Quant = quantile(Procent.tæthed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.nektar.5130 <- ggplot(nektar.5130, 
                          aes(Dansk.navn, Procent.tæthed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis dækning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Enebærkrat (5130)") +
  annotate("text", x=28, y=0.95, label= "n = 8") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.nektar.5130

## nektar/pollenplanter i 6230
tk <- group_by(nektar.6230, Dansk.navn) %>%
  summarise(Mean = mean(Procent.tæthed),
            Quant = quantile(Procent.tæthed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.nektar.6230 <- ggplot(nektar.6230, 
                          aes(Dansk.navn, Procent.tæthed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis dækning (%)") +
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
  summarise(Mean = mean(Procent.tæthed),
            Quant = quantile(Procent.tæthed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.nektar.6410 <- ggplot(nektar.6410, 
                          aes(Dansk.navn, Procent.tæthed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis dækning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Tidvis våd eng (6410)") +
  annotate("text", x=28, y=0.95, label= "n = 2") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.nektar.6410

## nektar/pollenplanter i 7140
tk <- group_by(nektar.7140, Dansk.navn) %>%
  summarise(Mean = mean(Procent.tæthed),
            Quant = quantile(Procent.tæthed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.nektar.7140 <- ggplot(nektar.7140, 
                          aes(Dansk.navn, Procent.tæthed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis dækning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Hængesæk (7140)") +
  annotate("text", x=28, y=0.95, label= "n = 3") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.nektar.7140

## nektar/pollenplanter i 7230
tk <- group_by(nektar.7230, Dansk.navn) %>%
  summarise(Mean = mean(Procent.tæthed),
            Quant = quantile(Procent.tæthed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.nektar.7230 <- ggplot(nektar.7230, 
                          aes(Dansk.navn, Procent.tæthed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis dækning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Rigkær (7230)") +
  annotate("text", x=28, y=0.95, label= "n = 5") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.nektar.7230

## nektar/pollenplanter i 9100
tk <- group_by(nektar.9100, Dansk.navn) %>%
  summarise(Mean = mean(Procent.tæthed),
            Quant = quantile(Procent.tæthed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.nektar.9100 <- ggplot(nektar.9100, 
                          aes(Dansk.navn, Procent.tæthed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis dækning (%)") +
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
  summarise(Mean = mean(Procent.tæthed),
            Quant = quantile(Procent.tæthed, probs = 0.75))
tk$Fund <- ifelse(tk$Mean != 0, "+", "")
tk <- data.frame(tk)
tk

box.nektar.9190 <- ggplot(nektar.9190, 
                          aes(Dansk.navn, Procent.tæthed)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "Arter", y = "Procentvis dækning (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Stilkegekrat (9190)") +
  annotate("text", x=28, y=0.95, label= "n = 8") + 
  geom_text(data = tk, aes(label = Fund, x = Dansk.navn, y = Quant),
            vjust = -0.5, hjust = -1, size = 3)

box.nektar.9190


############################## Scriptet er slut ################################

