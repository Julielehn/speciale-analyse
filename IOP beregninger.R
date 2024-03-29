## IOP beregninger p� baggrund af udvalgte omr�der, hvor arternes ressourcebehov
## df observeret til at v�re mest d�kket 

##################################################

setwd("C:/Users/Julie Sander Lehnert/OneDrive/Dokumenter/Skole/UNI/Speciale")

list.files()

##################################################

# datafil, som indholder distance data indl�ses
data <- read.csv("Distance data - Behandlet.csv",
                 header = TRUE,
                 sep = ";")

head(data)

length(data)


# Deler data op - plot og dato i et dataframe og distancem�l i en anden dataframe. 
Plot <- data[,1]
Dist.data <- data[,3:449]

# Distance data konverteres
Dist.data <- 1-(Dist.data)^2/(500)^2

data <- cbind(Plot, Dist.data)

names(data)

setwd("C:/Users/Julie Sander Lehnert/OneDrive/Dokumenter/Skole/UNI/Speciale/Behandlet data/Brugbar")

list.files()

GPS.data <- read.csv("GPS-punkter - csv.csv",
                     header = T,
                     sep = ";")
GPS.data <- GPS.data[,c(2, 4:5)]
names(GPS.data) <- (c("Plot", "GPS.X", "GPS.Y"))
names(GPS.data)

data <- merge(data, GPS.data, by = "Plot")
length(unique(data$Plot))

#############################################################
# Trehornet skarnbasse
# Vigtige ressourcer: G�dning, bar jord, vegetationsh�jde


bar.jord <- data[,c("Plot", 
                    "GPS.X", 
                    "GPS.Y", 
                    "Bar_jord.over.30cm")]

g�dning <- data[,c("Plot",
                   "GPS.X",
                   "GPS.Y",
                   "Hjorte_pellets",
                   "Kokasse",
                   "Hestep�re",
                   "F�raff�ring")]

# orange jordbi
# Vigtigste ressourcer: v�rtsplanter (dj�velsbid og bl�hat)
# og pletter af bar jord

bl�hat.dj�velsbid <- data[,c("Plot",
                             "GPS.X",
                             "GPS.Y",
                             "Bl�hat_b",
                             "Dj�velsbid_b")]

## Ensian bl�fugl
# ressourcekrav: v�rt (klokkeensian - blomstrende), mieralsk myretue
# mikrotopografi, nektar (klokkelyng)

klokke.ensian <- data[,c("Plot",
                        "GPS.X",
                        "GPS.Y",
                        "Klokke.ensian_b")]

myre.min <- data[,c("Plot",
                    "GPS.X",
                    "GPS.Y",
                    "Myretue_min")]

mikrotop15 <- data[,c("Plot",
                      "GPS.X",
                      "GPS.Y",
                      "Mikrotop.over.15cm")]

klokkelyng <- data[,c("Plot",
                               "GPS.X",
                               "GPS.Y",
                               "Klokkelyng_b")] 



## Minimum distance til ressource
bar.jord$bar.jord <- bar.jord[,4]

g�dning$g�dning <-apply(X=g�dning[,4:7], MARGIN=1, FUN=max)

bl�hat.dj�velsbid$pollen <-apply(X=bl�hat.dj�velsbid[,4:5], MARGIN=1, FUN=max)

klokke.ensian$klokke.ensian <- klokke.ensian[,4]

myre.min$myre <- myre.min[,4]

mikrotop15$mikrotop <- mikrotop15[,4]

klokkelyng$klokkelyng <- klokkelyng[,4]


## Laver kollone med (0 eller 1), som fort�ller om ressourcen
## er tilstede i plottet
bar.jord$tilstede <- ifelse(bar.jord$bar.jord == 0,
                                       0,
                                       1)

g�dning$tilstede <- ifelse(g�dning$g�dning == 0,
                                      0,
                                      1)

bl�hat.dj�velsbid$tilstede <- ifelse(bl�hat.dj�velsbid$pollen == 0,
                                     0,
                                     1)

klokke.ensian$tilstede <- ifelse(klokke.ensian$klokke.ensian == 0,
                                 0,
                                 1)

myre.min$tilstede <- ifelse(myre.min$myre == 0,
                            0,
                            1)

mikrotop15$tilstede <- ifelse(mikrotop15$mikrotop == 0,
                            0,
                            1)

klokkelyng$tilstede <- ifelse(klokkelyng$klokkelyng == 0,
                              0,
                              1)



## Subsetter s� det kun er plots tilbage, hvor ressourcen er tilstede
bar.jord <- subset(bar.jord, tilstede == 1)

g�dning <- subset(g�dning, tilstede == 1)

bl�hat.dj�velsbid <- subset(bl�hat.dj�velsbid, tilstede == 1)

klokke.ensian <- subset(klokke.ensian, tilstede == 1)

myre.min <- subset(myre.min, tilstede == 1)

mikrotop15 <- subset(mikrotop15, tilstede == 1)

klokkelyng <- subset(klokkelyng, tilstede == 1)


### Finder de plots hvor der er overlap af alle ressourcer
skarnbasse.overlap <- merge(bar.jord,
                            g�dning,
                            by = "Plot")

jordbi.overlap <- merge(bar.jord,
                        bl�hat.dj�velsbid,
                        by = "Plot")

bl�fugl.overlap <- merge(klokke.ensian,
                      myre.min,
                      by = "Plot")

names(bl�fugl.overlap)

bl�fugl.overlap <- bl�fugl.overlap[,c(1:3,
                                5,
                                10)]


bl�fugl.overlap <- merge(bl�fugl.overlap,
                      mikrotop15,
                      by = "Plot")
names(bl�fugl.overlap)

bl�fugl.overlap <- bl�fugl.overlap[,c(1:5,
                                9)]

bl�fugl.overlap <- merge(bl�fugl.overlap,
                      klokkelyng,
                      by = "Plot")

bl�fugl.overlap <- bl�fugl.overlap[,c(1:6,
                                10)]

#### udskriv datas�t, s� de kan importeres til QGIS
setwd("C:/Users/Julie Sander Lehnert/OneDrive/Dokumenter/Skole/UNI/Speciale/Behandlet data/Brugbar/Venn-diagrammer")

write.csv(bar.jord, "bar jord - 9.6.22.csv")

write.csv(g�dning, "g�dning - 9.6.22.csv")

write.csv(bl�hat.dj�velsbid, "bl�hat.dj�velsbid - 9.6.22.csv")

write.csv(klokke.ensian, "klokkeensian - 9.6.22.csv")

write.csv(mikrotop15, "mikrotop - 9.6.22.csv")

write.csv(myre.min, "myre - 9.6.22.csv")

write.csv(klokkelyng, "klokkelyng - 9.6.22.csv")

write.csv(skarnbasse.overlap, "Skarnbasse - overlap 9.6.22.csv")

write.csv(bl�fugl.overlap, "Bl�fugl - overlap 9.6.22.csv")

write.csv(jordbi.overlap, "Jordbi - overlap 9.6.22.csv")

#### Filer er blevet importeret i QGIS og en liste af punker for omr�der, hvor 
## der er h�jere densitet af ressourcer importeres nu tilbage til Rstudio

#########################
# Orange jordbi

list.files()

jordbi.omr�der <- read.csv("jordbi - omr�de plots.csv",
                           header = T,
                           sep = ";")

skarnbasse.omr�der <- read.csv("skarnbasse - omr�de plots.csv",
                               header = T,
                               sep = ";")

bl�fugl.omr�der <- read.csv("bl�fugl - ommr�de plots.csv",
                            header = T,
                            sep = ";")

jordbi.omr�de.1 <- subset(jordbi.omr�der, Omr�de == 1)

jordbi.omr�de.2 <- subset(jordbi.omr�der, Omr�de == 2)

jordbi.omr�de.3 <- subset(jordbi.omr�der, Omr�de == 3)

jordbi.omr�de.4 <- subset(jordbi.omr�der, Omr�de == 4)


skarnbasse.omr�de.1 <- subset(skarnbasse.omr�der, Omr�de == 1)

skarnbasse.omr�de.2 <- subset(skarnbasse.omr�der, Omr�de == 2)

skarnbasse.omr�de.3 <- subset(skarnbasse.omr�der, Omr�de == 3)

skarnbasse.omr�de.4 <- subset(skarnbasse.omr�der, Omr�de == 4)


bl�fugl.omr�de.1 <- subset(bl�fugl.omr�der, Omr�de == 1)

bl�fugl.omr�de.2 <- subset(bl�fugl.omr�der, Omr�de == 2)

bl�fugl.omr�de.3 <- subset(bl�fugl.omr�der, Omr�de == 3)

bl�fugl.omr�de.4 <- subset(bl�fugl.omr�der, Omr�de == 4)

############

jordbi.omr�de.1.bar.jord <- merge(jordbi.omr�de.1,
                                  bar.jord,
                                  by = "Plot",
                                  all.x = T)
jordbi.omr�de.1.bar.jord[is.na(jordbi.omr�de.1.bar.jord)] <- 0

jordbi.omr�de.2.bar.jord <- merge(jordbi.omr�de.2,
                                  bar.jord,
                                  by = "Plot",
                                  all.x = T)
jordbi.omr�de.2.bar.jord[is.na(jordbi.omr�de.2.bar.jord)] <- 0


jordbi.omr�de.3.bar.jord <- merge(jordbi.omr�de.3,
                                  bar.jord,
                                  by = "Plot",
                                  all.x = T)
jordbi.omr�de.3.bar.jord[is.na(jordbi.omr�de.3.bar.jord)] <- 0

jordbi.omr�de.4.bar.jord <- merge(jordbi.omr�de.4,
                                  bar.jord,
                                  by = "Plot",
                                  all.x = T)
jordbi.omr�de.4.bar.jord[is.na(jordbi.omr�de.4.bar.jord)] <- 0

##################

jordbi.omr�de.1.pollen <- merge(jordbi.omr�de.1,
                                bl�hat.dj�velsbid,
                                by = "Plot",
                                all.x = T)
jordbi.omr�de.1.pollen[is.na(jordbi.omr�de.1.pollen)] <- 0

jordbi.omr�de.2.pollen <- merge(jordbi.omr�de.2,
                                bl�hat.dj�velsbid,
                                by = "Plot",
                                all.x = T)
jordbi.omr�de.2.pollen[is.na(jordbi.omr�de.2.pollen)] <- 0

jordbi.omr�de.3.pollen <- merge(jordbi.omr�de.3,
                                bl�hat.dj�velsbid,
                                by = "Plot",
                                all.x = T)
jordbi.omr�de.3.pollen[is.na(jordbi.omr�de.3.pollen)] <- 0

jordbi.omr�de.4.pollen <- merge(jordbi.omr�de.4,
                                bl�hat.dj�velsbid,
                                by = "Plot",
                                all.x = T)
jordbi.omr�de.4.pollen[is.na(jordbi.omr�de.4.pollen)] <- 0

#####################################

skarnbasse.omr�de.1.bar.jord <- merge(skarnbasse.omr�de.1,
                                      bar.jord,
                                      by = "Plot",
                                      all.x = T)
skarnbasse.omr�de.1.bar.jord[is.na(skarnbasse.omr�de.1.bar.jord)] <- 0

skarnbasse.omr�de.2.bar.jord <- merge(skarnbasse.omr�de.2,
                                      bar.jord,
                                      by = "Plot",
                                      all.x = T)
skarnbasse.omr�de.2.bar.jord[is.na(skarnbasse.omr�de.2.bar.jord)] <- 0

skarnbasse.omr�de.3.bar.jord <- merge(skarnbasse.omr�de.3,
                                      bar.jord,
                                      by = "Plot",
                                      all.x = T)
skarnbasse.omr�de.3.bar.jord[is.na(skarnbasse.omr�de.3.bar.jord)] <- 0

skarnbasse.omr�de.4.bar.jord <- merge(skarnbasse.omr�de.4,
                                      bar.jord,
                                      by = "Plot",
                                      all.x = T)
skarnbasse.omr�de.4.bar.jord[is.na(skarnbasse.omr�de.4.bar.jord)] <- 0

######################################

skarnbasse.omr�de.1.g�dning <- merge(skarnbasse.omr�de.1,
                                     g�dning,
                                     by = "Plot",
                                     all.x = T)
skarnbasse.omr�de.1.g�dning[is.na(skarnbasse.omr�de.1.g�dning)] <- 0

skarnbasse.omr�de.2.g�dning <- merge(skarnbasse.omr�de.2,
                                     g�dning,
                                     by = "Plot",
                                     all.x = T)
skarnbasse.omr�de.2.g�dning[is.na(skarnbasse.omr�de.2.g�dning)] <- 0

skarnbasse.omr�de.3.g�dning <- merge(skarnbasse.omr�de.3,
                                     g�dning,
                                     by = "Plot",
                                     all.x = T)
skarnbasse.omr�de.3.g�dning[is.na(skarnbasse.omr�de.3.g�dning)] <- 0

skarnbasse.omr�de.4.g�dning <- merge(skarnbasse.omr�de.4,
                                     g�dning,
                                     by = "Plot",
                                     all.x = T)
skarnbasse.omr�de.4.g�dning[is.na(skarnbasse.omr�de.4.g�dning)] <- 0

#############################

bl�fugl.omr�de.1.v�rt <- merge(bl�fugl.omr�de.1,
                               klokke.ensian,
                               by.x = "Plot",
                               all.x = T)
bl�fugl.omr�de.1.v�rt[is.na(bl�fugl.omr�de.1.v�rt)] <- 0


bl�fugl.omr�de.2.v�rt <- merge(bl�fugl.omr�de.2,
                               klokke.ensian,
                               by = "Plot",
                               all.x = T)
bl�fugl.omr�de.2.v�rt[is.na(bl�fugl.omr�de.2.v�rt)] <- 0


bl�fugl.omr�de.3.v�rt <- merge(bl�fugl.omr�de.3,
                               klokke.ensian,
                               by = "Plot",
                               all.x = T)
bl�fugl.omr�de.3.v�rt[is.na(bl�fugl.omr�de.3.v�rt)] <- 0

bl�fugl.omr�de.4.v�rt <- merge(bl�fugl.omr�de.4,
                               klokke.ensian,
                               by = "Plot",
                               all.x = T)
bl�fugl.omr�de.4.v�rt[is.na(bl�fugl.omr�de.4.v�rt)] <- 0
bl�fugl.omr�de.4.v�rt

####################################

bl�fugl.omr�de.1.myre <- merge(bl�fugl.omr�de.1,
                               myre.min,
                               by = "Plot",
                               all.x = T)
bl�fugl.omr�de.1.myre[is.na(bl�fugl.omr�de.1.myre)] <- 0

bl�fugl.omr�de.2.myre <- merge(bl�fugl.omr�de.2,
                               myre.min,
                               by = "Plot",
                               all.x = T)
bl�fugl.omr�de.2.myre[is.na(bl�fugl.omr�de.2.myre)] <- 0

bl�fugl.omr�de.3.myre <- merge(bl�fugl.omr�de.3,
                               myre.min,
                               by = "Plot",
                               all.x = T)
bl�fugl.omr�de.3.myre[is.na(bl�fugl.omr�de.3.myre)] <- 0

bl�fugl.omr�de.4.myre <- merge(bl�fugl.omr�de.4,
                               myre.min,
                               by = "Plot",
                               all.x = T)
bl�fugl.omr�de.4.myre[is.na(bl�fugl.omr�de.4.myre)] <- 0

################################

bl�fugl.omr�de.1.mikrotop <- merge(bl�fugl.omr�de.1,
                                   mikrotop15,
                                   by = "Plot",
                                   all.x = T)
bl�fugl.omr�de.1.mikrotop[is.na(bl�fugl.omr�de.1.mikrotop)] <- 0

bl�fugl.omr�de.2.mikrotop <- merge(bl�fugl.omr�de.2,
                                   mikrotop15,
                                   by = "Plot",
                                   all.x = T)
bl�fugl.omr�de.2.mikrotop[is.na(bl�fugl.omr�de.2.mikrotop)] <- 0

bl�fugl.omr�de.3.mikrotop <- merge(bl�fugl.omr�de.3,
                                   mikrotop15,
                                   by = "Plot",
                                   all.x = T)
bl�fugl.omr�de.3.mikrotop[is.na(bl�fugl.omr�de.3.mikrotop)] <- 0

bl�fugl.omr�de.4.mikrotop <- merge(bl�fugl.omr�de.4,
                                   mikrotop15,
                                   by = "Plot",
                                   all.x = T)
bl�fugl.omr�de.4.mikrotop[is.na(bl�fugl.omr�de.4.mikrotop)] <- 0

##############################

bl�fugl.omr�de.1.nektar <- merge(bl�fugl.omr�de.1,
                                 klokkelyng,
                                 by = "Plot",
                                 all.x = T)
bl�fugl.omr�de.1.nektar[is.na(bl�fugl.omr�de.1.nektar)] <- 0

bl�fugl.omr�de.2.nektar <- merge(bl�fugl.omr�de.2,
                                 klokkelyng,
                                 by = "Plot",
                                 all.x = T)
bl�fugl.omr�de.2.nektar[is.na(bl�fugl.omr�de.2.nektar)] <- 0

bl�fugl.omr�de.3.nektar <- merge(bl�fugl.omr�de.3,
                                 klokkelyng,
                                 by = "Plot",
                                 all.x = T)
bl�fugl.omr�de.3.nektar[is.na(bl�fugl.omr�de.3.nektar)] <- 0

bl�fugl.omr�de.4.nektar <- merge(bl�fugl.omr�de.4,
                                 klokkelyng,
                                 by = "Plot",
                                 all.x = T)
bl�fugl.omr�de.4.nektar[is.na(bl�fugl.omr�de.4.nektar)] <- 0

#################################
## IOP beregninger for Orange jordbi


jordbi.omr�de.1.bar.jord.IOP <- mean(jordbi.omr�de.1.bar.jord$bar.jord)
jordbi.omr�de.1.bar.jord.IOP

jordbi.omr�de.2.bar.jord.IOP <- mean(jordbi.omr�de.2.bar.jord$bar.jord)
jordbi.omr�de.2.bar.jord.IOP

jordbi.omr�de.3.bar.jord.IOP <- mean(jordbi.omr�de.3.bar.jord$bar.jord)
jordbi.omr�de.3.bar.jord.IOP

jordbi.omr�de.4.bar.jord.IOP <- mean(jordbi.omr�de.4.bar.jord$bar.jord)
jordbi.omr�de.4.bar.jord.IOP

###########


jordbi.omr�de.1.pollen.IOP <- mean(jordbi.omr�de.1.pollen$pollen)
jordbi.omr�de.1.pollen.IOP

jordbi.omr�de.2.pollen.IOP <- mean(jordbi.omr�de.2.pollen$pollen)
jordbi.omr�de.2.pollen.IOP

jordbi.omr�de.3.pollen.IOP <- mean(jordbi.omr�de.3.pollen$pollen)
jordbi.omr�de.3.pollen.IOP

jordbi.omr�de.4.pollen.IOP <- mean(jordbi.omr�de.4.pollen$pollen)
jordbi.omr�de.4.pollen.IOP

##################

jordbi.omr�de.1.IOP.total <- jordbi.omr�de.1.bar.jord.IOP*jordbi.omr�de.1.pollen.IOP
jordbi.omr�de.1.IOP.total

jordbi.omr�de.3.IOP.total <- jordbi.omr�de.3.bar.jord.IOP*jordbi.omr�de.3.pollen.IOP
jordbi.omr�de.3.IOP.total

jordbi.omr�de.4.IOP.total <- jordbi.omr�de.4.bar.jord.IOP*jordbi.omr�de.4.pollen.IOP
jordbi.omr�de.4.IOP.total

#################################

## IOP beregninger for trehornet skarnbasse
skarnbasse.omr�de.1.bar.jord.IOP <- mean(skarnbasse.omr�de.1.bar.jord$bar.jord)
skarnbasse.omr�de.1.bar.jord.IOP

skarnbasse.omr�de.2.bar.jord.IOP <- mean(skarnbasse.omr�de.2.bar.jord$bar.jord)
skarnbasse.omr�de.2.bar.jord.IOP

skarnbasse.omr�de.3.bar.jord.IOP <- mean(skarnbasse.omr�de.3.bar.jord$bar.jord)
skarnbasse.omr�de.3.bar.jord.IOP

skarnbasse.omr�de.4.bar.jord.IOP <- mean(skarnbasse.omr�de.4.bar.jord$bar.jord)
skarnbasse.omr�de.4.bar.jord.IOP

#############


skarnbasse.omr�de.1.g�dning.IOP <- mean(skarnbasse.omr�de.1.g�dning$g�dning)
skarnbasse.omr�de.1.g�dning.IOP

skarnbasse.omr�de.2.g�dning.IOP <- mean(skarnbasse.omr�de.2.g�dning$g�dning)
skarnbasse.omr�de.2.g�dning.IOP

skarnbasse.omr�de.3.g�dning.IOP <- mean(skarnbasse.omr�de.3.g�dning$g�dning)
skarnbasse.omr�de.3.g�dning.IOP

skarnbasse.omr�de.4.g�dning.IOP <- mean(skarnbasse.omr�de.4.g�dning$g�dning)
skarnbasse.omr�de.4.g�dning.IOP

############


skarnbasse.omr�de.1.IOP.total <- skarnbasse.omr�de.1.bar.jord.IOP*skarnbasse.omr�de.1.g�dning.IOP
skarnbasse.omr�de.1.IOP.total

skarnbasse.omr�de.2.IOP.total <- skarnbasse.omr�de.2.bar.jord.IOP*skarnbasse.omr�de.2.g�dning.IOP
skarnbasse.omr�de.2.IOP.total

skarnbasse.omr�de.3.IOP.total <- skarnbasse.omr�de.3.bar.jord.IOP*skarnbasse.omr�de.3.g�dning.IOP
skarnbasse.omr�de.3.IOP.total

skarnbasse.omr�de.4.IOP.total <- skarnbasse.omr�de.4.bar.jord.IOP*skarnbasse.omr�de.4.g�dning.IOP
skarnbasse.omr�de.4.IOP.total


#######################################################
# IOP beregninger for Ensianbl�fugl

bl�fugl.omr�de.1.v�rt.IOP <- mean(bl�fugl.omr�de.1.v�rt$klokke.ensian)
bl�fugl.omr�de.1.v�rt.IOP

bl�fugl.omr�de.2.v�rt.IOP <- mean(bl�fugl.omr�de.2.v�rt$klokke.ensian)
bl�fugl.omr�de.2.v�rt.IOP

bl�fugl.omr�de.3.v�rt.IOP <- mean(bl�fugl.omr�de.3.v�rt$klokke.ensian)
bl�fugl.omr�de.3.v�rt.IOP

bl�fugl.omr�de.4.v�rt.IOP <- mean(bl�fugl.omr�de.4.v�rt$klokke.ensian)
bl�fugl.omr�de.4.v�rt.IOP

############


bl�fugl.omr�de.1.myre.IOP <- mean(bl�fugl.omr�de.1.myre$myre)
bl�fugl.omr�de.1.myre.IOP

bl�fugl.omr�de.2.myre.IOP <- mean(bl�fugl.omr�de.2.myre$myre)
bl�fugl.omr�de.2.myre.IOP

bl�fugl.omr�de.3.myre.IOP <- mean(bl�fugl.omr�de.3.myre$myre)
bl�fugl.omr�de.3.myre.IOP

bl�fugl.omr�de.4.myre.IOP <- mean(bl�fugl.omr�de.4.myre$myre)
bl�fugl.omr�de.4.myre.IOP

##############

bl�fugl.omr�de.1.mikrotop.IOP <- mean(bl�fugl.omr�de.1.mikrotop$mikrotop)
bl�fugl.omr�de.1.mikrotop.IOP

bl�fugl.omr�de.2.mikrotop.IOP <- mean(bl�fugl.omr�de.2.mikrotop$mikrotop)
bl�fugl.omr�de.2.mikrotop.IOP

bl�fugl.omr�de.3.mikrotop.IOP <- mean(bl�fugl.omr�de.3.mikrotop$mikrotop)
bl�fugl.omr�de.3.mikrotop.IOP

bl�fugl.omr�de.4.mikrotop.IOP <- mean(bl�fugl.omr�de.4.mikrotop$mikrotop)
bl�fugl.omr�de.4.mikrotop.IOP

##############


bl�fugl.omr�de.1.nektar.IOP <- mean(bl�fugl.omr�de.1.nektar$klokkelyng)
bl�fugl.omr�de.1.nektar.IOP

bl�fugl.omr�de.2.nektar.IOP <- mean(bl�fugl.omr�de.2.nektar$klokkelyng)
bl�fugl.omr�de.2.nektar.IOP

bl�fugl.omr�de.3.nektar.IOP <- mean(bl�fugl.omr�de.3.nektar$klokkelyng)
bl�fugl.omr�de.3.nektar.IOP

bl�fugl.omr�de.4.nektar.IOP <- mean(bl�fugl.omr�de.4.nektar$klokkelyng)
bl�fugl.omr�de.4.nektar.IOP

############


bl�fugl.omr�de.1.v�rt.myre.IOP <- bl�fugl.omr�de.1.v�rt.IOP * bl�fugl.omr�de.1.myre.IOP
bl�fugl.omr�de.1.v�rt.myre.IOP

bl�fugl.omr�de.2.v�rt.myre.IOP <- bl�fugl.omr�de.2.v�rt.IOP * bl�fugl.omr�de.2.myre.IOP
bl�fugl.omr�de.2.v�rt.myre.IOP

bl�fugl.omr�de.3.v�rt.myre.IOP <- bl�fugl.omr�de.3.v�rt.IOP * bl�fugl.omr�de.3.myre.IOP
bl�fugl.omr�de.3.v�rt.myre.IOP

bl�fugl.omr�de.4.v�rt.myre.IOP <- bl�fugl.omr�de.4.v�rt.IOP * bl�fugl.omr�de.4.myre.IOP
bl�fugl.omr�de.4.v�rt.myre.IOP

############

bl�fugl.omr�de.1.v�rt.mikrotop.IOP <- bl�fugl.omr�de.1.v�rt.IOP * bl�fugl.omr�de.1.mikrotop.IOP
bl�fugl.omr�de.1.v�rt.mikrotop.IOP

bl�fugl.omr�de.2.v�rt.mikrotop.IOP <- bl�fugl.omr�de.2.v�rt.IOP * bl�fugl.omr�de.2.mikrotop.IOP
bl�fugl.omr�de.2.v�rt.mikrotop.IOP

bl�fugl.omr�de.3.v�rt.mikrotop.IOP <- bl�fugl.omr�de.3.v�rt.IOP * bl�fugl.omr�de.3.mikrotop.IOP
bl�fugl.omr�de.3.v�rt.mikrotop.IOP

bl�fugl.omr�de.4.v�rt.mikrotop.IOP <- bl�fugl.omr�de.4.v�rt.IOP * bl�fugl.omr�de.4.mikrotop.IOP
bl�fugl.omr�de.4.v�rt.mikrotop.IOP

###############


bl�fugl.omr�de.1.v�rt.nektar.IOP <- bl�fugl.omr�de.1.v�rt.IOP * bl�fugl.omr�de.1.nektar.IOP
bl�fugl.omr�de.1.v�rt.nektar.IOP

bl�fugl.omr�de.2.v�rt.nektar.IOP <- bl�fugl.omr�de.2.v�rt.IOP * bl�fugl.omr�de.2.nektar.IOP
bl�fugl.omr�de.2.v�rt.nektar.IOP

bl�fugl.omr�de.3.v�rt.nektar.IOP <- bl�fugl.omr�de.3.v�rt.IOP * bl�fugl.omr�de.3.nektar.IOP
bl�fugl.omr�de.3.v�rt.nektar.IOP

bl�fugl.omr�de.4.v�rt.nektar.IOP <- bl�fugl.omr�de.4.v�rt.IOP * bl�fugl.omr�de.4.nektar.IOP
bl�fugl.omr�de.4.v�rt.nektar.IOP

#############

bl�fugl.omr�de.1.mikrotop.myre.IOP <- bl�fugl.omr�de.1.mikrotop.IOP * bl�fugl.omr�de.1.myre.IOP
bl�fugl.omr�de.1.mikrotop.myre.IOP

bl�fugl.omr�de.2.mikrotop.myre.IOP <- bl�fugl.omr�de.2.mikrotop.IOP * bl�fugl.omr�de.2.myre.IOP
bl�fugl.omr�de.2.mikrotop.myre.IOP

bl�fugl.omr�de.3.mikrotop.myre.IOP <- bl�fugl.omr�de.3.mikrotop.IOP * bl�fugl.omr�de.3.myre.IOP
bl�fugl.omr�de.3.mikrotop.myre.IOP

bl�fugl.omr�de.4.mikrotop.myre.IOP <- bl�fugl.omr�de.4.mikrotop.IOP * bl�fugl.omr�de.4.myre.IOP
bl�fugl.omr�de.4.mikrotop.myre.IOP

###############

bl�fugl.omr�de.1.mikrotop.nektar.IOP <- bl�fugl.omr�de.1.mikrotop.IOP * bl�fugl.omr�de.1.nektar.IOP
bl�fugl.omr�de.1.mikrotop.nektar.IOP

bl�fugl.omr�de.2.mikrotop.nektar.IOP <- bl�fugl.omr�de.2.mikrotop.IOP * bl�fugl.omr�de.2.nektar.IOP
bl�fugl.omr�de.2.mikrotop.nektar.IOP

bl�fugl.omr�de.3.mikrotop.nektar.IOP <- bl�fugl.omr�de.3.mikrotop.IOP * bl�fugl.omr�de.3.nektar.IOP
bl�fugl.omr�de.3.mikrotop.nektar.IOP

bl�fugl.omr�de.4.mikrotop.nektar.IOP <- bl�fugl.omr�de.4.mikrotop.IOP * bl�fugl.omr�de.4.nektar.IOP
bl�fugl.omr�de.4.mikrotop.nektar.IOP

#####################

bl�fugl.omr�de.1.myre.nektar.IOP <- bl�fugl.omr�de.1.myre.IOP * bl�fugl.omr�de.1.nektar.IOP
bl�fugl.omr�de.1.myre.nektar.IOP

bl�fugl.omr�de.2.myre.nektar.IOP <- bl�fugl.omr�de.2.myre.IOP * bl�fugl.omr�de.2.nektar.IOP
bl�fugl.omr�de.2.myre.nektar.IOP

bl�fugl.omr�de.3.myre.nektar.IOP <- bl�fugl.omr�de.3.myre.IOP * bl�fugl.omr�de.3.nektar.IOP
bl�fugl.omr�de.3.myre.nektar.IOP

bl�fugl.omr�de.4.myre.nektar.IOP <- bl�fugl.omr�de.4.myre.IOP * bl�fugl.omr�de.4.nektar.IOP
bl�fugl.omr�de.4.myre.nektar.IOP

###################

bl�fugl.omr�de.1.v�rt.myre.mikro.IOP <- bl�fugl.omr�de.1.v�rt.IOP * bl�fugl.omr�de.1.myre.IOP * bl�fugl.omr�de.1.mikrotop.IOP
bl�fugl.omr�de.1.v�rt.myre.mikro.IOP

bl�fugl.omr�de.2.v�rt.myre.mikro.IOP <- bl�fugl.omr�de.2.v�rt.IOP * bl�fugl.omr�de.2.myre.IOP * bl�fugl.omr�de.2.mikrotop.IOP
bl�fugl.omr�de.2.v�rt.myre.mikro.IOP

bl�fugl.omr�de.3.v�rt.myre.mikro.IOP <- bl�fugl.omr�de.3.v�rt.IOP * bl�fugl.omr�de.3.myre.IOP * bl�fugl.omr�de.3.mikrotop.IOP
bl�fugl.omr�de.3.v�rt.myre.mikro.IOP

bl�fugl.omr�de.4.v�rt.myre.mikro.IOP <- bl�fugl.omr�de.4.v�rt.IOP * bl�fugl.omr�de.4.myre.IOP * bl�fugl.omr�de.4.mikrotop.IOP
bl�fugl.omr�de.4.v�rt.myre.mikro.IOP

####################

bl�fugl.omr�de.1.mikro.nektar.myre.IOP <- bl�fugl.omr�de.1.mikrotop.IOP * bl�fugl.omr�de.1.nektar.IOP * bl�fugl.omr�de.1.myre.IOP
bl�fugl.omr�de.1.mikro.nektar.myre.IOP

bl�fugl.omr�de.2.mikro.nektar.myre.IOP <- bl�fugl.omr�de.2.mikrotop.IOP * bl�fugl.omr�de.2.nektar.IOP * bl�fugl.omr�de.2.myre.IOP
bl�fugl.omr�de.2.mikro.nektar.myre.IOP

bl�fugl.omr�de.3.mikro.nektar.myre.IOP <- bl�fugl.omr�de.3.mikrotop.IOP * bl�fugl.omr�de.3.nektar.IOP * bl�fugl.omr�de.3.myre.IOP
bl�fugl.omr�de.3.mikro.nektar.myre.IOP

bl�fugl.omr�de.4.mikro.nektar.myre.IOP <- bl�fugl.omr�de.4.mikrotop.IOP * bl�fugl.omr�de.4.nektar.IOP * bl�fugl.omr�de.4.myre.IOP
bl�fugl.omr�de.4.mikro.nektar.myre.IOP

###########################

bl�fugl.omr�de.1.v�rt.mikro.nektar.myre.IOP <- bl�fugl.omr�de.1.mikrotop.IOP * bl�fugl.omr�de.1.nektar.IOP * bl�fugl.omr�de.1.myre.IOP * bl�fugl.omr�de.1.v�rt.IOP
bl�fugl.omr�de.1.v�rt.mikro.nektar.myre.IOP

bl�fugl.omr�de.2.v�rt.mikro.nektar.myre.IOP <- bl�fugl.omr�de.2.mikrotop.IOP * bl�fugl.omr�de.2.nektar.IOP * bl�fugl.omr�de.2.myre.IOP * bl�fugl.omr�de.2.v�rt.IOP
bl�fugl.omr�de.2.v�rt.mikro.nektar.myre.IOP

bl�fugl.omr�de.3.v�rt.mikro.nektar.myre.IOP <- bl�fugl.omr�de.3.mikrotop.IOP * bl�fugl.omr�de.3.nektar.IOP * bl�fugl.omr�de.3.myre.IOP * bl�fugl.omr�de.3.v�rt.IOP
bl�fugl.omr�de.3.v�rt.mikro.nektar.myre.IOP

bl�fugl.omr�de.4.v�rt.mikro.nektar.myre.IOP <- bl�fugl.omr�de.4.mikrotop.IOP * bl�fugl.omr�de.4.nektar.IOP * bl�fugl.omr�de.4.myre.IOP * bl�fugl.omr�de.4.v�rt.IOP
bl�fugl.omr�de.4.v�rt.mikro.nektar.myre.IOP

## v�rdierne inds�ttes herefter i venn.digrammer i online programmet "Visual Paradigm online".


## Script done



