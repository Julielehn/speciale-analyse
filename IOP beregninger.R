## IOP beregninger på baggrund af udvalgte områder, hvor arternes ressourcebehov
## df observeret til at være mest dækket 

##################################################

setwd("C:/Users/Julie Sander Lehnert/OneDrive/Dokumenter/Skole/UNI/Speciale")

list.files()

##################################################

# datafil, som indholder distance data indlæses
data <- read.csv("Distance data - Behandlet.csv",
                 header = TRUE,
                 sep = ";")

head(data)

length(data)


# Deler data op - plot og dato i et dataframe og distancemål i en anden dataframe. 
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
# Vigtige ressourcer: Gødning, bar jord, vegetationshøjde


bar.jord <- data[,c("Plot", 
                    "GPS.X", 
                    "GPS.Y", 
                    "Bar_jord.over.30cm")]

gødning <- data[,c("Plot",
                   "GPS.X",
                   "GPS.Y",
                   "Hjorte_pellets",
                   "Kokasse",
                   "Hestepære",
                   "Fårafføring")]

# orange jordbi
# Vigtigste ressourcer: værtsplanter (djævelsbid og blåhat)
# og pletter af bar jord

blåhat.djævelsbid <- data[,c("Plot",
                             "GPS.X",
                             "GPS.Y",
                             "Blåhat_b",
                             "Djævelsbid_b")]

## Ensian blåfugl
# ressourcekrav: vært (klokkeensian - blomstrende), mieralsk myretue
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

gødning$gødning <-apply(X=gødning[,4:7], MARGIN=1, FUN=max)

blåhat.djævelsbid$pollen <-apply(X=blåhat.djævelsbid[,4:5], MARGIN=1, FUN=max)

klokke.ensian$klokke.ensian <- klokke.ensian[,4]

myre.min$myre <- myre.min[,4]

mikrotop15$mikrotop <- mikrotop15[,4]

klokkelyng$klokkelyng <- klokkelyng[,4]


## Laver kollone med (0 eller 1), som fortæller om ressourcen
## er tilstede i plottet
bar.jord$tilstede <- ifelse(bar.jord$bar.jord == 0,
                                       0,
                                       1)

gødning$tilstede <- ifelse(gødning$gødning == 0,
                                      0,
                                      1)

blåhat.djævelsbid$tilstede <- ifelse(blåhat.djævelsbid$pollen == 0,
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



## Subsetter så det kun er plots tilbage, hvor ressourcen er tilstede
bar.jord <- subset(bar.jord, tilstede == 1)

gødning <- subset(gødning, tilstede == 1)

blåhat.djævelsbid <- subset(blåhat.djævelsbid, tilstede == 1)

klokke.ensian <- subset(klokke.ensian, tilstede == 1)

myre.min <- subset(myre.min, tilstede == 1)

mikrotop15 <- subset(mikrotop15, tilstede == 1)

klokkelyng <- subset(klokkelyng, tilstede == 1)


### Finder de plots hvor der er overlap af alle ressourcer
skarnbasse.overlap <- merge(bar.jord,
                            gødning,
                            by = "Plot")

jordbi.overlap <- merge(bar.jord,
                        blåhat.djævelsbid,
                        by = "Plot")

blåfugl.overlap <- merge(klokke.ensian,
                      myre.min,
                      by = "Plot")

names(blåfugl.overlap)

blåfugl.overlap <- blåfugl.overlap[,c(1:3,
                                5,
                                10)]


blåfugl.overlap <- merge(blåfugl.overlap,
                      mikrotop15,
                      by = "Plot")
names(blåfugl.overlap)

blåfugl.overlap <- blåfugl.overlap[,c(1:5,
                                9)]

blåfugl.overlap <- merge(blåfugl.overlap,
                      klokkelyng,
                      by = "Plot")

blåfugl.overlap <- blåfugl.overlap[,c(1:6,
                                10)]

#### udskriv datasæt, så de kan importeres til QGIS
setwd("C:/Users/Julie Sander Lehnert/OneDrive/Dokumenter/Skole/UNI/Speciale/Behandlet data/Brugbar/Venn-diagrammer")

write.csv(bar.jord, "bar jord - 9.6.22.csv")

write.csv(gødning, "gødning - 9.6.22.csv")

write.csv(blåhat.djævelsbid, "blåhat.djævelsbid - 9.6.22.csv")

write.csv(klokke.ensian, "klokkeensian - 9.6.22.csv")

write.csv(mikrotop15, "mikrotop - 9.6.22.csv")

write.csv(myre.min, "myre - 9.6.22.csv")

write.csv(klokkelyng, "klokkelyng - 9.6.22.csv")

write.csv(skarnbasse.overlap, "Skarnbasse - overlap 9.6.22.csv")

write.csv(blåfugl.overlap, "Blåfugl - overlap 9.6.22.csv")

write.csv(jordbi.overlap, "Jordbi - overlap 9.6.22.csv")

#### Filer er blevet importeret i QGIS og en liste af punker for områder, hvor 
## der er højere densitet af ressourcer importeres nu tilbage til Rstudio

#########################
# Orange jordbi

list.files()

jordbi.områder <- read.csv("jordbi - område plots.csv",
                           header = T,
                           sep = ";")

skarnbasse.områder <- read.csv("skarnbasse - område plots.csv",
                               header = T,
                               sep = ";")

blåfugl.områder <- read.csv("blåfugl - ommråde plots.csv",
                            header = T,
                            sep = ";")

jordbi.område.1 <- subset(jordbi.områder, Område == 1)

jordbi.område.2 <- subset(jordbi.områder, Område == 2)

jordbi.område.3 <- subset(jordbi.områder, Område == 3)

jordbi.område.4 <- subset(jordbi.områder, Område == 4)


skarnbasse.område.1 <- subset(skarnbasse.områder, Område == 1)

skarnbasse.område.2 <- subset(skarnbasse.områder, Område == 2)

skarnbasse.område.3 <- subset(skarnbasse.områder, Område == 3)

skarnbasse.område.4 <- subset(skarnbasse.områder, Område == 4)


blåfugl.område.1 <- subset(blåfugl.områder, Område == 1)

blåfugl.område.2 <- subset(blåfugl.områder, Område == 2)

blåfugl.område.3 <- subset(blåfugl.områder, Område == 3)

blåfugl.område.4 <- subset(blåfugl.områder, Område == 4)

############

jordbi.område.1.bar.jord <- merge(jordbi.område.1,
                                  bar.jord,
                                  by = "Plot",
                                  all.x = T)
jordbi.område.1.bar.jord[is.na(jordbi.område.1.bar.jord)] <- 0

jordbi.område.2.bar.jord <- merge(jordbi.område.2,
                                  bar.jord,
                                  by = "Plot",
                                  all.x = T)
jordbi.område.2.bar.jord[is.na(jordbi.område.2.bar.jord)] <- 0


jordbi.område.3.bar.jord <- merge(jordbi.område.3,
                                  bar.jord,
                                  by = "Plot",
                                  all.x = T)
jordbi.område.3.bar.jord[is.na(jordbi.område.3.bar.jord)] <- 0

jordbi.område.4.bar.jord <- merge(jordbi.område.4,
                                  bar.jord,
                                  by = "Plot",
                                  all.x = T)
jordbi.område.4.bar.jord[is.na(jordbi.område.4.bar.jord)] <- 0

##################

jordbi.område.1.pollen <- merge(jordbi.område.1,
                                blåhat.djævelsbid,
                                by = "Plot",
                                all.x = T)
jordbi.område.1.pollen[is.na(jordbi.område.1.pollen)] <- 0

jordbi.område.2.pollen <- merge(jordbi.område.2,
                                blåhat.djævelsbid,
                                by = "Plot",
                                all.x = T)
jordbi.område.2.pollen[is.na(jordbi.område.2.pollen)] <- 0

jordbi.område.3.pollen <- merge(jordbi.område.3,
                                blåhat.djævelsbid,
                                by = "Plot",
                                all.x = T)
jordbi.område.3.pollen[is.na(jordbi.område.3.pollen)] <- 0

jordbi.område.4.pollen <- merge(jordbi.område.4,
                                blåhat.djævelsbid,
                                by = "Plot",
                                all.x = T)
jordbi.område.4.pollen[is.na(jordbi.område.4.pollen)] <- 0

#####################################

skarnbasse.område.1.bar.jord <- merge(skarnbasse.område.1,
                                      bar.jord,
                                      by = "Plot",
                                      all.x = T)
skarnbasse.område.1.bar.jord[is.na(skarnbasse.område.1.bar.jord)] <- 0

skarnbasse.område.2.bar.jord <- merge(skarnbasse.område.2,
                                      bar.jord,
                                      by = "Plot",
                                      all.x = T)
skarnbasse.område.2.bar.jord[is.na(skarnbasse.område.2.bar.jord)] <- 0

skarnbasse.område.3.bar.jord <- merge(skarnbasse.område.3,
                                      bar.jord,
                                      by = "Plot",
                                      all.x = T)
skarnbasse.område.3.bar.jord[is.na(skarnbasse.område.3.bar.jord)] <- 0

skarnbasse.område.4.bar.jord <- merge(skarnbasse.område.4,
                                      bar.jord,
                                      by = "Plot",
                                      all.x = T)
skarnbasse.område.4.bar.jord[is.na(skarnbasse.område.4.bar.jord)] <- 0

######################################

skarnbasse.område.1.gødning <- merge(skarnbasse.område.1,
                                     gødning,
                                     by = "Plot",
                                     all.x = T)
skarnbasse.område.1.gødning[is.na(skarnbasse.område.1.gødning)] <- 0

skarnbasse.område.2.gødning <- merge(skarnbasse.område.2,
                                     gødning,
                                     by = "Plot",
                                     all.x = T)
skarnbasse.område.2.gødning[is.na(skarnbasse.område.2.gødning)] <- 0

skarnbasse.område.3.gødning <- merge(skarnbasse.område.3,
                                     gødning,
                                     by = "Plot",
                                     all.x = T)
skarnbasse.område.3.gødning[is.na(skarnbasse.område.3.gødning)] <- 0

skarnbasse.område.4.gødning <- merge(skarnbasse.område.4,
                                     gødning,
                                     by = "Plot",
                                     all.x = T)
skarnbasse.område.4.gødning[is.na(skarnbasse.område.4.gødning)] <- 0

#############################

blåfugl.område.1.vært <- merge(blåfugl.område.1,
                               klokke.ensian,
                               by.x = "Plot",
                               all.x = T)
blåfugl.område.1.vært[is.na(blåfugl.område.1.vært)] <- 0


blåfugl.område.2.vært <- merge(blåfugl.område.2,
                               klokke.ensian,
                               by = "Plot",
                               all.x = T)
blåfugl.område.2.vært[is.na(blåfugl.område.2.vært)] <- 0


blåfugl.område.3.vært <- merge(blåfugl.område.3,
                               klokke.ensian,
                               by = "Plot",
                               all.x = T)
blåfugl.område.3.vært[is.na(blåfugl.område.3.vært)] <- 0

blåfugl.område.4.vært <- merge(blåfugl.område.4,
                               klokke.ensian,
                               by = "Plot",
                               all.x = T)
blåfugl.område.4.vært[is.na(blåfugl.område.4.vært)] <- 0
blåfugl.område.4.vært

####################################

blåfugl.område.1.myre <- merge(blåfugl.område.1,
                               myre.min,
                               by = "Plot",
                               all.x = T)
blåfugl.område.1.myre[is.na(blåfugl.område.1.myre)] <- 0

blåfugl.område.2.myre <- merge(blåfugl.område.2,
                               myre.min,
                               by = "Plot",
                               all.x = T)
blåfugl.område.2.myre[is.na(blåfugl.område.2.myre)] <- 0

blåfugl.område.3.myre <- merge(blåfugl.område.3,
                               myre.min,
                               by = "Plot",
                               all.x = T)
blåfugl.område.3.myre[is.na(blåfugl.område.3.myre)] <- 0

blåfugl.område.4.myre <- merge(blåfugl.område.4,
                               myre.min,
                               by = "Plot",
                               all.x = T)
blåfugl.område.4.myre[is.na(blåfugl.område.4.myre)] <- 0

################################

blåfugl.område.1.mikrotop <- merge(blåfugl.område.1,
                                   mikrotop15,
                                   by = "Plot",
                                   all.x = T)
blåfugl.område.1.mikrotop[is.na(blåfugl.område.1.mikrotop)] <- 0

blåfugl.område.2.mikrotop <- merge(blåfugl.område.2,
                                   mikrotop15,
                                   by = "Plot",
                                   all.x = T)
blåfugl.område.2.mikrotop[is.na(blåfugl.område.2.mikrotop)] <- 0

blåfugl.område.3.mikrotop <- merge(blåfugl.område.3,
                                   mikrotop15,
                                   by = "Plot",
                                   all.x = T)
blåfugl.område.3.mikrotop[is.na(blåfugl.område.3.mikrotop)] <- 0

blåfugl.område.4.mikrotop <- merge(blåfugl.område.4,
                                   mikrotop15,
                                   by = "Plot",
                                   all.x = T)
blåfugl.område.4.mikrotop[is.na(blåfugl.område.4.mikrotop)] <- 0

##############################

blåfugl.område.1.nektar <- merge(blåfugl.område.1,
                                 klokkelyng,
                                 by = "Plot",
                                 all.x = T)
blåfugl.område.1.nektar[is.na(blåfugl.område.1.nektar)] <- 0

blåfugl.område.2.nektar <- merge(blåfugl.område.2,
                                 klokkelyng,
                                 by = "Plot",
                                 all.x = T)
blåfugl.område.2.nektar[is.na(blåfugl.område.2.nektar)] <- 0

blåfugl.område.3.nektar <- merge(blåfugl.område.3,
                                 klokkelyng,
                                 by = "Plot",
                                 all.x = T)
blåfugl.område.3.nektar[is.na(blåfugl.område.3.nektar)] <- 0

blåfugl.område.4.nektar <- merge(blåfugl.område.4,
                                 klokkelyng,
                                 by = "Plot",
                                 all.x = T)
blåfugl.område.4.nektar[is.na(blåfugl.område.4.nektar)] <- 0

#################################
## IOP beregninger for Orange jordbi


jordbi.område.1.bar.jord.IOP <- mean(jordbi.område.1.bar.jord$bar.jord)
jordbi.område.1.bar.jord.IOP

jordbi.område.2.bar.jord.IOP <- mean(jordbi.område.2.bar.jord$bar.jord)
jordbi.område.2.bar.jord.IOP

jordbi.område.3.bar.jord.IOP <- mean(jordbi.område.3.bar.jord$bar.jord)
jordbi.område.3.bar.jord.IOP

jordbi.område.4.bar.jord.IOP <- mean(jordbi.område.4.bar.jord$bar.jord)
jordbi.område.4.bar.jord.IOP

###########


jordbi.område.1.pollen.IOP <- mean(jordbi.område.1.pollen$pollen)
jordbi.område.1.pollen.IOP

jordbi.område.2.pollen.IOP <- mean(jordbi.område.2.pollen$pollen)
jordbi.område.2.pollen.IOP

jordbi.område.3.pollen.IOP <- mean(jordbi.område.3.pollen$pollen)
jordbi.område.3.pollen.IOP

jordbi.område.4.pollen.IOP <- mean(jordbi.område.4.pollen$pollen)
jordbi.område.4.pollen.IOP

##################

jordbi.område.1.IOP.total <- jordbi.område.1.bar.jord.IOP*jordbi.område.1.pollen.IOP
jordbi.område.1.IOP.total

jordbi.område.3.IOP.total <- jordbi.område.3.bar.jord.IOP*jordbi.område.3.pollen.IOP
jordbi.område.3.IOP.total

jordbi.område.4.IOP.total <- jordbi.område.4.bar.jord.IOP*jordbi.område.4.pollen.IOP
jordbi.område.4.IOP.total

#################################

## IOP beregninger for trehornet skarnbasse
skarnbasse.område.1.bar.jord.IOP <- mean(skarnbasse.område.1.bar.jord$bar.jord)
skarnbasse.område.1.bar.jord.IOP

skarnbasse.område.2.bar.jord.IOP <- mean(skarnbasse.område.2.bar.jord$bar.jord)
skarnbasse.område.2.bar.jord.IOP

skarnbasse.område.3.bar.jord.IOP <- mean(skarnbasse.område.3.bar.jord$bar.jord)
skarnbasse.område.3.bar.jord.IOP

skarnbasse.område.4.bar.jord.IOP <- mean(skarnbasse.område.4.bar.jord$bar.jord)
skarnbasse.område.4.bar.jord.IOP

#############


skarnbasse.område.1.gødning.IOP <- mean(skarnbasse.område.1.gødning$gødning)
skarnbasse.område.1.gødning.IOP

skarnbasse.område.2.gødning.IOP <- mean(skarnbasse.område.2.gødning$gødning)
skarnbasse.område.2.gødning.IOP

skarnbasse.område.3.gødning.IOP <- mean(skarnbasse.område.3.gødning$gødning)
skarnbasse.område.3.gødning.IOP

skarnbasse.område.4.gødning.IOP <- mean(skarnbasse.område.4.gødning$gødning)
skarnbasse.område.4.gødning.IOP

############


skarnbasse.område.1.IOP.total <- skarnbasse.område.1.bar.jord.IOP*skarnbasse.område.1.gødning.IOP
skarnbasse.område.1.IOP.total

skarnbasse.område.2.IOP.total <- skarnbasse.område.2.bar.jord.IOP*skarnbasse.område.2.gødning.IOP
skarnbasse.område.2.IOP.total

skarnbasse.område.3.IOP.total <- skarnbasse.område.3.bar.jord.IOP*skarnbasse.område.3.gødning.IOP
skarnbasse.område.3.IOP.total

skarnbasse.område.4.IOP.total <- skarnbasse.område.4.bar.jord.IOP*skarnbasse.område.4.gødning.IOP
skarnbasse.område.4.IOP.total


#######################################################
# IOP beregninger for Ensianblåfugl

blåfugl.område.1.vært.IOP <- mean(blåfugl.område.1.vært$klokke.ensian)
blåfugl.område.1.vært.IOP

blåfugl.område.2.vært.IOP <- mean(blåfugl.område.2.vært$klokke.ensian)
blåfugl.område.2.vært.IOP

blåfugl.område.3.vært.IOP <- mean(blåfugl.område.3.vært$klokke.ensian)
blåfugl.område.3.vært.IOP

blåfugl.område.4.vært.IOP <- mean(blåfugl.område.4.vært$klokke.ensian)
blåfugl.område.4.vært.IOP

############


blåfugl.område.1.myre.IOP <- mean(blåfugl.område.1.myre$myre)
blåfugl.område.1.myre.IOP

blåfugl.område.2.myre.IOP <- mean(blåfugl.område.2.myre$myre)
blåfugl.område.2.myre.IOP

blåfugl.område.3.myre.IOP <- mean(blåfugl.område.3.myre$myre)
blåfugl.område.3.myre.IOP

blåfugl.område.4.myre.IOP <- mean(blåfugl.område.4.myre$myre)
blåfugl.område.4.myre.IOP

##############

blåfugl.område.1.mikrotop.IOP <- mean(blåfugl.område.1.mikrotop$mikrotop)
blåfugl.område.1.mikrotop.IOP

blåfugl.område.2.mikrotop.IOP <- mean(blåfugl.område.2.mikrotop$mikrotop)
blåfugl.område.2.mikrotop.IOP

blåfugl.område.3.mikrotop.IOP <- mean(blåfugl.område.3.mikrotop$mikrotop)
blåfugl.område.3.mikrotop.IOP

blåfugl.område.4.mikrotop.IOP <- mean(blåfugl.område.4.mikrotop$mikrotop)
blåfugl.område.4.mikrotop.IOP

##############


blåfugl.område.1.nektar.IOP <- mean(blåfugl.område.1.nektar$klokkelyng)
blåfugl.område.1.nektar.IOP

blåfugl.område.2.nektar.IOP <- mean(blåfugl.område.2.nektar$klokkelyng)
blåfugl.område.2.nektar.IOP

blåfugl.område.3.nektar.IOP <- mean(blåfugl.område.3.nektar$klokkelyng)
blåfugl.område.3.nektar.IOP

blåfugl.område.4.nektar.IOP <- mean(blåfugl.område.4.nektar$klokkelyng)
blåfugl.område.4.nektar.IOP

############


blåfugl.område.1.vært.myre.IOP <- blåfugl.område.1.vært.IOP * blåfugl.område.1.myre.IOP
blåfugl.område.1.vært.myre.IOP

blåfugl.område.2.vært.myre.IOP <- blåfugl.område.2.vært.IOP * blåfugl.område.2.myre.IOP
blåfugl.område.2.vært.myre.IOP

blåfugl.område.3.vært.myre.IOP <- blåfugl.område.3.vært.IOP * blåfugl.område.3.myre.IOP
blåfugl.område.3.vært.myre.IOP

blåfugl.område.4.vært.myre.IOP <- blåfugl.område.4.vært.IOP * blåfugl.område.4.myre.IOP
blåfugl.område.4.vært.myre.IOP

############

blåfugl.område.1.vært.mikrotop.IOP <- blåfugl.område.1.vært.IOP * blåfugl.område.1.mikrotop.IOP
blåfugl.område.1.vært.mikrotop.IOP

blåfugl.område.2.vært.mikrotop.IOP <- blåfugl.område.2.vært.IOP * blåfugl.område.2.mikrotop.IOP
blåfugl.område.2.vært.mikrotop.IOP

blåfugl.område.3.vært.mikrotop.IOP <- blåfugl.område.3.vært.IOP * blåfugl.område.3.mikrotop.IOP
blåfugl.område.3.vært.mikrotop.IOP

blåfugl.område.4.vært.mikrotop.IOP <- blåfugl.område.4.vært.IOP * blåfugl.område.4.mikrotop.IOP
blåfugl.område.4.vært.mikrotop.IOP

###############


blåfugl.område.1.vært.nektar.IOP <- blåfugl.område.1.vært.IOP * blåfugl.område.1.nektar.IOP
blåfugl.område.1.vært.nektar.IOP

blåfugl.område.2.vært.nektar.IOP <- blåfugl.område.2.vært.IOP * blåfugl.område.2.nektar.IOP
blåfugl.område.2.vært.nektar.IOP

blåfugl.område.3.vært.nektar.IOP <- blåfugl.område.3.vært.IOP * blåfugl.område.3.nektar.IOP
blåfugl.område.3.vært.nektar.IOP

blåfugl.område.4.vært.nektar.IOP <- blåfugl.område.4.vært.IOP * blåfugl.område.4.nektar.IOP
blåfugl.område.4.vært.nektar.IOP

#############

blåfugl.område.1.mikrotop.myre.IOP <- blåfugl.område.1.mikrotop.IOP * blåfugl.område.1.myre.IOP
blåfugl.område.1.mikrotop.myre.IOP

blåfugl.område.2.mikrotop.myre.IOP <- blåfugl.område.2.mikrotop.IOP * blåfugl.område.2.myre.IOP
blåfugl.område.2.mikrotop.myre.IOP

blåfugl.område.3.mikrotop.myre.IOP <- blåfugl.område.3.mikrotop.IOP * blåfugl.område.3.myre.IOP
blåfugl.område.3.mikrotop.myre.IOP

blåfugl.område.4.mikrotop.myre.IOP <- blåfugl.område.4.mikrotop.IOP * blåfugl.område.4.myre.IOP
blåfugl.område.4.mikrotop.myre.IOP

###############

blåfugl.område.1.mikrotop.nektar.IOP <- blåfugl.område.1.mikrotop.IOP * blåfugl.område.1.nektar.IOP
blåfugl.område.1.mikrotop.nektar.IOP

blåfugl.område.2.mikrotop.nektar.IOP <- blåfugl.område.2.mikrotop.IOP * blåfugl.område.2.nektar.IOP
blåfugl.område.2.mikrotop.nektar.IOP

blåfugl.område.3.mikrotop.nektar.IOP <- blåfugl.område.3.mikrotop.IOP * blåfugl.område.3.nektar.IOP
blåfugl.område.3.mikrotop.nektar.IOP

blåfugl.område.4.mikrotop.nektar.IOP <- blåfugl.område.4.mikrotop.IOP * blåfugl.område.4.nektar.IOP
blåfugl.område.4.mikrotop.nektar.IOP

#####################

blåfugl.område.1.myre.nektar.IOP <- blåfugl.område.1.myre.IOP * blåfugl.område.1.nektar.IOP
blåfugl.område.1.myre.nektar.IOP

blåfugl.område.2.myre.nektar.IOP <- blåfugl.område.2.myre.IOP * blåfugl.område.2.nektar.IOP
blåfugl.område.2.myre.nektar.IOP

blåfugl.område.3.myre.nektar.IOP <- blåfugl.område.3.myre.IOP * blåfugl.område.3.nektar.IOP
blåfugl.område.3.myre.nektar.IOP

blåfugl.område.4.myre.nektar.IOP <- blåfugl.område.4.myre.IOP * blåfugl.område.4.nektar.IOP
blåfugl.område.4.myre.nektar.IOP

###################

blåfugl.område.1.vært.myre.mikro.IOP <- blåfugl.område.1.vært.IOP * blåfugl.område.1.myre.IOP * blåfugl.område.1.mikrotop.IOP
blåfugl.område.1.vært.myre.mikro.IOP

blåfugl.område.2.vært.myre.mikro.IOP <- blåfugl.område.2.vært.IOP * blåfugl.område.2.myre.IOP * blåfugl.område.2.mikrotop.IOP
blåfugl.område.2.vært.myre.mikro.IOP

blåfugl.område.3.vært.myre.mikro.IOP <- blåfugl.område.3.vært.IOP * blåfugl.område.3.myre.IOP * blåfugl.område.3.mikrotop.IOP
blåfugl.område.3.vært.myre.mikro.IOP

blåfugl.område.4.vært.myre.mikro.IOP <- blåfugl.område.4.vært.IOP * blåfugl.område.4.myre.IOP * blåfugl.område.4.mikrotop.IOP
blåfugl.område.4.vært.myre.mikro.IOP

####################

blåfugl.område.1.mikro.nektar.myre.IOP <- blåfugl.område.1.mikrotop.IOP * blåfugl.område.1.nektar.IOP * blåfugl.område.1.myre.IOP
blåfugl.område.1.mikro.nektar.myre.IOP

blåfugl.område.2.mikro.nektar.myre.IOP <- blåfugl.område.2.mikrotop.IOP * blåfugl.område.2.nektar.IOP * blåfugl.område.2.myre.IOP
blåfugl.område.2.mikro.nektar.myre.IOP

blåfugl.område.3.mikro.nektar.myre.IOP <- blåfugl.område.3.mikrotop.IOP * blåfugl.område.3.nektar.IOP * blåfugl.område.3.myre.IOP
blåfugl.område.3.mikro.nektar.myre.IOP

blåfugl.område.4.mikro.nektar.myre.IOP <- blåfugl.område.4.mikrotop.IOP * blåfugl.område.4.nektar.IOP * blåfugl.område.4.myre.IOP
blåfugl.område.4.mikro.nektar.myre.IOP

###########################

blåfugl.område.1.vært.mikro.nektar.myre.IOP <- blåfugl.område.1.mikrotop.IOP * blåfugl.område.1.nektar.IOP * blåfugl.område.1.myre.IOP * blåfugl.område.1.vært.IOP
blåfugl.område.1.vært.mikro.nektar.myre.IOP

blåfugl.område.2.vært.mikro.nektar.myre.IOP <- blåfugl.område.2.mikrotop.IOP * blåfugl.område.2.nektar.IOP * blåfugl.område.2.myre.IOP * blåfugl.område.2.vært.IOP
blåfugl.område.2.vært.mikro.nektar.myre.IOP

blåfugl.område.3.vært.mikro.nektar.myre.IOP <- blåfugl.område.3.mikrotop.IOP * blåfugl.område.3.nektar.IOP * blåfugl.område.3.myre.IOP * blåfugl.område.3.vært.IOP
blåfugl.område.3.vært.mikro.nektar.myre.IOP

blåfugl.område.4.vært.mikro.nektar.myre.IOP <- blåfugl.område.4.mikrotop.IOP * blåfugl.område.4.nektar.IOP * blåfugl.område.4.myre.IOP * blåfugl.område.4.vært.IOP
blåfugl.område.4.vært.mikro.nektar.myre.IOP

## værdierne indsættes herefter i venn.digrammer i online programmet "Visual Paradigm online".


## Script done



