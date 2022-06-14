library(corrplot)
library(spdep)
library(readxl)
library(nlme)
library(stats)
library(MASS)
library(fastDummies)

library(plyr)
library(vegan)


### Modellering

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######## Load PP data ###########
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## loader Pin-Point data

setwd("C:/Users/Julie Sander Lehnert/OneDrive/Dokumenter/Skole/UNI/Speciale/Behandlet data/Brugbar/Pin point - Indtasning")

list.files()
file_list <- list.files() #Opretter en list navne på de filer, som jeg ønsker at loade

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

## Loader data om værtsplanter og nektar/pollenplanter
setwd("C:/Users/Julie Sander Lehnert/OneDrive/Dokumenter/Skole/UNI/Speciale/Behandlet data/Brugbar")
list.files()

vært.nektar.data <- data.frame(read_excel("Artliste - kun vegeative ressourcer.xlsx"))

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Oprydning og sammensætning af data #######
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## omrydning af PP data
#PP.data <- subset(PP.data, Fund == 1) ## Subset, hvor der kun beholder alle observationer, hvor fund = 1

PP.data$Plot[PP.data$Plot == "TRUE"] <- 1 ## ved loading er plot 1 blevet til TRUE. Dette ændres tilbage


## Oprydning af Værtsplante og nektarplante data
vært.nektar.data[is.na(vært.nektar.data)] = 0 # Alle de arter, som ikke har nogle værdier, får værdien 0

names(vært.nektar.data)
vært.nektar.data <- vært.nektar.data[,c("Danske.arter",
                                        "Værtsplante", 
                                        "Nektar" )] # subsetter til kun ønskede kolloner

names(vært.nektar.data) <- c("Dansk.navn", 
                             "Værtsplante", 
                             "Nektar") #Omdøber koloner, så de stemmer overens med andre datafiler

## Merger alt data
PP.data <- merge(vært.nektar.data,
                 PP.data,
                 by = "Dansk.navn")


length(unique(PP.data$Plot))# Alle plots er der stadig


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###### Ny variabel - Tæthed af planterarter #######
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Variablen tæthed tilføjes. Tætheden bestemmes på baggrund af en PinPointramme
#og 5 meter cirkel. Det vil sige at der er 3 forskellige positioner. 
# 1: Antal pins ramt i PinPoint. (0 til 25)
# 2: Findes i PinPoint men ikke ramt (1 eller 0)
# 3: Findes i 5 meter cirkelen (1 eller 0)
# i Datasættet vil hver observation kun kun tage en positiv værdi i en af de 3 kategorier
# For at kunne medregne observationerne for PP med ikke ramt og 5 meter cirklen skaleres 
# data, så der hypotetisk er 26 punkter i rammen, hvor det 26'ende punkt repreæsenerer
# fund i PP eller i 5 meter cirklen
# Tæthedsvariablen tilføjes med en ifelse statement
PP.data$Tæthed <- ifelse(PP.data$Antal.pins != 0, 
                         PP.data$Antal.pins + 1,
                         PP.data$Tilstede.PP + PP.data$Tilstede.5m)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
############# Community dataframe ###############
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#værtsplanter

names(PP.data)

PP.data <- PP.data[,c("Plot", "Dansk.navn", "Tæthed")]

PP.data.wide <- reshape(PP.data, 
                        idvar = "Plot", 
                        timevar = "Dansk.navn", 
                        direction = "wide")

names(PP.data.wide) <- sub('^Tæthed.', '', names(PP.data.wide))

Værtsplante.wide[is.na(Værtsplante.wide)] <- 0


########### udregning af shannon index #########
## Værtsplanter
shannon.PP.data <- ddply(PP.data.wide, ~Plot, function(x) {
  data.frame(SHANNON=diversity(x[-1], index="shannon"))})


########## indlæs andet data ###############

setwd("C:/Users/Julie Sander Lehnert/OneDrive/Dokumenter/Skole/UNI/Speciale/Behandlet data/Brugbar/Modellering")

list.files()

Struktur.data <- read.csv("Strukturdata modellering - behandlet.csv",
                          sep = ";",
                          header = T)

Solar.index <- read.csv("Solar radiation index - beregnet.csv",
                        sep = ";",
                        header = T)

Forvaltning <- read.csv("Forvaltningsvariable - wide.csv",
                        sep = ";",
                        header = T)
                      

####################################################
#data til modellerings dataframe trækkes ud og sættes sammen

data <- data.frame(Plot = shannon.PP.data$Plot, 
                         Shannon.index = shannon.PP.data$SHANNON)

data <- merge(data,
              Struktur.data, 
              by = "Plot")

data <- merge(data,
              Solar.index,
              by = "Plot")

data <- merge(data,
              Forvaltning,
              by = "Plot")
model.data <- data[2:19]

length(data$Plot)

####################################################

model.data$Invasiv.dækning <- as.factor(model.data$Invasiv.dækning)


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Tjek outliers for kontinuerlige variable
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
names(model.data)

# Create an object named PredNames that has the names of the variables you want to plot.
PredNames <- names(model.data[2:11])

PredNames
              
# initialize your loop using i as an iterator
for (i in PredNames){ # Here you need specify a object `in` range of values 
  # Create and object names EvalVar with the variables of interest
  EvalVar <- model.data[,i]
  # Sort the EvalVar object in increasing order for this you need to use the function sort()
  EvalVar <- sort(EvalVar, # define the object to be sorted
                  decreasing = F) # is the sorting in (de)increasing order
  plot(x = EvalVar, # the values of the variable of interest 
       y = 1:length(EvalVar), # a vector with the Order of the data
       xlab = i, # set the x-axis label to the name of the variable
       ylab = "Order of the data") # set the y-axis label to "Order of the data"
}

## alle plots ser pæne ud, så der er ingen outliers

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######### varianshomogenitet og normalfordeling ##############
######### af residualerne for samlet model ##################
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

FullMod <- lm(Shannon.index ~ . ,
              data = model.data)
plot(FullMod)


Lm.Resid <- residuals(FullMod)

shapiro.test(Lm.Resid)

# Det ser relativt pænt ud

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######## collinerity - Korrelation af predictorer ########
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

PredNames <- names(model.data[c(2:8, 10)])

names(model.data)

PredNames

# For ease, generate an object with only the predictors. Name this object Pre.paruelo.
model.data.col <- model.data[,PredNames] # use the PredNames object to extract only the predictors from paruelo


cor1 = cor(model.data.col, method = "spearman", use = "complete.obs")
corrplot.mixed(cor1, lower.col = "black", number.cex = .7, tl.pos = "lt")


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Variansinflation - tolance for korrelation af predictor #####
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Before you loop trough the variables, create a vector names Tol.Summ to store the tolerance values
Tol.Summ <- rep(x = NA,
                times = length(PredNames))
# give names to each place in the vector
names(Tol.Summ) <- PredNames


# Now, let's loop across predictors to estimate the tolerance.
for (i in PredNames){
  # Create new data.frame named Tol.DF was the first variable tested.
  Tol.DF <- data.frame(Pred = model.data.col[,i], # the data of the predictor variable being tested should be included here  - call it using the i iterator.
                       model.data.col[,PredNames[!PredNames%in%i]])# Add the other predictor variables here. For this, use the logical test !PredNames%in%i (each select all predictors variable EXECEPT i). 
  
  
  # Build an lm model to predict the predictor of interest as a function of all other predictors. Save this model as Tol.LM.
  Tol.LM <- lm(Pred~.,
               data = Tol.DF) 
  # To estimate the tolerance for the predictor of interest, the first step is to extract the R2 from the regression model and store it as an object name Tol.R2
  Tol.R2 <- summary(Tol.LM)$r.squared #
  
  # estimate the tolerance (1-R2) and save it in the corresponding position of the Tol.Summ you created before the loop. 
  Tol.Summ[i] <- 1 - Tol.R2
}    

# now print the vector Tol.Summ you created before the loop 
1/Tol.Summ

#Ingen af værdierne er over 10. Derfor beholder vi alle variablende


##############################################
#### Model selektion ########################
#############################################

str(model.data)

Full.model <- lm(Shannon.index ~ .,
                 data = model.data)

summary(Full.model)

model.data

# Stepwise regression model
step.model <- stepAIC(Full.model, direction = "both", 
                      trace = FALSE)

summary(step.model)

list.files()

habitat <- read.csv("Strukturdata til modellering.csv",
                    header = T,
                    sep = ";")

habitat <- habitat[c(1,3)]
model.data.hab <- merge(data,
                        habitat,
                        by = "Plot")

Full.model.random <- lme(Shannon.index ~ Vanddækket.areal
                         + Hydrologi
                         + Krondække.total
                         + Vegetationshøjde.total
                         + Træer.buske.samlet
                         + Græsser
                         + Mosser
                         + Invasiv.dækning
                         + SIR
                         + Forvaltning.Skovdrift
                         + Forvaltning.Ingen.indsats         
                         + Forvaltning.Afbrænding
                         + Forvaltning.Rydning.af.opvækst   
                         + Forvaltning.Græsning
                         + Forvaltning.Græsning.med.nyhegning
                         + Forvaltning.Urørt.skov,            
                         data = model.data.hab,
                         random = ~ 1|Habitatskode,
                         method = "ML")

summary(Full.model.random)

step.model.random <- stepAIC(Full.model.random, 
                             direction = "both",
                             trace = FALSE) 

summary(step.model.random)


## test viser at modellen uden den random effect er bedre
AIC(Full.model)
AIC(Full.model.random)

###############################

library(tidyverse)
install.packages("pixiedust", dependencies = T)
library(pixiedust)
install.packages("kableExtra")
library(kableExtra)

dust(step.model) %>%
  sprinkle(col = 2:4, round = 3) %>%
  sprinkle(col = 5, fn = quote(pvalString(value))) %>%
  sprinkle_colnames(term = "",
                    estimate = "Estimat",
                    std.error = "SE",
                    statistic = "T-statistik",
                    p.value = "P-værdi") %>%
  kable() %>%
  kable_styling()


RStudio.Version()



