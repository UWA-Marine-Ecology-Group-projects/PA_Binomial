######### Binomal Power Analysis of AUV Data #######


library(stringr)
library(dplyr)
library(tidyr)

# Clear memory ----
rm(list=ls())

# Set working directory ####
#w.dir<-dirname(rstudioapi::getActiveDocumentContext()$path)
w.dir <- "~/PA_Binomial"
w.dir
# Set data directory - to read the data from
#setwd(paste(w.dir, "outputs/", sep='/'))
setwd("~/PA_Binomial/outputs")

dir("~/PA_Binomial/data/")

## Set file name --
filen <- "AUV-NPZ-seag-epower.csv"

# Load data

df <- read.csv(paste(w.dir, "data", filen, sep='/'))
str(df) # 65420 obs
names(df)
head(df)

summary(df)

library(epower)
#packageVersion("epower")
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA, verbose=TRUE)

### Set design
names(df)
str(df)
#df$Period <- as.factor(df$Period)
#df$CvI <- as.factor(df$CvI)
#df$Time <- as.factor(df$Time)
summary(df)
#dat<-read.csv(paste(working.dir, "Data/raw", "BRUV-sg-test1.csv", sep='/'))

dat <- as.data.frame(df)
summary(dat)

str(dat)


# Set design

dataComponents<-supplyData(
  dat=dat,
  variableType="binomial",
  design.matrix=list(
    Response="Seagrass",
    Trials="no.scored",
    Location="ZoneName",
    sublocation="Transect",
    Time="Time",
    "subtime"=NA,
    BvA="Period",
    CvI="CvI"),
  levels.dat=list(
    Before="Before",
    Control="Control",
    After="After",
    Impact="Impact"),
  scenario.data=list(
    Number.of.iterations=100,
    filename="AUV-NPZ-100it-binomal",
    Number.of.Impact.Locations=1,
    Number.of.Control.Locations=2,
    Number.of.sublocations.within.Location="1;2;3",
    Number.of.sample.times.Before=3,  # this number needs to be higher than 2 (replicates)
    Number.of.sample.times.After="1;2;3",
    #Number.of.sample.times.After=1,
    #Number.of.sample.times.After=2,
    #Number.of.sample.times.After=3,
    Number.of.subtimes.within.Time=NA,
    Number.of.trials="500; 1500; 2500",
    Number.of.replicate.measurements=1),
  effect.info=list(
    Multiplicative=1,
    Fixed.change=0,
    Effect.values="-0.2;-0.4;-0.6;-0.8"),
  ncores = 30)

### The supply data function is not reading the ncores
dataComponents$ncores <- 30

require(INLA,quietly=TRUE)
scenarioParams<<-do.call(powerScenario,list(inputData=dataComponents))
assessPower()
