######### Binomal Power Analysis of Towed Video Data #######


library(stringr)
library(dplyr)
library(tidyr)

# Clear memory ----
rm(list=ls())

# Set working directory ####
#w.dir<-dirname(rstudioapi::getActiveDocumentContext()$path)
w.dir <- "~/PA_binomal"
w.dir
# Set data directory - to read the data from
#setwd(paste(w.dir, "outputs/", sep='/'))
setwd("~/PA_binomal/outputs")

dir("~/PA_binomal/data/")

## Set file name --
filen <- "Towed-Video-MUZ-seag-epower.csv"

# Load data

df <- read.csv(paste(w.dir, "data", filen, sep='/'))
str(df) # 65420 obs
names(df)
head(df)

summary(df)

library(epower)
#devtools::install_github("bmtglobal/epower",dep=TRUE, force =TRUE)
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
    Location="zone.sim",
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
    Number.of.iterations=400,
    filename="TV-MUZ-400it-binomal",
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


## Check memory ----

memfree <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE))

memfree #  5392800 kb - at the start of sesson : 64877516, after 150 its: 64013040; after 300 its: 62975212

x <- system('grep MemTotal /proc/meminfo', intern = TRUE)
x # 65965264 kB

system('free -m') # 61499 300 its -- 500 its :5323 
system('grep MemTotal /proc/meminfo') # 65965264 kB 200 its
system('lshw -class memory') # 62GiB (after 200 its)

# memory usage
library(pryr)
mem_used() # after 150 its 300 MB -- after 300 its : 573 MB-- for 500 its: 1.19 GB
