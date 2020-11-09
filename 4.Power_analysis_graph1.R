### Plots for Power Analysis of Seagrass ####

### Set libraries ----

library(ggplot2)
library(ggthemes)
library(extrafont)

## Clear workspace ----
rm(list = ls())

# Set working directory ####
#w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# Set data directory - to read the data from
## Clear workspace ----
rm(list = ls())

# Set working directory ####
#w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
w.dir <- "~/PA_Binomial"
w.dir
# Set data directory - to read the data from
#setwd(paste(w.dir, "outputs/", sep='/'))
setwd("~/PA_Binomial/outputs")
o.dir <- "~/PA_Binomial/outputs"

# Set data directory - to read the data from
d.dir <- paste(w.dir, "data", sep='/')

dir("~/PA_Binomial/outputs/")


########################

#####   BRUVs   #####

### Load data ----

# Set method name --
method <- "Stereo-BRUVs"
zone  <- "Special Purpose Zone"

# Set file name --
filen <- "BRUV-NPZ-400it_binomial_22092020__scenario_power_summary.csv"

# set plot name --
plotn <- gsub("csv$", "png", filen)



# Load data
df <- read.csv(paste(o.dir, filen, sep='/'))
str(df)
names(df)

# Make the effects column positive
#df$effect.p <- ifelse(df$effect=1, 
df$effect.p <- df$effect*(-1)
head(df)

#df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)
df$replicates <- as.factor(df$replicates)
df$times.after <- as.factor(df$times.after)
df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)

## Plot results

## Set colours

theme_set(theme_bw())


# give the plot a title according to method and zone
titleplot <- paste(method, zone, sep=' - ')


p <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = replicates, linetype = times.after),
                          data = df, stat="identity", cex = 1) # BRUVs

# for a auto color scale --
#p  + scale_color_brewer(palette="GnBu") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


p <- p + 
  #scale_color_manual(values = c("grey60", "lawngreen", "blue3", "red", "blue")) + # for TV
  scale_color_manual(values = c("grey60", "yellow","lawngreen", "blue3", "red")) + 
  scale_x_continuous (name = "Decrease in % cover of seagrasses") +
  scale_y_continuous( name = "Power")+
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  #geom_hline(yintercept = 0.6, linetype ="dashed", color = "grey81", size = 1) +
  #labs(color = "Replicates", linetype = "Times after", title = "National Park Zone - Stereo-BRUVs survey") +
  labs(color = "Replicates", linetype = "Times after", title = titleplot) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"), plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey") 
p
## save plot


#ggsave(paste(p.dir, "Bruv-HPZ-500it_power.png", sep='/'), plot = p, device = "png", scale =1, dpi = 300)
ggsave(paste(p.dir, plotn, sep='/'), plot = p, device = "png", scale =1, dpi = 300)

### Faceted ###

timesafter <- c(
  "1" = "1 time after", 
  "2" = "2 times after", 
  "3" = "3 times after"
)

p4 <- ggplot() +
  #geom_line(aes(y = sig.outcomes, x = effect.p, colour = replicates), data = df, stat="identity", cex = 1) +  # BRUV
  geom_line(aes(y = sig.outcomes, x = effect.p, colour = sublocations.within.locations), data = df, stat="identity", cex = 1) + #TV
  facet_grid(times.after~., labeller = as_labeller(timesafter)) +
  #scale_color_manual(values = c("grey", "yellow","greenyellow", "green4", "blue")) + 
  scale_color_manual(values = c("yellow","greenyellow",  "blue3")) + # TV
  scale_x_continuous (name = "Decrease in % cover of seagrasses") +
  scale_y_continuous( name = "Power", breaks = c(0.4,0.6,0.8,1))+
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  #labs(color = "Replicates", linetype = "Control locations", title = "National Park Zone - Stereo-BRUVs survey") +
  labs(color = "sublocations", linetype = "Control locations", title = "Habitat Protection Zone - Towed video survey") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 11, angle = 360, face = "italic"), panel.background = element_rect(color = "black"),
        strip.background = element_rect(fill = "white", color = NA) , plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey")
p4

## Save plot
ggsave(paste(p.dir, "Tv-HPZ-100it__scenario_power_summary_facet.png", sep='/'), plot = p4, device = "png", scale =1, dpi = 300)



## BRUVS 2 ####

# Make the effects column positive
#df$effect.p <- ifelse(df$effect=1, 
df$effect.p <- df$effect*(-1)
head(df)

#df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)
df$replicates <- as.factor(df$replicates)
df$trials <- as.factor(df$trials)
df$times.after <- as.factor(df$times.after)
df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)
df$locations.control <- as.factor(df$locations.control)
str(df)

## Plot results

## Set colours

theme_set(theme_bw())


# give the plot a title according to method and zone
titleplot <- paste(zone, method, sep=' - ')


p <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = trials, linetype = times.after),
                          data = df, stat="identity", cex = 1) # BRUVs

# for a auto color scale --
#p  + scale_color_brewer(palette="GnBu") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


p1 <- p + 
  #scale_color_manual(values = c("grey60", "lawngreen", "blue3", "red", "blue")) + # for TV
  scale_color_manual(values = c("grey60", "yellow","lawngreen", "blue3", "red")) + 
  scale_x_continuous (name = "Decrease in % cover of seagrasses") +
  scale_y_continuous( name = "Power")+
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  #geom_hline(yintercept = 0.6, linetype ="dashed", color = "grey81", size = 1) +
  #labs(color = "Replicates", linetype = "Times after", title = "National Park Zone - Stereo-BRUVs survey") +
  labs(color = "Trials", linetype = "Times after", title = titleplot) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"), plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey") 
p1
## save plot


#ggsave(paste(p.dir, "Bruv-HPZ-500it_power.png", sep='/'), plot = p, device = "png", scale =1, dpi = 300)
#ggsave(paste(p.dir, plotn, sep='/'), plot = p, device = "png", scale =1, dpi = 300)



### Faceted ###

p <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = trials, linetype = locations.control),
                          data = df, stat="identity", cex = 1) # BRUVs


# plot with scenario of control locations


timesafter <- c(
  "1" = "1 time after", 
  "2" = "2 times after", 
  "3" = "3 times after"
)

pf <- p +
  #scale_color_manual(values = c("grey60", "lawngreen", "blue3", "red", "blue")) + # for TV
  facet_grid(.~times.after, labeller = as_labeller(timesafter)) +
  scale_color_manual(values = c("grey60", "yellow","lawngreen", "blue3", "red")) + 
  scale_x_continuous (name = "Decrease in % cover of seagrasses") +
  #ylim(0, 1) +
  scale_y_continuous( name = "Power", limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1))+
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  #geom_hline(yintercept = 0.6, linetype ="dashed", color = "grey81", size = 1) +
  #labs(color = "Replicates", linetype = "Times after", title = "National Park Zone - Stereo-BRUVs survey") +
  labs(color = "Trials", title = titleplot) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"), plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey") 

pf


#######################

#####   TV   ######

### Load data ----

method <- "Towed Video"
zone  <- "Multiple Use Zone"

# Set file name --
filen <- "TV-MUZ-100it_50replicates_20datapoints__scenario_power_summary.csv"

# set plot name --
plotn <- gsub("csv$", "png", filen)



# Load data
df <- read.csv(paste(d.dir, filen, sep='/'))
str(df)
names(df)

# Make the effects column positive
#df$effect.p <- ifelse(df$effect=1, 
df$effect.p <- df$effect*(-1)
head(df)

#df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)
df$replicates <- as.factor(df$replicates)
df$times.after <- as.factor(df$times.after)
df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)

## Plot results

theme_set(theme_bw())

titleplot <- paste(method, zone, sep=' - ')
                                  

p <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = times.after, linetype = sublocations.within.locations),
                           data = df, stat="identity", cex = 1) # TV
 
  
p  + scale_color_brewer(palette="GnBu") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
                                                  

p <- p + 
  scale_color_manual(values = c("grey60", "lawngreen", "blue3", "red", "blue")) + # for TV
  #scale_color_manual(values = c("grey60", "yellow","lawngreen", "green4", "blue3")) + 
  scale_x_continuous (name = "Decrease in % cover of seagrasses") +
  scale_y_continuous( name = "Power")+
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  #geom_hline(yintercept = 0.6, linetype ="dashed", color = "grey81", size = 1) +
  #labs(color = "Replicates", linetype = "Times after", title = "National Park Zone - Stereo-BRUVs survey") +
  labs(color = "Times after", linetype = "sublocations", title = titleplot) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"), plot.title=element_text(size=14, face = "bold")) 
  #labs(color = "Times after", linetype = "Control locations", title = "BRUV survey") 
p
## save plot

ggsave(paste(p.dir, "Tv-HPZ-100it__scenario_power_summary.png", sep='/'), plot = p, device = "png", scale =1, dpi = 300)


### Faceted ###

timesafter <- c(
  "1" = "1 time after", 
  "2" = "2 times after", 
  "3" = "3 times after"
  )

p4 <- ggplot() +
  #geom_line(aes(y = sig.outcomes, x = effect.p, colour = replicates), data = df, stat="identity", cex = 1) +  # BRUV
  geom_line(aes(y = sig.outcomes, x = effect.p, colour = sublocations.within.locations), data = df, stat="identity", cex = 1) + #TV
  facet_grid(times.after~., labeller = as_labeller(timesafter)) +
  #scale_color_manual(values = c("grey", "yellow","greenyellow", "green4", "blue")) + 
  scale_color_manual(values = c("yellow","greenyellow",  "blue3")) + # TV
  scale_x_continuous (name = "Decrease in % cover of seagrasses") +
  scale_y_continuous( name = "Power", breaks = c(0.4,0.6,0.8,1))+
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  #labs(color = "Replicates", linetype = "Control locations", title = "National Park Zone - Stereo-BRUVs survey") +
  labs(color = "sublocations", linetype = "Control locations", title = "Habitat Protection Zone - Towed video survey") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 11, angle = 360, face = "italic"), panel.background = element_rect(color = "black"),
        strip.background = element_rect(fill = "white", color = NA) , plot.title=element_text(size=14, face = "bold")) 
  #labs(color = "Times after", linetype = "Control locations", title = "BRUV survey")
p4

## Save plot
ggsave(paste(p.dir, "Tv-HPZ-100it__scenario_power_summary_facet.png", sep='/'), plot = p4, device = "png", scale =1, dpi = 300)


#####################

#####   AUV   #####

library(ggplot2)
library(ggthemes)
library(extrafont)

## Clear workspace ----
rm(list = ls())

# Set working directory ####
#w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# Set data directory - to read the data from
## Clear workspace ----
rm(list = ls())

# Set working directory ####
#w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
w.dir <- "~/PA_Binomial"
w.dir
# Set data directory - to read the data from
#setwd(paste(w.dir, "outputs/", sep='/'))
setwd("~/PA_Binomial/outputs")
o.dir <- "~/PA_Binomial/outputs"

# Set data directory - to read the data from
d.dir <- paste(w.dir, "data", sep='/')
d.dir
dir("~/PA_Binomial/outputs/")
# Set file name --
method <- "AUV"

zone <- "National Park Zone"

# Set file name --
#filen <- "AUV-HPZ-500it_50replicates_20datapoints__scenario_power_summary.csv"
#test files
#filen <- "AUV-MUZ-50it_Test.csv"
filen <- "AUV-NPZ-400it-binomal__scenario_power_summary.csv"

# for tests
#filen <- "Test_Auv-HPZ.csv"

# set plot name --
plotn <- gsub("csv$", "png", filen)

# Load data
df <- read.csv(paste(o.dir, filen, sep='/'))
str(df)
names(df)


df$trials <- as.factor(df$trials)
df$times.after <- as.factor(df$times.after)
df$sublocations.within.locations <- as.factor(df$ sublocations.within.locations)

df$effect.p <- df$effect*(-1)

theme_set(theme_bw())

titleplot <- paste(method, zone, sep=' - ')

pauv <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = times.after, linetype = sublocations.within.locations),
                           data = df, stat="identity", cex = 1, position=position_dodge(w=0.4)) 

pauv <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = trials, linetype = sublocations.within.locations),
                             data = df, stat="identity", cex = 0.8, position=position_dodge(w=0.2)) +  
                             # scale_color_brewer(palette="GnBu") + 
  scale_color_manual(values = c("grey60", "lawngreen", "blue3")) + 
  scale_x_continuous (name = "Decrease in % cover", limits =c(0,1), breaks = c(0.2,0.4,0.6,0.8)) +
  #scale_x_discrete (name = "Decrease in % cover", limits =c(0,0.2,0.4,0.6,0.8,1)) +
  scale_y_continuous( name = "Power", limits =c(0,1))+
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  facet_wrap(~times.after) +
  #geom_hline(yintercept = 0.6, linetype ="dashed", color = "grey81", size = 1) +
  labs(color = "Trials", linetype = "Sublocations", title = titleplot) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"), plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey") 
pauv
## save plot

#ggsave(paste(p.dir, "Auvs-NPZ-100it__scenario_power_summary.png", sep='/'), plot = pauv2, device = "png", scale =1, dpi = 300)
#ggsave(paste(p.dir, "tests", "Auv-NPZ-5it_50replicates_50datapoints.png", sep='/'), plot = pauv2, device = "png", scale =1, dpi = 300)
ggsave(paste(p.dir, plotn, sep='/'), plot = pauv2, device = "png", scale =1, dpi = 300)

### Faceted ###

controlnames <- c(
  "2" = "2 controls", 
  "3" = "3 controls", 
  "5" = "5 controls",
  "10"= "10 controls"
)

timesafter <- c(
  "1" = "1 time after", 
  "2" = "2 times after", 
  "3" = "3 times after"
)

pauv4 <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = sublocations.within.locations),
                           data = df, stat="identity", cex = 1) +
  facet_grid(times.after~., labeller = as_labeller(timesafter)) +
  scale_color_manual(values = c("grey60", "lawngreen", "blue3")) + 
  scale_x_continuous (name = "Decrease in % cover") +
  scale_y_continuous( name = "Power", breaks = c(0,0.2,0.4,0.6,0.8,1))+
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  labs(color = "No. sublocations", linetype = "Control locations", title = "National Park zone - AUV survey")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 11, angle = 360, face = "italic"), panel.background = element_rect(color = "black"),
        strip.background = element_rect(fill = "white", color = NA) , plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey")
pauv4

## Save plot
ggsave(paste(p.dir, "Auvs-NPZ-100it__scenario_power_summary_facet.png", sep='/'), plot = pauv4, device = "png", scale =1, dpi = 300)


### Towed Video ####

setwd("C:/Users/00093391/Dropbox/UWA/Research Associate/PowAn/Seagrass_TV")

## Set main directory ----

w.directory <- dirname("C:/Users/00093391/Dropbox/UWA/Research Associate/PowAn/Seagrass_TV/ ")


# Load data
pa <- read.csv(paste(w.directory, "seagrass_tv4__scenario_power_summary.csv", sep='/'))
str(pa)
names(pa)

pa$locations.control <- as.factor(pa$locations.control)
pa$times.after <- as.factor(pa$times.after)

theme_set(theme_bw())


pauv <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = times.after, linetype = locations.control),
                             data = pa, stat="identity", cex = 1) 


pauv  + scale_color_brewer(palette="GnBu") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


pauv2 <- pauv + scale_color_manual(values = c("grey", "yellow","greenyellow", "green4")) + 
  scale_x_continuous (name = "Decrease in % cover") +
  scale_y_continuous( name = "Power")+
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  #geom_hline(yintercept = 0.6, linetype ="dashed", color = "grey81", size = 1) +
  labs(color = "Times after", linetype = "Control locations", title = "Towed video survey") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"), plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey") 
pauv2
## save plot

ggsave(paste(w.directory, "Plots", "Tvs_v4.png", sep='/'), plot = pauv2, device = "png", scale =1, dpi = 300)


### Faceted ###

controlnames <- c(
  "2" = "2 controls", 
  "3" = "3 controls", 
  "5" = "5 controls",
  "10"= "10 controls"
)

pauv4 <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = times.after),
                              data = pa, stat="identity", cex = 1) +
  facet_grid(locations.control~., labeller = as_labeller(controlnames)) +
  scale_color_manual(values = c("grey", "yellow","greenyellow", "green4")) + 
  scale_x_continuous (name = "Decrease in % cover") +
  scale_y_continuous( name = "Power")+
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  labs(color = "Times after", linetype = "Control locations", title = "Towed video survey")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 11, angle = 360, face = "italic"), panel.background = element_rect(color = "black"),
        strip.background = element_rect(fill = "white", color = NA) , plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey")
pauv4

## Save plot
ggsave(paste(w.directory, "Plots", "TVs_facet_v5.png", sep='/'), plot = pauv4, device = "png", scale =1, dpi = 300)


##########



#####   DTV   ######

### Load data ----

method <- "Downward Towed Video"
#zone  <- "National Park Zone"
zone <- "Special Purpose Zone"

o.dir <- paste(w.dir, "outputs", sep='/')
p.dir <- paste(w.dir, "plots", sep='/')

dir(o.dir)

# Set file name --
filen <- "DTV-SPZ-400it-binomal-22102020__scenario_power_summary.csv"

# set plot name --
plotn <- gsub("csv$", "png", filen)



# Load data
df <- read.csv(paste(o.dir, filen, sep='/'))
str(df)
names(df)

# Make the effects column positive
#df$effect.p <- ifelse(df$effect=1, 
df$effect.p <- df$effect*(-1)*100
head(df)

#df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)
df$trials <- as.factor(df$trials)
df$times.after <- as.factor(df$times.after)
df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)

## Plot results

theme_set(theme_bw())

titleplot <- paste(method, zone, sep=' - ')



### Faceted ###

timesafter <- c(
  "1" = "1 time", 
  "2" = "2 times", 
  "3" = "3 times"
)

p4 <- ggplot() +
  #geom_line(aes(y = sig.outcomes, x = effect.p, colour = replicates), data = df, stat="identity", cex = 1) +  # BRUV
  geom_line(aes(y = sig.outcomes, x = effect.p, color = trials, 
                linetype = sublocations.within.locations), data = df, stat="identity", 
                cex = 1, position=position_dodge(w=0.4)) + #TV
  facet_grid(.~times.after, labeller = as_labeller(timesafter)) +
  #scale_color_manual(values = c("grey", "yellow","greenyellow", "green4", "blue")) + 
  #scale_color_manual(values = c("yellow","greenyellow",  "blue3")) +
  scale_color_manual(values = c("grey60",  "#18BDB0", "#26185F", "#FCFFDD")) +
  scale_x_continuous (name = "Change in % cover of seagrasses") +
  scale_y_continuous( name = "Power", limits =c(0,1), breaks = c(0.2,0.4,0.6,0.8,1))+
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  #labs(color = "Replicates", linetype = "Control locations", title = "National Park Zone - Stereo-BRUVs survey") +
  labs(color = "Trials", linetype = "No. sublocations", title = titleplot) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 11, face = "bold", color = "grey25"), panel.background = element_rect(color = "black", size = 1.2),
        strip.background = element_rect(fill = "white", color = NA) , plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey")
p4

## Save plot
ggsave(paste(p.dir, plotn, sep='/'), plot = p4, device = "png", width = 6.23, height = 4.18, dpi = 300)
#ggsave(paste(p.dir, plotn, sep='/'), plot = p4, device = "png", scale =1, dpi = 300)
