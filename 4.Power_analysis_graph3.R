### Plots for Power Analysis of Seagrass ####

### Set libraries ----

library(ggplot2)
library(ggthemes)
library(extrafont)
library(colorspace)

## To select colors #
#pal <- choose_palette()
#pal(3) 

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
p.dir <- "~/PA_Binomial/plots"

# Set data directory - to read the data from
d.dir <- paste(w.dir, "data", sep='/')

dir("~/PA_Binomial/outputs/")


########################

#####   BRUVs   #####

### Load data ----

# Set method name --
method <- "Stereo-BRUVs"
zone  <- "National Park Zone"

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
df$trials <- as.factor(df$trials)
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

### Load data ----

# Set method name --
method <- "Stereo-BRUVs"
zone  <- "National Park Zone"
zone <- "Habitat Protection Zone"

# Set file name --
filen <- "BRUV-HPZ-400it_binomial_22092020__scenario_power_summary.csv"

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
df$locations.control <- as.factor(df$locations.control)

## Plot results

## Set colours


theme_set(theme_bw())


# give the plot a title according to method and zone
titleplot <- paste(method, zone, sep=' - ')


p <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = trials, linetype = locations.control),
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
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 11, face = "bold", color = "grey25"), panel.background = element_rect(color = "black", size = 1.2),
        strip.background = element_rect(fill = "white", color = NA) , plot.title=element_text(size=14, face = "bold")) 
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
  "1" = "1 time", 
  "2" = "2 times", 
  "3" = "3 times"
)

pf <- p +
  #scale_color_manual(values = c("grey60", "lawngreen", "blue3", "red", "blue")) + # for TV
  facet_grid(.~times.after, labeller = as_labeller(timesafter)) +
  #facet_grid(.~times.after)+
  scale_color_manual(values = c("grey60", "#BCE9C5", "#18BDB0" ,"#007EB3" ,"#26185F")) + 
  scale_x_continuous (name = "Decrease in % cover of seagrasses") +
  #ylim(0, 1) +
  scale_y_continuous( name = "Power", limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1))+
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  #geom_hline(yintercept = 0.6, linetype ="dashed", color = "grey81", size = 1) +
  #labs(color = "Replicates", linetype = "Times after", title = "National Park Zone - Stereo-BRUVs survey") +
  labs(color = "Trials", linetype = "No. control locations", title = titleplot) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 11, face = "bold", color = "grey25"), panel.background = element_rect(color = "black", size = 1.2),
        strip.background = element_rect(fill = "white", color = NA) , plot.title=element_text(size=14, face = "bold")) 

#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey") 

pf

ggsave(paste(p.dir, plotn, sep='/'), plot = pf, device = "png", scale =1, dpi = 300)

# Saving 6.23 x 4.18 in image




#######################

#####   TV   ######

dir(o.dir)

### Load data ----

method <- "Towed Video"
zone  <- "Habitat Protection Zone"

# Set file name --
filen <- "TV-HPZ-400it-binomal__scenario_power_summary.csv"

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

#ggsave(paste(p.dir, "Tv-HPZ-100it__scenario_power_summary.png", sep='/'), plot = p, device = "png", scale =1, dpi = 300)


### Faceted ###

timesafter <- c(
  "1" = "1 time", 
  "2" = "2 times", 
  "3" = "3 times"
  )

p4 <- ggplot() +
  #geom_line(aes(y = sig.outcomes, x = effect.p, colour = replicates), data = df, stat="identity", cex = 1) +  # BRUV
  geom_line(aes(y = sig.outcomes, x = effect.p, colour = trials, linetype = sublocations.within.locations), data = df, stat="identity", cex = 1) + #TV
  facet_grid(.~times.after, labeller = as_labeller(timesafter)) +
  #facet_grid(.~times.after) +
  #scale_color_manual(values = c("grey", "yellow","greenyellow", "green4", "blue")) + 
  #scale_color_manual(values = c("yellow","greenyellow",  "blue3")) + # TV
  scale_color_manual(values = c("grey60",  "#18BDB0", "#26185F", "#FCFFDD")) +
  scale_x_continuous (name = "Change in % cover of seagrasses") +
  scale_y_continuous( name = "Power", breaks = c(0,0.2,0.4,0.6,0.8,1))+
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
#ggsave(paste(p.dir, "Tv-HPZ-100it__scenario_power_summary_facet.png", sep='/'), plot = p4, device = "png", scale =1, dpi = 300)
ggsave(paste(p.dir, plotn, sep='/'), plot = p4, device = "png", scale =1, dpi = 300) 

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
p.dir <- "~/PA_Binomial/plots"

# Set data directory - to read the data from
d.dir <- paste(w.dir, "data", sep='/')
d.dir
dir("~/PA_Binomial/outputs/")
# Set file name --
method <- "AUV"



# Set file name --
#filen <- "AUV-HPZ-500it_50replicates_20datapoints__scenario_power_summary.csv"
#test files
#filen <- "AUV-MUZ-50it_Test.csv"
filen <- "AUV-HPZ-400it-binomal__scenario_power_summary.csv"

zone <- "Habitat Protetion Zone"

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

df$effect.p <- df$effect*(-1)*100

theme_set(theme_bw())

titleplot <- paste(method, zone, sep=' - ')

timesafter <- c(
  "1" = "1 time", 
  "2" = "2 times", 
  "3" = "3 times"
)

#pauv <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = times.after, linetype = sublocations.within.locations),
                          # data = df, stat="identity", cex = 1, position=position_dodge(w=0.4)) 

pauv <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = trials, linetype = sublocations.within.locations),
                             data = df, stat="identity", cex = 1, position=position_dodge(w=0.4)) +  
                             # scale_color_brewer(palette="GnBu") + 
  #scale_color_manual(values = c("grey60", "lawngreen", "blue3")) + 
  #scale_color_manual(values = c("grey60",  "#4A83BA","#00366C","#F9F9F9")) +
  scale_color_manual(values = c("grey60",  "#18BDB0", "#26185F", "#FCFFDD")) +
  #scale_x_continuous (name = "Change in % cover of seagrass", limits =c(0,1), breaks = c(0.2,0.4,0.6,0.8)) +
  scale_x_continuous (name = "Change in % cover of seagrass", breaks = c(20,40,60,80)) +
  #scale_x_discrete (name = "Decrease in % cover", limits =c(0,0.2,0.4,0.6,0.8,1)) +
  scale_y_continuous( name = "Power", limits =c(0,1))+
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  facet_wrap(~times.after, labeller = as_labeller(timesafter)) +
  #geom_hline(yintercept = 0.6, linetype ="dashed", color = "grey81", size = 1) +
  labs(color = "Trials", linetype = "No. sublocations", title = titleplot) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 11, face = "bold", color = "grey25"), panel.background = element_rect(color = "black", size = 1.2),
        strip.background = element_rect(fill = "white", color = NA) , plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey") 
pauv
## save plot

#ggsave(paste(p.dir, "Auvs-NPZ-100it__scenario_power_summary.png", sep='/'), plot = pauv2, device = "png", scale =1, dpi = 300)
#ggsave(paste(p.dir, "tests", "Auv-NPZ-5it_50replicates_50datapoints.png", sep='/'), plot = pauv2, device = "png", scale =1, dpi = 300)
ggsave(paste(p.dir, plotn, sep='/'), plot = pauv, device = "png", scale =1, dpi = 300)

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
        strip.background = element_rect(fill = "white", color = "black") , plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey")
pauv4

## Save plot
#ggsave(paste(p.dir, "Auvs-NPZ-100it__scenario_power_summary_facet.png", sep='/'), plot = pauv4, device = "png", scale =1, dpi = 300)


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
