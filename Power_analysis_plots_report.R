### Plots for Power Analysis of Seagrass ####

### Set libraries ----

library(ggplot2)
library(ggthemes)
library(extrafont)
library(broman)

## Clear workspace ----
rm(list = ls())

# Set working directory ####
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
w.dir 

# Set data directory - to read the data from
#setwd(paste(w.dir, "outputs/", sep='/'))
#setwd("~/PA_Binomial/outputs")
o.dir <- paste(w.dir, "outputs", sep='/')
d.dir <- paste(w.dir, "data", sep='/')

## see all the files in this directory ----
dir(o.dir)


########################

#####   BRUVs   #####

### Load NPZ data ----

# Set method name --
method <- "Stereo-BRUVs"
zone  <- "National Park Zone"

# Set file name --
filen <- "BRUV-NPZ-400it_binomal__scenario_power_summary.csv"

# set plot name --
plotn <- gsub("csv$", "png", filen)



# Load data
df <- read.csv(paste(o.dir, filen, sep='/'))
str(df)
names(df)

# Make the effects column positive
#df$effect.p <- ifelse(df$effect=1, 
df$effect.p <- (df$effect*(-1))*100
head(df)

#df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)
df$trials <- as.factor(df$trials)
df$times.after <- as.factor(df$times.after)
df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)


# --- # --- #

# Set PLOT COLORS ----

# from broman package --
plot_crayons() # to see color names
col1 <- crayons("mountain m")
crayons(c("black", "dandelion", "navy b"))
col2 <- c("#000000", "#fddb6d",       "#1974d2")

## Plot results

## Set colours

theme_set(theme_bw())


# give the plot a title according to method and zone
titleplot <- paste(method, zone, sep=' - ')

# New facet label names for times after variable
times.labs <- c("1" = "1 Time after", "2" = "2 Times after", "3" = "3 Times after")
names(times.labs) <- c("1", "2", "3")


p1 <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = trials, linetype = sublocations.within.locations),
                           data = df, stat="identity", cex = 1.2) + # BRUVs
  #scale_color_manual(values = c("grey60", "lawngreen", "blue3", "red", "blue")) + # for TV
  scale_color_manual(values = c("grey70", "deepskyblue","blue3")) + 
  #scale_color_manual(values=col2) + # to use with brocolors
  scale_x_continuous (name = "Decrease in % cover of seagrasses") +
  #scale_y_discrete(name = "Power", breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous( name = "Power",  breaks = c(0,0.2,0.4,0.6,0.8,1))+
  #geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "black", size = 0.8) +
  facet_wrap(~times.after, labeller = as_labeller(times.labs))  +
  #facet_grid(times.after~., labeller = as_labeller(times.labs)) +
  #geom_hline(yintercept = 0.6, linetype ="dashed", color = "grey81", size = 1) +
  #labs(color = "Replicates", linetype = "Times after", title = "National Park Zone - Stereo-BRUVs survey") +
  labs(color = "Trials", linetype = "No. sublocations", title = titleplot) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"), plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey") 
p1

## save plot
#ggsave(paste(p.dir, "Bruv-HPZ-500it_power.png", sep='/'), plot = p, device = "png", scale =1, dpi = 300)
#ggsave(paste(p.dir, plotn, sep='/'), plot = p, device = "png", scale =1, dpi = 300)



### Load HPZ data ----

# Set method name --
method <- "Stereo-BRUVs"
zone  <- "Habitat Protection Zone"

# Set file name --
filen <- "BRUV-HPZ-400it_binomal__scenario_power_summary.csv"

# set plot name --
plotn <- gsub("csv$", "png", filen)



# Load data
df <- read.csv(paste(o.dir, filen, sep='/'))
str(df)
names(df)

# Make the effects column positive
#df$effect.p <- ifelse(df$effect=1, 
df$effect.p <- (df$effect*(-1))*100
head(df)

#df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)
df$trials <- as.factor(df$trials)
df$times.after <- as.factor(df$times.after)
df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)


# --- # --- #

# Set PLOT COLORS ----

# from broman package --
plot_crayons() # to see color names
col1 <- crayons("mountain m")
crayons(c("black", "dandelion", "navy b"))
col2 <- c("#000000", "#fddb6d",       "#1974d2")

## Plot results

## Set colours

theme_set(theme_bw())


# give the plot a title according to method and zone
titleplot <- paste(method, zone, sep=' - ')

# New facet label names for times after variable
times.labs <- c("1" = "1 Time after", "2" = "2 Times after", "3" = "3 Times after")
#names(times.labs) <- c("1", "2", "3")


p1 <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = trials, linetype = sublocations.within.locations),
                           data = df, stat="identity", cex = 1.2) + # BRUVs
  #scale_color_manual(values = c("grey60", "lawngreen", "blue3", "red", "blue")) + # for TV
  scale_color_manual(values = c("grey70", "deepskyblue","blue3")) + 
  #scale_color_manual(values=col2) + # to use with brocolors
  scale_x_continuous (name = "Decrease in % cover of seagrasses") +
  #scale_y_discrete(name = "Power", breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous( name = "Power",  breaks = c(0,0.2,0.4,0.6,0.8,1))+
  #geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "black", size = 0.8) +
  facet_wrap(~times.after, labeller = as_labeller(times.labs))  +
  #facet_grid(times.after~., labeller = as_labeller(times.labs)) +
  #geom_hline(yintercept = 0.6, linetype ="dashed", color = "grey81", size = 1) +
  #labs(color = "Replicates", linetype = "Times after", title = "National Park Zone - Stereo-BRUVs survey") +
  labs(color = "Trials", linetype = "No. sublocations", title = titleplot) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"), plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey") 
p1

## save plot
#ggsave(paste(p.dir, "Bruv-HPZ-500it_power.png", sep='/'), plot = p, device = "png", scale =1, dpi = 300)
#ggsave(paste(p.dir, plotn, sep='/'), plot = p, device = "png", scale =1, dpi = 300)




### Load MUZ data ----
dir(o.dir)
# Set method name --
method <- "Stereo-BRUVs"
zone  <- "Multiple Use Zone"

# Set file name --
filen <- "BRUV-MUZ-400it_binomal__scenario_power_summary.csv"

# set plot name --
plotn <- gsub("csv$", "png", filen)



# Load data
df <- read.csv(paste(o.dir, filen, sep='/'))
str(df)
names(df)

# Make the effects column positive
#df$effect.p <- ifelse(df$effect=1, 
df$effect.p <- (df$effect*(-1))*100
head(df)

#df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)
df$trials <- as.factor(df$trials)
df$times.after <- as.factor(df$times.after)
df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)


# --- # --- #

# Set PLOT COLORS ----

# from broman package --
plot_crayons() # to see color names
col1 <- crayons("mountain m")
crayons(c("black", "dandelion", "navy b"))
col2 <- c("#000000", "#fddb6d",       "#1974d2")

## Plot results

## Set colours

theme_set(theme_bw())


# give the plot a title according to method and zone
titleplot <- paste(method, zone, sep=' - ')

# New facet label names for times after variable
times.labs <- c("1" = "1 Time after", "2" = "2 Times after", "3" = "3 Times after")
#names(times.labs) <- c("1", "2", "3")


p1 <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = trials, linetype = sublocations.within.locations),
                           data = df, stat="identity", cex = 1.2) + # BRUVs
  #scale_color_manual(values = c("grey60", "lawngreen", "blue3", "red", "blue")) + # for TV
  scale_color_manual(values = c("grey70", "deepskyblue","blue3")) + 
  #scale_color_manual(values=col2) + # to use with brocolors
  scale_x_continuous (name = "Decrease in % cover of seagrasses") +
  #scale_y_discrete(name = "Power", breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous( name = "Power",  breaks = c(0,0.2,0.4,0.6,0.8,1))+
  #geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "black", size = 0.8) +
  facet_wrap(~times.after, labeller = as_labeller(times.labs))  +
  #facet_grid(times.after~., labeller = as_labeller(times.labs)) +
  #geom_hline(yintercept = 0.6, linetype ="dashed", color = "grey81", size = 1) +
  #labs(color = "Replicates", linetype = "Times after", title = "National Park Zone - Stereo-BRUVs survey") +
  labs(color = "Trials", linetype = "No. sublocations", title = titleplot) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"), plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey") 
p1

## save plot
#ggsave(paste(p.dir, "Bruv-HPZ-500it_power.png", sep='/'), plot = p, device = "png", scale =1, dpi = 300)
#ggsave(paste(p.dir, plotn, sep='/'), plot = p, device = "png", scale =1, dpi = 300)



### Load SPZ data ----
dir(o.dir)
# Set method name --
method <- "Stereo-BRUVs"
zone  <- "Special Purpose Zone"

# Set file name --
filen <- "BRUV-SPZclustered-400it_binomal__scenario_power_summary.csv"

# set plot name --
plotn <- gsub("csv$", "png", filen)



# Load data
df <- read.csv(paste(o.dir, filen, sep='/'))
str(df)
names(df)

# Make the effects column positive
#df$effect.p <- ifelse(df$effect=1, 
df$effect.p <- (df$effect*(-1))*100
head(df)

#df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)
df$trials <- as.factor(df$trials)
df$times.after <- as.factor(df$times.after)
df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)


# --- # --- #

# Set PLOT COLORS ----

# from broman package --
plot_crayons() # to see color names
col1 <- crayons("mountain m")
crayons(c("black", "dandelion", "navy b"))
col2 <- c("#000000", "#fddb6d",       "#1974d2")

## Plot results

## Set colours

theme_set(theme_bw())


# give the plot a title according to method and zone
titleplot <- paste(method, zone, sep=' - ')

# New facet label names for times after variable
times.labs <- c("1" = "1 Time after", "2" = "2 Times after", "3" = "3 Times after")
#names(times.labs) <- c("1", "2", "3")


p1 <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = trials, linetype = sublocations.within.locations),
                           data = df, stat="identity", cex = 1.2) + # BRUVs
  #scale_color_manual(values = c("grey60", "lawngreen", "blue3", "red", "blue")) + # for TV
  scale_color_manual(values = c("grey70", "deepskyblue","blue3")) + 
  #scale_color_manual(values=col2) + # to use with brocolors
  scale_x_continuous (name = "Decrease in % cover of seagrasses") +
  #scale_y_discrete(name = "Power", breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous( name = "Power",  breaks = c(0,0.2,0.4,0.6,0.8,1))+
  #geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "black", size = 0.8) +
  facet_wrap(~times.after, labeller = as_labeller(times.labs))  +
  #facet_grid(times.after~., labeller = as_labeller(times.labs)) +
  #geom_hline(yintercept = 0.6, linetype ="dashed", color = "grey81", size = 1) +
  #labs(color = "Replicates", linetype = "Times after", title = "National Park Zone - Stereo-BRUVs survey") +
  labs(color = "Trials", linetype = "No. sublocations", title = titleplot) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"), plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey") 
p1

## save plot
#ggsave(paste(p.dir, "Bruv-HPZ-500it_power.png", sep='/'), plot = p, device = "png", scale =1, dpi = 300)
#ggsave(paste(p.dir, plotn, sep='/'), plot = p, device = "png", scale =1, dpi = 300)









#######################

#####   TV   ######

### Load HPZ data ----

dir(o.dir)

method <- "Towed Video"
zone  <- "Habitat Protection Zone"

# Set file name --
filen <- "TV-HPZ-400it-binomal__scenario_power_summary.csv"

# set plot name --
plotn <- gsub("csv$", "png", filen)




df <- read.csv(paste(o.dir, filen, sep='/'))
str(df)
names(df)

# Make the effects column positive
#df$effect.p <- ifelse(df$effect=1, 
df$effect.p <- (df$effect*(-1))*100
head(df)

#df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)
df$trials <- as.factor(df$trials)
df$times.after <- as.factor(df$times.after)
df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)

## Plot results

theme_set(theme_bw())

# give the plot a title according to method and zone
titleplot <- paste(method, zone, sep=' - ')

# New facet label names for times after variable
times.labs <- c("1" = "1 Time after", "2" = "2 Times after", "3" = "3 Times after")
#names(times.labs) <- c("1", "2", "3")


p1 <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = trials, linetype = sublocations.within.locations),
                           data = df, stat="identity", cex = 1.2) + # BRUVs
  #scale_color_manual(values = c("grey60", "lawngreen", "blue3", "red", "blue")) + # for TV
  scale_color_manual(values = c("grey70", "deepskyblue","blue3")) + 
  #scale_color_manual(values=col2) + # to use with brocolors
  scale_x_continuous (name = "Decrease in % cover of seagrasses") +
  #scale_y_discrete(name = "Power", breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous( name = "Power",  breaks = c(0,0.2,0.4,0.6,0.8,1))+
  #geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "black", size = 0.8) +
  facet_wrap(~times.after, labeller = as_labeller(times.labs))  +
  #facet_grid(times.after~., labeller = as_labeller(times.labs)) +
  #geom_hline(yintercept = 0.6, linetype ="dashed", color = "grey81", size = 1) +
  #labs(color = "Replicates", linetype = "Times after", title = "National Park Zone - Stereo-BRUVs survey") +
  labs(color = "Trials", linetype = "No. sublocations", title = titleplot) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"), plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey") 
p1

## save plot
#ggsave(paste(p.dir, "Bruv-HPZ-500it_power.png", sep='/'), plot = p, device = "png", scale =1, dpi = 300)
#ggsave(paste(p.dir, plotn, sep='/'), plot = p, device = "png", scale =1, dpi = 300)



### Load MUZ data ----

dir(o.dir)

method <- "Towed Video"
zone  <- "Multiple Use Zone"

# Set file name --
filen <- "TV-MUZ-400it-binomal__scenario_power_summary.csv"

# set plot name --
plotn <- gsub("csv$", "png", filen)




df <- read.csv(paste(o.dir, filen, sep='/'))
str(df)
names(df)

# Make the effects column positive
#df$effect.p <- ifelse(df$effect=1, 
df$effect.p <- (df$effect*(-1))*100
head(df)

#df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)
df$trials <- as.factor(df$trials)
df$times.after <- as.factor(df$times.after)
df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)

## Plot results

theme_set(theme_bw())

# give the plot a title according to method and zone
titleplot <- paste(method, zone, sep=' - ')

# New facet label names for times after variable
times.labs <- c("1" = "1 Time after", "2" = "2 Times after", "3" = "3 Times after")
#names(times.labs) <- c("1", "2", "3")


p1 <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = trials, linetype = sublocations.within.locations),
                           data = df, stat="identity", cex = 1.2) + # BRUVs
  #scale_color_manual(values = c("grey60", "lawngreen", "blue3", "red", "blue")) + # for TV
  scale_color_manual(values = c("grey70", "deepskyblue","blue3")) + 
  #scale_color_manual(values=col2) + # to use with brocolors
  scale_x_continuous (name = "Decrease in % cover of seagrasses") +
  #scale_y_discrete(name = "Power", breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous( name = "Power",  breaks = c(0,0.2,0.4,0.6,0.8,1))+
  #geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "black", size = 0.8) +
  facet_wrap(~times.after, labeller = as_labeller(times.labs))  +
  #facet_grid(times.after~., labeller = as_labeller(times.labs)) +
  #geom_hline(yintercept = 0.6, linetype ="dashed", color = "grey81", size = 1) +
  #labs(color = "Replicates", linetype = "Times after", title = "National Park Zone - Stereo-BRUVs survey") +
  labs(color = "Trials", linetype = "No. sublocations", title = titleplot) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"), plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey") 
p1

## save plot
#ggsave(paste(p.dir, "Bruv-HPZ-500it_power.png", sep='/'), plot = p, device = "png", scale =1, dpi = 300)
#ggsave(paste(p.dir, plotn, sep='/'), plot = p, device = "png", scale =1, dpi = 300)














#####################

#####   AUV   #####

### Load NPZ data ----

# Set method name --
method <- "AUV"
zone  <- "National Park Zone"


dir(o.dir)

# Set file name --
filen <- "AUV-NPZ-400it-binomal__scenario_power_summary.csv"

# set plot name --
plotn <- gsub("csv$", "png", filen)



# Load data
df <- read.csv(paste(o.dir, filen, sep='/'))
str(df)
names(df)

# Make the effects column positive
#df$effect.p <- ifelse(df$effect=1, 
df$effect.p <- (df$effect*(-1))*100
head(df)

#df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)
df$trials <- as.factor(df$trials)
df$times.after <- as.factor(df$times.after)
df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)


# --- # --- #

# Set PLOT COLORS ----

# from broman package --
plot_crayons() # to see color names
col1 <- crayons("mountain m")
crayons(c("black", "dandelion", "navy b"))
col2 <- c("#000000", "#fddb6d",       "#1974d2")

## Plot results

## Set colours

theme_set(theme_bw())


# give the plot a title according to method and zone
titleplot <- paste(method, zone, sep=' - ')

# New facet label names for times after variable
times.labs <- c("1" = "1 Time after", "2" = "2 Times after", "3" = "3 Times after")
names(times.labs) <- c("1", "2", "3")


p1 <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = trials, linetype = sublocations.within.locations),
                           data = df, stat="identity", cex = 1.2) + # BRUVs
  #scale_color_manual(values = c("grey60", "lawngreen", "blue3", "red", "blue")) + # for TV
  scale_color_manual(values = c("grey70", "deepskyblue","blue3")) + 
  #scale_color_manual(values=col2) + # to use with brocolors
  scale_x_continuous (name = "Decrease in % cover of seagrasses") +
  #scale_y_discrete(name = "Power", breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous( name = "Power",  breaks = c(0,0.2,0.4,0.6,0.8,1), limits = c(0,1))+
  #geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "black", size = 0.8) +
  facet_wrap(~times.after, labeller = as_labeller(times.labs))  +
  #facet_grid(times.after~., labeller = as_labeller(times.labs)) +
  #geom_hline(yintercept = 0.6, linetype ="dashed", color = "grey81", size = 1) +
  #labs(color = "Replicates", linetype = "Times after", title = "National Park Zone - Stereo-BRUVs survey") +
  labs(color = "Trials", linetype = "No. sublocations", title = titleplot) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"), plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey") 
p1

## save plot
#ggsave(paste(p.dir, "Bruv-HPZ-500it_power.png", sep='/'), plot = p, device = "png", scale =1, dpi = 300)
#ggsave(paste(p.dir, plotn, sep='/'), plot = p, device = "png", scale =1, dpi = 300)



### Load HPZ data ----

# Set method name --
method <- "AUV"
zone  <- "Habitat Protection Zone"


dir(o.dir)

# Set file name --
filen <- "AUV-HPZ-400it-binomal__scenario_power_summary.csv"

# set plot name --
plotn <- gsub("csv$", "png", filen)



# Load data
df <- read.csv(paste(o.dir, filen, sep='/'))
str(df)
names(df)

# Make the effects column positive
#df$effect.p <- ifelse(df$effect=1, 
df$effect.p <- (df$effect*(-1))*100
head(df)

#df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)
df$trials <- as.factor(df$trials)
df$times.after <- as.factor(df$times.after)
df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)


# --- # --- #

# Set PLOT COLORS ----

# from broman package --
plot_crayons() # to see color names
col1 <- crayons("mountain m")
crayons(c("black", "dandelion", "navy b"))
col2 <- c("#000000", "#fddb6d",       "#1974d2")

## Plot results

## Set colours

theme_set(theme_bw())


# give the plot a title according to method and zone
titleplot <- paste(method, zone, sep=' - ')

# New facet label names for times after variable
times.labs <- c("1" = "1 Time after", "2" = "2 Times after", "3" = "3 Times after")
names(times.labs) <- c("1", "2", "3")


p1 <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = trials, linetype = sublocations.within.locations),
                           data = df, stat="identity", cex = 1.2) + # BRUVs
  #scale_color_manual(values = c("grey60", "lawngreen", "blue3", "red", "blue")) + # for TV
  scale_color_manual(values = c("grey70", "deepskyblue","blue3")) + 
  #scale_color_manual(values=col2) + # to use with brocolors
  scale_x_continuous (name = "Decrease in % cover of seagrasses") +
  #scale_y_discrete(name = "Power", breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous( name = "Power",  breaks = c(0,0.2,0.4,0.6,0.8,1), limits = c(0,1))+
  #geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "black", size = 0.8) +
  facet_wrap(~times.after, labeller = as_labeller(times.labs))  +
  #facet_grid(times.after~., labeller = as_labeller(times.labs)) +
  #geom_hline(yintercept = 0.6, linetype ="dashed", color = "grey81", size = 1) +
  #labs(color = "Replicates", linetype = "Times after", title = "National Park Zone - Stereo-BRUVs survey") +
  labs(color = "Trials", linetype = "No. sublocations", title = titleplot) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"), plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey") 
p1

## save plot
#ggsave(paste(p.dir, "Bruv-HPZ-500it_power.png", sep='/'), plot = p, device = "png", scale =1, dpi = 300)
#ggsave(paste(p.dir, plotn, sep='/'), plot = p, device = "png", scale =1, dpi = 300)




### Load MUZ data ----

# Set method name --
method <- "AUV"
zone  <- "Multiple Use Zone"


dir(o.dir)

# Set file name --
filen <- "AUV-MUZ-400it-binomal__scenario_power_summary.csv"

# set plot name --
plotn <- gsub("csv$", "png", filen)



# Load data
df <- read.csv(paste(o.dir, filen, sep='/'))
str(df)
names(df)

# Make the effects column positive
#df$effect.p <- ifelse(df$effect=1, 
df$effect.p <- (df$effect*(-1))*100
head(df)

#df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)
df$trials <- as.factor(df$trials)
df$times.after <- as.factor(df$times.after)
df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)


# --- # --- #

# Set PLOT COLORS ----

# from broman package --
plot_crayons() # to see color names
col1 <- crayons("mountain m")
crayons(c("black", "dandelion", "navy b"))
col2 <- c("#000000", "#fddb6d",       "#1974d2")

## Plot results

## Set colours

theme_set(theme_bw())


# give the plot a title according to method and zone
titleplot <- paste(method, zone, sep=' - ')

# New facet label names for times after variable
times.labs <- c("1" = "1 Time after", "2" = "2 Times after", "3" = "3 Times after")
names(times.labs) <- c("1", "2", "3")


p1 <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = trials, linetype = sublocations.within.locations),
                           data = df, stat="identity", cex = 1.2) + # BRUVs
  #scale_color_manual(values = c("grey60", "lawngreen", "blue3", "red", "blue")) + # for TV
  scale_color_manual(values = c("grey70", "deepskyblue","blue3")) + 
  #scale_color_manual(values=col2) + # to use with brocolors
  scale_x_continuous (name = "Decrease in % cover of seagrasses") +
  #scale_y_discrete(name = "Power", breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous( name = "Power",  breaks = c(0,0.2,0.4,0.6,0.8,1), limits = c(0,1))+
  #geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "black", size = 0.8) +
  facet_wrap(~times.after, labeller = as_labeller(times.labs))  +
  #facet_grid(times.after~., labeller = as_labeller(times.labs)) +
  #geom_hline(yintercept = 0.6, linetype ="dashed", color = "grey81", size = 1) +
  #labs(color = "Replicates", linetype = "Times after", title = "National Park Zone - Stereo-BRUVs survey") +
  labs(color = "Trials", linetype = "No. sublocations", title = titleplot) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"), plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey") 
p1

## save plot
#ggsave(paste(p.dir, "Bruv-HPZ-500it_power.png", sep='/'), plot = p, device = "png", scale =1, dpi = 300)
#ggsave(paste(p.dir, plotn, sep='/'), plot = p, device = "png", scale =1, dpi = 300)



### Load SPZ data ----

# Set method name --
method <- "AUV"
zone  <- "Special Purpose Zone"


dir(o.dir)

# Set file name --
filen <- "AUV-SPZ-400it-binomal__scenario_power_summary.csv"

# set plot name --
plotn <- gsub("csv$", "png", filen)



# Load data
df <- read.csv(paste(o.dir, filen, sep='/'))
str(df)
names(df)

# Make the effects column positive
#df$effect.p <- ifelse(df$effect=1, 
df$effect.p <- (df$effect*(-1))*100
head(df)

#df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)
df$trials <- as.factor(df$trials)
df$times.after <- as.factor(df$times.after)
df$sublocations.within.locations <- as.factor(df$sublocations.within.locations)


# --- # --- #

# Set PLOT COLORS ----

# from broman package --
plot_crayons() # to see color names
col1 <- crayons("mountain m")
crayons(c("black", "dandelion", "navy b"))
col2 <- c("#000000", "#fddb6d",       "#1974d2")

## Plot results

## Set colours

theme_set(theme_bw())


# give the plot a title according to method and zone
titleplot <- paste(method, zone, sep=' - ')

# New facet label names for times after variable
times.labs <- c("1" = "1 Time after", "2" = "2 Times after", "3" = "3 Times after")
names(times.labs) <- c("1", "2", "3")


p1 <- ggplot() + geom_line(aes(y = sig.outcomes, x = effect.p, colour = trials, linetype = sublocations.within.locations),
                           data = df, stat="identity", cex = 1.2) + # BRUVs
  #scale_color_manual(values = c("grey60", "lawngreen", "blue3", "red", "blue")) + # for TV
  scale_color_manual(values = c("grey70", "deepskyblue","blue3")) + 
  #scale_color_manual(values=col2) + # to use with brocolors
  scale_x_continuous (name = "Decrease in % cover of seagrasses") +
  #scale_y_discrete(name = "Power", breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous( name = "Power",  breaks = c(0,0.2,0.4,0.6,0.8,1), limits = c(0,1))+
  #geom_hline(yintercept = 0.8, linetype ="dashed", color = "grey81", size = 1.2) +
  geom_hline(yintercept = 0.8, linetype ="dashed", color = "black", size = 0.8) +
  facet_wrap(~times.after, labeller = as_labeller(times.labs))  +
  #facet_grid(times.after~., labeller = as_labeller(times.labs)) +
  #geom_hline(yintercept = 0.6, linetype ="dashed", color = "grey81", size = 1) +
  #labs(color = "Replicates", linetype = "Times after", title = "National Park Zone - Stereo-BRUVs survey") +
  labs(color = "Trials", linetype = "No. sublocations", title = titleplot) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"), plot.title=element_text(size=14, face = "bold")) 
#labs(color = "Times after", linetype = "Control locations", title = "BRUV survey") 
p1

## save plot
#ggsave(paste(p.dir, "Bruv-HPZ-500it_power.png", sep='/'), plot = p, device = "png", scale =1, dpi = 300)
#ggsave(paste(p.dir, plotn, sep='/'), plot = p, device = "png", scale =1, dpi = 300)