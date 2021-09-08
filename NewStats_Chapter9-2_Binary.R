# 24/6/2015

##### The New Statistics with R, Chapter 9: Logistic regression GLM analysis of binary data
 
### Metadata
 # Source: Gelman & Hill (2007)
 # Data & script: http://www.stat.columbia.edu/~gelman/arm/examples/arsenic
 # Exercise: Model the probability of switching wells...
 # ...as a function of distance (dist) and arsenic level and their interaction
 # switch: binary response; 1 = switched wells, 0 = not.
 # arsenic: Level of arsenic in water (micrograms per liter)
 # dist: Distance to nearest safe well (m)
 # assoc: Member of an association (NOT USED HERE)
 # educ: Level of eduction (NOT USED HERE)

### Set up
# Clear workspace
rm(list=ls(all=TRUE)) 
# Turn off significance stars
options(show.signif.stars= FALSE) 
# set working directory (customise to your own settings)
# setwd(...)

## Load packages
library(ggplot2)
library(arm)

## Functions
# backtransform logits:
invlogit <- function(x) {1 / ( 1+exp(-x) ) }
invlogit(0) # 0 logit = 0.5

## Load data
wells <- read.table(file.choose(), header=T) # Data_Binary_Wells.txt

### Box 9.3
str(wells)

#################### Supplementary R code ###################################
## Histogram of distances (Gelman & Hill, Figure 5.8)
qplot(dist, data= wells, geom= "histogram", binwidth=10, 
      xlab="Distance to the nearest safe well (m)")+theme_bw()
#################### End of Supplementary R code ############################

### Fig. 9.2: Using a smoother to get a sense of the general trend
qplot(dist, switch, data= wells,
      xlab= "Distance to nearest well (m)", ylab= "Probability of switching", 
      geom= c("point", "smooth") )+theme_bw()
# ggsave("Fig9-2.tiff")
dev.off()

### Binary GLM of switching vs. distance

# Distance in hundreds of meters simply to avoid inconveniently small coefficient values: 
wells$dist100 <- wells$dist/100

# Binary GLM
fit.1 <- glm(switch~dist100, family= binomial(link="logit"), data= wells)

## Fig.9.3: Default residual plots (no use)
par(mfrow=c(2,2))
plot(fit.1)

###  Fig 9.3 EPS file ###
setEPS()
postscript("Fig9-3.eps")
par(mfrow=c(2,2))
plot(fit.1)
dev.off()
#########

### Fig 9.4: arm package binnedplot
library(arm)
# Fig 9.4 EPS file
# postscript("Fig9-4.eps")
x <- predict(fit.1)
y <- resid(fit.1)
binnedplot(x, y)
dev.off()

# Point estimates and CI
coef(fit.1)
confint(fit.1)

# "Divide by 4" rule of thumb: 
-0.062/4
# [1] -0.0155 reduction in probability of switching...
#             ...with 100 m increase in distance

### Fig 9.5: Graphing the fitted model with one predictor (Gelman and Hill Figure 5.9)
qplot(dist, switch, data= wells,
      xlab= "Distance to nearest well", ylab= "Probability of switching", 
      geom= c("point", "smooth"), method= "glm", family= binomial )+theme_bw()
# ggsave("Fig9-5.tiff")
dev.off()
#########

## Binary GLM with arsenic
fit.2 <- glm(switch~arsenic, family= binomial(link="logit"), data= wells)
display(fit.2)

## Fig 9.6: Graphing the fitted model with one predictor (Gelman and Hill Figure 5.9)
qplot(arsenic, switch, data= wells,
      xlab= "Arsenic concentration", ylab= "Probability of switching", 
      geom= c("point", "smooth"), method= "glm", family= binomial )+theme_bw()
# ggsave("Fig9-6.tiff")
dev.off()

# Centred versions of explanatory variables
wells$c.dist100 <- wells$dist100 - mean(wells$dist100) 
wells$c.arsenic <- wells$arsenic - mean(wells$arsenic)
# regression intercepts are then for average values of x
# especially useful with interactions

# Binary GLM with centered distance (100s m) and centred arsenic:
fit.5 <- glm(switch~c.dist100+c.arsenic+c.dist100:c.arsenic, binomial, data= wells)
display(fit.5)

# backtransform logits:
invlogit <- function(x) {1 / ( 1+exp(-x) ) }
# backtransformed intercept: 
invlogit(coef(fit.5))[1]
