# 25/6/2015

##### The New Statistics with R, Chapter 9: Count Data on Plant Species Loss

### Metadata (* = created below)
 # Source: Based on Hautier, Niklaus & Hector (2009) Science 324 p.626.
 # Fertilizer: Fertilizer applied (F+) or not (F-)
 # Light supplemented in grassland understory (L+) or not (L-)
 # FL / LF: The four treatment combinations
 # Diversity: Counts of the number of plant species per plot
  
### Set up
# Clear workspace
rm(list=ls(all=TRUE)) 
# Turn off significance stars
options(show.signif.stars= FALSE) 
# set working directory (customise to your own settings)
# setwd(...)

### Installing packages if required and you have internet
 # install.packages("arm")
 # install.packages("ggplot2") 

### Loading all libraries used
library(arm)
library(ggplot2)
library(MASS)

Hautier09 <- read.table(file.choose(), header= T) # Data_Hautier09_Counts
str(Hautier09)
### Box 9.4
Hautier09

# Poisson GLM
pois1 <- glm(Diversity~Light*Fertilizer, family= poisson(link=log), data= Hautier09)

# check assumption of residual dev. to resid df ~ 1:1:
summary(pois1)

### Fig. 9.7: Boxplots
qplot(FL, Diversity, geom= "boxplot", data= Hautier09,
      main= "Fertilizer & Light Additions")+theme_bw()
# ggsave(file= "Fig9-7.eps")
dev.off()

# Analysis is underdispersed (residual deviance < residual DF)
# Variance does not increase equal to mean as assumed
# Quasi-Maximum Likelihood will take actual variance into account

qpois1 <- glm(Diversity~Light*Fertilizer, quasipoisson, data= Hautier09)

coef(qpois1) # note overdispersion parameter
confint(qpois1) # interaction significant

### Fig 9.8: interaction plot  #####
attach(Hautier09)
interaction.plot(Fertilizer, Light, Diversity)
detach(Hautier09)
#################

##### Fig 9.8 EPS file #####
setEPS()
postscript("Fig9-8.eps")
attach(Hautier09)
interaction.plot(Fertilizer, Light, Diversity)
detach(Hautier09)
dev.off()
#########

### Fig 9.9: Box-Cox transformation
library(MASS)
boxcox( glm(Diversity~Light*Fertilizer, data= Hautier09) )

##### Fig 9.9 #####
setEPS()
postscript("Fig9-9.eps")
library(MASS)
boxcox( glm(Diversity~Light*Fertilizer, data= Hautier09) )
dev.off()
#########

# CI wide and includes no transformation
# log transformation best to linearize relationship
# of course, we would then expect that the interaction could become Non-Sig...
# when switching from multiplicative effects on original scale
# to additive effects on the log scale
# so actually transforming is probably NOT what we want to do
# but lets see anyway for interests sake...

# GLM with log link function:
log.lin <- glm(Diversity~Light*Fertilizer, family= gaussian(link=log), data= Hautier09)
# residuals:
# par(mfrow=c(2,2))
# plot(log.norm)

# As it turns out results are qualitatively the same
# and similar to the quasipoisson GLM
coef(log.lin)
confint(log.lin)

# Note: the GLM still assumes normal residuals (family= gaussian)
# lm  of log transformed data instead assumes log-normal residuals 
# but in this case the residual plots are similar (try it)
