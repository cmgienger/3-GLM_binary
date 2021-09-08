# 24/6/2015

##### The New Statistics with R, Chapter 9: Binomial Count GLM

### Metadata
 # From Young & Young 1998 (p.510-14) after Bliss (1935)
 # Mortality of Tribolium confusa beetles...
 # after 5 hour exposure to carbon disulphide
 # Dose:  concentration of CS2 in mg/L
 # Number_tested: No. of beetles in each batch (binomial denominator)
 # Number_killed: "Successes"
 # Mortality_rate: Proportion killed over 5 hour period

### Set up
# Clear workspace
rm(list=ls(all=TRUE)) 
# Turn off significance stars
options(show.signif.stars= FALSE) 
# set working directory (customise to your own settings)
# setwd(...)

### Load packages
library(ggplot2)
library(arm)
library(AICcmodavg)
library(grid)

### Functions
# to back-transform logits to proportions: 
expit <- function(x) {1 / ( 1+exp(-x) ) }
expit(0)

### Flour Beetle Binomial Counts GLM
# Box 9.1
library(AICcmodavg)
# try(data(package= "AICcmodavg") ) # lists the dataframes in this package
data(beetle)
beetle$Number_alive <- beetle$Number_tested - beetle$Number_killed

### Box 9.1
beetle
str(beetle) 

### Fig Box 9.2
xseq <- 1:99
Proportion <- xseq/100
Logit <- log(Proportion/(1-Proportion))
( A <- qplot(Proportion, Logit, ylab= "logit(Proportion) or log(Odds)")+theme_bw() )
### Fig 6.3
ps <- seq(0.01, 0.99, 0.01)
pf <- 1-ps
bd <- rep(1, 99)
Variance <- ps*pf*bd
( B <- qplot(ps, Variance, xlab= "Proportion")+theme_bw() )
# Combine qplots into a side by side layout: 
# library(grid)
# create viewport with a 1 row by 2 column layout:
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2) ) )
vplayout <- function(x, y)
viewport(layout.pos.row=x, layout.pos.col=y)
# print a on the left and b on the right: 
print(A, vp= vplayout(1,1))
print(B, vp= vplayout(1,2))
# dev.copy(postscript, file="Fig_Box9-2.eps", height=8, width=12, horizontal=F, onefile=F)
dev.off()

##### Fig Box 9.2 eps file #####
setEPS()
postscript("Fig_Box9-2.eps")
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2) ) )
vplayout <- function(x, y)
viewport(layout.pos.row=x, layout.pos.col=y)
# print a on the left and b on the right: 
print(A, vp= vplayout(1,1))
print(B, vp= vplayout(1,2))
dev.off()
#########

# Binomial GLM
m1 <- glm(cbind(Number_killed, Number_alive) ~ Dose, data= beetle, binomial)

# Equivalent regression of proportions by weighted regression as used in figures
# wr <- glm(Mortality_rate~Dose, data= beetle, family= binomial, weight= Number_tested)

### Fig 9.1
library(ggplot2)
qplot(Dose, Mortality_rate, data= beetle,
      geom= c("point","smooth"), method= "glm", 
      family= binomial, weight= Number_tested,
      ylab= "Mortality rate" ) +theme_bw()
ggsave("Fig9-1.tiff")
dev.off()

# Intercept and slope of logistic regression
coef(m1)

# estimates on logit scale
confint(m1)
summary(m1)

mq1 <- glm(cbind(Number_alive, Number_killed) ~ Dose, data= beetle, quasibinomial)

#coef(mq1)
#confint(mq1)
summary(mq1)
#display(mq1)

