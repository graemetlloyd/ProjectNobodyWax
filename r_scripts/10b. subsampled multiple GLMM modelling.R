####################################################################################################################
# step 10b
# multiple GLMM modelling
# subsampled data
####################################################################################################################
####################################################################################################################
####################################################################################################################
#################################################################################################################

# set working directory

setwd("c:/users/earadun/documents/geographic range ecoTJ/data/subsampled data/")

library(MASS)

# load data

ALL<- read.csv("all_subsampled_data.csv")

###############################################################
# mass extinction interval
######################################################

mass <- ALL[grep(TRUE,ALL[,"Regime"] == "mass"),]

##############################################################
# model extinction (0/1) as a function of all range metrics together
# model massMultiGLM

# full model
massMultiGLM_full <- glm(Extinct ~ GCD + LatRange + LongRange,
			family = binomial,
			data = mass)

summary(massMultiGLM_full)

drop1(massMultiGLM_full, test ="F")

# none significant but GCD strongest* predictor

###############################################################
# background intervals
######################################################

background <- ALL[grep(TRUE,ALL[,"Regime"] == "background"),]

##############################################################
# model extinction (0/1) as a function of all range metrics together
# model backMultiGLM_full

# full model
backMultiGLM_full <- glm(Extinct ~ GCD + LatRange + LongRange,
                         family = binomial,
                         data = background)

summary(backMultiGLM_full)

drop1(backMultiGLM_full, test ="F")

# Only LatRange significant
