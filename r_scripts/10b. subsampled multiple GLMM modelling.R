####################################################################################################################
# step 10b
# multiple GLMM modelling
# subsampled data
####################################################################################################################
####################################################################################################################
####################################################################################################################
#################################################################################################################

library(MASS)

# load data

ALL <- utils::read.csv("https://raw.githubusercontent.com/graemetlloyd/ProjectNobodyWax/main/input_data/all_subsampled_data.csv")

###############################################################
# mass extinction interval
######################################################

mass <- ALL[ALL[, "Regime"] == "mass", ]

##############################################################
# model extinction (0/1) as a function of all range metrics together
# model massMultiGLM

# full model
massMultiGLM_full <- stats::glm(
  Extinct ~ GCD + LatRange + LongRange,
  family = binomial,
  data = mass
)

summary(massMultiGLM_full)

stats::drop1(massMultiGLM_full, test = "F")

# none significant but GCD strongest* predictor

###############################################################
# background intervals
######################################################

background <- ALL[ALL[,"Regime"] == "background", ]

##############################################################
# model extinction (0/1) as a function of all range metrics together
# model backMultiGLM_full

# full model
backMultiGLM_full <- stats::glm(
  Extinct ~ GCD + LatRange + LongRange,
  family = binomial,
  data = background
)

summary(backMultiGLM_full)

stats::drop1(backMultiGLM_full, test ="F")

# Only LatRange significant
