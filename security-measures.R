rm(list=ls()) # clear global environment
getwd() # double check wd
install.packages("tidyr")
install.packages("rlang")

#setup
knitr::opts_chunk$set(echo = TRUE)
options(digits=3)
library(dplyr)
library(tidyr)

security_wide <- read.csv('http://dept.stat.lsa.umich.edu/~bbh/s485/data/security_wide.csv')

#**********************************************************************************

# Replication of the measurement model used by Finn and Servoss to
#combine ELS:2002 data elements into an index of the security 
#environment at U.S. schools (Rasch modeling)

fs_items <- c(paste0('q38', c('c', 'd', 'h', 'g', 'n')), "q40a", "q38f")
security_wide[fs_items] %>%
  sapply(function(vec) table(vec, exclude=NULL)) %>%
  knitr::kable()

security_wide_fs <- security_wide[c("school", fs_items)]

#reshape data
security_tall_fs1 <-  security_wide_fs %>% 
  tidyr::gather("item", "response", -school)

#fitting the model (print amount of time it takes to fit the model, called rasch1)
rasch1 = glm(response ~item + school -1, family = binomial, data=security_tall_fs1)
#head(coef(rasch1), 10)

#bookkeeping - schools
(nschools <- nlevels(security_wide$school))
school_coef_names <- paste0("school", levels(security_wide$school))
table(school_coef_names %in% names(coef(rasch1)) )

#**********************************************************************************

#Using the bootstrap to study the form and magnitude of sampling 
#variability of the security indices that attach to schools in this manner

# ***** parametric bootstrapping *****

#bootstrapping the fitted model: simulator
logit_mod_simulator <- function() {
  probs <- predict(rasch1, newdata=security_tall_fs1, type="response")
  data.frame(response=rbinom(length(probs), 1, probs), 
             item=security_tall_fs1$item, 
             school=security_tall_fs1$school)}

#bootstrapping the fitted model: statistic
logit_coefs = function(bootspl) {
  refitted.mod <- update(rasch1, data=bootspl)
  unlist(coef(refitted.mod))
}

#all.equal(logit_coefs(security_tall_fs1), coef(rasch1))
#all.equal returned TRUE

#applying the bootstrap
rboot <- function(statistic, simulator, B) {
  tboots <- replicate(B, statistic(simulator()))
  if (is.null(dim(tboots))) {
    tboots <-array(tboots, dim = c(1,B))
  }
  return(tboots)
}

boot.stats <- rboot(logit_coefs, logit_mod_simulator, 5)

#inspect distributions

dim(boot.stats)

boot.stats[,1:3]

apply(boot.stats, 1, summary)

boxplot(apply(boot.stats[10:15, ], 1, scale))

#**********************************************************************************

#Considering whether simple variations of the model might improve it
# fit variants of F. & S.'s model

#using a different question set than the original
fs_items_c <- c(paste0('q38', c('f', 'g', 'h', 'i', 'j')), "q40d", "q40e")
security_wide[fs_items_c] %>%
  sapply(function(vec) table(vec, exclude=NULL)) %>%
  knitr::kable()

security_wide_fs <- security_wide[c("school", fs_items_c)]

#reshape data
security_tall_fs1 <-  security_wide_fs %>% 
  tidyr::gather("item", "response", -school)

#fitting the model (print amount of time it takes to fit the model, called rasch1)
rasch1 = glm(response ~item + school -1, family = binomial, data=security_tall_fs1)
head(coef(rasch1), 10)

#bookkeeping - schools
(nschools <- nlevels(security_wide$school))
school_coef_names <- paste0("school", levels(security_wide$school))
table(school_coef_names %in% names(coef(rasch1)) )

#**********************************************************************************
install.packages("profileModel")
library(brglm)
#fitting using brglm

#fitting the model (Called rasch_brglm)
rasch_brglm = brglm(response ~item + school -1, family = binomial, data=security_tall_fs1)
head(coef(rasch_brglm), 10)

#bookkeeping - schools
(nschools <- nlevels(security_wide$school))
school_coef_names <- paste0("school", levels(security_wide$school))
table(school_coef_names %in% names(coef(rasch_brglm)) )

#**********************************************************************************

#Quantify the extent to which plausible variations of the index
#model change the index that results

#differing indices on the bases of ordering of the schools
#choosing index described by rasch_brglm above
#fitting using brglm

boot.stats_brglm <- rboot(logit_coefs, logit_mod_simulator, 5)

#inspect distributions

dim(boot.stats_brglm)

boot.stats_brglm[,1:3]

apply(boot.stats_brglm, 1, summary)

boxplot(apply(boot.stats_brglm[20:24, ], 1, scale))
