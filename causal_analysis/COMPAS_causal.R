# require(devtools)
# require(httpuv)
# devtools::install_github("dplecko/CFA")
require(faircause)
require(ggplot2)
require(latex2exp)
require(dplyr)

##########################
### Manually load data ###
##########################

get.COMPAS <- function(frac = 0.6, seed = 15, limit = 4){
  
  # Load Data
  COMPAS <- read.csv("./compas-scores-two-years.csv")
  
  # Filter Data
  COMPAS <- dplyr::select(COMPAS, age, c_charge_degree, race, age_cat, score_text, sex, priors_count, 
                          days_b_screening_arrest, decile_score, is_recid, two_year_recid, c_jail_in, 
                          c_jail_out, juv_fel_count, juv_misd_count, juv_other_count) %>% 
    filter(days_b_screening_arrest <= 30) %>%
    filter(days_b_screening_arrest >= -30) %>%
    filter(is_recid != -1) %>%
    filter(c_charge_degree != "O") %>%
    filter(score_text != 'N/A')
  
  COMPAS$pred <- as.integer(COMPAS$decile_score > limit)
  COMPAS$truth <- COMPAS$two_year_recid == 1
  
  COMPAS$race <- factor(COMPAS$race)
  
  levels(COMPAS$race) <- c("Minority", "Minority", "Majority", "Minority",
                                "Minority", "Minority")
  COMPAS$race <- relevel(COMPAS$race, "Majority")
  
  #------------------Training/Test Data--------------------------
  set.seed(seed)
  n.train <- round(frac * nrow(COMPAS))
  index <- sample(1:n.train, n.train, replace=FALSE)
  data.train <- COMPAS[index,]
  data.test <- COMPAS[-index,]
  
  return(list(data = COMPAS, train = data.train, test = data.test))
  
}

COMPAS <- get.COMPAS()
data.COMPAS <- COMPAS$data

###############################################
########## Causal Analysis of COMPAS ##########
###############################################

set.seed(101)
ctf.ci <- function(x, alpha = 0.025, text = "CI-95%", cat = TRUE, signed = TRUE){
  m <- mean(x)
  s <- sd(x)
  margin <- qt(alpha,df=length(x)-1, lower.tail = FALSE)*s
  if(!signed){m <- abs(m)}
  if(cat){cat(text," = [",m-margin,",",m+margin,"] / mean = ",m,", sd = ",s,"\n")}
  return(c(l = m-margin, u = m+margin, m = m, margin = margin))
}

TVdecompos <- function(tv, de, ie, se, text = "TV_{x_0,x_1}(y)"){
  
  tvci <- ctf.ci(tv, cat = FALSE, signed = FALSE)
  deci <- ctf.ci(de, cat = FALSE, signed = FALSE)
  ieci <- ctf.ci(ie, cat = FALSE, signed = FALSE)
  seci <- ctf.ci(se, cat = FALSE, signed = FALSE)
  
  cat(text," = ",round(100*tvci[3],digits=1),"% ( ±",round(100*tvci[4],digits=1),"% ) = ",
      round(100*deci[3],digits=1),"% ( ±",round(100*deci[4],digits=1),"% ) + ",
      round(100*ieci[3],digits=1),"% ( ±",round(100*ieci[4],digits=1),"% ) + ",
      round(100*seci[3],digits=1),"% ( ±",round(100*seci[4],digits=1),"% )",sep = "")
  
}

### Parity Gap: PG_{x_0,x_1}(y) = P(Y=1 | X=x_1) - P(Y=1 | X=x_0) (Task 1 - Bias detection)

### Y disparity (Real Bias)
X <- "race"
Z <- c("age", "sex")
W <- c("juv_fel_count", "juv_misd_count", "juv_other_count", "priors_count", "c_charge_degree")
Y <- c("two_year_recid")
COMPAS.cfa <- fairness_cookbook(data.COMPAS, X = X, W = W, Z = Z, Y = Y,
                              x0 = "Majority", x1 = "Minority")
COMPAS.cfa
summary(COMPAS.cfa)

tapply(data.COMPAS$two_year_recid, data.COMPAS$race, mean)
cat(paste("TV_{x_0,x_1}(y) =", round(100*(tapply(data.COMPAS$two_year_recid, data.COMPAS$race, mean)[2]),digits = 2), "% - ",
          round(100*(tapply(data.COMPAS$two_year_recid, data.COMPAS$race, mean)[1]),digits = 2), "% = ",
          round(100*(tapply(data.COMPAS$two_year_recid, data.COMPAS$race, mean)[2]-tapply(data.COMPAS$two_year_recid, data.COMPAS$race, mean)[1]),digits = 2),"%"))

# Causal decomposition of the TV measure (PG) for two-year recidivism.
autoplot(COMPAS.cfa, decompose = "xspec", signed = FALSE) + 
  ggtitle(TeX("$Y$ disparity decomposition COMPAS"))

tv <- COMPAS.cfa$measures[COMPAS.cfa$measures$measure == "tv",]
ctfde <- COMPAS.cfa$measures[COMPAS.cfa$measures$measure == "ctfde",]
ctfie <- COMPAS.cfa$measures[COMPAS.cfa$measures$measure == "ctfie",]
ctfse <- COMPAS.cfa$measures[COMPAS.cfa$measures$measure == "ctfse",]

cat("Table 1:")

tv_ci <- ctf.ci(tv$value, text = "TV CI-95%")
ctfde_ci <- ctf.ci(ctfde$value, text = "x-DE CI-95%")
ctfie_ci <- ctf.ci(ctfie$value, text = "x-IE CI-95%")
ctfse_ci <- ctf.ci(ctfse$value, text = "x-SE CI-95%")

TVdecompos(tv$value,ctfde$value,ctfie$value,ctfse$value)

cat("-------------------------------------------------")

### Yhat disparity (COMPAS bias)
tapply(data.COMPAS$pred, data.COMPAS$race, mean)
cat(paste("TV_{x_0,x_1}(y_hat) =", round(100*(tapply(data.COMPAS$pred, data.COMPAS$race, mean)[2]),digits = 2), "% - ",
          round(100*(tapply(data.COMPAS$pred, data.COMPAS$race, mean)[1]),digits = 2), "% = ",
          round(100*(tapply(data.COMPAS$pred, data.COMPAS$race, mean)[2]-tapply(data.COMPAS$pred, data.COMPAS$race, mean)[1]),digits = 2),"%"))

Yhat <- "pred"
COMPAShat.cfa <- fairness_cookbook(data.COMPAS, X = X, W = W, Z = Z, Y = Yhat,
                                     x0 = "Majority", x1 = "Minority")
# Causal decomposition of the TV measure (PG) for pred’s predictions.
autoplot(COMPAShat.cfa, decompose = "xspec", signed = FALSE) +
  ggtitle(TeX("$\\widehat{Y}$ disparity decomposition COMPAS"))

tvbis <- COMPAShat.cfa$measures[COMPAShat.cfa$measures$measure == "tv",]
ctfdebis <- COMPAShat.cfa$measures[COMPAShat.cfa$measures$measure == "ctfde",]
ctfiebis <- COMPAShat.cfa$measures[COMPAShat.cfa$measures$measure == "ctfie",]
ctfsebis <- COMPAShat.cfa$measures[COMPAShat.cfa$measures$measure == "ctfse",]

cat("Table 2:")

tvbis_ci <- ctf.ci(tvbis$value, text = "TV CI-95%")
ctfdebis_ci <- ctf.ci(ctfdebis$value, text = "x-DE CI-95%")
ctfiebis_ci <- ctf.ci(ctfiebis$value, text = "x-IE CI-95%")
ctfsebis_ci <- ctf.ci(ctfsebis$value, text = "x-SE CI-95%")

TVdecompos(tvbis$value,ctfdebis$value,ctfiebis$value,ctfsebis$value)

cat("-------------------------------------------------")

### C: Yhat disparity for those who did not recidivate

ind <- data.COMPAS$two_year_recid == 0
tapply(data.COMPAS$pred[ind], data.COMPAS$race[ind], mean)
cat(paste("TV_{x_0,x_1}(y_hat | y=0) =", round(100*(tapply(data.COMPAS$pred[ind], data.COMPAS$race[ind], mean)[2]),digits = 2), "% - ",
          round(100*(tapply(data.COMPAS$pred[ind], data.COMPAS$race[ind], mean)[1]),digits = 2), "% = ",
          round(100*(tapply(data.COMPAS$pred[ind], data.COMPAS$race[ind], mean)[2]-tapply(data.COMPAS$pred[ind], data.COMPAS$race[ind], mean)[1]),digits = 2),"%"))

COMPAShat.cfa_eo <- fairness_cookbook_eo(data.COMPAS, X = X, W = W, Z = Z, Y = Y,
                                     Yhat = Yhat, x0 = "Majority", x1 = "Minority",
                                     ylvl = 0)
autoplot(COMPAShat.cfa_eo, decompose = "xspec", signed = FALSE, eo = TRUE) +
  ggtitle(TeX("$\\widehat{Y}$ decomposition for non-reoffenders"))

tvbisbis <- COMPAShat.cfa_eo$measures[COMPAShat.cfa_eo$measures$measure == "tv",]
ctfdebisbis <- COMPAShat.cfa_eo$measures[COMPAShat.cfa_eo$measures$measure == "ctfde",]
ctfiebisbis <- COMPAShat.cfa_eo$measures[COMPAShat.cfa_eo$measures$measure == "ctfie",]
ctfsebisbis <- COMPAShat.cfa_eo$measures[COMPAShat.cfa_eo$measures$measure == "ctfse",]

cat("Table 3:")

tvbisbis_ci <- ctf.ci(tvbisbis$value, text = "TV CI-95%")
ctfdebisbis_ci <- ctf.ci(ctfdebisbis$value, text = "x-DE CI-95%")
ctfiebisbis_ci <- ctf.ci(ctfiebisbis$value, text = "x-IE CI-95%")
ctfsebisbis_ci <- ctf.ci(ctfsebisbis$value, text = "x-SE CI-95%")

TVdecompos(tvbisbis$value,ctfdebisbis$value,ctfiebisbis$value,ctfsebisbis$value)

cat("-------------------------------------------------")

