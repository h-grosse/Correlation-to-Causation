# require(devtools)
# require(httpuv)
# devtools::install_github("dplecko/CFA")
require(faircause)
require(ggplot2)
require(latex2exp)

##########################
### Manually load data ###
##########################

get.CRIME <- function(frac = 0.6, seed = 15){
  CRIME <- read.table("communities.data", sep = ",", fill = TRUE, strip.white = TRUE)
  CRIME <- as.data.frame(CRIME[,c(6,7,9,13,15,17,26,34,35,37,38,128)])
  
  colnames(CRIME) <- c("pop","hhsize","pctwhite","age12t29",
                       "age65up","urban","percapinc","underpov",
                       "less9thGrade","BSormore","unemployed","crimes_per_pop")
  
  mean(CRIME$crimes_per_pop)
  mean(CRIME$crimes_per_pop >= 0.16)
  
  mean(CRIME$less9thGrade)
  mean(CRIME$less9thGrade >= 0.5)
  
  mean(CRIME$age12t29)
  mean(CRIME$age12t29 >= 0.5)
  
  mean(CRIME$pctwhite >= 0.8) # 1990 US White Population = 80%
  # See https://web.archive.org/web/20190312090613/https://www.census.gov/population/www/documentation/twps0076/twps0076.pdf
  
  # We consider a community to be a minority if they have a higher minority pct than the country-wide pct
  race <- rep("Majority",nrow(CRIME))
  race[CRIME$pctwhite >= 0.8] <- "Minority"
  
  CRIME$race <- as.factor(race)
  
  CRIME$crimes <- as.integer(CRIME$crimes_per_pop <= 0.16)
  
  #------------------Training/Test Data--------------------------
  set.seed(seed)
  n.train <- round(frac * nrow(CRIME))
  index <- sample(1:n.train, n.train, replace=FALSE)
  data.train <- CRIME[index,]
  data.test <- CRIME[-index,]
  
  return(list(data = CRIME, train = data.train, test = data.test))
}

CRIME <- get.CRIME()

data.CRIME <- CRIME$data
colnames(data.CRIME)
head(data.CRIME, n = 5)

data.CRIME$race <- relevel(data.CRIME$race, "Majority")

#####################################
########## Causal Analysis ##########
#####################################

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
Z <- c("age12t29", "age65up","hhsize")
W <- c("urban", "percapinc", "underpov", "unemployed")
Y <- c("crimes")
fair_income <- fairness_cookbook(data.CRIME, X = X, W = W, Z = Z, Y = Y,
                                 x0 = "Majority", x1 = "Minority")
fair_income
summary(fair_income)

tapply(data.CRIME$crimes, data.CRIME$race, mean)
cat(paste("PG_{x_0,x_1}(y) =", round(100*(tapply(data.CRIME$crimes, data.CRIME$race, mean)[2]),digits = 2), "% - ",
          round(100*(tapply(data.CRIME$crimes, data.CRIME$race, mean)[1]),digits = 2), "% = ",
          round(100*(tapply(data.CRIME$crimes, data.CRIME$race, mean)[2]-tapply(data.CRIME$crimes, data.CRIME$race, mean)[1]),digits = 2),"%"))

# Causal decomposition of the TV measure (PG) for two-year recidivism.
autoplot(fair_income, decompose = "xspec", signed = FALSE) + 
  ggtitle(TeX("$Y$ disparity decomposition Comm. & Crimes"))

ctfde <- fair_income$measures[fair_income$measures$measure == "ctfde",]
ctfie <- fair_income$measures[fair_income$measures$measure == "ctfie",]
ctfse <- fair_income$measures[fair_income$measures$measure == "ctfse",]

ctfde_ci <- ctf.ci(ctfde$value, text = "x-DE CI-95%")
ctfie_ci <- ctf.ci(ctfie$value, text = "x-IE CI-95%")
ctfse_ci <- ctf.ci(ctfse$value, text = "x-SE CI-95%")

TVdecompos(ctfde$value,ctfie$value,ctfse$value)


