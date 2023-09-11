# require(devtools)
# require(httpuv)
# devtools::install_github("dplecko/CFA")
require(faircause)
require(ggplot2)
require(latex2exp)

##########################
### Manually load data ###
##########################

get.Statlog <- function(frac = 0.6, seed = 15){
  Statlog <- read.table("german.data", sep = " ", fill = TRUE, strip.white = TRUE)
  Statlog <- as.data.frame(Statlog[,c(2,3,4,5,7,9,13,16,20,21)])
  
  colnames(Statlog) <- c("duration","credhist","purp","credamount",
                         "empl","pers_status","age",
                         "exist_cred","foreign","credit_risk")
  
  sex <- rep("Male",nrow(Statlog))
  sex[Statlog$pers_status == "A92"] <- "Female"
  sex[Statlog$pers_status == "A95"] <- "Female"
  
  single <- rep("No",nrow(Statlog))
  single[Statlog$pers_status == "A93"] <- "Yes"
  single[Statlog$pers_status == "A95"] <- "Yes"
  
  Statlog$sex <- as.factor(sex)
  Statlog$single <- as.factor(single)
  Statlog$credit_risk <- Statlog$credit_risk-1
  
  Statlog$pers_status <- NULL
  
  Statlog$sex <- relevel(Statlog$sex, "Male")
  
  #------------------Training/Test Data--------------------------
  set.seed(seed)
  n.train <- round(frac * nrow(Statlog))
  index <- sample(1:n.train, n.train, replace=FALSE)
  data.train <- Statlog[index,]
  data.test <- Statlog[-index,]
  
  return(list(data = Statlog, train = data.train, test = data.test))
}

baserates <- function(dat, x = "is_recid", y = "race"){
  temp <- xtabs(as.formula(paste0("~ ",x," + ",y)), data = dat)
  return(round(t(t(temp)/(temp[1,]+temp[2,])),digits=2))
}

Statlog <- get.Statlog()

data.Statlog <- Statlog$data

# Base rates
baserates(Statlog$data,x="credit_risk",y="sex")

#############################
# Measure Parity (only PPR) #
#############################

error.bar <- function(x, y, upper, lower=upper, length=0.08,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

cm.boot.bis <- function(data, B = 100){
  
  n <- nrow(data)
  Vboot <- integer(B)
  
  for (boot in 1:B){
    ## non-parametric bootstraop 
    bootsample <- sample(1:n, n, replace=TRUE)
    datb <- data[bootsample,]
    Vboot[boot] <- mean(datb$credit_risk)
  }
  
  return(list(m = mean(Vboot),s = sd(Vboot)))
}

# Colors
col <- c("skyblue" , "blue","DarkOrchid4","DarkOrchid1","DeepPink4")

# Bootstrap
set.seed(24)
dat <- data.Statlog

col <- col[c(4,3)]
n <- nrow(dat)

CMboot <- cm.boot.bis(dat[dat$sex == "Male",])
Mboot <- CMboot$m
Sboot <- CMboot$s

temp <- cm.boot.bis(dat[dat$sex == "Female",])
Mboot <- rbind(Mboot,temp$m)
Sboot <- rbind(Sboot,temp$s)

leg <- c("Female","Male")
# Graph
rownames(Mboot) <- leg
colnames(Mboot) <- c("PPR")
lim <- 1.2*max(Mboot)

stdev <- Sboot*qnorm(0.975)
rownames(stdev) <- leg
colnames(stdev) <- c("PPR")

ze_barplot <- barplot(Mboot, beside=T, space = c(0,0.5), legend.text=T, 
                      col=col, ylim=c(0,0.52),
                      main = "", sub = "", args.legend = list(14.7,y=0.52,xpd = TRUE,inset=c(-2, 1.15)))
error.bar(ze_barplot,Mboot, stdev, length = 0.24)

Mboot
Sboot
CI <- cbind(Mboot-stdev,Mboot+stdev)
colnames(CI) <- c("Lower","Upper")
CI

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
X <- "sex"
Z <- c("age","foreign")
W <- c("duration","single", "credhist", "purp", "credamount","empl","exist_cred")
Y <- c("credit_risk")
fair_credit <- fairness_cookbook(data.Statlog, X = X, W = W, Z = Z, Y = Y,
                                 x0 = "Male", x1 = "Female")

# Correction for "signed" bug
fair_credit$measures[fair_credit$measures$measure == "ctfie",]$value <- -fair_credit$measures[fair_credit$measures$measure == "ctfie",]$value

# Causal decomposition of the TV measure for German Credit.
autoplot(fair_credit, decompose = "xspec", signed = FALSE) + 
  ggtitle(TeX("$Y$ disparity decomposition German Credit"))

fair_credit$measures[fair_credit$measures$measure == "ctfie",]$value <- -fair_credit$measures[fair_credit$measures$measure == "ctfie",]$value

tv <- fair_credit$measures[fair_credit$measures$measure == "tv",]
ctfde <- fair_credit$measures[fair_credit$measures$measure == "ctfde",]
ctfie <- fair_credit$measures[fair_credit$measures$measure == "ctfie",]
ctfse <- fair_credit$measures[fair_credit$measures$measure == "ctfse",]

cat("Table :\n")

tv_ci <- ctf.ci(tv$value, text = "TV CI-95%")
ctfde_ci <- ctf.ci(ctfde$value, text = "x-DE CI-95%")
ctfie_ci <- ctf.ci(ctfie$value, text = "x-IE CI-95%")
ctfse_ci <- ctf.ci(ctfse$value, text = "x-SE CI-95%")

cat("Decomposition :\n")

TVdecompos(tv$value,ctfde$value,ctfie$value,ctfse$value)




