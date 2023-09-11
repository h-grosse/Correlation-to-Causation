# require(devtools)
# require(httpuv)
# devtools::install_github("dplecko/CFA")
require(faircause)
require(ggplot2)
require(latex2exp)

##########################
### Manually load data ###
##########################

get.ADULT <- function(frac = 0.5, seed = 15){
  ADULT <- read.table("ADULT.data", sep = ",", fill = TRUE, strip.white = TRUE)
  colnames(ADULT) <- c("age","workclass","fnlwgt","edu","edu_num","marital","occup","relat","race","sex","gain","loss","hpw","nati","income")
  
  ADULT$edu <- NULL
  ADULT$fnlwgt <- NULL
  ADULT$relat <- NULL
  ADULT$nati <- NULL
  ADULT$gain <- NULL
  ADULT$loss <- NULL
  
  # Work Class
  ADULT[ADULT$workclass == 'Federal-gov',]$workclass <- "Government"
  ADULT[ADULT$workclass == 'Local-gov',]$workclass <- 'Government'
  ADULT[ADULT$workclass == 'State-gov',]$workclass <- 'Government' 
  ADULT[ADULT$workclass == 'Self-emp-inc',]$workclass <-  'Self-Employed'
  ADULT[ADULT$workclass == 'Self-emp-not-inc',]$workclass <-  'Self-Employed'
  ADULT[ADULT$workclass == 'Never-worked',]$workclass <-  'Other/Unknown'
  ADULT[ADULT$workclass == 'Without-pay',]$workclass <-  'Other/Unknown'
  ADULT[ADULT$workclass == '?',]$workclass <-  'Other/Unknown'
  
  ADULT$workclass <- as.factor(ADULT$workclass)
  
  # Marital Status
  ADULT[ADULT$marital == 'Never-married',]$marital <-  'Single'
  ADULT[ADULT$marital == 'Married-AF-spouse',]$marital <-  'Married'
  ADULT[ADULT$marital == 'Married-civ-spouse',]$marital <-  'Married'
  ADULT[ADULT$marital == 'Married-spouse-absent',]$marital <-  'Married'
  
  ADULT$marital <- as.factor(ADULT$marital)
  
  # Race
  ADULT[ADULT$race == 'White',]$race <-  'Caucasian'
  ADULT[ADULT$race == 'Black',]$race <-  'African-American'
  ADULT[ADULT$race == 'Amer-Indian-Eskimo',]$race <-  'Native-American'
  ADULT[ADULT$race == 'Asian-Pac-Islander',]$race <-  'Asian'
  
  ADULT$race <- as.factor(ADULT$race)
  
  # Occupation
  ADULT[ADULT$occup == 'Adm-clerical',]$occup <-  'White-Collar'
  ADULT[ADULT$occup == 'Craft-repair',]$occup <-  'Blue-Collar'
  ADULT[ADULT$occup == 'Exec-managerial',]$occup <-  'White-Collar'
  ADULT[ADULT$occup == 'Farming-fishing',]$occup <-  'Blue-Collar'
  ADULT[ADULT$occup == 'Handlers-cleaners',]$occup <-   'Blue-Collar'
  ADULT[ADULT$occup == 'Machine-op-inspct',]$occup <-   'Blue-Collar'
  ADULT[ADULT$occup == 'Other-service',]$occup <-   'Service'
  ADULT[ADULT$occup == 'Priv-house-serv',]$occup <-   'Service'
  ADULT[ADULT$occup == 'Prof-specialty',]$occup <-   'Professional'
  ADULT[ADULT$occup == 'Protective-serv',]$occup <-   'Service'
  ADULT[ADULT$occup == 'Tech-support',]$occup <-   'Service'
  ADULT[ADULT$occup == 'Transport-moving',]$occup <-   'Blue-Collar'
  ADULT[ADULT$occup == '?',]$occup <-   'Other/Unknown'
  ADULT[ADULT$occup == 'Armed-Forces',]$occup <-   'Other/Unknown'
  
  ADULT$occup <- as.factor(ADULT$occup)
  ADULT$income <- as.factor(ADULT$income)
  ADULT$sex <- as.factor(ADULT$sex)
  
  levels(ADULT$race) <- c("Minority", "Minority", "Majority", "Minority",
                               "Minority")
  
  ADULT$race <- relevel(ADULT$race, "Majority")
  ADULT$income <- as.integer(ADULT$income == "<=50K")
  
  #------------------Training/Test Data--------------------------
  set.seed(seed)
  n.train <- round(frac * nrow(ADULT))
  index <- sample(1:n.train, n.train, replace=FALSE)
  data.train <- ADULT[index,]
  data.test <- ADULT[-index,]
  
  return(list(data = ADULT, train = data.train, test = data.test))
}

ADULT <- get.ADULT()

data.ADULT <- ADULT$data
training <- ADULT$train
training$income <- as.factor(training$income)

# Logistic Regression
ADULT.lr <- ADULT$test

glm1 <- glm(income ~ ., data = training, family = binomial('logit'))

ADULT.lr$prob <- predict(glm1, ADULT.lr, type = 'response')
ADULT.lr$pred <- as.integer(ADULT.lr$prob >= .5)

# NN
require(nnet)
nn1 <- nnet(income ~ ., data = training, size = 40, maxit = 500)

ADULT.nn <- ADULT$test

ADULT.nn$prob <- predict(nn1, newdata = ADULT.nn, type = 'raw')
ADULT.nn$pred <- as.integer(ADULT.nn$prob >= .5)

# Classification and Regression Tree (CART)
require(rpart)
tree <- rpart(income ~ ., data = training, method = 'class', cp = 1e-3)

ADULT.cart <- ADULT$test

ADULT.cart$prob <- predict(tree, newdata = ADULT.cart, type = 'prob')[,2]
ADULT.cart$pred <- predict(tree, newdata = ADULT.cart, type = 'class')

# Random Forest
require(randomForest)
rf <- randomForest(income ~ ., data = training, ntree = 1000)

ADULT.rf <- ADULT$test

ADULT.rf$prob <- predict(rf, newdata = ADULT.rf, type = 'prob')[,2]
ADULT.rf$pred <- predict(rf, newdata = ADULT.rf, type = 'class')

#####################################
########## Causal Analysis ##########
#####################################

ADULT_faircaus <- function(data, seed = 101, Y = "pred", title = "$Y$ disparity decomposition"){
  
  set.seed(seed)
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
  W <- c("marital", "edu_num", "workclass", "occup", "hpw")
  
  fair_income <- fairness_cookbook(data, X = X, W = W, Z = Z, Y = Y,
                                   x0 = "Majority", x1 = "Minority")
  fair_income
  summary(fair_income)
  
  tapply(data$income, data$race, mean)
  cat(paste("TV_{x_0,x_1}(y) =", round(100*(tapply(data$income, data$race, mean)[2]),digits = 2), "% - ",
            round(100*(tapply(data$income, data$race, mean)[1]),digits = 2), "% = ",
            round(100*(tapply(data$income, data$race, mean)[2]-tapply(data$income, data$race, mean)[1]),digits = 2),"%\n"))
  
  # Causal decomposition of the TV measure (PG) for two-year recidivism.
  plot <- autoplot(fair_income, decompose = "xspec", signed = FALSE) + 
    ggtitle(TeX(title))
  
  tv <- fair_income$measures[fair_income$measures$measure == "tv",]
  ctfde <- fair_income$measures[fair_income$measures$measure == "ctfde",]
  ctfie <- fair_income$measures[fair_income$measures$measure == "ctfie",]
  ctfse <- fair_income$measures[fair_income$measures$measure == "ctfse",]
  
  cat("Table :\n")
  
  tv_ci <- ctf.ci(tv$value, text = "TV CI-95%")
  ctfde_ci <- ctf.ci(ctfde$value, text = "x-DE CI-95%")
  ctfie_ci <- ctf.ci(ctfie$value, text = "x-IE CI-95%")
  ctfse_ci <- ctf.ci(ctfse$value, text = "x-SE CI-95%")
  
  cat("Decomposition :\n")
  
  TVdecompos(tv$value,ctfde$value,ctfie$value,ctfse$value)
  
  return(plot = plot)
  
}

ADULT_faircaus(data.ADULT, Y = "income", title = "Truth")

ADULT_faircaus(ADULT.lr, title = "LR")

ADULT_faircaus(ADULT.nn, title = "NN")

ADULT_faircaus(ADULT.cart, title = "CART")

ADULT_faircaus(ADULT.rf, title = "RF")





