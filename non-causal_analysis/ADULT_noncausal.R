require(dplyr)
require(ggplot2)

#################################################
# Boostrap 95% CI for Confusion Matrix Measures #
#################################################

# Functions
get.ADULT <- function(frac = 0.6, seed = 15){
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
  
  #------------------Training/Test Data--------------------------
  set.seed(seed)
  n.train <- round(frac * nrow(ADULT))
  index <- sample(1:n.train, n.train, replace=FALSE)
  data.train <- ADULT[index,]
  data.test <- ADULT[-index,]
  
  return(list(data = ADULT, train = data.train, test = data.test))
}

baserates <- function(dat, x = "is_recid", y = "race"){
  temp <- xtabs(as.formula(paste0("~ ",x," + ",y)), data = dat)
  return(round(t(t(temp)/(temp[1,]+temp[2,])),digits=2))
}

# M is an xtabs result which, by default, displays False before True
conf.matrix <- function(pred, truth){ 
  if(length(pred) != length(truth)){return(Error = "Yes")}
  
  TP <- length(which(pred & truth))
  FP <- length(which(pred & !truth))
  FN <- length(which(!pred & truth))
  TN <- length(which(!pred & !truth))
  
  M <- matrix(c(TP,FN,FP,TN), nrow = 2)
  colnames(M) <- c("P","N")
  rownames(M) <- c("PP","PN")
  
  P <- TP+FN
  N <- FP+TN
  PP <- TP+FP
  PN <- FN+TN
  n <- TP+FP+FN+TN
  
  if((P==0) || (N==0) || (PP==0) || (PN==0)){return(list(Error = "Yes"))}
  
  M2 <- matrix(nrow = 1, ncol = 7)
  
  colnames(M2) <- c("PPV", "NPV", "TPR", "TNR", "PPR", "p", "ACC")
  
  M2[1,1] <- TP/PP     # PPV
  M2[1,2] <- TN/PN     # NPV
  M2[1,3] <- TP/P      # TPR
  M2[1,4] <- TN/N      # TNR
  M2[1,5] <- PP/n      # PPR
  M2[1,6] <- P/n       # p
  M2[1,7] <- (TP+TN)/n # ACC
  
  return(list(CM = M, CM.Ext = M2, Error = "No"))
}

error.bar <- function(x, y, upper, lower=upper, length=0.08,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

cm.boot <- function(data, B = 100){
  
  n <- nrow(data)
  
  bootsample <- sample(1:n, n, replace=TRUE)
  datb <- data[bootsample,]
  
  Mboot <- conf.matrix(datb$pred,datb$truth)$CM.Ext
  
  for (boot in 2:B){
    ## non-parametric bootstraop 
    bootsample <- sample(1:n, n, replace=TRUE)
    datb <- data[bootsample,]
    temp <- conf.matrix(datb$pred,datb$truth)
    if(temp$Error == "No"){Mboot <- rbind(Mboot,temp$CM.Ext)}
  }
  
  return(list(m = sapply(as.data.frame(Mboot), function(x) mean(x)),
              s = sapply(as.data.frame(Mboot), function(x) sd(x))))
}

parity.graph <- function(df, race = "Whole", sex = "Whole",
                         seed = 24, B = 100, main = "", sub = "",
                         legend.x = 14.7){
  # Legend
  if(length(race)>1){leg <- race}
  if(length(sex)>1){leg <- sex}
  
  # Colors
  col <- c("skyblue" , "blue","DarkOrchid4","DarkOrchid1","DeepPink4")
  
  # Renaming Race
  race[race == "White"] <- "Caucasian"
  race[race == "Black"] <- "African-American"
  
  # Check Dimensions
  if(min(length(race),length(sex))>1){return("Error")}
  
  # Bootstrap
  set.seed(seed)
  dat <- df
  
  #------------------------Race Version--------------------------------
  if(length(race)>1){
    col <- col[1:length(race)]
    if(sex!="Whole"){dat <- df[df$sex == sex,]}
    n <- nrow(dat)
    
    if(race[1] == "Non-White"){
      CMboot <- cm.boot(dat[dat$race != "Caucasian",])
      Mboot <- CMboot$m
      Sboot <- CMboot$s
    }
    if(race[1] == "Non-Black"){
      CMboot <- cm.boot(dat[dat$race != "African-American",])
      Mboot <- CMboot$m
      Sboot <- CMboot$s
    }
    if((race[1] != "Non-White") && (race[1] != "Non-Black")){
      CMboot <- cm.boot(dat[dat$race == race[1],])
      Mboot <- CMboot$m
      Sboot <- CMboot$s
    }
    
    for (i in 2:length(race)) {
      if(race[i] == "Non-White"){
        temp <- cm.boot(dat[dat$race != "Caucasian",])
        Mboot <- rbind(Mboot,temp$m)
        Sboot <- rbind(Sboot,temp$s)
      }
      if(race[i] == "Non-Black"){
        temp <- cm.boot(dat[dat$race != "African-American",])
        Mboot <- rbind(Mboot,temp$m)
        Sboot <- rbind(Sboot,temp$s)
      }
      if((race[i] != "Non-White") && (race[i] != "Non-Black")){
        temp <- cm.boot(dat[dat$race == race[i],])
        Mboot <- rbind(Mboot,temp$m)
        Sboot <- rbind(Sboot,temp$s)
      }
    }
  }
  #------------------------Sex Version--------------------------------
  if(length(race)==1){
    col <- col[c(4,3)]
    if(race!="Whole"){dat <- df[df$race == race,]}
    n <- nrow(dat)
    
    CMboot <- cm.boot(dat[dat$sex == "Male",])
    Mboot <- CMboot$m
    Sboot <- CMboot$s
    
    temp <- cm.boot(dat[dat$sex == "Female",])
    Mboot <- rbind(Mboot,temp$m)
    Sboot <- rbind(Sboot,temp$s)
  }
  
  # Graph
  rownames(Mboot) <- leg
  colnames(Mboot) <- c("PPV", "NPV", "TPR", "TNR", "PPR", "p", "ACC")
  lim <- 1.2*max(Mboot)
  
  stdev <- Sboot*qnorm(0.975)
  rownames(stdev) <- leg
  colnames(stdev) <- c("PPV", "NPV", "TPR", "TNR", "PPR", "p", "ACC")
  
  ze_barplot <- barplot(Mboot, beside=T, space = c(0,0.5), legend.text=T, 
                        col=col, ylim=c(0,1),
                        main = main, sub = sub, args.legend = list(x=legend.x,y=1,xpd = TRUE,inset=c(-2, 1.15)))
  error.bar(ze_barplot,Mboot, stdev, length = 0.08)
  
}

######################
### ADULT analysis ###
######################

ADULT <- get.ADULT()

# Base rates
baserates(ADULT$data[ADULT$data$sex == "Male",],x="income")
baserates(ADULT$data[ADULT$data$sex == "Female",],x="income")

# Logistic Regression
ADULT.lr <- ADULT$test

glm1 <- glm(income ~ ., data = ADULT$train, family = binomial('logit'))

ADULT.lr$prob <- predict(glm1, ADULT.lr, type = 'response')
ADULT.lr$pred <- ADULT.lr$prob >= .5
ADULT.lr$truth <- ADULT.lr$income == ">50K"

# Graphs
parity.graph(ADULT.lr, race = c("White","Black"), main = "Algo: LR", sub = "Whole Population")
# parity.graph(ADULT.lr, sex = c("Male","Female"), main = "Algo: LR", sub = "Whole Population")
# parity.graph(ADULT.lr, race = c("White","Black"), sex = "Female", main = "Algo: LR", sub = "Female Population")
# parity.graph(ADULT.lr, sex = c("Male","Female"), race = "Black", main = "Algo: LR", sub = "Black Population")

# NN
require(nnet)
nn1 <- nnet(income ~ ., data = ADULT$train, size = 40, maxit = 500)

ADULT.nn <- ADULT$test

ADULT.nn$prob <- predict(nn1, newdata = ADULT.nn, type = 'raw')
ADULT.nn$pred <- ADULT.nn$prob >= .5
ADULT.nn$truth <- ADULT.nn$income == ">50K"

# Graphs
parity.graph(ADULT.nn, race = c("White","Black"), main = "Algo: NN", sub = "Whole Population")
# parity.graph(ADULT.nn, sex = c("Male","Female"), main = "Algo: NN", sub = "Whole Population")
# parity.graph(ADULT.nn, sex = c("Male","Female"), race = "Black", main = "Algo: NN", sub = "Black Population")
# parity.graph(ADULT.nn, race = c("White","Black"), sex = "Female", main = "Algo: NN", sub = "Female Population")

# Classification and Regression Tree (CART)
require(rpart)
tree <- rpart(income ~ ., data = ADULT$train, method = 'class', cp = 1e-3)

ADULT.cart <- ADULT$test

ADULT.cart$prob <- predict(tree, newdata = ADULT.cart, type = 'prob')[,2]
ADULT.cart$pred <- predict(tree, newdata = ADULT.cart, type = 'class') == ">50K"
ADULT.cart$truth <- ADULT.cart$income == ">50K"

# Graphs
parity.graph(ADULT.cart, race = c("White","Black"), main = "Algo: CART", sub = "Whole Population")
# parity.graph(ADULT.cart, sex = c("Male","Female"), main = "Algo: CART", sub = "Whole Population")
# parity.graph(ADULT.cart, sex = c("Male","Female"), race = "Black", main = "Algo: CART", sub = "Black Population")
# parity.graph(ADULT.cart, race = c("White","Black"), sex = "Female", main = "Algo: CART", sub = "Female Population")

# Random Forest
require(randomForest)
rf <- randomForest(income ~ ., data = ADULT$train, ntree = 1000)

ADULT.rf <- ADULT$test

ADULT.rf$prob <- predict(rf, newdata = ADULT.rf, type = 'prob')[,2]
ADULT.rf$pred <- predict(rf, newdata = ADULT.rf, type = 'class') == ">50K"
ADULT.rf$truth <- ADULT.rf$income == ">50K"

# Graphs
parity.graph(ADULT.rf, race = c("White","Black"), main = "Algo: RF", sub = "Whole Population")
# parity.graph(ADULT.rf, sex = c("Male","Female"), main = "Algo: RF", sub = "Whole Population")
# parity.graph(ADULT.rf, sex = c("Male","Female"), race = "Black", main = "Algo: RF", sub = "Black Population")
# parity.graph(ADULT.rf, race = c("White","Black"), sex = "Female", main = "Algo: RF", sub = "Female Population")

# Support Vector Machine (SVM)
require(kernlab)
set.seed(9)
svm <- ksvm(income ~ ., data = ADULT$train, prob.model = TRUE)

ADULT.svm <- ADULT$test

ADULT.svm$prob <- predict(svm, newdata = ADULT.svm, type = 'probabilities')[,2]
ADULT.svm$pred <- predict(svm, newdata = ADULT.svm, type = 'response') == ">50K"
ADULT.svm$truth <- ADULT.svm$income == ">50K"

# Graphs
parity.graph(ADULT.svm, race = c("White","Black"), main = "Algo: SVM", sub = "Whole Population")
# parity.graph(ADULT.svm, sex = c("Male","Female"), main = "Algo: SVM", sub = "Whole Population")
# parity.graph(ADULT.svm, sex = c("Male","Female"), race = "Black", main = "Algo: SVM", sub = "Black Population")
# parity.graph(ADULT.svm, race = c("White","Black"), sex = "Female", main = "Algo: SVM", sub = "Female Population")

# ROC
require(pROC)

roc.plot <- function(data){
  roc <- roc(data$income ~ data$prob, plot = FALSE, print.auc = TRUE, smooth = FALSE, direction = "<")
  return(list(curve = data.frame(spec = 1-roc$specificities, sens = roc$sensitivities), auc = as.numeric(roc$auc)))
}

roc.graph <- function(data1,data2,data3,data4,data5){
  
  CURVE.lr <- roc.plot(data1)$curve
  AUC.lr <- roc.plot(data1)$auc
  AUC.lrw <- roc.plot(data1[data1$race == "Caucasian",])$auc
  AUC.lrb <- roc.plot(data1[data1$race == "African-American",])$auc
  AUC.lrm <- roc.plot(data1[data1$sex == "Male",])$auc
  AUC.lrf <- roc.plot(data1[data1$sex == "Female",])$auc
  
  CURVE.nn <- roc.plot(data2)$curve
  AUC.nn <- roc.plot(data2)$auc
  AUC.nnw <- roc.plot(data2[data2$race == "Caucasian",])$auc
  AUC.nnb <- roc.plot(data2[data2$race == "African-American",])$auc
  AUC.nnm <- roc.plot(data2[data2$sex == "Male",])$auc
  AUC.nnf <- roc.plot(data2[data2$sex == "Female",])$auc
  
  CURVE.cart <- roc.plot(data3)$curve
  AUC.cart <- roc.plot(data3)$auc
  AUC.cartw <- roc.plot(data3[data3$race == "Caucasian",])$auc
  AUC.cartb <- roc.plot(data3[data3$race == "African-American",])$auc
  AUC.cartm <- roc.plot(data3[data3$sex == "Male",])$auc
  AUC.cartf <- roc.plot(data3[data3$sex == "Female",])$auc
  
  CURVE.rf <- roc.plot(data4)$curve
  AUC.rf <- roc.plot(data4)$auc
  AUC.rfw <- roc.plot(data4[data4$race == "Caucasian",])$auc
  AUC.rfb <- roc.plot(data4[data4$race == "African-American",])$auc
  AUC.rfm <- roc.plot(data4[data4$sex == "Male",])$auc
  AUC.rff <- roc.plot(data4[data4$sex == "Female",])$auc
  
  CURVE.svm <- roc.plot(data5)$curve
  AUC.svm <- roc.plot(data5)$auc
  AUC.svmw <- roc.plot(data5[data5$race == "Caucasian",])$auc
  AUC.svmb <- roc.plot(data5[data5$race == "African-American",])$auc
  AUC.svmm <- roc.plot(data5[data5$sex == "Male",])$auc
  AUC.svmf <- roc.plot(data5[data5$sex == "Female",])$auc
  
  # plot ROC curve for logistic regression
  p <- ggplot() + 
    geom_line(data = CURVE.lr, aes(x = spec, y = sens, color = 'LR'), size=1) + 
    geom_line(data = CURVE.nn, aes(x = spec, y = sens, color = 'NN'), size=1) + 
    geom_line(data = CURVE.cart, aes(x = spec, y = sens, color = 'CART'), size=1) + 
    geom_line(data = CURVE.rf, aes(x = spec, y = sens, color = 'RF'), size=1) + 
    geom_line(data = CURVE.svm, aes(x = spec, y = sens, color = 'SVM'), size=1) + 
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), size=1) +
    ggtitle('ROC Curve') + 
    labs(x = 'FPR', y = 'TPR') + 
    scale_colour_manual(name = 'Classifier',
                        values = c('LR'='#E69F00', 
                                   'NN'='#56B4E9', 'CART'='#009E73', 
                                   'RF'='#D55E00', 'SVM'='#0072B2'))
  
  AUC.sex <- data.frame(All = c(AUC.lr, AUC.nn, AUC.cart, AUC.rf, AUC.svm),
                        Male = c(AUC.lrm, AUC.nnm, AUC.cartm, AUC.rfm, AUC.svmm),
                        Female = c(AUC.lrf, AUC.nnf, AUC.cartf, AUC.rff, AUC.svmf),
                        Diff = c(AUC.lrm-AUC.lrf, AUC.nnm-AUC.nnf, AUC.cartm-AUC.cartf,
                                 AUC.rfm-AUC.rff, AUC.svmm-AUC.svmf))
  rownames(AUC.sex) <- c("LR","NN","CART","RF","SVM")
  print(round(AUC.sex,digits = 3))
  
  AUC.race <- data.frame(All = c(AUC.lr, AUC.nn, AUC.cart, AUC.rf, AUC.svm),
                         White = c(AUC.lrw, AUC.nnw, AUC.cartw, AUC.rfw, AUC.svmw),
                         Black = c(AUC.lrb, AUC.nnb, AUC.cartb, AUC.rfb, AUC.svmb),
                         Diff = c(AUC.lrw-AUC.lrb, AUC.nnw-AUC.nnb, AUC.cartw-AUC.cartb,
                                  AUC.rfw-AUC.rfb, AUC.svmw-AUC.svmb))
  rownames(AUC.race) <- c("LR","NN","CART","RF","SVM")
  print(round(AUC.race,digits = 3))
  
  return(p)
}

roc.graph(ADULT.lr,ADULT.nn,ADULT.cart,ADULT.rf,ADULT.svm)




