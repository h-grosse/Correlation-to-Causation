require(dplyr)
require(ggplot2)

#################################################
# Boostrap 95% CI for Confusion Matrix Measures #
#################################################

# Functions
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
  
  COMPAS$pred <- COMPAS$decile_score > limit
  COMPAS$truth <- COMPAS$two_year_recid == 1
  
  #------------------Training/Test Data--------------------------
  set.seed(seed)
  n.train <- round(frac * nrow(COMPAS))
  index <- sample(1:n.train, n.train, replace=FALSE)
  data.train <- COMPAS[index,]
  data.test <- COMPAS[-index,]
  
  COMPAS$race <- factor(COMPAS$race)
  
  return(list(data = COMPAS, train = data.train, test = data.test))

}

COMPAS.delim.graph <- function(size = 1, print.table = FALSE){
  
  temp <- get.COMPAS(limit = 1)$data
  tab <- conf.matrix(temp$pred,temp$truth)$CM.Ext
  
  for (i in 2:9) {
    temp <- get.COMPAS(limit = i)$data
    tab <- rbind(tab,conf.matrix(temp$pred,temp$truth)$CM.Ext)
  }
  if(print.table){print(data.frame(tab))}
  
  dat <- data.frame(cbind(value=c(tab),type = rep(colnames(tab),each = 9),delim = rep(1:9,times = 7)))
  dat$value <- as.numeric(dat$value)
  dat <- dat %>% mutate(label = if_else(delim == max(delim), as.character(type), NA_character_))
  
  ggplot(data = dat, aes(x = delim, y = value, group = type, colour = type)) + 
    geom_line(size = size) + 
    geom_label_repel(aes(label = label),
                     nudge_x = 1,
                     na.rm = TRUE) +
    scale_y_continuous(labels = percent) +
    scale_color_discrete(guide = "none") +
    labs(x = "Delimiter", y = "")
  
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

##############
### COMPAS ###
##############
require(scales)
require(ggrepel)

COMPAS <- get.COMPAS()

conf.matrix(COMPAS$data[COMPAS$data$sex == "Male",]$pred,COMPAS$data[COMPAS$data$sex == "Male",]$truth)
conf.matrix(COMPAS$data[COMPAS$data$sex == "Female",]$pred,COMPAS$data[COMPAS$data$sex == "Female",]$truth)

# Delimiter Analysis
COMPAS.delim.graph(print.table = TRUE)

# Graphs
parity.graph(COMPAS$data, race = c("White","Black"), main = "Algo: COMPAS", sub = "Whole Population")

parity.graph(COMPAS$data, sex = c("Male","Female"), main = "Algo: COMPAS", sub = "Whole Population")

parity.graph(COMPAS$data, race = c("White","Black"), sex = "Female", main = "Algo: COMPAS", sub = "Female Population")

parity.graph(COMPAS$data, sex = c("Male","Female"), race = "Black", main = "Algo: COMPAS", sub = "Black Population")




