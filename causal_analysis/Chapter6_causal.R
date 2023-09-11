# College Admissons
require(Rlab)
require(ggplot2)

#########
# Intro #
#########

ctf.ci <- function(x, alpha = 0.025, text = "CI-95%", cat = TRUE, signed = TRUE){
  m <- mean(x)
  s <- sd(x)
  margin <- qt(alpha,df=length(x)-1, lower.tail = FALSE)*s
  if(!signed){m <- abs(m)}
  if(cat){cat(text," = [",m-margin,",",m+margin,"] / mean = ",m,", sd = ",s,"\n")}
  return(c(l = m-margin, u = m+margin, m = m, margin = margin))
}

gen_admissions <- function(N = 100, x = 0.5, d = 0.5, dx = 0.2, y = 0.1, yx = 0, yd = 0.7){
  
  X <- rbern(N, prob = x)
  D <- rbern(N, prob = d+X*dx)
  Y <- rbern(N, prob = y+X*yx+D*yd)
  
  return(data.frame(cbind(X = X, D = D, Y = Y)))
  
}

gen_admissions.true <- function(N = 100, x0 = FALSE, x1 = FALSE, yx0 = FALSE, 
                                yx1 = FALSE, yx1Wx0 = FALSE, yx0_x0 = FALSE,
                                yx1_x0 = FALSE, yx1_x1 = FALSE, yx1Wx0_x0 = FALSE){
  
  temp <- gen_admissions(N = N)
  
  if(x0){temp <- gen_admissions(N = N, x = 0)}
  if(x1){temp <- gen_admissions(N = N, x = 1)}
  if(yx0){temp <- gen_admissions(N = N, x = 0)}
  if(yx1){temp <- gen_admissions(N = N, x = 1)}
  if(yx1Wx0){temp <- gen_admissions(N = N, dx = 0)}
  
  if(yx0_x0){temp <- gen_admissions(N = N, x = 0)}
  if(yx1_x0){temp <- gen_admissions(N = N, x = 1)}
  if(yx1_x1){temp <- gen_admissions(N = N, x = 1)}
  if(yx1Wx0_x0){temp <- gen_admissions(N = N, dx = 0)}
  
  return(temp)
  
}

gen_admissions.evil <- function(N = 100, x0 = FALSE, x1 = FALSE ,yx0 = FALSE,
                                yx1 = FALSE, yx1Wx0 = FALSE, yx0_x0 = FALSE,
                                yx1_x0 = FALSE, yx1_x1 = FALSE, yx1Wx0_x0 = FALSE){
  
  temp <- gen_admissions(N = N, yx = 0.14, yd = 0)
  
  if(x0){temp <- gen_admissions(N = N, yx = 0, yd = 0)}
  if(x1){temp <- gen_admissions(N = N, y = 0.24, yx = 0, yd = 0)}
  if(yx0){temp <- gen_admissions(N = N, yx = 0, yd = 0)}
  if(yx1){temp <- gen_admissions(N = N, y = 0.24, yx = 0, yd = 0)}
  if(yx1Wx0){temp <- gen_admissions(N = N, dx = 0, y = 0.24, yx = 0, yd = 0)}
  
  if(yx0_x0){temp <- gen_admissions(N = N, yx = 0, yd = 0)}
  if(yx1_x0){temp <- gen_admissions(N = N, y = 0.24, yx = 0, yd = 0)}
  if(yx1_x1){temp <- gen_admissions(N = N, y = 0.24, yx = 0, yd = 0)}
  if(yx1Wx0_x0){temp <- gen_admissions(N = N, dx = 0, y = 0.24, yx = 0, yd = 0)}
  
  return(temp)
  
}

gen_startup <- function(N = 100, x = 0, xu = 1, w = 0.3, yx0 = 0, yx1 = 0){
  
  U <- rnorm(N)
  X <- rbern(N, prob = x + xu*exp(U)/(1+exp(U)))
  Z <- rbern(N, prob = exp(U)/(1+exp(U)))
  W <- rbern(N, prob = w)
  Y <- rbern(N, prob = ((1-yx0-yx1)*X+yx1)*(1-2*Z)/5+Z/5+W/6)
  
  return(data.frame(cbind(U = U, X = X, Z = Z, W = W, Y = Y)))
  
}

gen_startup_know <- function(N = 100, x = 0, xu = 1, w = 0.3, yx0 = 0, yx1 = 0, knowing = 0){
  
  U <- integer(N)
  X <- integer(N)
  Z <- integer(N)
  
  i <- 0
  
  while (i < N) {
    Ub <- rnorm(1)
    Xb <- rbern(1, prob = x + xu*exp(Ub)/(1+exp(Ub)))
    if(Xb == knowing){
      i <- i+1
      U[i] <- Ub
      X[i] <- Xb
      Z[i] <- rbern(1, prob = exp(Ub)/(1+exp(Ub)))
    }
  }

  W <- rbern(N, prob = w)
  Y <- rbern(N, prob = ((1-yx0-yx1)*X+yx1)*(1-2*Z)/5+Z/5+W/6)
  
  return(data.frame(cbind(U = U, X = X, Z = Z, W = W, Y = Y)))
  
}

gen_startup.true <- function(N = 100, x0 = FALSE, x1 = FALSE, yx0 = FALSE, 
                             yx1 = FALSE, yx1Wx0 = FALSE, yx0_x0 = FALSE,
                             yx1_x0 = FALSE, yx1_x1 = FALSE, yx1Wx0_x0 = FALSE){
  
  temp <- gen_startup(N = N)
  
  if(x0){temp <- gen_startup(N = N, x = 0, xu = 0)}
  if(x1){temp <- gen_startup(N = N, x = 1, xu = 0)}
  if(yx0){temp <- gen_startup(N = N, yx0 = 1)}
  if(yx1){temp <- gen_startup(N = N, yx1 = 1)}
  if(yx1Wx0){temp <- gen_startup(N = N, x = 1, xu = 0)}
  
  if(yx0_x0){temp <- gen_startup_know(N = N, yx0 = 1, knowing = 0)}
  if(yx1_x0){temp <- gen_startup_know(N = N, yx1 = 1, knowing = 0)}
  if(yx1_x1){temp <- gen_startup_know(N = N, yx1 = 1, knowing = 1)}
  if(yx1Wx0_x0){temp <- gen_startup_know(N = N, x = 0, xu = 0, yx1 = 1, knowing = 0)}
  
  return(temp)
  
}

sim.general <- function(N = 100, gen.true, gen.evil = NULL, intercept = FALSE, breaks.width = 0.2){
  
  data_true.x0 <- gen.true(N = N, x0 = TRUE)
  data_true.x1 <- gen.true(N = N, x1 = TRUE)
  data_true.do_yx0 <- gen.true(N = N, yx0 = TRUE)
  data_true.do_yx1 <- gen.true(N = N, yx1 = TRUE)
  data_true.do_yx1_Wx0 <- gen.true(N = N, yx1Wx0 = TRUE)
  
  if(!is.null(gen.evil)){
    
    data_evil.x0 <- gen.evil(N = N, x0 = TRUE)
    data_evil.x1 <- gen.evil(N = N, x1 = TRUE)
    data_evil.do_yx0 <- gen.evil(N = N, yx0 = TRUE)
    data_evil.do_yx1 <- gen.evil(N = N, yx1 = TRUE)
    data_evil.do_yx1_Wx0 <- gen.evil(N = N, yx1Wx0 = TRUE)
    
  }
  
  N1 <- floor(N/10)*10
  D <- c(1:10,(6:50)*2,(11:(N1/10))*10)
  if(N >= 10000){
    N1 <- floor(N/1000)*1000
    D <- c(1:10,(6:50)*2,(11:100)*10,(11:100)*100,(11:(N1/1000))*1000)
  }
  lD <- length(D)

  dataTV.progr <- data.frame(N = c(D,D), TV = c(1:lD,1:lD), 
                             M = c(rep("true",lD),rep("evil",lD)))
  dataNDE.progr <- data.frame(N = c(D,D), NDE = c(1:lD,1:lD), 
                           M = c(rep("true",lD),rep("evil",lD)))
  dataNIE.progr <- data.frame(N = c(D,D), NIE = c(1:lD,1:lD), 
                            M = c(rep("true",lD),rep("evil",lD)))
  dataSE.progr <- data.frame(N = c(D,D), SE = c(1:lD,1:lD), 
                           M = c(rep("true",lD),rep("evil",lD)))
  
  if(is.null(gen.evil)){
    dataTV.progr <- data.frame(N = D, TV = 1:lD, 
                               M = rep("true",lD))
    dataNDE.progr <- data.frame(N = D, NDE = 1:lD, 
                                M = rep("true",lD))
    dataNIE.progr <- data.frame(N = D, NIE = 1:lD, 
                                M = rep("true",lD))
    dataSE.progr <- data.frame(N = D, SE = 1:lD, 
                               M = rep("true",lD))
  }

  for (i in 1:lD) {
    j <- D[i]
    dataTV.progr$TV[i] <- mean(data_true.x1[1:j,]$Y)-mean(data_true.x0[1:j,]$Y)
    dataNDE.progr$NDE[i] <- mean(data_true.do_yx1_Wx0[1:j,]$Y)-mean(data_true.do_yx0[1:j,]$Y)
    dataNIE.progr$NIE[i] <- mean(data_true.do_yx1_Wx0[1:j,]$Y)-mean(data_true.do_yx1[1:j,]$Y)
    dataSE.progr$SE[i] <- mean(data_true.do_yx0[1:j,]$Y)-mean(data_true.x0[1:j,]$Y)-
                          mean(data_true.do_yx1[1:j,]$Y)+mean(data_true.x1[1:j,]$Y)
    if(!is.null(gen.evil)){
      dataTV.progr$TV[i+lD] <- mean(data_evil.x1[1:j,]$Y)-mean(data_evil.x0[1:j,]$Y)
      dataNDE.progr$NDE[i+lD] <- mean(data_evil.do_yx1_Wx0[1:j,]$Y)-mean(data_evil.do_yx0[1:j,]$Y)
      dataNIE.progr$NIE[i+lD] <- mean(data_evil.do_yx1_Wx0[1:j,]$Y)-mean(data_evil.do_yx1[1:j,]$Y)
      dataSE.progr$SE[i+lD] <- mean(data_evil.do_yx0[1:j,]$Y)-mean(data_evil.x0[1:j,]$Y)-
                               mean(data_evil.do_yx1[1:j,]$Y)+mean(data_evil.x1[1:j,]$Y)
    }
    cat("[",i,"/",lD,"]","\n", sep = " ")
  }

  breaks.TV <- seq(-1,1,by = breaks.width)
  if(intercept){breaks.TV <- c(seq(-1,1,by = breaks.width),0.14)}
  breaks.NDE <- seq(-1,1,by = breaks.width)
  if(intercept){breaks.NDE <- c(seq(-1,1,by = breaks.width),0.14)}
  breaks.NIE <- seq(-1,1,by = breaks.width)
  if(intercept){breaks.NIE <- c(seq(-1,1,by = breaks.width),-0.14)}
  breaks.SE <- seq(-1,1,by = breaks.width)
  
  plot.TV <- ggplot(data = dataTV.progr, aes(x = N, y = TV, color = M)) + geom_line(linewidth = 1) + 
    scale_x_continuous(trans = 'log2', breaks = c(10^(0:9))) +
    scale_y_continuous(breaks = breaks.TV) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "N", y = "TV", colour = "Model") +
    theme(plot.title = element_text(hjust = 0.5))

  plot.NDE <- ggplot(data = dataNDE.progr, aes(x = N, y = NDE, color = M)) + geom_line(linewidth = 1) + 
    scale_x_continuous(trans = 'log2', breaks = c(10^(0:9))) +
    scale_y_continuous(breaks = breaks.NDE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "N", y = "NDE", colour = "Model") +
    theme(plot.title = element_text(hjust = 0.5))

  plot.NIE <- ggplot(data = dataNIE.progr, aes(x = N, y = NIE, color = M)) + geom_line(linewidth = 1) + 
    scale_x_continuous(trans = 'log2', breaks = c(10^(0:9))) +
    scale_y_continuous(breaks = breaks.NIE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "N", y = "NIE", colour = "Model") +
    theme(plot.title = element_text(hjust = 0.5))
  
  plot.SE <- ggplot(data = dataSE.progr, aes(x = N, y = SE, color = M)) + geom_line(linewidth = 1) + 
    scale_x_continuous(trans = 'log2', breaks = c(10^(0:9))) +
    scale_y_continuous(breaks = breaks.SE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "N", y = "Exp-SE", colour = "Model") +
    theme(plot.title = element_text(hjust = 0.5))
  
  if(intercept){
    plot.TV <- plot.TV  + geom_hline(yintercept = 0.14, linetype = "dashed")
    plot.NDE <- plot.NDE +
      geom_hline(yintercept = 0.14, linetype = "dashed")
    plot.NIE <- plot.NIE +
      geom_hline(yintercept = -0.14, linetype = "dashed")
  }
  
  TV.ci <- ctf.ci(data_true.x1$Y-data_true.x0$Y, text = "TV CI-95%")
  NDE.ci <- ctf.ci(data_true.do_yx1_Wx0$Y-data_true.do_yx0$Y, text = "NDE CI-95%")
  NIE.ci <- ctf.ci(data_true.do_yx1_Wx0$Y-data_true.do_yx1$Y, text = "NIE CI-95%")
  SE.ci <- ctf.ci(data_true.do_yx0$Y-data_true.x0$Y-data_true.do_yx1$Y+data_true.x1$Y, text = "SE CI-95%")
  
  return(list(TV = plot.TV, NDE = plot.NDE, NIE = plot.NIE, SE = plot.SE))
  
}

sim.x_spec <- function(N = 100, gen.true, gen.evil = NULL, intercept = FALSE, breaks.width = 0.2,
                       hTV = 0, hDE = 0, hIE = 0, hSE = 0){
  
  # Setup
  
  cat("0 / 6 \n", sep = " ")
  data_true.x0 <- gen.true(N = N, x0 = TRUE)
  cat("1 / 6 \n", sep = " ")
  data_true.x1 <- gen.true(N = N, x1 = TRUE)
  cat("2 / 6 \n", sep = " ")
  data_true.do_yx0_x0 <- gen.true(N = N, yx0_x0 = TRUE)
  cat("3 / 6 \n", sep = " ")
  data_true.do_yx1_x0 <- gen.true(N = N, yx1_x0 = TRUE)
  cat("4 / 6 \n", sep = " ")
  data_true.do_yx1_x1 <- gen.true(N = N, yx1_x1 = TRUE)
  cat("5 / 6 \n", sep = " ")
  data_true.do_yx1_Wx0_x0 <- gen.true(N = N, yx1Wx0_x0 = TRUE)
  
  cat("Setup Completed!\n", sep = " ")
  
  if(!is.null(gen.evil)){
    
    data_evil.x0 <- gen.evil(N = N, x0 = TRUE)
    data_evil.x1 <- gen.evil(N = N, x1 = TRUE)
    data_evil.do_yx0_x0 <- gen.evil(N = N, yx0_x0 = TRUE)
    data_evil.do_yx1_x0 <- gen.evil(N = N, yx1_x0 = TRUE)
    data_evil.do_yx1_x1 <- gen.evil(N = N, yx1_x1 = TRUE)
    data_evil.do_yx1_Wx0_x0 <- gen.evil(N = N, yx1Wx0_x0 = TRUE)
    
  }
  
  N1 <- floor(N/10)*10
  D <- c(1:10,(6:50)*2,(11:(N1/10))*10)
  if(N >= 10000){
    N1 <- floor(N/1000)*1000
    D <- c(1:10,(6:50)*2,(11:100)*10,(11:100)*100,(11:(N1/1000))*1000)
  }
  lD <- length(D)
  
  dataTV.progr <- data.frame(N = c(D,D), TV = c(1:lD,1:lD), 
                             M = c(rep("true",lD),rep("evil",lD)))
  data_xDE.progr <- data.frame(N = c(D,D), xDE = c(1:lD,1:lD), 
                               M = c(rep("true",lD),rep("evil",lD)))
  data_xIE.progr <- data.frame(N = c(D,D), xIE = c(1:lD,1:lD), 
                               M = c(rep("true",lD),rep("evil",lD)))
  data_xSE.progr <- data.frame(N = c(D,D), xSE = c(1:lD,1:lD), 
                               M = c(rep("true",lD),rep("evil",lD)))
  
  if(is.null(gen.evil)){
    dataTV.progr <- data.frame(N = D, TV = 1:lD, 
                               M = rep("true",lD))
    data_xDE.progr <- data.frame(N = D, xDE = 1:lD, 
                                 M = rep("true",lD))
    data_xIE.progr <- data.frame(N = D, xIE = 1:lD, 
                                 M = rep("true",lD))
    data_xSE.progr <- data.frame(N = D, xSE = 1:lD, 
                                 M = rep("true",lD))
  }
  
  for (i in 1:lD) {
    j <- D[i]
    dataTV.progr$TV[i] <- mean(data_true.x1[1:j,]$Y)-mean(data_true.x0[1:j,]$Y)
    data_xDE.progr$xDE[i] <- mean(data_true.do_yx1_Wx0_x0[1:j,]$Y)-mean(data_true.do_yx0_x0[1:j,]$Y)
    data_xIE.progr$xIE[i] <- mean(data_true.do_yx1_Wx0_x0[1:j,]$Y)-mean(data_true.do_yx1_x0[1:j,]$Y)
    data_xSE.progr$xSE[i] <- mean(data_true.do_yx1_x0[1:j,]$Y)-mean(data_true.do_yx1_x1[1:j,]$Y)
    if(!is.null(gen.evil)){
      dataTV.progr$TV[i+lD] <- mean(data_evil.x1[1:j,]$Y)-mean(data_evil.x0[1:j,]$Y)
      data_xDE.progr$xDE[i+lD] <- mean(data_evil.do_yx1_Wx0_x0[1:j,]$Y)-mean(data_evil.do_yx0_x0[1:j,]$Y)
      data_xIE.progr$xIE[i+lD] <- mean(data_evil.do_yx1_Wx0_x0[1:j,]$Y)-mean(data_evil.do_yx1_x0[1:j,]$Y)
      data_xSE.progr$xSE[i+lD] <- mean(data_evil.do_yx1_x0[1:j,]$Y)-mean(data_evil.do_yx1_x1[1:j,]$Y)
    }
    cat("D =",D[i],"|","(",i,"/",lD,")","\n", sep = " ")
  }
  summary(data_xDE.progr)
  
  breaks.TV <- seq(-1,1,by = breaks.width)
  if(intercept){breaks.TV <- c(seq(-1,1,by = breaks.width),hTV)}
  breaks.xDE <- seq(-1,1,by = breaks.width)
  if(intercept){breaks.xDE <- c(seq(-1,1,by = breaks.width),hDE)}
  breaks.xIE <- seq(-1,1,by = breaks.width)
  if(intercept){breaks.xIE <- c(seq(-1,1,by = breaks.width),hIE)}
  breaks.xSE <- seq(-1,1,by = breaks.width)
  if(intercept){breaks.xSE <- c(seq(-1,1,by = breaks.width),hSE)}
  
  plot.TV <- ggplot(data = dataTV.progr, aes(x = N, y = TV, color = M)) + geom_line(linewidth = 1) + 
    scale_x_continuous(trans = 'log2', breaks = c(10^(0:9))) +
    scale_y_continuous(breaks = breaks.TV) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "N", y = "TV", colour = "Model") +
    theme(plot.title = element_text(hjust = 0.5))
  
  plot.xDE <- ggplot(data = data_xDE.progr, aes(x = N, y = xDE, color = M)) + geom_line(linewidth = 1) + 
    scale_x_continuous(trans = 'log2', breaks = c(10^(0:9))) +
    scale_y_continuous(breaks = breaks.xDE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "N", y = "x-DE", colour = "Model") +
    theme(plot.title = element_text(hjust = 0.5))
  
  plot.xIE <- ggplot(data = data_xIE.progr, aes(x = N, y = xIE, color = M)) + geom_line(linewidth = 1) + 
    scale_x_continuous(trans = 'log2', breaks = c(10^(0:9))) +
    scale_y_continuous(breaks = breaks.xIE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "N", y = "x-IE", colour = "Model") +
    theme(plot.title = element_text(hjust = 0.5))
  
  plot.xSE <- ggplot(data = data_xSE.progr, aes(x = N, y = xSE, color = M)) + geom_line(linewidth = 1) + 
    scale_x_continuous(trans = 'log2', breaks = c(10^(0:9))) +
    scale_y_continuous(breaks = breaks.xSE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "N", y = "x-SE", colour = "Model") +
    theme(plot.title = element_text(hjust = 0.5))
  
  if(intercept){
    # plot.TV <- plot.TV  + geom_hline(yintercept = 0.14, linetype = "dashed")
    plot.xDE <- plot.xDE +
      geom_hline(yintercept = 0.018, linetype = "dashed")
    plot.xIE <- plot.xIE +
      geom_hline(yintercept = -0.018, linetype = "dashed")
    plot.xSE <- plot.xSE +
      geom_hline(yintercept = 0.036, linetype = "dashed")
  }
  
  TV.ci <- ctf.ci(data_true.x1$Y-data_true.x0$Y, text = "TV CI-95%")
  xDE.ci <- ctf.ci(data_true.do_yx1_Wx0_x0$Y-data_true.do_yx0_x0$Y, text = "x-DE CI-95%")
  xIE.ci <- ctf.ci(data_true.do_yx1_Wx0_x0$Y-data_true.do_yx1_x0$Y, text = "x-IE CI-95%")
  xSE.ci <- ctf.ci(data_true.do_yx1_x0$Y-data_true.do_yx1_x1$Y, text = "x-SE CI-95%")

  return(list(TV = plot.TV, xDE = plot.xDE, xIE = plot.xIE, xSE = plot.xSE))
  
}

##########
# Test 1 #
##########

set.seed(108)

plots <- sim.general(100000, gen_admissions.true, gen_admissions.evil, intercept = TRUE)

plots$TV
plots$NDE
plots$NIE
plots$SE

plots2 <- sim.general(100000, gen_admissions.true, intercept = TRUE)

plots2$TV
plots2$NDE
plots2$NIE
plots2$SE

##########
# Test 2 #
##########

set.seed(102)

plots3 <- sim.general(200000, gen_startup.true, breaks.width = 0.1)

plots3$TV
plots3$NDE
plots3$NIE
plots3$SE

# This illustrates the possible lack of power of the NDE measure, i.e. in this case,  
# there is a direct effect from X to Y, however the empirical NDE seems to tend to 0

##########
# Test 3 #
##########

set.seed(111)

plots4 <- sim.x_spec(400000, gen_startup.true, intercept = TRUE, breaks.width = 0.1,
                     hDE = 0.018, hIE = -0.018, hSE = 0.036)

plots4$TV
plots4$xDE
plots4$xIE
plots4$xSE



