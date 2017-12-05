# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
# set the working directory
setwd("/Users/qianya/Library/Mobile Documents/com~apple~CloudDocs/ffdata/test/fac")

# install and load packages
libraries = c("fGarch", "quantreg", "dplyr", "igraph", "qgraph")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
source("lqrl1_test.r")
source("quantilelasso.r")

#read datafile
D                = read.csv("49IPM1970.CSV", header = TRUE, sep = ",", dec = "." )
FAC              = read.csv("FF3factors.CSV", header = TRUE, sep = ",", dec = "." )
rownames(D)      = D[ ,1]
DD               = D[ ,-1]/100
rownames(FAC)    = FAC[,1]
FAC              = FAC[,-1]/100
DD               = DD - FAC[,4]
xvar             = cbind(DD, FAC[,1:3])

industrynames        = read.csv("industrynames.csv", header = TRUE, sep = ",")
industrynames        = industrynames[,-1]

# quantile level
tauvalues             = c(0.50,0.05,0.95)
filenames1            = c("beta_L_median","beta_L_lowertail","beta_L_uppertail")
filenames2            = c("preret_net_median","preret_net_lowertail","preret_net_uppertail")
filenames3            = c("preret_all_median","preret_all_lowertail","preret_all_uppertail")
for (s in 1:3){
  print(s)
  tau                  = tauvalues[s]
  
  # save predictive values into matrix
  returns              = matrix(0, 253, 4)
  pre_ret_net          = matrix(0, 253, 49)
  pre_ret_all          = matrix(0, 253, 49)
  # moving window estimation
  for (i in 1:253){
    print(i)
    xx1                = xvar[1:(311+i), ]
    
    # coefficients betas calculated from linear quantile lasso
    beta_l             = matrix(0, ncol(DD), (ncol(xx1)+1))
    for (k in 1:ncol(DD)) {
      cat("Industry:", k)
      # return of industry k
      yw               = as.matrix(xx1[-1, k])
      # returns of all industries
      xxw              = as.matrix(xx1[-nrow(xx1),])  
      
      # start the quantile lasso estimation for each firm in one specific moving window
      fit              = linear(yw, xxw, tau, i, k)
      beta_l[k, ]      = fit$beta_in
    }
    ret_net          = as.matrix(unlist(xx1[nrow(xx1),1:49 ]),ncol = 1)
    beta_net         = beta_l[ ,2:50]
    pre_ret_net[i,]  = beta_l%*%ret_net
    ret_all          = cbind(c(1, xx1[nrow(xx1),]))
    ret_all          = matrix(unlist(ret_all), ncol = 1)
    pre_ret_all[i,]  = beta_l%*%ret_all
    write.csv(beta_l, file = paste(filenames1[s], i, ".csv"))
  }     
  write.csv(pre_ret_net, file = paste(filenames2[s],".csv"))
  write.csv(pre_ret_all, file = paste(filenames3[s],".csv"))
}

