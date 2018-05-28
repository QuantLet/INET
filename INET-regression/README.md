[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **INET-regression** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml


Name of QuantLet: INET-regression

Published in: Industry Interdependency Dynamics in a Network Context

Description: 'Regress the moving-window connectedness and clustering coefficients of industry network on volatilities of Fama-French three risk factors'

Keywords: 'risk factors, regression, connectedness, clustering, volatility'

Author: Ya Qian

Submitted: Ya Qian

Datafile: 'cc_mw.csv, conn_mw.csv, FF3factors.CSV'


```

### R Code
```r

#Close windows and clear variables                                                                   
graphics.off()
rm(list = ls(all=TRUE))

# install and load packages
libraries = c("stats")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# read market factor and firm characteristics
fffactors                 = read.csv("F-F_3Factors.csv", header = TRUE, sep = ",", dec = ".")
rownames(fffactors)       = fffactors[,1]
fffactors[,1]             = fffactors[,2]+fffactors[,5]
fffactors                 = fffactors[,c(1,3,4)]
factorvol                 = matrix(0,177,3)
for (i in 1:177){
  for (j in 1:3)
    factorvol[i,j]        = sd(fffactors[((3*i-2):(3*i+33)),j])
}
factorvol                 = as.data.frame(factorvol)

totalconn                 = read.csv("conn_mw.csv", header = TRUE, sep = ",", dec = ".")
rownames(totalconn)       = totalconn[,1]
totalconn                 = totalconn[,-1]
for (i in 1:3){
  lm_reg                  = lm(clustering[,i]~ factorvol[,1]+factorvol[,2]+factorvol[,3])
  summary(lm_reg)
}


clustering                = read.csv("cc_mw.csv", header = TRUE, sep = ",", dec = ".")
clustering                = clustering[,-1]
rownames(clustering)      = rownames(totalconn)
for (j in 1:12){
  lm_reg                  = lm(clustering[,j]~ factorvol[,1]+factorvol[,2]+factorvol[,3])
  summary(lm_reg)
}




```

automatically created on 2018-05-28