# -----------------------------------------------------------------------------#
# This function plots the standardized residuals of a mixed model
# Please cite the paper where this plot was presented:
# Ian Meneghel Danilevicz, Pascal Bondon, Valdério Anselmo Reisen & Faradiba Sarquis Serpa (2023) 
# A longitudinal study of the influence of air pollutants on children: a robust multivariate approach, 
# Journal of Applied Statistics, DOI: 10.1080/02664763.2023.2272228
# Author: Ian Meneghel Danilevicz
# LICENSE: MIT License

plot_std_res = function(res, id, time, xlab="Time", ylab="Std. residuals"){
  mr1 = min(res)
  mr2 = max(res)
  mt1 = min(time)
  mt2 = max(time)
  id1 = id
  N = length(res)
  data = cbind(res, id, time, id1)
  data_sorted = data[order(data[,2],data[,3]), ]
  data_sorted[1,4] = 1
  c = 1
  init = data_sorted[1,2]
  for(i in 2:N){
    if(data_sorted[i,2]==init){
      data_sorted[i,4] = c 
    }else{
      c = c+1
      init = data_sorted[i,2]
      data_sorted[i,4] = c
    }
  }
  n = length(table(data_sorted[,4]))
  nis = table(data_sorted[,4])
  plot(c(mt1, mt2), c(mr1, mr2), main="", xlab=xlab, ylab=ylab, type="n")
  lines(data_sorted[1:nis[1],3], data_sorted[1:nis[1],1], col="gray")
  for(i in 2:n){
    n2 = sum(nis[1:i])
    n1 = n2 - nis[i] + 1
    lines(data_sorted[n1:n2,3], data_sorted[n1:n2,1], col="gray")    
  }
  lines(c(mt1, mt2),rep(2,2), lty=2)
  lines(c(mt1, mt2),rep(-2,2), lty=2)
}

# -----------------------------------------------------------------------------#
# Example
# fit a model

library(nlme)
fm1 = nlme(height ~ SSasymp(age, Asym, R0, lrc),
           data = Loblolly,
           fixed = Asym + R0 + lrc ~ 1,
           random = Asym ~ 1,
           start = c(Asym = 103, R0 = -8.5, lrc = -3.3))

# -----------------------------------------------------------------------------#
# objects 
time = Loblolly$age
id = Loblolly$Seed
res =residuals(object=fm1, type = "pearson") # this is equivalent to Ri in equation (3) in Danilevicz 2023

# -----------------------------------------------------------------------------#
# plot
plot_std_res(res, id, time, xlab="Time", ylab="Std. residuals")

# -----------------------------------------------------------------------------#