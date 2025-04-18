
library("RND")

df<- day1



#df[,1]<- (df[,1]-1670274000)/(60 * 60 * 24 * 365)




dfnew <- data.frame(matrix(ncol = 4, nrow = 0))


r <- 0.018 #from treasuries
for (i in 1:nrow(df)) {
  for (j in 2:(ncol(df)-1)){
    if (!is.na(df[i,j])) {
      S <- df[i,ncol(df)]
      TK <- as.numeric(gsub("\\D", "", colnames(df[j]) ))
      T<- as.numeric(substr(TK, 1, 6))
      K<- as.numeric(substr(TK, 7, 9))
      obs_price <- df[i,j]
      dfnew[nrow(dfnew) + 1,] = c(S,K,T,obs_price)
    }      
  }
}


dfnew$X3[dfnew$X3==230106]<-(32-8)/365
dfnew$X3[dfnew$X3==230217]<-(74-8)/365
dfnew$X3[dfnew$X3==230317]<-(102-8)/365
dfnew$X3[dfnew$X3==230421]<-(137-8)/365
dfnew$X3[dfnew$X3==230519]<-(165-8)/365
dfnew$X3[dfnew$X3==230616]<-(193-8)/365
dfnew$X3[dfnew$X3==230721]<-(228-8)/365
dfnew$X3[dfnew$X3==230915]<-(284-8)/365





#dfnew$X3[dfnew$X3==230106]<-32/365
#dfnew$X3[dfnew$X3==230217]<-74/365
#dfnew$X3[dfnew$X3==230317]<-102/365
#dfnew$X3[dfnew$X3==230421]<-137/365
#dfnew$X3[dfnew$X3==230519]<-165/365
#dfnew$X3[dfnew$X3==230616]<-193/365
#dfnew$X3[dfnew$X3==230721]<-228/365
#dfnew$X3[dfnew$X3==230915]<-284/365

colnames(dfnew) <- c("S", "K", "tau", "observed price")

write.csv(dfnew, "2022-12-13.csv")



testcol<-which(dfnew$X3==32/365)

dftestcol<-dfnew[testcol,]

compute.implied.volatility(r=0.018, te=dftestcol[1,3], 146.63, k=dftestcol[1,2], y=0, call.price=dftestcol[1,4],lower=0.1, upper=0.6)

ivtest<-1:nrow(dftestcol)

for(i in 1:nrow(dftestcol)){
  ivtest[i]<-compute.implied.volatility(r=0.018, te=dftestcol[i,3], 146.63, k=dftestcol[i,2], y=0, call.price=dftestcol[i,4],lower=0.1, upper=0.6)
}
  

plot(dftestcol[,2],ivtest,xlim=c(140,160))




