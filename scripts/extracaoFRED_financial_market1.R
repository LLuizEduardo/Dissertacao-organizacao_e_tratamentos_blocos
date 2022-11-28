

# libraries -------------------------------------------------------------------
library(quantmod)   #FRED source
library(zoo)        #time series
library(dplyr)      #time filter
library(tseries)    #testes adf
library(seastests)  #seasonal test


# set folder ------------------------------------------------------------------
  rootFolder = 'C:/Users/luiz.eduardo/OneDrive/Documentos/Dissertação/base de dados - tratamento/'
  folder = '_EXTRACAO_GERAL_BLOCOS'
  setwd(paste0(rootFolder,folder))
  
  lista <- 
    financial_market1 <- list('GSPC','DJI')
          substitute(
          financial_market1
          )->nomeBloco

  

  
# extract code first (gdp)-----------------------------------------------------
c<-getSymbols(paste0('^',lista[[1]]), src = 'yahoo', env = NULL, from = '1990-01-01', periodicity = 'monthly')
  
c<-c[,6]
c<-na.approx(c, rule = 3)
#c<-apply.quarterly(c, mean, na.rm = TRUE)
c<-data.frame(date = index(c), coredata(c))
c$date<-substr(c$date, start = 1, stop = 7)
names(c) <- sub(".Adjusted", "", names(c))


x<-getSymbols(paste0('^',lista[[2]]), src = 'yahoo', env = NULL, from = '1990-01-01', periodicity = 'monthly')

x<-x[,6]
x<-na.approx(x, rule = 3)
#x<-apply.quarterly(x, mean, na.rm = TRUE)
x<-data.frame(date = index(x), coredata(x))
x$date<-substr(x$date, start = 1, stop = 7)
names(x) <- sub(".Adjusted", "", names(x))

#x<-x[-305,]

c<-merge(c,x, by = c('date'), all = TRUE)  

rm(x)


BaseBruta<-c[,-1]  
rownames(BaseBruta)<-c[,1]  

# ajusting the window  -----------------------------------------------------------
d<-na.omit(c)
max(d$date)
min(d$date)

#c<-c[,-37]

#========== seasonality

k<-d[,-1]
k<-ts(k)
rownames(k)<-d[,1]




for (i in 1:ncol(k)) {
 # if (colnames(d[i+1]) %in% t || colnames(d[i+1]) %in% ListaDesazonalizada) {
 #   k[,i]<-k[,i]
 # }else{
   k[,i]<-periodReturn(k[,i], period='monthly')
# }
}
 


View(k)

# c is a complete database without the financial market and data cutting
# d is a complete database without treatments
# k is database with calculated returns



for (i in 1:ncol(k)) {
  print( paste0(isSeasonal(k[,i], test = "combined", freq = 12),' - ',i))
}
#k<-k[,-1]

# evaluating the plot of the graphic
for (i in 1:ncol(k)) {
  plot.ts(k[,i], main = i)
}


# testing stationarity of the series ---------------------------------------
k1<-k[-1,]
k2<-k1[-1,]
k3<-k2[-1,]
k4<-k3[-1,]

diffAp<-result<-pValores<-matrix(ncol = 4, nrow = ncol(k))

colnames(diffAp)<-colnames(result)<-colnames(pValores)<-c('1r','2r','3r','4r')
pValoresa<-pValores3<-pValores2<-pValores1<-pValores0<-list()


#=============================================================
for (i in 1:ncol(k)) {
  # 1r=======================================================
  pValores0[i]<-list(suppressWarnings(adf.test(k1[,i])))
  pValores[i,1]<-pValores0[[i]][["p.value"]]
 
  if (pValores[i,1] > 0.05){
     k2[,i]<- diff(k1[,i],lag = 1, differences = 1)
     diffAp[i,1]<-1
  }else{
    result[i,1]<-'Estacionario'
  }
  
  # 2r=======================================================
  pValores1[i]<-list(suppressWarnings(adf.test(k2[,i])))
  pValores[i,2]<-pValores1[[i]][["p.value"]]
  
  if (pValores[i,2] > 0.05){
    k3[,i]<- diff(k2[,i],lag = 1, differences = 1)
    diffAp[i,2]<-1
  }else{
    result[i,2]<-'Estacionario'
  }
  # 3r=======================================================
   pValores2[i]<-list(suppressWarnings(adf.test(k3[,i])))
   pValores[i,3]<-pValores2[[i]][["p.value"]]

  if (pValores[i,3] > 0.05){
     k4[,i]<- diff(k3[,i],lag = 1, differences = 1)
     diffAp[i,3]<-1
   }else{
     result[i,3]<-'Estacionario'
   }
   
   pValores3[i]<-list(suppressWarnings(adf.test(k4[,i])))
   pValores[i,4]<-pValores3[[i]][["p.value"]]
   if (pValores[i,3] > 0.05){
     k4[,i]<- diff(k3[,i],lag = 1, differences = 1)
     diffAp[i,4]<-1
   }else{
     result[i,4]<-'Estacionario'
   }
}
View(result)




# # testing out
# a<-1
# k2[,a]<- diff(k1[,a],lag = 1, differences = 1)


 #k2<-k2[,-34]


a<-k1
resulta<-matrix(ncol = 4, nrow = ncol(a))
#---------------------------------
for (i in 1:ncol(a)) {
  # 1r=======================================================
  pValoresa[i]<-list(suppressWarnings(adf.test(a[,i])))
  pValores[i,1]<-pValoresa[[i]][["p.value"]]
  
  if (pValores[i,1] < 0.05){
    resulta[i,2]<-'Estacionario'
  }
}
#---------------------------------
View(resulta)

rm(pValores0,pValores1,pValores2,pValores3,pValoresa)


row.names(diffAp)<-row.names(result)<-row.names(pValores)<-colnames(k)
row.names(diffAp)<-row.names(result)<-row.names(pValores)<-colnames(k)
row.names(resulta)<-colnames(a)
colnames(resulta)<-colnames(result)


base<-a
rm(a)


save.image(paste0(rootFolder,folder,'/',nomeBloco,'.RData'))
write.csv(base, paste0(rootFolder,folder,'/baseCsv/',nomeBloco, '.csv'))
saveRDS(base,paste0(rootFolder,folder,'/baseRData/',nomeBloco,'.RDS'))

