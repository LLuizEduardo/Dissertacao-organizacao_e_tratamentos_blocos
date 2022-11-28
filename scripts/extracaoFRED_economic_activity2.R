

# libraries -------------------------------------------------------------------
library(quantmod)   #FRED source
library(zoo)        #time series
library(dplyr)      #time filter
library(tseries)    #testes adf
library(seastests)  #sazonal test


# set folder ------------------------------------------------------------------
  rootFolder = 'C:/Users/luiz.eduardo/OneDrive/Documentos/Dissertação/base de dados - tratamento/'
  folder = '_EXTRACAO_GERAL_BLOCOS'
  setwd(paste0(rootFolder,folder))
  
  lista <- 
  economic_activity2 <- list('BSEMFT02BRM460S','BRALOCOBPORSTSAM','BRAPRMITO01IXOBSAM','CHNPRINTO01IXPYM','INDLOCOPMNOSTSAM','INDPRENEL01IXOBM','INDPRINTO01IXOBM','INDLOCOPGNOSTSAM','INDPRMITO01IXOBM','MEXLOCOBPNOSTSAM','MEXPRENTO01IXOBSAM','MEXPRINTO02IXOBSAM','MEXLOCOBPORSTSAM','BSEMFT02RUM460S','RUSPRINTO01GPSAM','RUSLOCOBPNOSTSAM','RUSPRMITO01IXOBSAM','CP0430TRM086NEST','CP0440TRM086NEST','TURLOCOPEORMLSAM','TURLOCOPENOSTSAM','CP0730TRM086NEST','SERVTRTRM086NEST','SERVCOTRM086NEST')
  
  
                substitute(
    economic_activity2
                )->nomeBloco
  
  

  
# extract code first (gdp)-----------------------------------------------------
c<-getSymbols(lista[[1]], src = 'FRED', env = NULL)
  c<-na.approx(c, rule = 3)
  c<-apply.monthly(c, mean, na.rm = TRUE)
  c<-data.frame(date = index(c), coredata(c))
  c$date<-substr(c$date, start = 1, stop = 7)
  
  
    
    
if (isSeasonal(c[,2], test = "combined", freq = 12) == TRUE) {
    c[13:nrow(c),2]<-diff(c[,2], lag = 12, diff = 1)
    c[1:12,2]<-NA
    plot.ts(c[,2])
    print("Seazonality")
}else{
    plot.ts(c[,2])
    print("ok")}


Ltemp<-ListaDesazonalizada<-c()
#loop
for (i in 2:(length(lista))) {
    d<-getSymbols(lista[[i]], src = 'FRED', env = NULL)
    d<-na.approx(d, rule = 3)
    d<-apply.monthly(d, mean, na.rm = TRUE)
    d<-data.frame(date = index(d), coredata(d))
    d$date<-substr(d$date, start = 1, stop = 7)
    
      if (isSeasonal(d[,2], test = "combined", freq = 12) == TRUE) {
         d[13:nrow(d),2]<-diff(d[,2], lag = 12, diff = 1)
         d[1:12,2]<-NA
        print(paste0("Seazonality - ",i))
        Ltemp<-c(lista[i])
        ListaDesazonalizada<-c(ListaDesazonalizada,Ltemp)
      }
      
      if (max(d$date) >= '2021-12' & min(d$date) <= '2000-12') {
        #f<-merge(f,d, by = c('date'), all = TRUE)
        c<-merge(c,d, by = c('date'), all = TRUE)
      }
}

  
  
  
#testing seasonality
for (i in 2:ncol(c)) {
  if (isSeasonal(c[,i], test = "combined", freq = 12) == TRUE) {
    plot.ts(c[,i], main = i )
    print(paste0('erro - ',i))
  }else{print(paste0('ok - ',i))}
}
  

BaseBruta<-c[,-1]  
rownames(BaseBruta)<-c[,1]  

# ajusting the window  -----------------------------------------------------------
d<-na.omit(c)
max(d$date)
min(d$date)



#========== seasonality

k<-d[,-1]
k<-ts(k)
rownames(k)<-d[,1]



# # adjust series (apply returns)
# 
 t<-c(
'FRALOCOBPORSTSAM'
,'FRAPRCNTO01GPSAM'
,'BVDEFT02FRM460S'
,'DEUBRODFT02STSAM'
,'BVEMTE02DEM460S'
,'BCEMFT02DEM460S'
,'DEUPRINTO01GPSAM'
,'DEUPRCNTO01GPSAM'
,'DEUCP120000GPM'
,'BSEMFT02BRM460S'
,'BRALOCOBPORSTSAM'
,'MEXLOCOBPORSTSAM'
,'BSEMFT02RUM460S'
,'RUSPRINTO01GPSAM'
,'BVEMTE02BEM460S'
,'BSEMFT02BEM460S'
,'BCEMFT02BEM460S'
,'BVDEFT02BEM460S'
,'BVDETE02BEM460S'
,'BCEMFT02DKM460S'
,'BSEMFT02DKM460S'
,'IRLCP120000GPM'
,'BVEMTE02NLM460S'
,'BSEMFT02NLM460S'
,'BVDEFT02NLM460S'
,'ESPBRODFT02STSAM'
,'BVEMTE02ESM460S'
,'BVEMFT02ESM460S'
,'BVDETE02ESM460S'
,'ESPCPHP1200GPM'
,'SWEPRINTO01GPSAM'
,'BVDEFT02SEM460S'
,'LREMTTTTAUM156S'
,'LFEMTTTTAUM657S'
,'KORPISPPR02GPM'
,'KORCP120000GPM'
,'CANSTMNIS01STM'
,'BSPRFT02FRM460S'
,'DEUPIEAMP02GPM'
,'BSFGLV02DEM460S'
,'DEUPIEAFD02GPM'
,'BSCICP02GBM460S'
,'CAPUTLGMFNS'
,'CAPUTLGMFDS'
,'CAPUTLGMFOS'
,'BSPRFT02BRM460S'
,'BSCICP02BRM460S'
,'BSCURT02BRM160S'
,'INDWPIAMP01GPM'
,'BSCURT02MXM160S'
,'BSCICP02RUM460S'
,'PIEAMP02BEM657N'
,'BSSPFT02BEM460S'
,'DNKPIEAMP02GPM'
,'DNKPIEAMP01GPM'
,'BSCICP02IEM460S'
,'BSSPFT02IEM460S'
,'IRLPIEAFD02GPM'
,'IRLPIEAMP01GPM'
,'BSCICP02NLM460S'
,'BSFGLV02PLM460S'
,'BSPRFT02PLM460S'
,'BSSPFT02ESM460S'
,'BSPRFT02ESM460S'
,'BSPRTE02ESM460S'
,'ESPPIEAMP02GPM'
,'BSSPFT02SEM460S'
,'BSPRFT02SEM460S'
,'BSCICP02CHM460S'
,'LCEAMN01ILM657S'
,'ISRPIEAMP02GPM'
,'PIEAMP02KRM659N'
,'BSSPFT02KRM460S'
,'BSOITE02KRM460S'
,'KORPRMNTO01GPSAM'
,'IRSTCI01CAM156N'
,'CPGRLE01FRM657N'
,'CSINFT02DEM460S'
,'CPGRLE01DEM657N'
,'IRSTCI01DEM156N'
,'DEULOCOSIORSTM'
,'IRLTLT01GBM156N'
,'MICH'
,'HQMCB100YR'
,'HQMCB10YRP'
,'HQMCB1YR'
,'BAMLC0A2CAAEY'
,'BAMLH0A3HYCEY'
,'DTP30A29'
,'CPN3M'
,'T3MFFM'
,'T10Y2YM'
,'T10Y3MM'
,'T6MFFM'
,'IRSTCB01BRM156N'
,'IRSTCI01BRM156N'
,'IRSTPI01INM156N'
,'INTDSRINM193N'
,'IRSTCB01INM156N'
,'IRSTCI01INM156N'
,'INTDSRTRM193N'
,'CPGRLE01BEM657N'
,'IR3TIB01BEM156N'
,'IRSTCI01BEM156N'
,'IRLTCT01BEM156N'
,'IRLTLT01BEM156N'
,'CPGRLE01IEM657N'
,'CSINFT02NLM460S'
,'IRLTLT01NLM156N'
,'CPGRLE01ESM657N'
,'CPGRLE01CHM657N'
,'IRSTCI01CHM156N'
,'INTDSRKRM193N'
,'IRSTCI01KRM156N'
,'KORLOCOSIORSTM'
,'BSXRLV02USM086S'
,'ARGXTEXVA01GPSAM'
,'XTEITT01BRM156S'
,'XTEXVA01BRM657S'
,'XTIMVA01BRM657S'
,'XTEITT01CNM156S'
,'XTEITT01INM156S'
,'XTEXVA01INM657S'
,'XTIMVA01INM657S'
,'XTEXVA01RUM657S'
,'TURLOCOTXORSTM'
,'XTIMVA01IEM657S'
,'XTEXVA01NOM657S'
,'XTIMVA01ESM657S'
,'XTEXVA01CHM657S'
,'XTEXVA01KRM657S'
,'XTIMVA01KRM657S'
,'LRHUTTTTMXM156S'
,'MEXLOCOEMORSTM'
,'LMUNRRTTATM156S'
,'LRHUTTFEATM156S'
,'LMUNRRTTNOM156S'
,'LRHUTTTTKRM156S')

## listaIntersecao<-Reduce(intersect,  list(lista, ListaDesazonalizada) )
#t<- t[!(t %in% ListaDesazonalizada)]
 
 

 for (i in 1:ncol(k)) {
   if (colnames(d[i+1]) %in% t || colnames(d[i+1]) %in% ListaDesazonalizada) {
     k[,i]<-k[,i]
   }else{
     k[,i]<-periodReturn(k[,i], period='monthly')
   }
 }
 
 # /100
 for (i in 1:ncol(k)) {
   if (colnames(d[i+1]) %in% t) {
     k[,i]<-k[,i]/100
   }
 }
 

View(k)

# c is a complete database without the financial market and data cutting
# d is a complete database without treatments
# k is database with calculated returns



for (i in 1:ncol(k)) {
  print( paste0(isSeasonal(k[,i], test = "combined", freq = 12),' - ',i))
}


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
 a<-1
 k2[,a]<- diff(k1[,a],lag = 1, differences = 1)


 k2<-k2[,-23]


a<-k2
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

