
#Nurliana NIM 11210940000026
#Contoh soal bab 8

library(FinancialMath)
library(FinCal)
library(lifecontingencies)

#Contoh 8.3
cf=c(1000000,2000000,3000000)
t=c(1,2,3)
duration(cf,t,i=0.01)

#Contoh 8.4
cf=c(1000000,1000000,1000000)
t=c(1,2,3)
duration(cf,t,i=0.06)

#Contoh 8.5
bond(1000000,0.05,1000000,3,0.06,ic=1,cf=1,t=NA,plot=FALSE)[6]

#Contoh 8.9
cf_aset=c(59234580,64324590)
t=c(2,12)
convexity(cf_aset,t,i=0.05)
cf_liabilitas=c(120000000)
t1=c(6)
convexity(cf_liabilitas, t1,i=0.05)

#Contoh 8.10
D<-500
i<-0.05
g<-0.03
price<-D/(i-g)
price

#Nurliana NIM 11210940000026
#No 11
L1<-1000000
L2<-2000000
t1<-1
t2<-2
i1<-0.1
i2<-0.12
#Untuk liabilitas dengan jangka 1 tahun
Biaya1<-L1/(1+i1)^t1
#Untuk liabilitas dengan jangka 2 tahun
Biaya2<-L2/(1+i2)^t2
#Total biaya investasi yg diperlukan
Total<-Biaya1+Biaya2
Total