#Latsol BAB 3

library(FinancialMath)
library(FinCal)

#NO 1
annuity.level(pv=NA,fv=NA,n=8,pmt=1,i=0.1,ic=1,pf=1,imm=TRUE)
R<-80000000/5.334926
R

#No 3
annuity.level(pv=NA,fv=NA,n=8,pmt=2500000,i=0.08,ic=1,pf=1,imm=TRUE)[1,1]

#N0 4
ann1<-annuity.level(pv=NA,fv=NA,n=5,pmt=10000000,i=0.06,ic=1,pf=1,imm=TRUE)[1,1]
ann1
ann2<-annuity.level(pv=NA,fv=NA,n=5,pmt=30000000,i=0.06,ic=1,pf=1,imm=TRUE)[1,1]
ann2
PV<-ann1+(ann2*(1/(1.06)^5))
PV

#N0 5
irr(cf=c(-100000000,25000000,25000000,25000000,25000000,25000000))

#No 6
fv1<-annuity.level(pv=NA,fv=NA,n=9,pmt=1,i=0.05,ic=1,pf=1,imm=FALSE)[2,1]
pembayaran=120000000/fv1
pembayaran

#No 7
akumulasi<-annuity.level(pv=NA,fv=NA,n=25,pmt=1,i=0.04,ic=1,pf=1,imm=TRUE)[2,1]
pv1<-annuity.level(pv=NA,fv=NA,n=15,pmt=10000000,i=0.05,ic=1,pf=1,imm=TRUE)[1,1]
X<-pv1/akumulasi
X

#No 9
pv2<-annuity.level(pv=NA,fv=NA,n=40,pmt=5000000,i=0.02,ic=1,pf=1,imm=TRUE)[1,1]
pv2
