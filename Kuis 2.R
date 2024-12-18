
#Nurliana NIM 11210940000026
#Quiz 2

library(FinCal)
library(FinancialMath)

#NO 1 
#Hit R=Besar cicilan
nilai_akumulasi<-annuity.level(pv=NA,fv=NA,n=35,pmt=1,i=0.07,
                               ic=1,pf=1,imm=TRUE)[2,1] #FV nya
manfaat<-annuity.level(pv=NA,fv=NA,n=10,pmt=150000000,i=0.09,
                       ic=1,pf=1,imm=FALSE)[1,1] #PV nya
nilai_R=manfaat/nilai_akumulasi
nilai_R

#NO 2
#Hit nilai saat ini anuitas akhir meningkat, secara aritmatika
Nilai_saat_ini<-annuity.arith(pv=NA,fv=NA,n=20,p=300000,q=300000,
                 i=0.025,ic=1,pf=1,imm=TRUE,plot=FALSE)[1,1] #PV nya
Nilai_saat_ini

#NO 3
anuitas_1<-data.frame(t(annuity.level(pmt=50000000,n=13,i=0.09)))[1,1]
anuitas_2<-data.frame(t(annuity.level(pmt=30000000,n=7,i=0.09)))[1,1]
anuitas_3<-function(an,i,n){
  anuitas<- an*(1+i)^(-n)
}
anuitas_1+anuitas_3(anuitas_2,0.09,13)
