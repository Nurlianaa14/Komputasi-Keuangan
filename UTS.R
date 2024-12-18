
#Nurliana NIM 11210940000026
#UTS P.Komputasi Keuangan

library(FinCal)
library(FinancialMath)

#NO 1
i_1<-function(nom_i,n){
  efi<-(1+nom_i/n)^n-1
  return(efi)
}
i_1(0.12,12)

i_2<-function(nom_i,n){
  efi<-(1+nom_i/n)^n-1
  return(efi)
}
i_2(0.08,2)

fv_1=fv.simple(i_1(0.12,12),1,-2000000)
fv_2=fv.simple(i_2(0.08,2),3,-2000000)
fv__2=fv2*(1.1268)
fv=fv1+fv__2
fv

#NO 2
A=100000000
B=125000000
C="K"

#NO 3
nilai_fv<-annuity.level(pv=NA,fv=NA, n=25,pmt=1,i=0.04,
                         ic=1,pf=1,imm=TRUE)[2,1]
manfaat<-annuity.level(pv=NA,fv=NA, n=15,pmt=10000000,i=0.05,
                       ic=1,pf=1,imm=TRUE)[1,1]
nilai_X<-manfaat/nilai_fv
nilai_X

#NO 4
anuitas_1<-data.frame(t(annuity.geo(n=10,p=1000000,k=0.05,i=0.07)))
anuitas_2<-data.frame(t(annuity.geo(n=10,p=1000000,k=-0.05,i=0.07)))
PV=anuitas_1$PV+anuitas_2$PV*(1.07)^(-10)
PV
