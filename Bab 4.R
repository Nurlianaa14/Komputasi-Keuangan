#Nurliana NIM 11210940000026

#Contoh soal latihan Bab 4 dengan R

library(FinancialMath)
library(FinCal)
library(lifecontingencies)

#Contoh 4.1
perpetuitas_akhir<-annuity(0.1, 1/0,m=0,k=1,type="immediate")
perpetuitas_awal<-annuity(0.1, 1/0,m=0,k=1,type="due")
perpetuitas_akhir
perpetuitas_awal

#Contoh 4.2 
pv.annuity(0.055,pmt=25000000,n=12,type=0)*(0.055/log(1.055))*-1
fv.annuity(0.055,pmt=25000000,n=12,type=0)*(0.055/log(1.055))*-1

#Contoh 4.4 (hit nilai saat ini/pv akhir meningkat)
#p= nilai awalnya, q=naik brp
x<-annuity.arith(pv=NA,fv=NA,n=20,p=200000,q=200000,
                 i=0.02,ic=1,pf=1,imm=TRUE,plot=FALSE)[1,1]
x

#Contoh 4.5 (hit nilai saat ini awal meningkat)
PV=200000*increasingAnnuity(i=0.02,n=20,type="due")
PV

#Contoh 4.6 (hit nilai saat ini akhir menurun)
#q = -200.000 karna menurun
annuity.arith(pv=NA,n=20,p=4000000,q=-200000,
              i=0.04,ic=2,pf=2,imm=TRUE,plot=FALSE)

#Contoh 4.7 
anuitas<-data.frame(t(annuity.arith(p=2500000,q=250000,n=10,i=0.05)))
anuitas
anuitas$PV
anuitas$FV

#Contoh 4.8
#k=kelipatannya seperti r di deret geo)
anuitas<-data.frame(t(annuity.geo(n=20,p=5000000,k=0.01,i=0.02)))
anuitas
anuitas$PV
anuitas$FV
perpetuitas=data.frame(t(perpetuity.geo(p=5000000,k=0.01,i=0.02)))
perpetuitas
perpetuitas$PV

#Contoh 4.9
P1<-1000000
P2<-2000000
PV4<-3546000
v4<-0.8227
library(FinCal)
i<-function(v,n)
{
 vv<- v^(1/n)
 r<- (1-vv)/vv
 return(r)
}
i(0.8227,4)

PV8<-pv(r=i(0.8227,4),8,fv=0,pmt=-2000000,type=0)-PV4
PV8

#Contoh 4.10
anuitas1<-data.frame(t(annuity.level(pmt=40000000,n=10,i=0.08)))
anuitas2<-data.frame(t(annuity.level(pmt=20000000,n=5,i=0.08)))
anuitas3<-function(an,i,n){
anuitas<- an*(1+i)^(-n)
}
anuitas1$PV+anuitas3(anuitas2$PV,0.08,10)

#Contoh 4.11 (bingung)
FV1<-annuity.arith(fv=NA,n=5,p=1000000,q=1000000,i=0.083,ic=1
                   ,pf=1,imm=TRUE,plot=FALSE)[2,1]
FV1
PV2<-annuity.level(pv=NA,fv=NA, n=10,pmt=5000000,i=0.083,
                   ic=1,pf=1,imm=TRUE,plot=FALSE)[1,1]
PV2
#Nilai saat tahun ke 5=B
B=FV1+PV2
B
#Nilai saat ini semua anuitas =PV
n1=5
i=0.083
PV<-annuity.level(pv=NA,fv=B, n=5,pmt=NA,i=0.083,
                  ic=1,pf=1,imm=TRUE,plot=FALSE)[1,1]
PV
#Nilai masa mendatang semua anuitas=FV
FV<-annuity.level(pv=B,fv=NA, n=10,pmt=NA,i=0.083,
                  ic=1,pf=1,imm=TRUE,plot=FALSE)[2,1]
FV

#Contoh 4.12
pv1=pv(r=0.0367,n=5,fv=0,pmt=-50000000,type=0)
data=c(perpetuity.level(pmt=12500000,i=0.0367,imm=TRUE))
pv2=data*(1.0367)^(-5)
pv_total=pv2[1]+pv1
pv_total

#Contoh 4.13
y<-annuity.arith(pv=NA,fv=NA,n=38,p=15750000,q=250000,i=0.1225,
                 ic=1,pf=1,imm=TRUE,plot=FALSE)[1]
Y<-perpetuity.level(pv=NA,pmt=25000000,i=0.1225,ic=1,pf=1,imm=TRUE)
pv_total=y+(1.1225)^(-38)*Y
pv_total[1]

#Contoh 4.14
annuity.level(pv=30000000,fv=NA,n=120,pmt=NA,i=0.01,
              ic=1,pf=1,imm=TRUE)

#Contoh 4.15
pv1=pv(r=0.014889,n=28,fv=0,pmt=-15000000,type=0)
data=c(perpetuity.level(pmt=30000000,i=0.014889,imm=TRUE))
pv2=data*(1.0609)^(-10)
pv_total=pv1*(1.0609)^(-3)+pv2[1]
pv_total

#Contoh 4.16
n=((1.0542)^(1/12))-1
pv.perpetuity(n,-22500000,g=0,type=0)

#Contoh 4.17
i1<-EIR(r=0.04,p=2,type='p')
i1
i2<-EIR(r=0.08,p=12,type='p')
i2
FV_1<-(fv.annuity(r=i1,n=10,pmt=-100000000,type=1))*((1+i2)^24)
FV_1
FV_2<-(fv.annuity(r=i2,n=24,pmt=-10000000,type=1))
FV_2
FV<-FV_1+FV_2
FV
  
#Contoh 4.18
i_nom=0.09
n_nom=2
n_ef=12
bunga_efektif=(((i_nom/2)+1)^(n_nom/n_ef))-1;bunga_efektif
pv.annuity(r=0.007363,n=180,pmt=-200000,type=0)
fv.annuity(r=0.007363,n=180,pmt=-200000,type=0)


#Contoh 4.19
x<-annuity.arith(pv=NA,fv=NA,n=10,p=60000,q=60000,i=0.05,
                 ic=1,pf=1,imm=TRUE,plot=FALSE)[2]
FV_total=1000000+x
FV_total

#Contoh 4.21
fungsi_x<- function(i,r,pmt,n){
j<-((1+i)/(1+r))-1
pv.annuity(j,n,pmt,type=0)*-1
}
fungsi_x(0.06,0.03, 500000000,25)
