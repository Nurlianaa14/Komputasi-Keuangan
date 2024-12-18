#Nurliana
#NIM 11210940000026

#Latihan soal bab 3 dengan program R
library(FinancialMath)
library(FinCal)
library(lifecontingencies)

#contoh 3.1
#n=20 thn, i=0,025
annuity.level(pv=NA,fv=NA,n=20,pmt=1,i=0.025,ic=1,pf=1,imm=TRUE)

#contoh 3.2
#n=10 thn, i=0,025
annuity.level(pv=NA,fv=NA,n=10,pmt=1,i=0.025,ic=1,pf=1,imm=TRUE)

#contoh 3.3
#n=5, i=1%
anuitas_akhir<-annuity.level(pv=NA,fv=NA,n=5,pmt=1,i=0.01,ic=1,pf=1,imm=TRUE)
anuitas_akhir
anuitas_awal<-annuity.level(pv=NA,fv=NA,n=5,pmt=1,i=0.01,ic=1,pf=1,imm=FALSE)
anuitas_awal

#contoh 3.4
#n=15, i=12%
annuity_immediate<-annuity.level(pv=NA,fv=NA,n=15,pmt=1,i=0.12,ic=1,pf=1,imm=TRUE)
annuity_immediate
annuity_due<-annuity.level(pv=NA,fv=NA,n=15,pmt=1,i=0.12,ic=1,pf=1,imm=FALSE)
annuity_due

#contoh 3.5 (Hit PV dengan pembayaran P)
#n=5, i=8%, P=250000
annuity.level(pv=NA,fv=NA,n=5,pmt=250000,i=0.08,ic=1,pf=1,imm=TRUE)

#contoh 3.6
#Mencari Sn/FV
akumulasi<-annuity.level(pv=NA,fv=NA,n=30,pmt=1,i=0.06,ic=1,pf=1,imm=TRUE)
akumulasi
manfaat<-annuity.level(pv=NA,fv=NA,n=15,pmt=120000000,i=0.08,ic=1,pf=1,imm=FALSE)
manfaat
nilai_R=1.109308e+09/79.05819
nilai_R

#contoh 3.7 (hit PV dengan V^n)
pre_annuity<-function(delay,itr,pv_annuity){
faktor_diskon<-1/(1+itr)^delay
calculated<-faktor_diskon*pv_annuity
return(calculated)
}
pv_an<-round(pv.annuity(0.08,15,-120000000,0),3)
pre_annuity(30,0.06,pv_an)

#contoh 3.8
vpangkat6<-0.88797
i<-(1-vpangkat6^(1/6))/vpangkat6^(1/6)
i
lifecontingencies::annuity(i,6,m=0,k=1,type="immediate")

#contoh 3.9
a<-annuity(i=0.05000105,n=4,m=0,k=1,type="immediate")
p=17730000/a
b<-annuity(i=0.05000105,n=8,m=0,k=1,type="immediate")
PV=p*b
PV