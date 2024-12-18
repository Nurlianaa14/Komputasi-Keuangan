#Nurliana
#NIM 11210940000026

#Contoh soal yang menggunakan pemograman R

install.packages("FinCal",dependencies=TRUE)
install.packages("FinancialMath",dependencies=TRUE)
install.packages('lifecontingencies')

library(FinCal)
library(FinancialMath)
library(lifecontingencies)

#Contoh 1.4 (Hitung future value bunga sederhana)
fv_sederhana_rnup<-function(r,n,pv){
fvs=(1+n*r)*(pv)
return(fvs)
}
fv_sederhana_rndw<-function(r,n,pv){
fvs=(1+floor(n)*r)*(pv)
return(fvs)
}
fv_sederhana_rnup(0.1,4.25,50000000)
fv_sederhana_rndw(0.1,4.25,50000000)

#contoh 1.6 (Hitung future value dengan bunga majemuk)
fv.simple(0.1,10,-50000000)

#contoh 1.9 (Cari bunga efektif jk dik bunga nominal)
ef_interest<-function(nom_i,n){
efi<-(1+nom_i/n)^n-1
return(efi)
}
ef_interest(0.12,12)

#contoh 1.10 (Hitung future value jk dik bunga nominal)
# Cari i nya dulu
ef_interest <-function(nom_i,n){
  efi<-(1+nom_i/n)^n-1
  return (efi)
}
fv.simple(ef_interest(0.05,4),5,-50000000)

#Contoh 1.11
# Cari future value
a<-fv.simple(0.1,10,-10000000)
a
# Cari present value
b<-pv.simple(0.1,10,-10000000)
b

#Contoh 1.12
a<-pv.simple(0.08,0.5,-5000000)+pv.simple(0.08,1.5,-5000000)
a
b<-pv.simple(0.08,1,-10000000)
b

#contoh 1.14 (Hitung tingkat diskon efektif tahunan)
ef_diskon=function(nom_d,n){
efd=1-(1-nom_d/n)^n
return(efd)
}
ef_diskon(0.12,4)

#contoh 1.15 (Hitung tingkat diskon nominal tahunan jk dik bunga nominal tahunan)
ef_diskon_of_i<-function(nom_i,n,m){
efi<-(1-((1/(1+nom_i/n))^(n/m)))*m
return(efi)
}
ef_diskon_of_i(0.12,12,2)

#contoh 1.18 (Hitung akumulasi jika dik laju bunga)
fv_force_i<-function(i,n,k){
fvf<--k*exp(i[1]*n[1])*exp(i[2]*(n[2]-n[1]))
return(fvf)
}
fv_force_i(c(0.05,0.06),c(4,10),-1000000)

