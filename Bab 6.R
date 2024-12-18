
#Nurliana 11210940000026
#Contoh soal Bab 6

library(FinancialMath)
library(FinCal)

#Contoh 6.6
bond(1000000,0.05,1000000,30,0.06,ic=2,cf=2,t=NA,plot=FALSE)

#Contoh 6.7
bond(1000000,0.08,1000000,30,0.06,ic=2,cf=2,t=NA,plot=FALSE)

#Contoh 6.8
p<-function(f,r,n,i,c){
  p<-0
  for(k in 1:length(r)){
    nn<-sum(n)
    sn<-0
    for(j in k:length(r)){
      sn<-sn+n[j]
    }
    p<-p+f*r[k]*pv.annuity(i,n[k],-1)/((1+i)^(nn-sn))
  }
  p<-p+c/((1+i)^nn)
  return(p)
}
r<-c(0.04,0.045,0.05)
n<-c(10,10,20)
p(1000000,r,n,0.046,1000000)

#Contoh 6.11
F<-1000000
r<-0.03
i<-0.025
v<-1/(1+i)
n<-6
k<-5
amt5<-F*(r-i)*v^(n-k+1)
amt5

#Contoh 6.13
F<-1000000
r<-0.03
i<-0.035
v<-1/(1+i)
n<-6
k<-5
Amt5<-F*(r-i)*v^(n-k+1)
Amt5

#Contoh 6.14
bond(1000000,0.08,1000000,10,0.06,ic=1,cf=1,t=6,plot=FALSE)

#Contoh 6.16
harga=data.frame(t(bond(f=1000000,r=0.05,c=1000000,n=12,i=0.04,
                        ic=1)))
hargaobligasi=data.frame(t(harga$Price));hargaobligasi

#Contoh 6.17
harga=data.frame(t(bond(f=1000000,r=0.05,c=1000000,n=20,i=0.06,
                        ic=1)))
hargaobligasi=data.frame(t(harga$Price));hargaobligasi

#Conth 6.18
bondjuli=data.frame(t(bond(f=1000000,r=0.03,c=1000000,n=3,i=0.04,
                        ic=1,cf=1)))
bondjuli
bonjul=data.frame(t(bondjuli$Price))
bonjul
p2=bonjul+30000
p2
p=pv(r=0.04,n=136/181,fv=-p2,type=0)
p