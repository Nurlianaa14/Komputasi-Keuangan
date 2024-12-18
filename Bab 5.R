#Nurliana NIM 11210940000026
#Contoh soal Bab 5

library(FinCal)
library(FinancialMath)

#Contoh 5.1
r<-function(l,i,n){
  rr<-l*((i/(1-(1+i)^(-n))))
  return(rr)
}
r(30000000,0.08,5)

#Contoh 5.2
PV<-30000000
PV12<-pv.annuity(r=0.08,pmt=-5000000,n=2)
a345<-pv.annuity(r=0.08,pmt=-1,n=3)*pv.simple(r=0.08,n=2,fv=-1)
P345<-(PV-PV12)/a345
P345

#Contoh 5.3
P<-function(l,i,R){
  B2=l*(1+i)^2-fv.annuity(i,2,-R,type=0)
  P3=R-i*B2
  P3
}
P(30000000,0.08,7513693.637)

#Contoh 5.4
principal<-function(P,i,f,e){
  return(P*(1+i)^(e-f))
}
principal(5522789.13,0.08,2,4)

#Contoh 5.5
amort.period(Loan=300000000,n=5,pmt=NA,i=0.08,t=3)

#Contoh 5.6
jumlah<-function(a,r,n){
  return(a*(1-r^n)/(1-r))
}
jumlah(1000000*(1.02^4/1.08),1.02/1.08,6)

#Contoh 5.7
data<-annuity.arith(pv=NA,i=0.06,n=6,p=1400000,q=100000,imm=TRUE)
data

#Contoh 5.8
amortisasi<-data.frame(t(amort.period(Loan=3000000000,n=360,i=0.005)))
amortisasi$PMT
amortisasi<-data.frame(t(amort.table(Loan=3000000000,n=360,i=0.005)))
amortisasi<-data.frame(t(amortisasi$Other$Other))
amortisasi$Loan
amortisasi$Total.Interest

#Contoh 5.11
L<-150000000
i<-0.1
j<-0.08
sn<-fv.annuity(j,10,-1)
sfd<-150000000/sn
int<-i*L
pmt<-int+sfd
pmt

#Contoh 5.12
sn<-fv.annuity(0.08,10,-1)
sfd<-150000000/sn
fv(0.08,3,-sfd)

