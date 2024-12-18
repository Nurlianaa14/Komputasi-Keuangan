#Nurliana NIM 1120940000026
#UAS Pengantar Komputasi Keuangan

library(FinancialMath)
library(FinCal)
library(lifecontingencies)

#NO 1
a1=lifecontingencies::annuity(0.0058,12,m=0,k=1,type="immediate")
b1=lifecontingencies::annuity(0.0058,12,m=12,k=1,type="immediate") 
c1=lifecontingencies::annuity(0.0058,276,m=24,k=1,type="immediate") 
pinjaman=200000000
piu=(pinjaman-(70000*b1)-(150000*c1))/(a1+b1+c1)
d1=lifecontingencies::annuity(0.0058,300,m=0,k=1,type="immediate")
e1=lifecontingencies::annuity(0.0058,288,m=0,k=1,type="immediate")
PV=(piu*a1)+(piu+70000)*b1+(piu+150000)*c1
R=PV/d1
Sisa_hutang=R*e1
Sisa_hutang

#NO 2
bond_Juli<- data.frame(t(bond(f=1500000,r=0.035,c=1500000,
                              n=3,i=0.045,ic=1,cf=1)))
bond_jul<- data.frame(t(bond_Juli$Price))
P1<- bond_jul+50000
Ho<-pv(r=0.035,n=132/182,fv=P1,type=0)
Ho

#NO 3 (a)
t1= 0.035
t2=0.04
t3=0.045
t4=0.05
P= (50000/(1+t1)^1)+(50000/(1+t2)^2)+(50000/(1+t3)^3)+(1050000/(1+t4)^4)
P

#NO 3 (b)
s4=0.05
s5=0.055
f45= ((1+s5)^5/(1+s4)^4)-1
f45*100
