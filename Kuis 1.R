#Nurliana
#NIM 11210940000026
#QUIZ 1

library(FinCal)
#Soal No 1

#Cari bunga efektifnya dulu
interest<-function(nom_i,n){
  i<-(1+nom_i/n)^n-1
  return(i)
}
bunga<-interest(0.13,12)
bunga

#Cari nilai saat ini
A<-pv.simple(bunga,1,-6000000)+pv.simple(bunga,2,-6000000)
A
B<-pv.simple(bunga,1.5,-12000000)
B

#Soal No 2
a<-1020000/1000000 #1+j1
a
b<-990000/970000  #1+j2
b
c<-1100000/1060000 #1+j3
c
d<-1050000/980000  #1+j4
d

#TWYR rekening ini
twyr<-(a*b*c*d)-1
twyr

#Soal No 3
saldo_awal<-250000000
saldo_akhir<-250000000
p<-70000000
q<-120000000
aktivitas<-p-q
I<-saldo_akhir-saldo_awal-aktivitas
sum_Ct<-p*(1-(4/12))-q*(1-(8/12))  #sigma Ct (1-t) 
i_dwyr<-I/(saldo_awal+sum_Ct)
i_dwyr
  
  