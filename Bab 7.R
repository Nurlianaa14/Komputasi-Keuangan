
#Nurliana NIM 11210940000026
#Contoh soal bab 7

#Contoh 7.1
s1<-0.02
s2<-0.03
s3<-0.035
s4<-0.04
t1<-1
t2<-2
t3<-3
t4<-4
hargaobligasi<-30000/((1+s1)^t1)+30000/((1+s2)^t2)+30000/((1+s3)^t3)+1030000/((1+s4)^t4)
hargaobligasi

#Contoh 7.2
s1<-0.02
s2<-0.03
s3<-0.035
s4<-0.04
s5<-0.045

f12<-(((((1+0.03)^2)/((1+0.02)^1))^(1/(2-1)))-1)*100;f12
f23<-(((((1+0.035)^3)/((1+0.03)^2))^(1/(3-2)))-1)*100;f23

paste("Forward rates di tahun kedua=",f12,"%")
paste("Forward rates di tahun ketiga=",f23,"%")

