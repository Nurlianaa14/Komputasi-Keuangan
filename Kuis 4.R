
#Nurliana NIM 11210940000026
#Quiz 4 Pengantar Komputasi Keuangan

library(FinancialMath)

#No 1
s1=0.03
s2=0.035
s3=0.04
s4=0.045
s5=0.05

print("Forward rate di tahun ketiga")
print("Forward rate di tahun keempat")
f23=(((((1+s3)^3)/((1+s2)^2))^(1/(3-2)))-1)
f34=(((((1+s4)^4)/((1+s3)^3))^(1/(4-3)))-1)
paste("Forward rate di tahun ketiga= ",f23)
paste("Forward rate di tahun keempat= ",f34)

#NO 2
bond(1500000,0.06,1500000,5,0.07,ic=1,cf=1,t=NA,plot=FALSE)[6]
