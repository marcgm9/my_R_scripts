S0<-100
K<-101
sigma<-0.3
r<-0.1
Ti<-1
dt<-1/365
t<-0

### Valoracio Black-Scholes
d_1<-(log(S0/K)+(r+1/2*sigma^2)*Ti)/sigma*sqrt(Ti)
d_2<-d_1-sigma*sqrt(Ti)
call<-S0*pnorm(d_1)-exp(-r*Ti)*K*pnorm(d_2)
put<-exp(-r*Ti)*K-S0+call

#d_12<-(log(S0/K)+(r+1/2*sigma^2)*(Ti-dt))/sigma*sqrt(Ti-dt)
#d_22<-d_12-sigma*sqrt(Ti-dt)
#call2<-S0*pnorm(d_12)-exp(-r*Ti)*K*pnorm(d_22)
#put2<-exp(-r*(Ti-dt))*K-S0+call

### Gregues de les opcions
thetaC<--S0*dnorm(d_1)*sigma/(2*sqrt(Ti-t))-r*K*exp(-r*(Ti-t))*pnorm(d_2)
thetaP<--S0*dnorm(d_1)*sigma/(2*sqrt(Ti-t))+r*K*exp(-r*(Ti-t))*pnorm(-d_2)
deltaC<-pnorm(d_1)
deltaP<--pnorm(-d_1)
gammaCP<-dnorm(d_1)/(S0*sigma*sqrt(Ti-t))

### Valoracio en l'instant inicial MC
n<-10^6 ## mida mostra
Z<-rnorm(n) ## v.a normal
SC<-S0*exp((r-0.5*sigma^2)*Ti+sigma*sqrt(Ti)*Z)
## S0s de la put amb MC
payoffP<-exp(-r*Ti)*pmax(K-SC,0) ## Això és la V(S,t) de la put
vputMC<-mean(payoffP)

## S0s de la call amb  MC
payoffC<-exp(-r*Ti)*pmax(SC-K,0) ## Això és la V(S,t) de la call
vcallMC<-mean(payoffC)

cartera<-payoffC+0.5*payoffP
cartera[0.99*n]
### Valoracio en S+dS,t+dt
SigmaC<-(S0*sigma*sqrt(dt))^2 ## Generem l'increment de S0s de la call
#dSC<-rnorm(n,sd=sqrt(SigmaC))
#SigmaP<-(S0*sigma*sqrt(dt))^2 ## Generem l'increment de S0s de la put
#dSP<-rnorm(n,sd=sqrt(SigmaP))

dS<-rnorm(n,mean=0,sd=(S0*sigma*sqrt(dt))) #amb aquesta el metode de dV "surt"
#dS<-S0*rnorm(n,sd=sigma^2*dt)
#dS<-r*SC*dt+sigma*SC*sqrt(dt)*Z

dt<-10/365
dV<--(dt*(thetaC+0.5*thetaP)+dS*(deltaC+0.5*deltaP)+(dS*dS)*0.5*1.5*gammaCP)
dV<-dV[order(dV)]
dV[0.99*n]


#dVC<--(dt*(thetaC)+dS*(deltaC)+dS^2*0.5*(gammaCP))
#dVP<--(dt*(0.5*thetaP)+dS*(0.5*deltaP)+dS^2*0.5*(0.5*gammaCP))
#dVTotal<-dVC+dVP
#dVTotal<-dVTotal[order(dVTotal)]
#dVTotal[0.99*n]*sqrt(10)

### Valoració put en S+dS,t+dt

#SC2<-SC+dS
#dS<-r*SC2*dt+sigma*SC2*sqrt(dt)*Z
dt<-10/365

SC2<-(S0)*exp((r-0.5*sigma^2)*dt+sigma*sqrt(dt)*Z) 

d_12<-(log(SC2/K)+(r+1/2*sigma^2)*(Ti-dt))/sigma*sqrt(Ti-dt)
d_22<-d_12-sigma*sqrt(Ti-dt)

call2<-SC2*pnorm(d_12)-exp(-r*(Ti-dt))*K*pnorm(d_22)
put2<-exp(-r*(Ti-dt))*K-SC2+call2

cartera<-call+0.5*put
cartera2<-call2+0.5*put2
loss<-cartera-cartera2
loss<-sort(loss)
loss[ceiling(0.99*n)]


#SP3<-(S0)+r*S0*dt+sigma*S0*Z*sqrt(dt)
#payoffP2<-exp(-r*(Ti-dt))*pmax(K-(SP+dS),0)

#dS<-rnorm(n,mean=0,sd=(S0*sigma*sqrt(dt)))
#payoffP2<-exp(-r*(Ti-dt))*pmax(K-(dS),0)
#vputMC2<-mean(payoffP2)

### Valoració call en S+dS,t+dt
#SC2<-SC+dS

#SC2<-(rep(S0,n)+dS)*exp((r-0.5*sigma^2)*(Ti-dt)+sigma*sqrt(Ti-dt)*Z)

#SC3<-(S0)+r*S0*dt+sigma*S0*Z*sqrt(dt)

#payoffC2<-exp(-r*(Ti-dt))*pmax((dS)-K,0)

payoffC2<-exp(-r*(Ti-dt))*pmax((dS)-K,0)
vcallMC2<-mean(payoffC2)

cartera2<-payoffC2+0.5*payoffP2
cartera2[0.99*n]
L<-(cartera)-(cartera2) ## Valoració de la cartera
L_ord<-sort(L)
L_ord[ceiling(0.99*n)]
L_ord[n]## Trobem el VaR


### Partial MC
dS<-rnorm(n,mean=0,sd=S0*sigma*sqrt(dt))
dVp<--((deltaC*dS+0.5*deltaP*dS))
dVp<-dVp[order(dVp)]
dVp[0.99*n]
