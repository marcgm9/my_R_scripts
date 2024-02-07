require(quantmod)
require(fBasics)
require(ggplot2)
require(tseries)
require(moments)


## Agafem les dades de l'actiu
getSymbols("JNJ",src="yahoo", start="2012-01-01", to="2022-10-25")
jhonson=JNJ$JNJ.Adjusted["2017-01-01/2022-10-20"]
summary(jhonson)
chart_Series(jhonson)
## variable per al número de xifres significatives
s=3
## funció per a fer l'histograma i llei normal amb els estadístics calculats
dens.plot<-function(v,main="title",xlab="xlab",ylab="ylab")
{
  
  v<-100*as.vector(v) #returns in percentage
  mv<-mean(v)
  sdv<-sd(v)
  xl<-c(mv-3*sdv,mv+3*sdv)
  dades<-data.frame(v=v)
  p<- ggplot(dades, aes(x=v)) + coord_cartesian(xlim = xl)+
    #histogram
    geom_histogram(aes(y=..density..),show.legend=F, position = "identity",
                   binwidth=(xl[2]-xl[1])/30,
                   colour="black", fill="white") +
    #empirical density esimation
    geom_density(alpha=.2, fill="#FF6666")  + #alpha=transparency 
    #normal density
    stat_function(fun = dnorm, args = list(mean = mv, sd = sdv),geom="area",
                  colour="black", fill="lightblue",alpha=0.4,n=1000)+
    #labels
    xlab(xlab)+ylab(ylab)
  
  p=p+ggtitle(main)
  return(p)
}

### Dades diàries de la companyia
jnjret=periodReturn(jhonson,period="daily")
jnjlogret_daily=periodReturn(jhonson,period="daily",type="log")
chart_Series(jnjlogret_daily)
plot(jnjret,lwd=3)
lines(jnjlogret_daily,col="gold")

##multipliquem per 100 per a tenir el percentatge del returns
#returns in percentage
ret_daily=jnjlogret_daily*100

##calculem els estadistics
me<-signif(mean(ret_daily),s);me
(std<-signif(sd(ret_daily),s))
(sk<-skewness(ret_daily))
(kurt<-kurtosis(ret_daily))
(exc.kurt<-kurtosis(ret_daily)-3)
(mx<-paste0(signif(max(ret_daily),s),"%"))
(min<-paste0(signif(min(ret_daily),s),"%"))

## histograma i boxplot
hist(jnjlogret_daily,probability=T, ylim=c(0, 30))
boxplot(jnjlogret_daily)
## llei normal amb els estadístics calculats
dens.plot(jnjlogret_daily,main="Daily returns",ylab="Density",xlab="Returns (%)")
## testos de normalitat
shapiro.test(as.vector(ret_daily))

jarque.bera.test(ret_daily)

## test de mitjana 0
t.test(as.vector(ret_daily))


### Dades setmanals de la companyia
jnjret=periodReturn(jhonson,period="weekly")
jnjlogret_weekly=periodReturn(jhonson,period="weekly",type="log")
chart_Series(jnjlogret_weekly)
plot(jnjret,lwd=3)
lines(jnjlogret_weekly,col="gold")

##multipliquem per 100 per a tenir el percentatge del returns
#returns in percentage
ret_weekly=jnjlogret_weekly*100

##calculem els estadistics
me<-signif(mean(ret_weekly),s);me
(std<-signif(sd(ret_weekly),s))
(sk<-skewness(ret_weekly))
(kurt<-kurtosis(ret_weekly))
(exc.kurt<-kurtosis(ret_weekly)-3)
(mx<-paste0(signif(max(ret_weekly),s),"%"))
(min<-paste0(signif(min(ret_weekly),s),"%"))

## histograma i boxplot
hist(jnjlogret_weekly,probability=T, ylim=c(0, 15))
boxplot(jnjlogret_weekly)
## llei normal amb els estadístics calculats
dens.plot(jnjlogret_weekly,main="Weekly returns",ylab="Density",xlab="Returns (%)")
## testos de normalitat
shapiro.test(as.vector(ret_weekly))

jarque.bera.test(ret_weekly)

## test de mitjana 0
t.test(as.vector(ret_weekly))


### Dades mensuals de la companyia
jnjret_monthly=periodReturn(jhonson,period="monthly")
jnjlogret_monthly=periodReturn(jhonson,period="monthly",type="log")
chart_Series(jnjlogret_monthly)
plot(jnjret_monthly,lwd=3)
lines(jnjlogret_monthly,col="gold")

##multipliquem per 100 per a tenir el percentatge del returns
#returns in percentage
ret_monthly=jnjlogret_monthly*100


##calculem els estadistics
me<-signif(mean(ret_monthly),s);me
(std<-signif(sd(ret_monthly),s))
(sk<-skewness(ret_monthly))
(kurt<-kurtosis(ret_monthly))
(exc.kurt<-kurtosis(ret_monthly)-3)
(mx<-paste0(signif(max(ret_monthly),s),"%"))
(min<-paste0(signif(min(ret_monthly),s),"%"))

## histograma i boxplot
hist(jnjlogret_monthly,probability=T, ylim=c(0, 15))
boxplot(jnjlogret_monthly)
## llei normal amb els estadístics calculats
dens.plot(jnjlogret_monthly,main="Monthly returns",ylab="Density",xlab="Returns (%)")
## testos de normalitat
shapiro.test(as.vector(ret_monthly))

jarque.bera.test(ret_monthly)

## test de mitjana 0
t.test(as.vector(ret_monthly))


### Anem ara amb les dades de l'índex de cotització
## N'agafem les dades
getSymbols("^NYA",src="yahoo",from="2017-01-01",to="2022-10-25")
NYSEad<-NYA$NYA.Adjusted["2017-01-01/2022-10-20"]
chart_Series(NYSEad)
nyse=cumsum((Delt(NYSEad)*100)[-1])
lim<-c(min(nyse),max(nyse))
plot(nyse,main="",ylim=lim,xlab="dates",ylab="%Beneficios")

summary(NYSEad)
chart_Series(NYSEad)

### Comencem amb els returns diaris
nyseret=periodReturn(NYSEad,period="daily")

nyselogret_daily=periodReturn(NYSEad,period="daily",type="log")
chart_Series(nyseret)
plot(nyseret,lwd=3)
lines(nyselogret_daily,col="gold")

##multipliquem per 100 per a tenir el percentatge del returns
#returns in percentage
index_daily=nyselogret_daily*100


##calculem els estadistics
me<-signif(mean(index_daily),s);me
(std<-signif(sd(index_daily),s))
(sk<-skewness(index_daily))
(kurt<-kurtosis(index_daily))
(exc.kurt<-kurtosis(index_daily)-3)
(mx<-paste0(signif(max(index_daily),s),"%"))
(min<-paste0(signif(min(index_daily),s),"%"))

## histograma i boxplot
hist(nyselogret_daily,probability=T, ylim=c(0, 15))
boxplot(nyselogret_daily)
dens.plot(nyselogret_daily,main="Daily returns",ylab="Density",xlab="Returns (%)")

## testos de normalitat
shapiro.test(as.vector(index_daily))

jarque.bera.test(index_daily)

## test de mitjana 0
t.test(as.vector(index_daily))


### Comencem amb els returns diaris
nyseret=periodReturn(NYSEad,period="daily")

nyselogret_weekly=periodReturn(NYSEad,period="daily",type="log")
chart_Series(nyseret)
plot(nyseret,lwd=3)
lines(nyselogret_weekly,col="gold")

##multipliquem per 100 per a tenir el percentatge del returns
#returns in percentage
index_weekly=nyselogret_weekly*100

##calculem els estadistics
me<-signif(mean(index_weekly),s);me
(std<-signif(sd(index_weekly),s))
(sk<-skewness(index_weekly))
(kurt<-kurtosis(index_weekly))
(exc.kurt<-kurtosis(index_weekly)-3)
(mx<-paste0(signif(max(index_weekly),s),"%"))
(min<-paste0(signif(min(index_weekly),s),"%"))

## histograma i boxplot
hist(nyselogret_weekly,probability=T, ylim=c(0, 15))
boxplot(nyselogret_weekly)
dens.plot(nyselogret_weekly,main="Weekly returns",ylab="Density",xlab="Returns (%)")

## testos de normalitat
shapiro.test(as.vector(index_weekly))

jarque.bera.test(index_weekly)

## test de mitjana 0
t.test(as.vector(index_weekly))

### Comencem amb els returns diaris
nyseret=periodReturn(NYSEad,period="daily")

nyselogret_monthly=periodReturn(NYSEad,period="daily",type="log")
chart_Series(nyseret)
plot(nyseret,lwd=3)
lines(nyselogret_monthly,col="gold")

##multipliquem per 100 per a tenir el percentatge del returns
#returns in percentage
index_monthly=nyselogret_monthly*100

##calculem els estadistics
me<-signif(mean(index_monthly),s);me
(std<-signif(sd(index_monthly),s))
(sk<-skewness(index_monthly))
(kurt<-kurtosis(index_monthly))
(exc.kurt<-kurtosis(index_monthly)-3)
(mx<-paste0(signif(max(index_monthly),s),"%"))
(min<-paste0(signif(min(index_monthly),s),"%"))

## histograma i boxplot
hist(nyselogret_monthly,probability=T, ylim=c(0, 15))
boxplot(nyselogret_monthly)
dens.plot(nyselogret_monthly,main="Monthly returns",ylab="Density",xlab="Returns (%)")

## testos de normalitat
shapiro.test(as.vector(index_monthly))

jarque.bera.test(index_monthly)

## test de mitjana 0
t.test(as.vector(index_monthly))


## testos de normalitat
shapiro.test(as.vector(v))

jarque.bera.test(v)

## test de mitjana 0
t.test(as.vector(v))
