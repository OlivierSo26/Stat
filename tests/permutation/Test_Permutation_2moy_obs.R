### Test de Permutation pour comparer 2 moyennes observées, lorsque les conditions d'application de sont pas réunies

###exemple 1
#longueur de machoire inférieure chez des males et femelles chacals
mach<-c(120,107,110,116,114,111,113,117,114,112,110,111,107,108,110,105,107,106,111,111)
mach
xmin<-min(mach)
xmax<-max(mach)
sex<-c(rep("male", 10), rep("female", 10))
sex

cachal<-data.frame(mach, sex)
cachal
table(cachal$sex)

machmalo<-cachal[cachal$sex=="male",]
machfemalo<-cachal[cachal$sex=="female",]
mean(machmalo$mach)
mean(machfemalo$mach)
var((machmalo$mach))
var((machfemalo$mach))

13.88/5.15555

#on examine les données
par(mfrow=c(1,1))
boxplot(mach~sex)
par(mfrow=c(2,1))
hist(machmalo$mach,  xlim=c(xmin, xmax), proba=T, xlab="", main="males")
lines(c(mean(machmalo$mach),mean(machmalo$mach)), c(0,1), col="red")
mtext("moy", side=1, line=0, at=mean(machmalo$mach), col="red")
hist(machfemalo$mach,    xlim=c(xmin, xmax), xlab="", proba=T, main="females")
lines(c(mean(machfemalo$mach),mean(machfemalo$mach)), c(0,1), col="red")
mtext("moy", side=1, line=0, at=mean(machfemalo$mach), col="red")

#H0: µmales=µfemelles  on fait un test de Student pour comparer les moyennes
#on teste l'égalité des variances
with(cachal, var.test(mach~sex))

#on teste la normalité des distributions de X chez les mâles et chez les femelles
with(cachal, tapply(mach, sex, shapiro.test))

#graphiquement
with(cachal,hist(mach[sex=="male"],  xlim=c(xmin, xmax), xlab="", proba=T, main="males"))
lines(c(mean(machmalo$mach),mean(machmalo$mach)), c(0,1), col="red")
mtext("moy", side=1, line=0, at=mean(machmalo$mach), col="red")
x<-seq(100, 120, le=300)
lines(x, dnorm(x, mean(machmalo$mach), sd(machmalo$mach)), col="red")

with(cachal,hist(mach[sex=="female"],   xlim=c(xmin, xmax), xlab="", proba=T, main="females"))
lines(c(mean(machfemalo$mach),mean(machfemalo$mach)), c(0,1), col="red")
mtext("moy", side=1, line=0, at=mean(machfemalo$mach), col="red")
x<-seq(100, 120, le=300)
lines(x, dnorm(x, mean(machfemalo$mach), sd(machfemalo$mach)), col="red")

# homoscédasticité
var.test(machfemalo$mach, machmalo$mach)

#on fait le test t
with(cachal, t.test(mach~sex, var.equal=T))
t.test(machmalo$mach, machfemalo$mach, var.equal = T)

t0<-with(cachal, t.test(mach~sex, var.equal=T))$statistic

x<-seq(-4, 4, le=300)
plot(x, dt(x, df=18), type="l", col="blue")
lines(c(t0,t0), c(0,dt(t0, df=18)), col="blue")
mtext("t0", side=1, line=0, at=t0, col="blue")
pt(t0, 18)*2
t.test(machmalo$mach, machfemalo$mach, var.equal = T)$p.value

# On peut faire le test de permutation pour voir
# On simule une permutation aléatoire, en respectant la taille initiale des échantillons
# La permutation signifie qu'on simule ce que pourrait donner un échantillonnage sous H0
sample(cachal$sex)
table(sample(cachal$sex))
# le test t sur ces données permutées
with(cachal, t.test(mach~sample(sex), var.equal=T)) # une permutation
with(cachal, t.test(mach~sample(sex), var.equal=T)) # une nouvelle permutation
with(cachal, t.test(mach~sample(sex), var.equal=T)) #ce n'est toujours pas la même permutation (chaque fois qu'on lance la commande sample() on refait une permutation aléatoire)

TEST<-with(cachal, t.test(mach~sample(sex), var.equal=T)) # cette fois on fixe la simulation dans l'objet TEST (si on veut)
TEST  
TEST$statistic 
TEST$p.value

# On lance 5000 fois la fonction: on fait 5000 simulations rangées dans l'objet `simult`
simult <- with(cachal, replicate(5000, t.test(mach~sample(sex), var.equal=T)$statistic) )
#calcul de la probabilité critique sous H0: même longueur moyenne de machoire entre les sexes
sum(t(abs(simult))>abs(t0))/length(simult)
#on compare à celle obtenue sur l'échantillon réel
with(cachal, t.test(mach~sex, var.equal=T, alternative="two.sided"))$p.value

# On comprend graphiquement: on compare la distribution de réechantillonnage avec la loi normale ou de student
par(mfrow=c(1,1))
hist(simult, nclass=50, freq=FALSE, col="lightblue",  main="Distribution de la stat tobs sous H0") # les 5000 valeurs tobs (sachant H0 vraie)
curve(dt(x,df=18), col="blue", lwd=1, add=TRUE) # la loi de Student à 18ddl à laquelle tobs est censée appartenir (si ttes les conditions d'application sont réunies)
legend("topright",col="blue", lty=1, lwd=1, legend="loi t à 18ddl", cex=0.7)
mtext(text = "tobs", side=1, at=t0, col="red") #où se positionne la tobs issue de l'échantillon réel dans cette distribution?
#probabilité que, sous H0, on obtienne un tel échantillonnage, ou des moyennes d'échantillons mâles-femelles encore plus distincte (proba critique)
# calculée avec le test t seul
2*(pt(t0,18))  # surface sous la courbe de la loi t à 18ddl extérieure à l'intervalle (-tobs;tobs) car on fait un test bilatéral
# calculée avec le test de permutation
sum(t(abs(simult))>abs(t0))/length(simult)

# ça colle pas mal! 



### exemple 2: temps de survie (j) chez des malades atteints d'un cancer au poumon ou à l'estomac

survie<-c(124,42,25,45,412,51,1112,46,103,876,146,340,396, 1235,24,1581,1166,40,727,3808,791,1804,3460,719)
organe<-c(rep("estomac", 13), rep("poumon", 11))
surv<-data.frame(survie, organe)
xmin<-min(survie)
xmax<-max(survie)
View(surv)
table(surv$organe)
par(mfrow=c(1,1))
boxplot(surv$survie~surv$organe)

moyp<-mean(surv$survie[surv$organe=="poumon"])
moye<-mean(surv$survie[surv$organe=="estomac"])

with(surv, var.test(survie[organe=="poumon"], survie[organe=="estomac"])) #hétéroscédasticité
par(mfrow=c(2,1))
hist(surv$survie[surv$organe=="poumon"],  xlab="", proba=T, main="poumon", xlim=c(xmin, xmax))
lines(c(moyp, moyp), c(0,1), col="red")
hist(surv$survie[surv$organe=="estomac"],  xlab="survie (j)", proba=T,  main="estomac", xlim=c(xmin, xmax))
lines(c(moye, moye), c(0,1), col="red")
# test de normalité
with(surv, tapply(survie, organe, shapiro.test)) 
# test d'homoscédasticité
with(surv, var.test(survie~organe))

# on peut techniquement parlant faire un test de Student
t.test(survie~organe, data=surv)


# mais il est plus prudent de faire un test de permutation
# On simule une permutation aléatoire
sample(surv$organe)
table(sample(surv$organe))

#si H0 vraie (on découple la survie de l'organe touché par le cancer)
with(surv, t.test(survie~sample(organe), var.equal=T)$statistic)

# On lance 5000 fois le test dont on récupère la statistique observée
simult <- with(cachal, replicate(5000, t.test(survie~sample(organe), var.equal=T)$statistic) )

# Graphiques pour comparer la distribution de réechantillonnage avec la loi normale ou de student
par(mfrow=c(1,1))
hist(simult, nclass=50, freq=FALSE, col="lightblue",  main="Distribution de rééchantillonnage sous H0")
curve(dt(x,df=22), col="blue", lwd=1, add=TRUE)
legend("topright",col="blue", lty=1, lwd=1, legend="loi t à 22 ddl", cex=0.7)

#calcul de la probabilité critique sous H0: les temps de survie ont même moyenne pour l'estomac et les poumons
sum(t(abs(simult))>abs(t0))/length(simult)
with(surv, t.test(survie~organe, var.equal=T, alternative="two.sided"))$p.value


# conclusions stats inchangées ici (on rejette H0 au risque alpha), 
# mais la proba critique obtenue par le test de permutation est assez différente de celle fournie par le test de student


