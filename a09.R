# ===============================#
# Alkalmazott statisztika        #
# Fõkomponens-analízis           #
# ===============================#

# Olvassuk be a cars.txt állomány tartalmát az 'adat' nevû táblázatba.
# cars.txt: Az amerikai piacon forgalmazott néhány autótípus fontosabb paraméterei.
# MANUFACTURER: az autó típusa
# MODEL: az autó modellje
# COUNTRY: származási ország
# VOL: az utastér térfogata (köbláb)
# HP: teljesítmény (lóerõ)
# SP: végsebesség (mérföld/óra)
# MPG: fogyasztás (mérföld/gallon)
# WT: a jármû tömege (font)

adat = read.table("cars.txt", header = TRUE)
attach(adat)

# Vizsgáljuk meg a teljesítmény és a végsebesség korrelációját.
plot(SP ~ HP)
cor.test(HP, SP, method = "pearson")

# Végezzünk fõkomponens-analízist a két változón, majd ábrázoljuk a
# megfigyeléseket a fõkomponensek által meghatározott koordináta rendszerben.
model = princomp(~ HP + SP, cor = TRUE)
summary(model)
adat2 = data.frame(model$scores) # a fõkomponensek
plot(adat2)
cor(adat2)

# Hogyan írható fel a két fõkomponens a 'HP' és az 'SP' változó segítségével?
# Ellenõrizzük le, hogy ez az elõállítás valóban mûködik.
(load = loadings(model)) # a fõkomponensek aij együtthatói
hp.mean = mean(HP)
sp.mean = mean(SP)
n = length(HP)
hp.sd = sqrt(n-1)/sqrt(n)*sd(HP)
sp.sd = sqrt(n-1)/sqrt(n)*sd(SP)
comp1.check = load[1,1]*(HP-hp.mean)/hp.sd+load[2,1]*(SP-sp.mean)/sp.sd
comp2.check = load[1,2]*(HP-hp.mean)/hp.sd+load[2,2]*(SP-sp.mean)/sp.sd
check = data.frame(comp1.check, comp2.check)
(check.1 = sum(round(check$comp1.check, 8) != round(adat2$Comp.1, 8)))
(check.2 = sum(round(check$comp2.check, 8) != round(adat2$Comp.2, 8)))

# Hogyan írható fel a 'HP' és az 'SP' változó a két fõkomponens
# segítségével? Ellenõrizzük le, hogy ez az elõállítás valóban mûködik.
(load.inv = t(load))
hp.check = (load.inv[1,1]*adat2$Comp.1+load.inv[2,1]*adat2$Comp.2)*hp.sd+hp.mean
sp.check = (load.inv[1,2]*adat2$Comp.1+load.inv[2,2]*adat2$Comp.2)*sp.sd+sp.mean
check = data.frame(hp.check, sp.check)
(check.1 = sum(round(check$hp.check, 8) != round(adat$HP, 8)))
(check.2 = sum(round(check$sp.check, 8) != round(adat$SP, 8)))

# Mennyire jó a modell illeszkedése, ha a formulákból elhagyjuk a második
# fõkomponenst?
summary(model)

# =================================================================================

# Végezzünk fõkomponens-analízist mind az öt változón. Hogyan írható fel
# az elsõ fõkomponens a változók segítségével? Hogyan írható fel a 'HP'
# változó a fõkomponensek segítségével?

model = princomp(~ VOL + HP + MPG + SP + WT, cor = TRUE)
(load = loadings(model)) # a fõkomponensek aij együtthatói
(load.inv = t(load))

# Hány fõkomponensre van szükség, ha legfeljebb 10 százalékos 
# információvesztést szeretnénk elérni?

summary(model)

detach(adat)

# =================================================================================

# A "wine" adatbázis Olaszország 3 különbözõ területérõl származõ borok 
# különbözõ kémiai vegyületeinek koncentrációját tartalmazza.
# Próbáljuk meg az adatbázis méretét csökkenteni.
# Hány korrelálatlan változóval jellemezhetõ az adatbázis, 
# ha legfeljebb 15 százalékos információvesztést szeretnénk elérni? 
# Pontosan mekkora az információvesztés mértéke?

wine = read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", 
                header = FALSE)
model = princomp(wine[2:14], cor = TRUE)
summary(model)
# Hogyan áll elõ az elsõ fõkomponens az eredeti változók lineáris kombinációjaként? 
(load = loadings(model))
# Hogyan áll elõ az eredeti V2 változó a kapott fõkomponensek lineáris kombinációjaként?
(load.inv = t(load))
