#KÖRNYEZETTÖRLÉS 
rm(list=ls())
#CONSOLE TÖRLÉSE
#Ctrl+L

#Olvassuk be a „MASS” csomagból a „survey” adatbázist, mely egyetemi hallgatók kérdõíves felmérésének eredményét tartalmazza. 
#Teszteljük 5%-os szignifikancia szinten azt a nullhipotézist, hogy a dohányzási szokás (Smoke) független a testmozgás gyakoriságától (Exer). 
#1. p = 0,4828.
#2. A kontingencia táblázat tartalmaz 5-nél kisebb gyakoriságot, így a kis elemszám miatt a döntésünk nem megbízható.

library(MASS)
import = survey
attach(import)

#p értéke
gyak.tablazat = table(Smoke, Exer)
chisq.test(gyak.tablazat, correct = FALSE)
#VAGY
chisq.test(Smoke, Exer, correct = FALSE)

#kontigencia táblázat
chisq.test(gyak.tablazat, correct = FALSE)$expected
#NEM AD WARNINGOT -> NINCS 5-NÉL KISSEBB
#WARNINGOL -> VAN 5-NÉL KISSEBB
#(nekem warningolt és látszik, hogy van kissebb szóval nem hiszem, hogy jó a feladat)
#(gyakveztõl infóztam)

#Helyes válasz:
#  Az elsõ állítás igaz, a második hamis.



#Olvassuk be az airquality adatbázist. Vizsgáljuk az ózon mennyisége (Ozone) és a hõmérséklet (Temp) közötti kapcsolatot. 
#Írjuk fel az ózonmennyiséget a hõmérséklet exponenciális függvényeként Ozone = exp(a + b*Temp) alakban. 
#Várhatóan mekkora lesz az ózonmennyiség 80 °F hõmérséklet esetén?

import = airquality
attach(import)
plot(Ozone ~ Temp)
model = lm(log(Ozone) ~ Temp)
summary(model)
# Mi a regressziós görbe egyenlete? 
# Ozone = exp(a + b*Temp)
a = model$coefficient[1]
b = model$coefficient[2]
f = function(x) exp(a + b*x)
curve(f, add = TRUE)
f(80)
#summary(model)$r.squared

#Megoldás: 35,24238 ± 1%


#Olvassuk be az airquality adatbázist. Vizsgáljuk az ózon mennyisége (Ozone) és a hõmérséklet (Temp) közötti kapcsolatot. 
#Írjuk fel az ózonmennyiséget a hõmérséklet lineáris függvényeként. A hõmérséklet hány százalékban magyarázza meg az ózonmennyiséget? 
#Százalékjel nélkül adjuk meg.

import = airquality
attach(import)
model = lm(Ozone ~ Temp)
summary(model)

#Multiple R-squared:  0.4877

#Megoldás: 48,77 ± 1%


#Olvassuk be az airquality adatbázist. Vizsgáljuk az ózon mennyisége (Ozone) és a hõmérséklet (Temp) közötti kapcsolatot. 
#Írjuk fel az ózonmennyiséget a hõmérséklet lineáris függvényeként. Adjuk meg az egyenes y-tengelymetszetének becslését.

import = airquality
attach(import)
model = lm(Ozone ~ Temp)
summary(model)
# Ábrázoljuk a regressziós egyenest, és adjuk meg az egyenletét!  
abline(model)
(tengelymetszet = model$coefficient[1])

#Megoldás: -146,9955 ± 1%


#Olvassuk be az airquality adatbázist. Vizsgáljuk az ózon mennyisége (Ozone) és a hõmérséklet (Temp) közötti kapcsolatot. 
#Írjuk fel az ózonmennyiséget a hõmérséklet lineáris függvényeként. Adjuk meg az egyenes meredekségének becslését.

import = airquality
attach(import)
model = lm(Ozone ~ Temp)
summary(model)
# Ábrázoljuk a regressziós egyenest, és adjuk meg az egyenletét!  
abline(model)
(meredekseg = model$coefficient[2])

#Megoldás: 2,4287 ± 1%


#Olvassuk be az airquality adatbázist. Vizsgáljuk az ózon mennyisége (Ozone) és a hõmérséklet (Temp) közötti kapcsolatot. 
#Írjuk fel az ózonmennyiséget a hõmérséklet exponenciális függvényeként Ozone = exp(a + b*Temp) alakban. Adjuk meg a b együttható becslését.

import = airquality
attach(import)
plot(Ozone ~ Temp)
model = lm(log(Ozone) ~ Temp)
b = model$coefficient[2]
b

#Megoldás: 0,06750 ± 1%


#Olvassuk be az airquality adatbázist. Vizsgáljuk az ózon mennyisége (Ozone) és a hõmérséklet (Temp) közötti kapcsolatot. 
#Írjuk fel az ózonmennyiséget a hõmérséklet exponenciális függvényeként Ozone = exp(a + b*Temp) alakban. Adjuk meg az a együttható becslését.

import = airquality
attach(import)
plot(Ozone ~ Temp)
model = lm(log(Ozone) ~ Temp)
a = model$coefficient[1]
a

#Megoldás: -1,83797 ± 1%


#Olvassuk be a chickwts adatbázist. Teszteljük 5%-os szignifikancia szinten azt a nullhipotézist, hogy a csirkék átlagos tömege (weight) 
#független a táplálék típusától (feed).
#p < 0,05.
#A csirkék átlagos tömege független a táplálék típusától. 

import = chickwts
attach(import)

#p értéke
chisq.test(weight, feed, correct = FALSE)

#p < 0.05 tehát elutasítjuk, függ
#Ha p kissebb mint a szignifikancia szint, akkor a hipotézist elutasítjuk.

#Helyes válasz:
#  Az elsõ állítás igaz, a második hamis.