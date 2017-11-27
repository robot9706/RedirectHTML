#KÖRNYEZETTÖRLÉS 
rm(list=ls())
#CONSOLE TÖRLÉSE
#Ctrl+L

#Olvassuk be a stackloss adatbázist. Milyen módszer segítségével tudjuk az adatbázis méretét csökkenteni?

library(datasets)
import = stackloss
model = princomp(import[3:20, 2:3], cor = TRUE)
summary(model)
#ez a megoldás a fõkomponens analízishez tartozik
#oszlopokat szûrhetünk (2:3), elõtte meg a sorokat is leszûrhetjük(3:20)

#Helyes válasz: Fõkomponens-analízis


#Olvassuk be a stackloss adatbázist. Hány korrelálatlan változóval jellemezhetõ az adatbázis, 
#ha legfeljebb 5 százalékos információvesztést szeretnénk elérni?
library(datasets)
import = stackloss
model = princomp(import, cor = TRUE)
summary(model)
#1.00000000-0.98670691 még legfeljebb 0.05-ön belül van ez Comp.3
#1.00000000-0.9328791 már több mint 0.05, így nem lehet

#Megoldás: 3


#Olvassuk be a stackloss adatbázist. Pontosan hány százalék az információvesztés mértéke, 
#ha az utolsó két fõkomponenst elhagyjuk? Adjuk meg százalékjel nélkül.
library(datasets)
import = stackloss
model = princomp(import, cor = TRUE)
summary(model)

sum(0.05382785, 0.01329309) #Comp.4, Comp.3 2. sora ezek amiket elhagyunk
#VAGY
1.00000000-0.9328791 #Comp.2 információvesztés mértéke 3. sorban kivonva a Comp.4 3. sorából

#Megoldás: 6,71


#Olvassuk be a stackloss adatbázist. Dimenziócsökkentés során hogyan áll elõ a legfontosabb 
#új változó az eredeti változók lineáris kombinációjaként? Három tizedes jegyre kerekítve adjuk 
#meg az Air.Flow változó együtthatóját.

library(datasets)
import = stackloss
model = princomp(import, cor = TRUE)
summary(model)
(load = loadings(model))
#Comp.1 oszlop, 1. sor Air.Flow-nál

#Megoldás: -0,547 ± 1%
