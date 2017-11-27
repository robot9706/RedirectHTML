# ===============================#
# Alkalmazott statisztika        #
# Diszkriminancia-analízis       #
# Klaszter-analízis              #
# ===============================#
  
# Olvassuk be a beépített "iris" állomány tartalmát az 'input' nevû
# táblázatba. Az állomány három Kanadában megtalálható íriszfajtától 
# származó megfigyeléseket tartalmaz.
# Sepal.Length: csészelevél hossza (cm)
# Sepal.Width: csészelevél szélessége (cm)
# Petal.Length: sziromlevél hossza (cm)
# Petal.Width: sziromlevél szélessége (cm)
# Species: a növény fajtája

input = data.frame(iris)
attach(input)

# Töltsük be a 'MASS' csomagot.

library(MASS)

# Ábrázoljuk a változókat páronként koordináta rendszerben.

plot(input[1:4], col = c("red", "blue", "green")[Species])
levels(Species)

# A három közül melyik az a fajta, mely jól elkülönül a másik kettõtõl?

# Végezzünk diszkriminancia-analízist a mintára a négy változó
# alkalmazásával, és határozzuk meg a diszkriminancia függvények egyenletét.

model = lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width)
model
mean(Sepal.Length)
mean(Sepal.Width)
mean(Petal.Length)
mean(Petal.Width)

# Határozzuk meg, hogy az egyes megfigyeléseknek mik a koordinátáik
# a diszkriminancia függvények által meghatározott koordináta-rendszerben.
# Határozzuk meg a három csoportközéppont koordinátáját is.

pred = data.frame(predict(model))
check.ld1 = scale(input[1:4], center = T, scale = F) %*% model$scaling[, 1]
sum(round(check.ld1, 8) != round(pred$x.LD1, 8))
check.ld2 = scale(input[1:4], center = T, scale = F) %*% model$scaling[, 2]
sum(round(check.ld2, 8) != round(pred$x.LD2, 8))

centers.LD1 = tapply(pred$x.LD1, Species, mean)
centers.LD2 = tapply(pred$x.LD2, Species, mean)

# Ábrázoljuk a fajtát, illetve az elõrejelzett csoportbesorolást a 
# diszkriminancia függvények által meghatározott koordinátarendszerben.
# Mindkét ábrára vegyük fel a tartományok középpontjait is.

plot(pred$x.LD1, pred$x.LD2, col = c("red", "blue", "green")[Species], asp = 1)
text(centers.LD1, centers.LD2, col = "black", labels = c("x","x","x"))

plot(pred$x.LD1, pred$x.LD2, col = c("red", "blue", "green")[pred$class], asp = 1)
text(centers.LD1, centers.LD2, col = "black", labels = c("x","x","x"))

# Milyen hatékonysággal mûködött az algoritmus? Mondjunk példát olyan
# megfigyelésre, mely rosszul lett besorolva. Ezen növény mekkora
# valószínûséggel került a modell által besorolt csoportba?

table(Species, pred$class, dnn = c("original", "predicted"))
mean(Species == pred$class)
pred
# Olvassuk be az 'irisz2.txt' állományt, mely egy ismeretlen fajtájú
# növény adatait tartalmazza. Mely fajtába sorolnánk be ezt a
# növényt? Mekkora valószínûséggel került ebbe a csoportba?
# Ábrázoljuk a növényt az elõzõ ábrán.

input2 = read.table("irisz2.txt", header = TRUE)
pred2 = data.frame(predict(model, input2))
text(pred2$x.LD1, pred2$x.LD2, labels = c("ÚJ"))

detach(input)

# =================================================================================

# Egy bank azt szeretné megjósolni, hogy az ügyfelek megfelelõen fogják-e törleszteni 
# a felvett hitelt vagy nem: 4 jellemzõ alapján szeretné eldönteni az új ügyfelekrõl, 
# hogy jó vagy rossz adósok lesznek-e. 

# Olvassuk be a bankloan.csv adatbázist, mely 499 korábbi ügyfél adatait tartalmazza:
#	default: 0 = jó adós, 1 = rossz adós
#	employ: a jelenlegi munkahelyén eltöltött évek száma
#	address: a jelenlegi lakhelyén eltöltött évek száma
#	debtinc: adósság / jövedelem arány százalékos formában
#	creddebt: folyószámla hitel (ezer dollár)

adat = read.csv2("bankloan.csv")
attach(adat)

# Végezzünk diszkriminancia-analízist a mintára. 
# Adjuk meg a diszkriminancia függvény egyenletét.

model = lda(default ~ employ + address + debtinc + creddebt)
model
mean(employ)
mean(address)
mean(debtinc)
mean(creddebt)

# Milyen hatékonysággal mûködött az algoritmus, 
# vagyis mekkora a helyes besorolások aránya?

pred = data.frame(predict(model))
table(default, pred$class, dnn = c("original", "predicted"))
mean(default == pred$class)

# Olvassuk be a bankloan2.csv állományt, mely leendõ ügyfelek 
# adatait tartalmazza. Milyen elõrejelzést tudunk adni, a leendõ
# ügyfelek közül ki lesz jó, és ki lesz rossz adós? A megfigyelések
# mekkora valószínûséggel kerültek a modell által jósolt csoportba?

adat2 = read.csv2("bankloan2.csv")
pred2 = data.frame(predict(model, adat2))

detach(adat)

# =================================================================================
# =================================================================================

# Olvassuk be az europa.txt állomány tartalmát az 'adat' nevû
# táblázatba.

adat = read.table("europa.txt", header = TRUE)
attach(adat)

# Ábrázoljuk a népsûrûséget a népesség függvényeként.

sorszam = c(1:length(orszag))
plot(nepesseg, nepsuruseg, type = "n")
text(nepesseg, nepsuruseg, sorszam)

# Ha csoportosítani szeretnénk az országokat ezen két változó szempontjából, 
# akkor hány csoportot (klasztert) lenne érdemes kialakítani?

# A népesség és a népsûrûség alapján végezzünk klaszteranalízist
# az adatsoron a legközelebbi szomszéd (single-linkage) módszert alkalmazva.

tavolsagmatrix = dist(adat[2:3])
model = hclust(tavolsagmatrix, method = "single")
plot(model)

# A dendrogram alapján hány klasztert érdemes definiálni? Ebben az esetben
# hány ország került az egyes klaszterekbe? Ábrázoljuk a klaszterbesorolást.

rect.hclust(model, 3)
model$merge # milyen sorrendben történt a klaszterek összevonása
model$height # az összevonás milyen távolságnál történt
klaszter = cutree(model, 3)
plot(nepesseg, nepsuruseg, type = "n")
text(nepesseg, nepsuruseg, sorszam, col = klaszter)

# Ha a változók nagyon eltérõ skálán mozognak, akkor célszerû õket
# a klaszteranalízis végrehajtása elõtt standardizálni:

nepesseg_st = scale(nepesseg)
nepsuruseg_st = scale(nepsuruseg)

tavolsagmatrix = dist(cbind(nepesseg_st, nepsuruseg_st))

# Hierarchikus klaszter-analízis a legközelebbi szomszéd távolság módszerével:

model = hclust(tavolsagmatrix, method = "single")
plot(model)
rect.hclust(model, 3) # Az országok csoportosítása 3 csoport kialakítása esetén
model$merge
magassag = model$height 
print(magassag, digits = 3)

klaszter = cutree(model, 3)
plot(nepesseg_st, nepsuruseg_st, type = "n")
text(nepesseg_st, nepsuruseg_st, sorszam, col = klaszter)

# A legközelebbi szomszéd módszernek (single-linkage) az az elõnye, hogy jól szeparált
# klaszterek alakulnak ki, de az a hátránya, hogy ezek a csoportok nagyon
# nagyok is lehetnek, mint a most vizsgált példában.

# Hierarchikus klaszter-analízis a legtávolabbi szomszéd módszer alkalmazásával:

model = hclust(tavolsagmatrix, method = "complete")
plot(model)
rect.hclust(model, 3)
klaszter = cutree(model, 3)
plot(nepesseg_st, nepsuruseg_st, type = "n")
text(nepesseg_st, nepsuruseg_st, sorszam, col = klaszter)

# A legtávolabbi szomszéd (complete linkage) módszer kevésbé szeparált, 
# de kis méretû klasztereket eredményez.

# Hierarchikus klaszter-analízis az átlagos távolság módszerrel:

model = hclust(tavolsagmatrix, method = "average")
plot(model)
rect.hclust(model, 4)
klaszter = cutree(model, 4)
plot(nepesseg_st, nepsuruseg_st, type = "n")
text(nepesseg_st, nepsuruseg_st, sorszam, col = klaszter)

# A legközelebbi és a legtávolabbi szomszéd két szélsõséges klaszterezési
# eljárás bizonyos elõnyökkel és hátrányokkal. A két szélsõség közötti
# középutat jelent az átlagos távolság (average linkage) és a középpont
# távolság (centroid clustering) módszer. 

detach(adat)

# =================================================================================

# Olvassuk be a csoki.csv állomány tartalmát az 'adat' nevû
# táblázatba. Összesen tíz csokimárkát ítéltek meg a személyek a 
# csoki nagysága, krémességének és töménységének tekintetében. 
# Vizsgáljuk meg, hogy mely csokoládék állnak a vizsgálati személyek 
# szerint közel egymáshoz. 

adat = read.csv("csoki.csv") # vesszõvel elválasztott adatbázis
attach(adat)

# Távolságmátrix kiszámítása:

tavolsagmatrix = dist(adat[2:4]) 

# Hierarchikus klaszteranalízis a legközelebbi szomszéd módszer alkalmazásával:

model = hclust(tavolsagmatrix, method = "single")
plot(model)

rect.hclust(model, 2) # a csokik csoportosítása két csoport kialakítása esetén
model$merge # a csokik összevonásának menete
magassag = model$height # az összevont csoportok közötti távolságok a fenti lépések során
print(magassag, digits = 3)

# Láthatjuk, hogy alapvetõen két nagy csoportja van a vizsgált 
# csokoládéknak. Az egyikbe tartoznak a táblás csokoládék 
# (Tibi, Milka és a Boci), míg a másikba a szeletes csokik. 
# Az utóbbiba vonta be a módszer a mûzliszeletet is, bár 
# meglehetõsen távol van a többi csokoládétól.

detach(adat)

