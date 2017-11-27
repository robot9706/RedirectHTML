# =============================== #
# Alkalmazott statisztika         #
# Függetlenségvizsgálat           #
# =============================== #

# Olvassuk be a salary.txt állomány tartalmát az 'input' nevû táblázatba. 

input = read.table("salary.txt", header = TRUE)
attach(input)

# Töltsük be a 'car' csomagot.

library(car)      #leveneTest

# ======================================================================

# Kérdezzük le a 'jobkat' és az 'educ' változó együttes gyakorisági
# táblázatát. 
gyak.tablazat = table(jobkat, educ)
addmargins(gyak.tablazat)

# A menedzserek hány százaléka rendelkezik a legmagasabb iskolai végzettséggel?
(rel.gyak.tablazat = prop.table(gyak.tablazat, 1))
addmargins(rel.gyak.tablazat)

# A legalacsonyabb iskolai végzettségûek hány százaléka menedzser?
(rel.gyak.tablazat2 = prop.table(gyak.tablazat, 2))
addmargins(rel.gyak.tablazat2)

# Milyen gyakoriságokat kellene kapnunk, ha a két változó
# független lenne egymástól?
gyak.fuggetlen = margin.table(gyak.tablazat, 1) %*% t(margin.table(gyak.tablazat, 2)) / margin.table(gyak.tablazat)
addmargins(gyak.fuggetlen)

# Mit jelent a függetlenség? Nézzük meg a két változó függetlensége esetén tapasztalható
# marginális eloszlásokat.
prop.table(gyak.fuggetlen, 1) # az iskolai végzettségek megoszlása minden pozíció esetén ugyanaz
prop.table(gyak.fuggetlen, 2) # a pozíciók megoszlása minden iskolai végzettség esetén ugyanaz

# Teszteljük azt a nullhipotézist, hogy a két változó független.
chisq.test(gyak.tablazat, correct = FALSE)
chisq.test(gyak.tablazat, correct = FALSE)$expected
chisq.test(jobkat, educ, correct = FALSE)

# Teszteljük az 'educ' és a 'minority' változó függetlenségét.

# Függ a gyerekek száma az alkalmazott nemétõl?

detach(input)

# ======================================================================

# Olvassuk be a „datasets” csomagból a „UCBAdmissions” adatbázist, 
# mely aggregált adatokat tartalmaz. 
# Teszteljük 5%-os szignifikancia szinten azt a nullhipotézist, hogy 
# a fiúk és a lányok egyforma arányban nyertek felvételt, vagyis, hogy 
# a felvételi eredmény („Admit”) független a nemtõl („Gender”).

d = data.frame(UCBAdmissions)
(gyak.tablazat = xtabs(Freq ~ Gender + Admit, data = d))
chisq.test(gyak.tablazat, correct = FALSE)

# ======================================================================

# Térjünk vissza az input táblázathoz.
# Ábrázoljuk a jelenlegi fizetés boxplotját iskolai végzettség
# szerinti bontásban. Teszteljük azt a nullhipotézist, hogy a jelenlegi
# fizetés várható értéke független a végzettségtõl.

attach(input)

boxplot(salary ~ educ)
leveneTest(salary ~ factor(educ), center = mean)
oneway.test(salary ~ factor(educ), var.equal = FALSE)

# Teszteljük azt az állítást, hogy a kezdõ fizetés nem függ
# a beosztástól. Függ a vállalatnál eltöltött idõ a nemtõl?

# ======================================================================

# Kérdezzük le a jelenlegi és a kezdõ fizetés Pearson-korrelációját.
# Függ a jelenlegi fizetés a kezdõ fizetéstõl? Ha igen, akkor pozitív
# vagy negatív irányú kapcsolat van a kettõ között? Adjunk meg egy 95 százalék
# megbízhatóságú konfidenciaintervallumot az elméleti korrelációra.

plot(salbegin, salary)
cor(salbegin, salary, method = "pearson")
cor.test(salbegin, salary, method = "pearson", conf.level = 0.95)

# Függ-e, és ha igen, milyen módon a jelenlegi fizetés attól,
# hogy az alkalmazott milyen régen áll alkalmazásban?

detach(input)
