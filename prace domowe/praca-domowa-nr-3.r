# --------------------------------------------------------
# Zadanie.12 
#
# W R dostępne są zbiory WWWusage, presidents oraz mdeaths. Wczytaj je i dokonaj eksploracji danych.
# Sprawdź czy są to zbiory typu "time series". Opisz jakich okresów dotyczą dane oraz jakim okresom 
# czasu odpowiadają poszczególne obserwacje. Sporządź również wykresy liniowe obrazujące te szeregi czasowe.
#
# Pełne rozwiązanie tego zadania musi zawierać dla każdego zbioru: 
#	- Twój kod R, 
#	- Twój opis i interpretację szeregów czasowych,
#   - czytelną wizualizację na wykresach. 
#
# --------------------------------------------------------
# tu wpisz swój kod: 

# WWWusage

?WWWusage

# Jest to szereg czasowy liczby użytkowników łączących się z Internetem 
# za pośrednictwem serwera co minutę.

head(WWWusage)
summary(WWWusage)
plot(WWWusage, main = "Użycie Internetu w czasie", xlab = "Czas (minuty)",
     ylab = "Liczba użytkowników")

# Wykres liniowy ukazuje trend wzrostowy w użyciu internetu - liczba użytkowników wzrasta wraz z
# upływem czasu.

# presidents

?presidents

# Jest to zbiór szereg czasowy 120 wartości zawierający przybliżony kwartalny wskaźnik
# poparcia dla prezydenta Stanów Zjednoczonych od pierwszego kwartału 1945 r. do ostatniego 
# kwartału 1974 r.

head(presidents)
summary(presidents)
plot(presidents, main = "Kwartalne poparcie dla prezydentów USA w latach 1945-1974", 
     xlab = "Rok", ylab = "Poparcie w %")

# Wykres liniowy ukazuje zmienne poparcie. Widać największą powtarzalność trendów dla okresów 1952-1958
# i 1968-1975. Poparcie dla prezydentów USA raczej spadało wraz z upływem czasu.

# mdeaths

?mdeaths

# mdeaths, ldeaths i fdeaths to trzy szeregi czasowe podające miesięczną liczbę
# zgonów z powodu zapalenia oskrzeli, rozedmy płuc i astmy w Wielkiej Brytanii w latach 
# 1974-1979, dla odpowiednio mężczyzn, obu płci i kobiet.

head(mdeaths)
summary(mdeaths)
plot(mdeaths, main = "Miesięczna liczba zgonów z powodu chorób płucnych", xlab = "Rok", 
     ylab = "Liczba zgonów")

# Wykres liniowy pokazuje sezonowość - zwiększenie liczby zgonów w okolicach ostatniego i pierwszego 
# kwartału danego roku.

# --------------------------------------------------------
# Zadanie.13
#
# Dostępny jest zbiór danych prezentujących liczbę małżeństw (v1). 
# Prezentuje on dane za okres od stycznia 2009 do grudnia 2011. 
# Dokonaj jego dekompozycji. Wybierz właściwy typ modelu wahań w czasie. 
# Wykreśl wyniki. Odpowiedz na pytania:
#
# W których miesiącach jest najwyższy dodatni poziom wahań sezonowych, 
# a w których jest najniższy? Uzasadnij odpowiedź i zinterpretuj wyniki.
#  
# O ile procent w każdym miesiącu wskaźniki sezonowości odchylają się od średniej dla danego miesiąca?
#
# --------------------------------------------------------
# tu wpisz swój kod: 

v1 <-  c(10.8, 12.9, 4.7, 28.1, 6.1, 23.1, 20.2, 23.7, 30.4, 24.6, 7.7, 14.8,
         10.2, 10.9, 4.9, 27.3, 6.0, 27.6, 16.7, 28.3, 26.7, 24.8, 8.2, 12.9,
         9.8,  7.9, 8.1, 25.7, 6.3, 25.2, 18.8, 29.6, 26.2, 26.1, 8.3, 12.9)

szereg_czasowy <- ts(v1, start = c(2009, 1), frequency = 12)

# Dekompozycja - podejście addytywne
dekompozycja_addytywna <- decompose(szereg_czasowy, type = "additive")

# Dekompozycja - multiplikatywne
dekompozycja_multiplikatywna <- decompose(szereg_czasowy, type = "multiplicative")

# Wykreślenie wyników
par(mfrow=c(3,1))
plot(szereg_czasowy, main="Oryginalny Szereg Czasowy", col="blue")
plot(dekompozycja_addytywna$seasonal, main="Składowa Sezonowa (Addytywna)", col="red")
plot(dekompozycja_multiplikatywna$seasonal, main="Składowa Sezonowa (Multiplikatywna)", col="green")
par(mfrow=c(1,1))

# Składowa sezonowa - addytywna
sezon_addytywny <- dekompozycja_addytywna$seasonal

# Składowa sezonowa - multiplikatywna
sezon_multiplikatywny <- dekompozycja_multiplikatywna$seasonal

# Najwyższy dodatni poziom wahań sezonowych - addytywny
miesiac_najwyzszy_addytywny <- which.max(sezon_addytywny) # wrzesien

# Najniższy dodatni poziom wahań sezonowych - addytywny
miesiac_najnizszy_addytywny <- which.min(sezon_addytywny) # maj

# Najwyższy dodatni poziom wahań sezonowych - multiplikatywny
miesiac_najwyzszy_multiplikatywny <- which.max(sezon_multiplikatywny) # wrzesien

# Najniższy dodatni poziom wahań sezonowych - multiplikatywny
miesiac_najnizszy_multiplikatywny <- which.min(sezon_multiplikatywny) # maj

# We wrześniu obserwujemy najwyższy dodatni poziom wahań sezonowych. To oznacza, że 
# w tym miesiącu liczba małżeństw wykazuje znaczące odchylenia od średniej sezonowej. 
# Może to wynikać z różnych czynników, takich jak sezon ślubny, który często jest 
# bardziej aktywny w okresie września. Natomiast w maju obserwujemy najniższy
# dodatni poziom wahań sezonowych. Oznacza to, że liczba małżeństw w maju jest bliżej 
# średniej sezonowej niż w innych miesiącach. Może to sugerować, że maj nie jest tak 
# aktywnym miesiącem pod względem zawierania małżeństw w porównaniu do innych miesięcy.

# Odchylenie sezonowe (addytywne)
odchylenie_addytywne <- (sezon_addytywny - mean(sezon_addytywny)) / mean(sezon_addytywny) * 100

# Odchylenie sezonowe (multiplikatywne)
odchylenie_multiplikatywne <- (sezon_multiplikatywny - mean(sezon_multiplikatywny)) / mean(sezon_multiplikatywny) * 100

# Wyniki
wyniki <- data.frame(
  Miesiac = month.abb,
  Odchylenie_Addytywne = odchylenie_addytywne,
  Odchylenie_Multiplikatywne = odchylenie_multiplikatywne
)

# --------------------------------------------------------
# Zadanie.14
#
# W pakiecie ISLR dostępny jest zbiór "Auto" z informacją o różnych modelach samochodów.
# Zapoznaj się ze znaczeniem zmiennych zbioru. Dokonaj eksploracji zbioru za pomocą
# poznanych funkcji (np. head, summary, itd.).
#
# Utwórz model1 regresji liniowej ze zmienną mpg jako objaśnianą a horsepower jako objaśniającą.
# Zastanów się co zawierają zmienne i czy istnieje związek przyczynowo-skutkowy. 
# Zinterpretuj zmienne i uzasadnij związek.
# 
# Dokonaj analizy parametrów modelu za pomocą funkcji summary(). Na podstawie jej wyników odpowiedz 
# na następujące pytania:
# a) Czy istnieje związek pomiędzy zmienną objaśniającą a objaśnianą?
# b) Jak można interpretować wartość współczynnika regresji?
# c) Jaką liczbę mil per galon paliwa model przewiduje dla 98 koni mechanicznych? 
# {wskazówka: użyj funkcji predict z argumentem interval="confidence"}
# d) Utwórz wykres rozrzutu dla tych dwóch zmiennych; dodaj linię regresji za pomocą funkcji "abline"
# e) Użyj funkcji "plot", żeby obejrzeć wykresy diagnostyczne
#
# Spróbuj stworzyć model dla większej liczby zmiennych. 
# Najpierw stwórz macierz korelacji dla zmiennych numerycznych używając funkcji cor(). 
# (pamiętaj, że w argumencie funkcji cor powinny być dane bez zmiennych kategorycznych)
# f) Które zmienne numeryczne są silnie skorelowane ze zmienną objaśnianą?
#
# Stwórz teraz model2 oparty na wszystkich zmiennych.
# g) Które zmienne są powiązane ze zmienną objaśnianą (mpg) w statystycznie istotnym stopniu?
# h) Jak można interpretować współczynnik przy zmiennej "year"?
#
# Porównaj modele ze względu na wartość R kwadrat 
# i stwórz wykresy diagnostyczne dla obu modeli.

?Auto

#
# --------------------------------------------------------
# tu wpisz swój kod:

install.packages("ISLR")
library(ISLR)

# Wyświetlenie kilku pierwszych wierszy zbioru danych Auto
head(Auto)

# Podsumowanie statystyczne zbioru danych Auto
summary(Auto)

# Struktura danych zbioru
str(Auto)

# Model
model <- lm(mpg ~ horsepower, data = Auto)

# Zmienne w modelu:
  
#   mpg (miles per gallon) - jest to zmienna objaśniana, czyli ta, którą staramy się przewidzieć.
#   horsepower - jest to zmienna objaśniająca, która wpływa na zmienną objaśnianą.

# Podsumowanie modelu:
  
#   Coefficients: - Przy horsepower znajdziemy współczynniki regresji, takie jak współczynnik 
#   nachylenia (slope) i wyraz wolny (intercept).

# Związek przyczynowo-skutkowy:
  
#   W kontekście tego modelu, możemy powiedzieć, że "horsepower" jest przyczyną zmian w wartości 
#   "mpg". Współczynniki regresji pomagają nam zrozumieć, jak zmiana jednostkowa w mocy silnika 
#   wpływa na zmianę jednostkową w zużyciu paliwa.

# Interpretacja zmiennych:
  
#   horsepower: Im większa moc silnika, tym mniej mil na galon paliwa może przejechać samochód. 
#   Współczynnik nachylenia będzie wskazywał, o ile jednostek zmniejszy się zużycie paliwa (mpg) 
#   w wyniku wzrostu o jednostkę mocy silnika.

# Uzasadnienie związku:
   
#   Moc silnika może wpływać na wydajność pojazdu, a co za tym idzie, na zużycie paliwa. Silniki 
#   o większej mocy często wymagają więcej paliwa na jednostkę przejechanego dystansu, co powoduje 
#   negatywną korelację między mocą silnika a wydajnością paliwową (mpg).

# Analiza parametrów modelu za pomocą funkcji summary()
summary(model)

# Odpowiedzi na pytania:

# a) Czy istnieje związek pomiędzy zmienną objaśniającą a objaśnianą?
#    - Tak, istnieje związek pomiędzy "horsepower" a "mpg". P-wartość poniżej 0.05 sugeruje 
#      istotność statystyczną.

# b) Jak można interpretować wartość współczynnika regresji?
#    - Wartość współczynnika regresji dla "horsepower" to liczba, o którą zmieni się "mpg" 
#      w wyniku jednostkowej zmiany w "horsepower".
#    - W przypadku ujemnego współczynnika, możemy powiedzieć, że wzrost "horsepower" zazwyczaj 
#      skutkuje spadkiem "mpg" i na odwrót.

# c) Jaką liczbę mil per galon paliwa model przewiduje dla 98 koni mechanicznych?
przewidziane_mpg <- predict(model, newdata = data.frame(horsepower = 98), interval = "confidence")
przewidziane_mpg

# d) Utwórz wykres rozrzutu dla tych dwóch zmiennych; dodaj linię regresji za pomocą funkcji "abline"
plot(Auto$horsepower, Auto$mpg, main = "Regresja liniowa", xlab = "Moc silnika", ylab = "MPG")
abline(model, col = "red")

# e) Użyj funkcji "plot", żeby obejrzeć wykresy diagnostyczne
plot(model)

# Stworzenie macierzy korelacji dla zmiennych numerycznych
macierz_korelacji <- cor(Auto[, sapply(Auto, is.numeric)])

# Wyświetlenie macierzy korelacji
print(macierz_korelacji)

# f) Które zmienne numeryczne są silnie skorelowane ze zmienną objaśnianą?

# Indeksy zmiennych numerycznych w Auto
indeksy_numeryczne <- sapply(Auto, is.numeric)

# Korelacje między "mpg" a innymi zmiennymi numerycznymi
korelacje_z_mpg <- abs(macierz_korelacji[, "mpg"])

# Wyświetlenie zmiennych silnie skorelowanych z "mpg"
zmienna_objasniajaca <- names(korelacje_z_mpg[abs(korelacje_z_mpg) > 0.7])
print(zmienna_objasniajaca)

# Stworzenie modelu opartego na wszystkich zmiennych
model2 <- lm(mpg ~ ., data = Auto)

# g) Które zmienne są powiązane ze zmienną objaśnianą (mpg) w statystycznie istotnym stopniu?
summary(model2)
# Wszystkie zmienne, których p-wartość (kolumna PR(>|t|)) < 0.05

# h) Jak można interpretować współczynnik przy zmiennej "year"?
#    - Wartość współczynnika przy "year" mówi nam o przewidywanej zmianie w "mpg" dla 
#      jednostkowej zmiany w "year".
#    - Pozytywny współczynnik sugeruje, że z czasem (zwiększaniem się roku produkcji) 
#      spodziewane jest zwiększenie "mpg".

# Porównanie modeli ze względu na R^2
R_kwadrat_model1 <- summary(model)$r.squared
R_kwadrat_model2 <- summary(model2)$r.squared

cat("Współczynnik determinacji R^2 dla modelu 1:", R_kwadrat_model1)
cat("Współczynnik determinacji R^2 dla modelu 2:", R_kwadrat_model2)

# Stworzenie wykresów diagnostycznych dla obu modeli
par(mfrow = c(2, 2))

# Wykresy diagnostyczne dla modelu 1
plot(model, which = c(1, 2, 3, 4), col = "blue")

# Wykresy diagnostyczne dla modelu 2
plot(model2, which = c(1, 2, 3, 4), col = "red")
par(mfrow = c(1, 1))

# --------------------------------------------------------
# Zadanie.15
#
# Wygeneruj 1000-elementowy wektor z rozkładu Poissona z parametrem lambda=6. Policz dla tego wektora:
# - średnią, 
# - rozstęp, 
# - wariancję, 
# - kwartyle, 
# - rozstęp międzykwartylowy, 
# - kurtozę, 
# - skośność ,
# - oraz dominantę.
# Zwizualizuj wektor za pomocą histogramu oraz wykresu pudełkowego (użyj kolorów, ustaw tytuły osi, wykresu).
# Jak mają się do siebie średnia, mediana i dominanta?
# Wygeneruj ponownie losowy wektor (korzystając z tej samej komendy) i uruchom ponownie napisane komendy. 
# Czy są wyraźne różnice?


#
# --------------------------------------------------------
# tu wpisz swój kod:

install.packages('moments')
library(moments)

set.seed(123)

# Wygenerowanie wektora z rozkładu Poissona
wektor_poisson <- rpois(1000, lambda = 6)

# Obliczenie średniej
srednia <- mean(wektor_poisson)

# Obliczenie rozstępu
rozstep <- range(wektor_poisson)

# Obliczenie wariancji
wariancja <- var(wektor_poisson)

# Obliczenie kwartyli
kwartyle <- quantile(wektor_poisson, c(0.25, 0.5, 0.75))

# Obliczenie rozstępu międzykwartylowego
rozstep_miedzykwartylowy <- IQR(wektor_poisson)

# Obliczenie kurtozy
kurtoza <- kurtosis(wektor_poisson)

# Obliczenie skośności
skosnosc <- skewness(wektor_poisson)

# Obliczenie dominującej wartości
dominanta <- table(wektor_poisson)
dominanta <- as.numeric(names(dominanta)[which.max(dominanta)])

# Wyświetlenie wyników
cat("Średnia:", srednia)
cat("Rozstęp:", rozstep)
cat("Wariancja:", wariancja)
cat("Kwartyle:", kwartyle)
cat("Rozstęp międzykwartylowy:", rozstep_miedzykwartylowy)
cat("Kurtoza:", kurtoza)
cat("Skośność:", skosnosc)
cat("Dominanta:", dominanta)

# Wykres histogramu
hist(wektor_poisson, col = "skyblue", main = "Histogram Wektora Poissona", xlab = "Wartości", ylab = "Ilość")

# Wykres pudełkowy (boxplot)
boxplot(wektor_poisson, col = "lightgreen", main = "Wykres Pudełkowy Wektora Poissona", ylab = "Wartości")

# Wygenerowanie losowego wektora (do porównania)
wektor_losowy <- rpois(1000, lambda = 6)

# Porównanie średniej, mediany i dominanty
cat("Wektor Poissona:")
cat("Średnia:", mean(wektor_poisson))
cat("Mediana:", median(wektor_poisson))
cat("Dominanta:", table(wektor_poisson)[which.max(table(wektor_poisson))])

cat("Wektor Losowy:")
cat("Średnia:", mean(wektor_losowy))
cat("Mediana:", median(wektor_losowy))
cat("Dominanta:", table(wektor_losowy)[which.max(table(wektor_losowy))])
