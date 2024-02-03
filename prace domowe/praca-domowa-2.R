Imię: Patryk
Nazwisko: Piesiak
Numer albumu: 30334

# --------------------------------------------------------
# Zadanie.06 
#
#	Firma sprzedaje trzy kategorie produktów A, B oraz C. Każdy z nich
# jest obłożony inną stawką podatku VAT (odpowiednio: 8%, 10% i 20%). Pobierz
# kategorię i cenę. Użyj wyrażenia warunkowego, aby wyliczyć cenę z podatkiem.
# Zakomunikuj wynik do użytkownika. {użyj if else}
#
# --------------------------------------------------------

##> Obowiązuje stawka VAT 8%. Cena wraz z podatkiem wynosi 54.
##> Obowiązuje stawka VAT 8%. Cena wraz z podatkiem wynosi 55.
##> Obowiązuje stawka VAT 8%. Cena wraz z podatkiem wynosi 60.

product_price <- function(category, netprice) {
  if (category == 'A') {
    cat('Obowiązuje stawka VAT 8%. Cena wraz z podatkiem wynosi', netprice*1.08)
  } else if (category == 'B') {
    cat('Obowiązuje stawka VAT 10%. Cena wraz z podatkiem wynosi', netprice*1.1)
  } else if (category == 'C') {
    cat('Obowiązuje stawka VAT 20%. Cena wraz z podatkiem wynosi', netprice*1.2)
  } else {
    print('Niepoprawna kategoria produktu. Nie można wyliczyć ceny brutto.')
  }
}

# --------------------------------------------------------
# Zadanie.07
#
#	Oblicz iloczyn elementów dowolnego wektora x za pomocą pętli while oraz repeat 
# {użyj while, repeat}
# 
#	x <- 1:5
#	
#
# --------------------------------------------------------

x <- 1:5
count <- 0
value <- 1
i <- 1

while (count != length(x)) {
  value <- value*x[i]
  count <- count + 1
  i <- i+1
}

# --------------------------------------------------------

x <- 1:5
count <- 0
value <- 1
i <- 1

repeat{
  value <- value*x[i]
  count <- count + 1
  i <- i+1
  
  if (count == length(x)){
    break
  }
}

# --------------------------------------------------------
# Zadanie.08
#
# Użyj zbioru airquality. Odpowiedz na pytania: 		
# a) ile jest przypadków w zbiorze
# b) ile jest przypadków z brakami danych
# c) ile, i jakich, jest zmiennych w zbiorze
# d) ile, i jakich, jest zmiennych z brakami danych
# 
# Sformułuj pełne odpowiedzi tekstowe. Wklej kod obliczeń.
#
# --------------------------------------------------------

?airquality

summary(airquality)
dim(airquality)

przypadki <- dim(airquality)[1]

braki_danych <- colSums(is.na(airquality))[1] + colSums(is.na(airquality))[2]

# W zbiorze są 153 przypadki (wiersze). Zmienne w zbiorze to Ozone, Solar.R,
# Wind, Temp, Month i Day. 44 przypadki nie zawierają danych, z czego
# 37 w zmiennej Ozone, a 7 w zmiennej Solar.R.

# --------------------------------------------------------
# Zadanie.09
#
#	Ciąg Fibonacciego to ciąg liczb naturalnych, taki że każdy kolejny
# wyraz stanowi sumę dwóch poprzednich. 
#	https://pl.wikipedia.org/wiki/Ci%C4%85g_Fibonacciego
#	
#	Użyj pętli for, aby stworzyć 20 pierwszych wyrazów ciągu.
#	Podaj ich sumę. {użyj pętli for}
#	
# --------------------------------------------------------  

a <- 1
b <- 1
c <- a + b
sum <- a + b + c

for (i in 1:17) {
  a <- b
  b <- c
  c <- a + b
  sum <- sum + c
}

# --------------------------------------------------------
# Zadanie.10
# 
# Wygeneruj 100-elementowy wektor z rozkładu Poissona z parametrem 
# lambda=5 za pomocą komendy {rpois} i przypisz go do zmiennej x.
# Policz dla tego wektora: średnią, sumę, odchylenie standardowe. 
# Zwizualizuj wektor za pomocą histogramu oraz wykresu pudełkowego (boxplot)
# (słupki w kolorze niebieskim, tytuł wykresu "Rozkład Poissona"). 
# 
# --------------------------------------------------------  

# Wektor
x <- rpois(100, lambda = 5)

# Statystyka
mean(x)
sum(x)
sd(x)

# Wizualizacja
boxplot(x, main = 'Rozkład Poissona', col = 'blue')
hist(x, main = "Rozkład Poissona", col = 'blue')


# --------------------------------------------------------
# Zadanie.11

# Wczytaj dane ze zbioru "countries of the world.csv" 
# Zapoznaj się z danymi korzystając np. z funkcji names(), summary(), str(), 
# dim(), itd., obejrzyj też dane za pomocą funkcji View()
# Zamień puste pola na wartości NA: data[data==""] <- NA
# zmień nazwy kolumn, zgodnie z następującym wzorem:
# Area_sq_mi -> Area
# Pop_Density_per_sq_mi -> Pop_dens
# GDPper_capita -> GDP
# Usuń ze zbioru wszystkie zmienne poza: Country, Region, Population, Area, 
# Pop_dens, GDP, Literacy, Birthrate, Deathrate.
# Usuń spacje ze zmiennej Region (z przodu i z tyłu nazw - funkcja str_trim)
#  
# Dla ilu krajów występują braki danych w zmiennej Literacy? Użyj funkcji is.na i sum
# Wyświetl wszystkie kategorie zmiennej Region. Jaka jest dominanta? (RVAideMemoire::mod(?))
# Sprawdź udział procentowy poszczególnych kategorii.
# Jaka jest średnia i mediana zmiennych Population, Area i GDP? 
# Stwórz histogram oraz wykres pudełkowy dla zmiennych GDP oraz Literacy. 
# Odpowiednio podpisz osie histogramu (uwzględniając też jednostki)
# Wyświetl kwartyle dla funkcji GDP. Teraz stwórz nową zmienną - będzie to zmienna 
# kategoryczna oparta na zmiennej GDP. Nowa zmienna:
# - powinna się nazywać "GDP_binned"
# - będzie przyjmować cztery wartości (kategorie):
# - pierwsza kategoria ma zawierać wartości od minimum do pierwszego kwartyla. Nazwij ją "Very_poor"
# - druga kategoria ma zawierać wartości od pierwszego kwartyla do mediany. Nazwij ją "Poor"
# - trzecia kategoria ma zawierać wartości od mediany do trzeciego. Nazwij ją "Medium"
# - czwarta kategoria ma zawierać wartości od trzeciego kwartyla do maksimum. Nazwij ją "Rich"
# - (użyj funkcji ifelse albo case_when)  
# Stwórz nową zmienną logiczną, która przyjmuje wartość „True”, kiedy wartość 
# zmiennej „Birthrate” jest większa lub równa od wartości zmiennej „Deathrate”,
# a wartość „False” w przeciwnym przypadku. Wartość „True” w nowej zmiennej
# oznacza – najprościej ujmując – że w roku badania w danym kraju więcej osób się
# urodziło niż umarło. Stwórz wykres słupkowy dla nowo powstałej zmiennej, 
#  pokazując częstości występowania pierwszej i drugiej kategorii.

https://www.kaggle.com/code/patrykpiesiak/university-r-in-statistics-homework
