# --------------------------------------------------------
#
# Imię: Patryk
# Nazwisko: Piesiak
# Numer albumu: 30334
#
# --------------------------------------------------------
#
# Zadanie.01 
#
# Wygeneruj wektor liczb losowych x1. 
#   Dziedzina losowania to liczby całkowite od 1 do 100.
#   Wektor posiada 100 elementów. 
#   Elementy wektora nie mogą się powtarzać.
# Podaj sumę wartości elementów wektora. 
# Wskazówka: skorzystaj z funkcji 'sample'.
#
# --------------------------------------------------------

?sample()

sum(x1 <- sample(1:100))

# --------------------------------------------------------
#
# Zadanie.02
#
# Wygeneruj wektor liczb losowych x2. 
#   Dziedzina losowania to liczby całkowite {1,2,3}.
#   Wektor posiada 50 elementów. 
#   Elementy wektora mogą się powtarzać.
# Policz liczbę wystąpień każdej liczby w wektorze. 
# Wskazówka: skorzystaj z funkcji 'table'.
#
# --------------------------------------------------------

?table()

table((x2 <- sample(1:3, 50, replace=T)))

# --------------------------------------------------------
#
# Zadanie.03
#
# Stwórz losowe słowo ośmioliterowe. 
#   Dziedzina losowania to wektor 'letters'.
#   Elementy wektora nie mogą się powtarzać.
# Wskazówka: skorzystaj z funkcji 'paste', aby połączyć elementy wektora w słowo. 
#
# --------------------------------------------------------

?paste()

x3 <- paste((sample(letters, 8)), collapse='')
x3

# --------------------------------------------------------
#
# Zadanie.04
#
# Stwórz wektor x4 kolejnych liczb całkowitych od 1 do 10. 
#   Każdy element wektora podstaw jako promień r i wylicz dla niego
#     - obwód koła
#     - pole koła
#     - powierzchnię kuli (sfera)
#     - objętość kuli
# Wszystkie wyniki zgromadź w macierzy o 10 wierszach i 5 kolumnach. 
# Wyświetl macierz wyników z dokładnością do dwóch miejsc dziesiętnych.
#
# --------------------------------------------------------

?matrix()

x4 <- 1:10
mtx <- matrix(nrow=10,ncol=5)

for (row in 1:10) {
  r <- x4[row]
  mtx[row, 1] <- r
  mtx[row, 2] <- 2 *pi * r
  mtx[row, 3] <- pi * r^2
  mtx[row, 4] <- 4 *pi * r^2
  mtx[row, 5] <- mtx[row, 4] * (1/3) * r
}

round(mtx,2)

# --------------------------------------------------------
#
# Zadanie.05
# 
# Policz ile jest wspólnych wielokrotności liczb 2 i 3
# w zbiorze licz całkowitych od 1 do 1500.  
# Wskazówka: skorzystaj z funkcji 'intersect'.
# 
# --------------------------------------------------------

?intersect()

a <- (1:1500)[(1:1500) %% 2 == 0]
b <- (1:1500)[(1:1500) %% 3 == 0]

x5 <- intersect(a, b)

length(x5)