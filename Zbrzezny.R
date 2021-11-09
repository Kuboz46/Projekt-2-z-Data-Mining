# Wczytywanie danych.

setwd("C:/Users/user/Downloads/Data Mining/Projekt 2/projekt2") # Moja œcie¿ka do plików.

ind_data <- read.table("individual_data.txt")

dim(ind_data) # 56218    42

MLA <- read.table("MLA.txt")

test_data <- read.table("test_data.txt")

train_data <- read.table("train_data.txt")

dim(test_data) # 3000 343
dim(train_data) # 6000 343

train_labels <- read.table("train_labels.txt") # W tym zbiorze jest zmienna poor.

valid_data <- read.table("validation_data.txt")

valid_labels <- read.table("validation_labels.txt")

# 1 klasyfikator.

# Najpierw robiê klasyfikacjê bez selekcji.

train_set <- c(train_data, train_labels)

train_set <- as.data.frame(train_set)

test_set <- c(test_data, valid_labels)

test_set <- as.data.frame(test_set)

# £¹czy³em zbiory, ¿eby czynniki by³y takie same, a nastêpnie indeksowa³em.

zbior_zloczony <- rbind(train_set, test_set)

idx_tren <- 1:6000

zbior_tren <- zbior_zloczony[idx_tren, ]

zbior_test <- zbior_zloczony[-idx_tren, ]

library(randomForest) 
model.ranFor <- randomForest(poor ~ ., data = zbior_tren, ntree = 50)
wynik1 <- predict(model.ranFor, zbior_test, type = "prob")
predykcje_ranFor <- predict(model.ranFor, zbior_test, type = "class")
wynik1 <- wynik1[, 2]

mean(predykcje_ranFor==zbior_test$poor) #  Dla walidacyjnego: 0.7766667
# Dla testowego: 0.5006667

# Wczytywanie do pliku.
write.table(wynik1, 'wynik1.txt', append = FALSE, sep = "\n ", dec = ".",
             row.names = FALSE, col.names = FALSE)

# AUC na walidacyjnym = 0,862
# Dla testowego: AUC = 

# 2 klasyfikator.

train_set <- c(train_data, train_labels)

train_set <- as.data.frame(train_set)

test_set <- c(test_data, valid_labels)

test_set <- as.data.frame(test_set)

zbior_zloczony <- rbind(train_set, test_set)

idx_tren <- 1:6000

zbior_tren <- zbior_zloczony[idx_tren, ]

zbior_test <- zbior_zloczony[-idx_tren, ]

# Zrobiê selekcjê zmiennych funkcj¹ CMIM z pakietu "praznik".

library(praznik)

y <- factor(ifelse(zbior_zloczony$poor == "Poor", 1, 0))

# Wartoœci zmienner "poor" s¹ w ostatniej kolumnie zbioru "zbior_zlaczony", czyli 344.

CMIM(zbior_zloczony[,-344], Y = y,  k = 6) # Dla walidacyjnego otrzymane zmienne: cons_0111     cons_0901  geo_district     cons_0106     cons_0501 hld_nbcellpho
# Dla testowego: cons_0901    cons_0106 geo_district    cons_0501    cons_0111    cons_0508

model.ranFor <- randomForest(poor ~ cons_0901 + cons_0106 + geo_district + cons_0501 + cons_0111 + cons_0508, data =zbior_tren, ntree = 50)
wynik2 <- predict(model.ranFor, zbior_test, type = "prob")
predykcje_ranFor <- predict(model.ranFor, zbior_test, type = "class")
wynik2 <- wynik2[, 2]

mean(predykcje_ranFor==zbior_test$poor) # Dla walidacyjnego: 0.7493333
# Dla testowego: 0.5056667

# Wczytywanie do pliku.
write.table(wynik2, 'wynik2.txt', append = FALSE, sep = "\n ", dec = ".",
            row.names = FALSE, col.names = FALSE)

# Gorsze wychodzi AUC (wynosi 0.820) (dla walidacyjnego).
# Dla testowego: AUC = 

# 3 klasyfikator.

# Sprawdzê t¹ sam¹ selekcjê z tym samym modelem, ale dla liczby zmiennych równej 12.

train_set <- c(train_data, train_labels)

train_set <- as.data.frame(train_set)

test_set <- c(test_data, valid_labels)

test_set <- as.data.frame(test_set)

zbior_zloczony <- rbind(train_set, test_set)

idx_tren <- 1:6000

zbior_tren <- zbior_zloczony[idx_tren, ]

zbior_test <- zbior_zloczony[-idx_tren, ]

y <- factor(ifelse(zbior_zloczony$poor == "Poor", 1, 0))

# Wartoœci zmienner "poor" s¹ w ostatniej kolumnie zbioru "zbior_zlaczony", czyli 344.

CMIM(zbior_zloczony[,-344], Y = y,  k = 12) # Dla walidacyjnego: cons_0111     cons_0901  geo_district     cons_0106     cons_0501 hld_nbcellpho
# cons_0401     cons_0801     cons_0508   hld_cooking     cons_1204     cons_1108 

# Dla testowego: cons_0901     cons_0106  geo_district     cons_0501     cons_0111     cons_0508 hld_nbcellpho 
# cons_0401     cons_0801     cons_1108     cons_1204 hld_headsleep 

model.ranFor <- randomForest(poor ~ cons_0901 + cons_0106 + geo_district + cons_0501 + cons_0111 + cons_0508 + hld_nbcellpho + cons_0401 + cons_0801 + cons_1108 + cons_1204 + hld_headsleep, data =zbior_tren, ntree = 50)
wynik3 <- predict(model.ranFor, zbior_test, type = "prob")
predykcje_ranFor <- predict(model.ranFor, zbior_test, type = "class")
wynik3 <- wynik3[, 2]

mean(predykcje_ranFor==zbior_test$poor) # Dla walidacyjnego: 0.7523333
# Dla testowego: 0.5

# Wczytywanie do pliku (ten plik wys³a³em mailem).
write.table(wynik3, 'JZB.txt', append = FALSE, sep = "\n ", dec = ".",
            row.names = FALSE, col.names = FALSE)

# Dla walidacyjnego: AUC = 0.840. Lepiej jest ni¿ w przypadku 6 zmiennych. AUC jest mniejsze tylko o 0,02 ni¿ w przypadku modelu pe³nego.
# Dla testowego: AUC = 

# 4 klasyfikator.

# Spróbujê teraz z³¹czyæ "zbior_zlaczony" z jakimiœ danymi indywidualnymi za pomoc¹ funkcji merge().

ind_data_7kol <- ind_data[, 1:7]
ind_data$ind_educ01
library(dplyr)

z_dan_ind <- inner_join(ind_data_7kol, zbior_zloczony, by = "hid")

# Usuwam wiersze z brakami danych.

sum(is.na(z_dan_ind[,(6:7)])) == sum(is.na(z_dan_ind)) # Zatem w kolumnach 6, 7 s¹ wszystkie braki danych.

idx_6_NA <- which(is.na(z_dan_ind[,6]))

z_dan_ind <- z_dan_ind[-idx_6_NA, ]

idx_7_NA <- which(is.na(z_dan_ind[, 7]))

z_dan_ind <- z_dan_ind[-idx_7_NA, ]

idx_test <- sample(1:nrow(z_dan_ind), 3000) # Chcê mieæ dok³adnie 3000 obserwacji w zbiorze testowym,
# poniewa¿ tyle wartoœci otrzymanych z funkcji predict() trzeba mieæ, by aplikacja licz¹ca AUC dzia³a³a. 
zbior_test <- z_dan_ind[idx_test, ]
zbior_tren <- z_dan_ind[-idx_tren, ]

y <- factor(ifelse(z_dan_ind$poor == "Poor", 1, 0))
# 344 kolumna z z³¹czonego zbioru jest kolumn¹ z wartoœciami zmiennej "poor".
z_dan_ind[, ncol(z_dan_ind)] # tutaj jest zmienna "poor"
CMIM(z_dan_ind[,-ncol(z_dan_ind)], Y = y,  k = 6)  # Dla walidacyjnego: cons_0111  geo_district     cons_0901 hld_nbcellpho     cons_0106     cons_0501 
# Dla testowego: cons_0801  geo_district     cons_0111     cons_0106 hld_nbcellpho     cons_0508 

model.ranFor <- randomForest(poor ~  cons_0801 + geo_district + cons_0111 + cons_0106 + hld_nbcellpho + cons_0508, data = zbior_tren, ntree = 20)

wynik4 <- predict(model.ranFor, zbior_test, type = "prob")
predykcje_ranFor <- predict(model.ranFor, zbior_test, type = "class")
wynik4 <- wynik4[, 2]

mean(predykcje_ranFor==zbior_test$poor) # Dla walidacyjnego: 0.754
# Dla testowego: 0.6616667


# Wczytywanie do pliku.
write.table(wynik4, 'wynik4.txt', append = FALSE, sep = "\n ", dec = ".",
            row.names = FALSE, col.names = FALSE)

# AUC ma³e (wynosi 0.495) (dla walidacyjnego)
# Dla testowego: AUC = 

# 5 klasyfikator.

train_set <- c(train_data, train_labels)

train_set <- as.data.frame(train_set)

test_set <- c(test_data, valid_labels)

test_set <- as.data.frame(test_set)

zbior_zloczony <- rbind(train_set, test_set)

idx_tren <- 1:6000

zbior_tren <- zbior_zloczony[idx_tren, ]

zbior_test <- zbior_zloczony[-idx_tren, ]

y <- factor(ifelse(zbior_zloczony$poor == "Poor", 1, 0))

library(adabag)
model.bag <- bagging(poor ~ ., data = zbior_tren, mfinal = 25)
wynik5 <- predict(model.bag, zbior_test, type = "prob")
predykcje_bag <- predict(model.bag, zbior_test, type = "class")
predykcje_bag <- predykcje_bag$class
wynik5 <- wynik5$prob
wynik5 <- wynik5[, 2]

mean(predykcje_bag==zbior_test$poor) # Dla walidacyjnego: 0.7543333
# Dla testowego: 0.4973333

# Wczytywanie do pliku.
write.table(wynik5, 'wynik5.txt', append = FALSE, sep = "\n ", dec = ".",
            row.names = FALSE, col.names = FALSE)

# AUC = 0,835 (dla walidacyjnego)
# Dla testowego: AUC = 

# 6 klasyfikator.

# Przetestujê bagging z selekcj¹ zmiennych (wezmê 12 zmiennych).

train_set <- c(train_data, train_labels)

train_set <- as.data.frame(train_set)

test_set <- c(test_data, valid_labels)

test_set <- as.data.frame(test_set)

zbior_zloczony <- rbind(train_set, test_set)

idx_tren <- 1:6000

zbior_tren <- zbior_zloczony[idx_tren, ]

zbior_test <- zbior_zloczony[-idx_tren, ]

y <- factor(ifelse(zbior_zloczony$poor == "Poor", 1, 0))

# Wartoœci zmienner "poor" s¹ w ostatniej kolumnie zbioru "zbior_zlaczony", czyli 344.

JMIM(zbior_zloczony[,-344], Y = y,  k = 12) # Dla walidacyjnego: cons_0111     cons_0901  geo_district     cons_0106     cons_0501 hld_nbcellpho
# cons_0801     cons_0401   hld_cooking     cons_1204 hld_headsleep     cons_0803

# Dla testowego: cons_0901  hld_dwateros  geo_district     cons_0106     cons_0111     cons_0501     cons_0801 
# hld_nbcellpho     cons_0401   hld_cooking     cons_0803     cons_1204 

library(adabag)
model.bag <- bagging(poor ~ cons_0901 + hld_dwateros + geo_district + cons_0106 + cons_0111 + cons_0501 + cons_0801 + hld_nbcellpho + cons_0401 + hld_cooking + cons_0803 + cons_1204, data = zbior_tren, mfinal = 25)
wynik6 <- predict(model.bag, zbior_test, type = "prob")
predykcje_bag <- predict(model.bag, zbior_test, type = "class")
predykcje_bag <- predykcje_bag$class
wynik6 <- wynik6$prob
wynik6 <- wynik6[, 2]

mean(predykcje_bag==zbior_test$poor) # Dla walidacyjnego: 0.7403333
# Dla testowego: 0.498

# Wczytywanie do pliku.
write.table(wynik6, 'wynik6.txt', append = FALSE, sep = "\n ", dec = ".",
            row.names = FALSE, col.names = FALSE)

# AUC = 0,82 (dla walidacyjnego)
# Dla testowego: AUC = 

# Wartoœæ AUC jest mniejsza od AUC dla baggingu z modelem pe³nym tylko 0,15.
# Ten klasyfikator jest wiêc lepszy, poniewa¿ dzia³a znacznie szybciej ni¿ klasyfikator 
# z modelem pe³nym ze wzglêdu na niewielk¹ ró¿nicê w wartoœci AUC.


# 7 klasyfikator.

# Ostatnim klasyfikatorem, jaki przetestujê, bêdzie klasyfikator z modelem opartym na funkcji ranger().
# Zrobiê te¿ selekcjê zmiennych z liczb¹ zmiennych zwracanych równ¹ 12.

train_set <- c(train_data, train_labels)

train_set <- as.data.frame(train_set)

test_set <- c(test_data, valid_labels)

test_set <- as.data.frame(test_set)

zbior_zloczony <- rbind(train_set, test_set)

idx_tren <- 1:6000

zbior_tren <- zbior_zloczony[idx_tren, ]

zbior_test <- zbior_zloczony[-idx_tren, ]

# Zrobiê selekcjê zmiennych funkcj¹ CMIM z pakietu "praznik".

library(praznik)

y <- factor(ifelse(zbior_zloczony$poor == "Poor", 1, 0))

# Wartoœci zmienner "poor" s¹ w ostatniej kolumnie zbioru "zbior_zlaczony", czyli 344.

JMIM(zbior_zloczony[,-344], Y = y,  k = 12) # Dla walidacyjnego: cons_0111     cons_0901  geo_district     cons_0106     cons_0501 hld_nbcellpho
# cons_0801     cons_0401   hld_cooking     cons_1204 hld_headsleep     cons_0803

# Dla testowego:

library(ranger)
model.ranger<- ranger(poor ~ cons_0901 + hld_dwateros + geo_district + cons_0106 + cons_0111 + cons_0501 + cons_0801 + hld_nbcellpho + cons_0401 + hld_cooking + cons_0803 + cons_1204, data = zbior_tren, num.trees = 50)
pred_ran <- predict(model.ranger, zbior_test, type = "response") 
pred_ran <- pred_ran$predictions
wynik7 <- predict(model.bag, zbior_test, type = "response")
wynik7 <- wynik7$prob
wynik7 <- wynik7[, 2]


mean(pred_ran==zbior_test$poor) # Dla walidacyjnego: 0.7466667
# Dla testowego: 0.503

# Wczytywanie do pliku.
write.table(wynik7, 'wynik7.txt', append = FALSE, sep = "\n ", dec = ".",
            row.names = FALSE, col.names = FALSE)

# AUC = 0,821 (dla walidacyjnego)
# Dla testowego: AUC = 








