# szybki

# Ladowanie wymaganych pakietow

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr) 
}

if(!require(infotheo)){ 
  install.packages("infotheo")
  library(infotheo) 
}

# Rozwiazanie

CMIMselection <- function(X, y, kmax){
  # Cialo funkcji
  
  # Sprawdzenie poprawnosci wpisywanych danych.
  stopifnot(ncol(X) > 1 & # length(X) = iloœæ zmiennych objaœniaj¹cych
              sum(is.na(X)) == 0 & 
              nrow(X) == length(y) & sum(is.na(y)) == 0 & 
              is.numeric(kmax) & kmax <= ncol(X))
  
  p <- ncol(X)
  
  S <- rep(0, p)
  
  # Schemat iteracyjny:
  # v(1) = argmax_n I(Y, X_n)
  # Dla kazdego k, 1 <= k < K: v(k + 1) = argmax_n (min_{l <= k} ( I(y, X_n|X_{v(l)}) ) ) (*)
  
  # Idea schematu polega na tym, by wybrac maly podzbior zmiennych objasniajacych, ktore daje tak duzo informacji, 
  # jak to mozliwe. Chodzi o minimalizacje warunkowej entropii: H(y|X_{v(1)}, ..., X_{v(k)}).  
  
  ps <- rep(0, p) 
  # ps - zmienna oznaczajaca partial score, tzn. czesciowy wynik. Jest to minimum z warunkowych 
  # informacji wzajemnych pojawiajacych sie w min, w rownaniu, w schemacie iteracyjnym (oznaczonym (*)), czyli:
  # ps[n] = min_{l <= k} ( I(y, X_n|X_{v(l)}) )
  
  score <- rep(0, p)
  m <- rep(0, p)
  
  # m jest p-elementowym wektorem, gdzie m[n] jest indeksem z ostatnio dobranej zmiennej X_n wzietej pod 
  # uwage podczas liczenia ps[n]. Zachodzi nastepujaca rownosc:
  # ps[n] = min_{ l <= m[n] } I(y, X_n|X_{v(l)})
  
  for(n in 1:p)
  {
    ps[n] = mutinformation(y, X[, n]) # mutinformation(y, X[, n] = I(y, X_n), gdzie X_n jest n-ta zmienna objaœniaj¹ca.
  }
  
  for(k in 1:kmax)
  {
    najl_wynik <- 0
    for(n in 1:p)
    {
      while(ps[n] > najl_wynik & m[n] < k - 1) 
      {
        m[n] <- m[n] + 1
        idx <- S[m[n]]
        ps[n] <- min(ps[n], condinformation(y, X[, n], X[, idx]))
        # condinformation(y, X[, n], X[, idx]) = I(y, X_n| X_{idx})
      }
      if(ps[n] > najl_wynik)
      {
        najl_wynik <- ps[n]
        S[k] <- n
      }
    }
    score[k] <- max(ps)
  }
  
  
  
  return(list(S = S, score = score))
} 
