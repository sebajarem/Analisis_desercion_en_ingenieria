freqtable <- function(X) {

    # X: data.frame of typically survey items 

    # Code assumes that all items are numeric (i.e., 1 to 5, rather than text labels)

    unique_values <- unique(unlist(sapply(X, unique)))

    su <- sort(unique_values)

    freq <- t(sapply(X, function(Z) table(factor(Z, su))))

    prop <- t(sapply(X, function(Z) prop.table(table(factor(Z, su)))))

    list(freq = freq, prop = prop)

}



# example

# library(psych)

# data(bfi)

# freqtable(bfi[,1:25])



# Produces something like

# $freq

#      1   2   3   4    5    6

# A1 922 818 402 337  223   82

# A2  47 126 151 553 1023  873

# A3  90 172 207 564  986  755

# A4 129 215 185 451  654 1147

# A5  59 186 254 617  973  695

# C1  73 161 275 655 1018  597

# C2  89 236 296 643  962  550