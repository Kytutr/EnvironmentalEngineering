library(optparse)

# Parsujemy argumenty z linii poleceń, w zasadzie musimy podać jedynie Q
# a program sam dobierze rurę i obliczy straty
option_list <- list(
    make_option(
        c("--Q"),
        type = "integer",
        default = 0,
        help = "Przepływ [dm3*s-1]"
    ),
    make_option(
        c("-v", "--verbose"),
        default = FALSE,
        action = "store_true",
        help = "To be verbose or not to be"
    )
)

opt <- parse_args(OptionParser(option_list = option_list))
verbose <- opt$verbose

Q <- opt$Q
# Jeśli nie chcemy uruchamiać programu z linii poleceń, to Q należy zdefiniować
# w linijce poniżej i odkomentować
# Q <- 10

# Deklarujemy funkcje

# Funkcja zwraca tablicę z parametrami rur (śr. zewnętrzna, grubość ścianki)
# Ta funkcja jest opracowana dla rur HDPE PE100 SDR17 PN10
# Dane spisane wg. katalogu Pipelife: https://www.pipelife.pl/content/dam/pipelife/poland/marketing/downloads/bibliotekapdf/katalogi_techniczne_pdf/K_SysCisnieniowe_Pipelfe.pdf
pipes_hdpe_pe100_sdr17_pn10 <- function() {
    pipes <- data.frame(
        PipeNumber = c(90, 110, 125, 140, 160, 180, 200, 225, 250, 280, 315, 355, 400),
        WallThickness = c(5.4, 6.6, 7.4, 8.3, 9.5, 10.7, 11.9, 13.4, 14.8, 16.6, 18.7, 21.1, 23.7),
        InternalDiameter = NA,
        InternalRadius = NA,
        Q = NA,
        u = NA
    )
    
    pipes$InternalDiameter <- (pipes$PipeNumber - (pipes$WallThickness) *
                                   2) / 1000
    pipes$InternalRadius <- (pipes$InternalDiameter / 2)
    
    return(pipes)
}

# Współczynnik oporu, przybliżenie Blasiusa (1913)
# ten wzór nie uwzględnia chropowatości rury
lambda_Blasius <- function(Re) {
    lambda <- 0.3164 / Re ** (1 / 4) # przybliżenie Blasiusa 1913
    return(lambda)
}


# Współczynnik oporu, przybliżenie Swamee–Jain (1976)
lambda_SwameeJain <- function(k, D, Re) {
    lambda <- 0.25 / (log10(((k / D) / 3.7) + (5.74 / (Re ** 0.9)))) ** 2
    return(lambda)
}

# Współczynnik oporu, przybliżenie Niazkar (2019)
lambda_Niazkar <- function(k, D, Re) {
    A <- -2 * log10((k / D) / 3.7 + 4.5547 / (Re ** 0.8784))
    B <- -2 * log10((k / D) / 3.7 + (2.51 * A) / Re)
    C <- -2 * log10((k / D) / 3.7 + (2.51 * B) / Re)
    lambda <- (1 / (A - (((
        B - A
    ) ** 2) / (C - 2 * B + A)))) ** 2
    return(lambda)
}

# Współczynnik oporu, przybliżenie Tkachenko-Mileikovskyi (2020)
lambda_TkachenkoMileikovskyi <- function(k, D, Re) {
    A0 <- -0.79638 * log(((k / D) / 8.208) + (7.3357 / Re))
    A1 <- Re * (k / D) + 9.3120665 * A0
    lambda <- ((8.128943 + A1) / (8.128943 * A0 - 0.86859209 * A1 * log(A1 / (3.7099535 * Re)))) ** 2
    return(lambda)
}

# Funkcja zwraca wielkość strat ciśnienia
deltah <- function(lambda, D, v, g) {
    delta <- (lambda * (1 / D) * ((v ** 2) / (2 * g))) * 1000
}

# Średnia prędkość wody w rurze
water_velocity <- function(Q, r) {
    velocity <- (Q / 1000) / ((pi * (r ** 2)))
    return(velocity)
}

Reynolds_number <- function(u, D, ro, mi) {
    Re <- (u * D * ro) / mi
    return(Re)
}

# Deklarujemy wartości do dalszych obliczeń
g = 9.81 # przyśpieszenie ziemskie [m*s-2]
ro = 999.7 # gęstość wody w temperaturze 10 [kg*m-3]
mi = 0.001307 # lepkość dynamiczna wody w temperaturze 10 [Pa*s]
k = 0.00005 # chropowatośc bezwzględna [m]

rury <- pipes_hdpe_pe100_sdr17_pn10()

# Tutaj zaczynamy dobierać odpowiednią średnicę rury
# Obliczamy prędkości wody dla każdej z rur jaką mamy do dyspozycji

rury$Q <- Q
rury$u <- apply(rury, 1, function(row) {
    water_velocity(row["Q"], row["InternalRadius"])
})

# Robimy listę preferowanych rur
prefferedPipes <- rury[rury$u >= 0.6 & rury$u <= 0.9, ]

if (nrow(prefferedPipes) == 0) {
    subset <- rury[rury$u > 0.9, ]
    prefferedPipe <- subset[nrow(subset), ]
} else{
    prefferedPipe <- prefferedPipes[1, ]
}

if (nrow(prefferedPipe) == 0) {
    subset <- rury[rury$u < 0.6, ]
    prefferedPipe <- subset[1, ]
}

# Pobieramy parametry do obliczania liczby Reynoldsa dla rury,
# która jest najbliżej optymalnych parametrów
D <- prefferedPipe$InternalDiameter
r <- prefferedPipe$InternalRadius
u <- prefferedPipe$u

# Liczymy liczbę Reynoldsa
Re = Reynolds_number(u, D, ro, mi)

# Obliczamy współczynnik oporu za pomocą przybliżenia
# Tkachenko-Mileikovskyi
# (zawsze możemy tez użyć innych zdefiniowanych metod, patrz wyżej)
lTM <- lambda_TkachenkoMileikovskyi(k, ((180 - 2 * 10.7) / 1000), Re)
deltahTM <- deltah(lTM, D, u, g)

print(paste("Po wykonaniu obliczeń dla Q=", Q, "[dm3*s-1]"))
print(paste("Preferowana średnica rury:", prefferedPipe$PipeNumber))
print(paste("Szybkość wody w rurze:", u, "[m*s-1]"))

if (verbose) {
    print(paste("Chropowatośc bezwzględna:", k, "[m]"))
    print(paste("Liczba Reynoldsa:", Re))
    print(paste("Współczynnik oporu:", lTM))
    print(paste("lepkość dynamiczna wody:", mi, "[Pa*s]"))
    
}
