# MIT License
# Copyright (c) 2025 Marek Pałach-Rydzy

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# Deklarujemy dane
# Deklarujemy wartości do dalszych obliczeń
g = 9.81 # przyśpieszenie ziemskie [m*s-2]
ro = 999.7 # gęstość wody w temperaturze 10 [kg*m-3]
mi = 0.001307 # lepkość dynamiczna wody w temperaturze 10 [Pa*s]
k = 0.00005 # chropowatośc bezwzględna [m]

# Deklarujemy funkcje

# Funkcja zwraca tablicę z parametrami rur (śr. zewnętrzna, grubość ścianki)
# Ta funkcja jest opracowana dla rur HDPE PE100 SDR17 PN10
# Dane spisane wg. katalogu Pipelife: https://www.pipelife.pl/content/dam/pipelife/poland/marketing/downloads/bibliotekapdf/katalogi_techniczne_pdf/K_SysCisnieniowe_Pipelfe.pdf
pipes_hdpe_pe100_sdr17_pn10 <- function() {
    pipes <- data.frame(
#        PipeNumber = c(90, 110, 125, 140, 160, 180, 200, 225, 250, 280, 315, 355, 400),
        PipeNumber = c(90, 110, 125, 160, 180, 200, 225, 250, 280, 315, 355, 400),
        WallThickness = c(
            5.4,
            6.6,
            7.4,
            9.5,
            10.7,
            11.9,
            13.4,
            14.8,
            16.6,
            18.7,
            21.1,
            23.7
        ),
        IntD = NA,
        InternalRadius = NA,
        Q = NA,
        u = NA
    )

    pipes$IntD <- (pipes$PipeNumber - (pipes$WallThickness) *
                                   2) / 1000
    pipes$InternalRadius <- (pipes$IntD / 2)

    return(pipes)
}

# Współczynnik oporu, przybliżenie Tkachenko-Mileikovskyi (2020)
lambda <- function(k, D, Re) {
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
WaterVelocity <- function(Q, r) {
    velocity <- (Q / 1000) / ((pi * (r ** 2)))
    return(velocity)
}

ReynoldsNumber <- function(u, D, ro, mi) {
    Re <- (u * D * ro) / mi
    return(Re)
}

PickAPipeByNumber <- function(Q,P){
    pipes <- pipes_hdpe_pe100_sdr17_pn10()
    prefferedPipe <- pipes

    if (P %in% pipes$PipeNumber) {
        prefferedPipe <- pipes[pipes$PipeNumber == P, ]
    } else{
        stop(paste(
            "Zła średnica rury. Użyj którejś z tej listy:",
        ))
    }

    prefferedPipe$Q <- Q
    prefferedPipe$u <- apply(prefferedPipe, 1, function(row) {
        WaterVelocity(row["Q"], row["InternalRadius"])
    })

    return(prefferedPipe)
}

PickAPipe <- function(Q){
    pipes <- pipes_hdpe_pe100_sdr17_pn10()

    pipes$Q <- Q
    pipes$u <- apply(pipes, 1, function(row) {
        WaterVelocity(row["Q"], row["InternalRadius"])
    })

    # Otherwise we let the script pick the right pipe
    prefferedPipes <- pipes[pipes$u >= 0.6 & pipes$u <= 0.9, ]

    if (nrow(prefferedPipes) == 0) {
        subset <- pipes[pipes$u > 0.9, ]
        prefferedPipe <- subset[nrow(subset), ]
    } else{
        prefferedPipe <- prefferedPipes[1, ]
    }

    if (nrow(prefferedPipe) == 0) {
        subset <- pipes[pipes$u < 0.6, ]
        prefferedPipe <- subset[1, ]
    }

    return(prefferedPipe)
}

PipeIntD <- function(PipeNumber){
    pipes <- pipes_hdpe_pe100_sdr17_pn10()
    pipe <- pipes[pipes$PipeNumber == PipeNumber, ]
    return((pipe)$IntD)
}


