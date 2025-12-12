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

library(readr)
library(writexl)

source("PipesCalculations.r")

cross <- read_csv("file.csv")
cross$Direction <- ifelse(cross$Qp >= cross$Qk,1,-1)
cross <- cross[ , c(7,2,1,3,4,5,6)]

### Data available at this step: (Direction,Ring) Odcinek, D*, L, Op, Qk

cross$Q0 <- (abs(cross$Qp - cross$Qk))
cross$Q055 <- (cross$Q0*0.55)

### Data available at this step: (...) Odcinek, D*, L, Op, Qk, Q0, 0.55xQ0

cross$Q1 <- ifelse(
    cross$Qp > cross$Qk, cross$Qk + cross$Q055,
    ifelse(
        cross$Qp < cross$Qk, cross$Qp + cross$Q055, cross$Qp
    )
)*cross$Direction

### Data available at this step: (...) Odcinek, D*, L, Op, Qk, Q0, 0.55xQ0, Q1

if(all(is.na(cross$D))){
    cross$D <- mapply(function(q) PickAPipe(q)$PipeNumber, abs(cross$Q1))
    cross$IntD <- mapply(function(q) PickAPipe(q)$IntD, abs(cross$Q1))
} else {
    cross$IntD <- mapply(function(q,p) PickAPipeByNumber(q,p)$IntD, abs(cross$Q1),cross$D)
}

### Data available at this step: (..., IntD) Odcinek, D, L, Op, Qk, Q0, 0.55xQ0, Q1,
### What we need: I1, h1, h1/Q1, deltaQ1

Approximation <- function(cross,step){
    print(paste("Przybliżenie nr",step))
    if(step > 1){
        cross[[paste0("Q",step)]] <- ifelse(cross$Ring == 1, cross[[paste0("Q",step-1)]] + cross[[paste0("deltaQ",step-1)]], NA)
    }

    cross$V <- mapply(function(q,r) WaterVelocity(q,r), abs(cross[[paste0("Q",step)]]), (cross$IntD/2))
    if(step == 1){cross$startV <- cross$V}

    #cross$V <- ifelse(cross$Ring == 1,WaterVelocity(abs(cross[[paste0("Q",step)]]),(cross$IntD/2)),cross$V)
    ### Data available at this step: (V, ...) Odcinek, D, L, Op, Qk, Q0, 0.55xQ0, Q1

    cross$Re <- mapply(function(v,r,ro,mi) ReynoldsNumber(v,r,ro,mi), cross$V, cross$IntD, ro, mi)
    ### Data available at this step: (Re, ...) Odcinek, D, L, Op, Qk, Q0, 0.55xQ0, Q1

    cross$Lambda <- mapply(function(k,D,Re) lambda(k,D,Re), k, cross$IntD, cross$Re)
    ### Data available at this step: (Lambda, ...) Odcinek, D, L, Op, Qk, Q0, 0.55xQ0, Q1

    cross[[paste0("I",step)]] <- mapply(function(lbd,D,v,g) deltah(lbd,D,v,g), cross$Lambda, cross$IntD, cross$V, g)
    ### Data available at this step: (...) Odcinek, D, L, Op, Qk, Q0, 0.55xQ0, Q1, I1

    cross[[paste0("h",step)]] <- (cross$Direction)*(cross[[paste0("I",step)]]/1000)*cross$L*1.1
    ### Data available at this step: (...) Odcinek, D, L, Op, Qk, Q0, 0.55xQ0, Q1, I1, h1

    if(abs(sum(cross[[paste0("h",step)]],na.rm = TRUE)) <= 0.5){
        cross[,"PWQp"] <- NA
        cross[,"PWQk"] <- NA

        rows <- which(cross$Ring == 1)
        for (i in rows) {
            cross[[i, paste0("Q",step)]]
            if(cross$Q1[i] < 0 & cross[[i, paste0("Q",step)]] < 0){
                cross$PWQp[i] <- abs(cross[[i, paste0("Q",step)]]) - cross$Q055[i]
                cross$PWQk[i] <- cross$PWQp[i] + cross$Q0[i]
            }
            if(cross$Q1[i] > 0 & cross[[i, paste0("Q",step)]] > 0){
                cross$PWQk[i] <- cross[[i, paste0("Q",step)]] - cross$Q055[i]
                cross$PWQp[i] <- cross$PWQk[i] + cross$Q0[i]
            }
            if(cross$Q1[i] > 0 & cross[[i, paste0("Q",step)]] < 0){
                cross$PWQk[i] <- abs(cross[[i, paste0("Q",step)]]) + cross$Q055[i]
                cross$PWQp[i] <- cross$PWQk[i] - cross$Q0[i]
            }
            if(cross$Q1[i] < 0 & cross[[i, paste0("Q",step)]] > 0){
                cross$PWQp[i] <- cross[[i, paste0("Q",step)]] + cross$Q055[i]
                cross$PWQk[i] <- cross$PWQp[i] - cross$Q0[i]
            }
        }

        spreadh <- sum(cross[[paste0("h",step)]],na.rm = TRUE)/sum(!is.na(cross[[paste0("h",step)]]))
        cross[[paste0("h",step)]] <- ifelse(cross$Ring == 1,cross[[paste0("h",step)]]-spreadh,NA)

        ### Data available at this step: (...) Odcinek, D, L, Op, Qk, Q0, 0.55xQ0, Q1, I1, h1, PWQp, PWQk
        ### Get back V for non-ring parts
        cross$V <- ifelse(is.na(cross$V),cross$startV,cross$V)
        cross$startV <- NULL

        ### Reorder colums:
        cross <- cross[ , c(12, 11, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15:ncol(cross))]
        cross$Odcinki <- cross$Odcinek
        target_pos <- ncol(cross) - 3
        cols <- names(cross)
        others <- cols[-1]
        new_order <- append(others, cols[1], after = target_pos - 1)
        cross <- cross[ , new_order]

        ### Write file:
        write_xlsx(cross, paste0("file-",format(Sys.time(), "%Y-%m-%d_%H%M"),".xlsx"))
        return(cross)
    } else {
        cross[[paste0("h",step,"Q",step)]] <- ifelse(cross$Ring == 1, cross[[paste0("h",step)]]/cross[[paste0("Q",step)]], NA)
        ### Data available at this step: (...) Odcinek, D, L, Op, Qk, Q0, 0.55xQ0, Q1, I1, h1, h1/Q1

        cross[[paste0("deltaQ",step)]] <- ifelse(cross$Ring == 1, -((sum(cross[[paste0("h",step)]],na.rm = TRUE))/(2*sum(cross[[paste0("h",step,"Q",step)]],na.rm = TRUE))), NA)
        ### Data available at this step: (...) Odcinek, D, L, Op, Qk, Q0, 0.55xQ0, Q1, I1, h1, h1/Q1, deltaQ

        return(Approximation(cross,step+1))
    }
}

cross <- Approximation(cross,1)cross <- Approximation(cross,1)