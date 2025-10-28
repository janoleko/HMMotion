tpm_g3 <- function(Eta, byrow = FALSE) {
  K <- ncol(Eta)
  N <- 0.5 + sqrt(0.25 + K)
  Gamma <- AD(array(1, dim = c(N, N, nrow(Eta))))
  
  expEta <- exp(Eta)
  
  ## Loop over entries (stuff over time happens vectorised which speeds up the tape)
  col_ind <- 1
  for(i in seq_len(N)){ # loop over rows
    for(j in seq_len(N)){ # loop over columns
      if(j != i){ # only if non-diagonal
        if(byrow){
          Gamma[i, j, ] <- expEta[, col_ind]
        } else{
          Gamma[j, i, ] <- expEta[, col_ind]
        }
        col_ind = col_ind + 1
      }
    }
  }
  # Normalise rows to sum to 1
  for(i in seq_len(N)){
    # transposing is necessary because Gamma[i,,] / colSums(Gamma[i,,]) does not work as expected
    Gamma[i, , ] <- t(t(Gamma[i, , ]) / rowSums(t(Gamma[i, , ])))
  }
  Gamma
}