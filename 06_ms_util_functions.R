# Function's we'll use

# Make a single U submatrix from a pi (transfer probs) vector
pi2u <- function(pivec, 
                 from ="H",
                 to = "H",
                 start_age = 20,
                 interval = 1) {
  out           <- cbind(rbind(0, diag(pivec)), 0)
  n             <- length(pivec)

  ages          <- ((0:n) * interval) + start_age 
  from_names    <- paste(from, ages, sep = "::")
  to_names      <- paste(to, ages, sep = "::")
  dimnames(out) <- list(to_names, from_names)
  out
}

# Compose u blocks into U (2 transient states assumed)
u2U <- function(HH, HU, UH, UU){
  rbind(
    cbind(HH, UH),
    cbind(HU, UU))
}

# convert transient dynamics into outcomes: the fundamental matrix, N
U2N <- function(U, 
                interval = 1, 
                dudel_discount = FALSE) {
  I   <- diag(nrow(U))
  # solve() is matrix inverse in this usage
  Nsx <- solve(I - U) * interval
  dimnames(Nsx) <- dimnames(U)
  
  if (dudel_discount){
    Nsx <- Nsx - diag(Nsx) * interval / 2
  }
  
  Nsx
}

