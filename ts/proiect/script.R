lambda <- function(t) {
  if (t < 12) {
    return (10 + 3 * t * cos(t) + 2 * t^3 + sin(t))
  }
  else {
    return (7)
  }
}

Ts <- function(s, l=5302) {
  t = s
  while(1) {
    U1 = runif(1, min=0, max=1)
    U2 = runif(1, min=0, max=1)
    t = t - (1 / l) * log(U1)
    if (U2 <= lambda(t) / l) {
      break
    }
  }
  return (t)
}
lambda(pi / 2)
Ts(10)
