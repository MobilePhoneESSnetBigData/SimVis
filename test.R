#check the sinal strength/quality in the field

#distance from the antenna
d = seq(from = 1000, to = 30000, by = 500)

#power of the antenna
P = 10

#antenna parameters
SStepp = 0.2
Smid = -92.5

S0 <- function(P) {
  30 + 10 * log10(P)
}

SDist <- function(dist) {
  10 * 3.2 * log10(dist)
}

Sd <- S0 - SDist(d)
quality = 1.0 / (1 + exp(-SSteep * (Sd - Smid)))
df <- cbind(result, d)
df
