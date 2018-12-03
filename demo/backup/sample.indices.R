sample.indices <- function(x, n) {
  x.orig = x
  sapply(1:get("n"), function(w) {
    tot = sum(x)
    probs = x / tot
    s = sample.int(n = length(x), size = 1, replace = F, prob = probs)
    x[s] <<- x[s] - 1
    s
  })
}