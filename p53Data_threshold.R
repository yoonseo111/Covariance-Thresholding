samplingIntoTwo <- function(data, num_sampling){
  samp <- samp(nrow(data), num_sampling)
  px <- t(data[, samp])
  py <- t(data[, -samp])
  
  cor_px <- cor(px)
  cor_py <- cor(py)
  
  return(list(cor_px=cor_px, cor_py=cor_py))
}

num_sampling <- 17

cor_matrix <- samplingIntoTwo(p, num_sampling)
cor_px <- cor_matrix$cor_px
cor_py <- cor_matrix$cor_py


# Threshold
calculateThreshold <- function(cor_matrix) {
  thr <- cor_matrix[upper.tri(cor_matrix)]
  
  gamma <- 1 - (1 / nrow(cor_matrix)) * norm(cor_matrix, type = "2")
  alpha <- ceiling(gamma * ((nrow(cor_matrix) * (nrow(cor_matrix) - 1)) / 2))
  
  threshold <- thr[order(thr) == alpha]
  
  return(threshold)
}

cor_px <- cor(px)
cor_py <- cor(py)

threshold1 <- calculateThreshold(cor_px)
threshold2 <- calculateThreshold(cor_py)