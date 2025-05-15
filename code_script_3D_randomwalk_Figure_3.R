if (!require(fractaldim)) {
  install.packages("fractaldim")
  library(fractaldim)
}

# 2. Simulate a highly persistent random walk
set.seed(2025)
n_steps <- 100      #change this to make mosquito paths longer
rho     <- 0.001    # Amount of autocorrelation: For D~1.05->0.99. Dor D~1.5->0.001. High autocorrelation: persists in its previous direction
step_sd <- 1         # overall step size

# Pre‐allocate
dx <- numeric(n_steps); dy <- numeric(n_steps); dz <- numeric(n_steps)

# First step random
dx[1] <- rnorm(1, 0, step_sd)
dy[1] <- rnorm(1, 0, step_sd)
dz[1] <- rnorm(1, 0, step_sd)

# Generate autocorrelated increments
for (i in 2:n_steps) {
  dx[i] <- rho*dx[i-1] + sqrt(1 - rho^2)*rnorm(1, 0, step_sd)
  dy[i] <- rho*dy[i-1] + sqrt(1 - rho^2)*rnorm(1, 0, step_sd)
  dz[i] <- rho*dz[i-1] + sqrt(1 - rho^2)*rnorm(1, 0, step_sd)
}

# Cumulative positions
x <- cumsum(dx); y <- cumsum(dy); z <- cumsum(dz)

# 3. Estimate fractal dimension on projections
estimate_D2 <- function(coords) {
  bc <- fd.estim.boxcount(coords, window.limit = max(diff(range(coords))), plot.loglog = FALSE)
  bc$fd
}
D_xy <- estimate_D2(cbind(x,y))
D_xz <- estimate_D2(cbind(x,z))
D_yz <- estimate_D2(cbind(y,z))
D_mean <- mean(c(D_xy, D_xz, D_yz))

cat(sprintf("Estimated D:  %.3f, %.3f, %.3f  → mean ≈ %.3f\n", D_xy, D_xz, D_yz, D_mean))

#Plot
lines3D(x, y, z, type="l",
        col="steelblue", lwd=2,
        xlab="X", ylab="Y", zlab="Z",
        main="3D Random Walk")

