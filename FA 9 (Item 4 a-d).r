# 1. Manually define the full mtcars dataset (32 observations)
mpg <- c(21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2, 17.8, 16.4, 
         17.3, 15.2, 10.4, 10.4, 14.7, 32.4, 30.4, 33.9, 21.5, 15.5, 15.2, 13.3, 
         19.2, 27.3, 26.0, 30.4, 15.8, 19.7, 15.0, 21.4)

wt  <- c(2.620, 2.875, 2.320, 3.215, 3.440, 3.460, 3.570, 3.190, 3.150, 3.440, 
         3.440, 4.070, 3.730, 3.780, 5.250, 5.424, 5.345, 2.200, 1.615, 1.835, 
         2.465, 3.520, 3.435, 3.840, 3.845, 1.935, 2.140, 1.513, 3.170, 2.770, 
         3.570, 2.780)

# (a) Fit regression: mpg (dependent) ~ wt (independent)
model_a <- lm(mpg ~ wt)
coef_a <- coef(model_a)

# (b) Fit regression: wt (dependent) ~ mpg (independent)
model_b <- lm(wt ~ mpg)
coef_b <- coef(model_b)

# (c) Plotting both on the same axes
plot(wt, mpg, main="Regression Line Comparison", 
     xlab="Weight (1000 lbs)", ylab="MPG", pch=19)

# Line A (Blue): Directly plot using abline
abline(model_a, col="blue", lwd=2)

# Line B (Red): Rearrange wt = b0 + b1*mpg into mpg = (wt - b0) / b1
# This is necessary because the plot axes are (wt, mpg)
b0 <- coef_b[1]
b1 <- coef_b[2]
curve((x - b0) / b1, add=TRUE, col="red", lwd=2, lty=2)

legend("topright", legend=c("mpg ~ wt", "wt ~ mpg"), 
       col=c("blue", "red"), lwd=2, lty=c(1,2))

# (d) Interpretations
print(paste("Slope A (mpg ~ wt):", round(coef_a[2], 3)))
print(paste("Slope B (wt ~ mpg):", round(coef_b[2], 3)))
