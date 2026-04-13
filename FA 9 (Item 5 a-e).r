# 1. Define the full cars dataset (50 observations)
speed <- c(4, 4, 7, 7, 8, 9, 10, 10, 10, 11, 11, 12, 12, 12, 12, 13, 13, 13, 
           13, 14, 14, 14, 14, 15, 15, 15, 16, 17, 17, 17, 18, 18, 18, 18, 
           19, 19, 19, 20, 20, 20, 20, 20, 22, 23, 24, 24, 24, 24, 25)
dist  <- c(2, 10, 4, 22, 16, 10, 18, 26, 34, 17, 28, 14, 20, 24, 28, 26, 34, 
           34, 46, 26, 36, 60, 80, 20, 26, 54, 32, 32, 40, 50, 42, 56, 76, 
           84, 36, 46, 68, 32, 48, 52, 56, 64, 66, 54, 70, 92, 93, 120, 85)

# (a) Scatter plot
plot(speed, dist, main="Stopping Distance vs Speed", 
     xlab="Speed (V) mph", ylab="Distance (D) ft", pch=19, col="darkgray")

# (b) Fit Least-Squares Quadratic Model: D = a0 + a1V + a2V^2
model_quad <- lm(dist ~ speed + I(speed^2))
summary(model_quad)

# (c) Overlay Quadratic Curve
# Create a range of speeds to draw a smooth curve
v_range <- seq(min(speed), max(speed), length.out=100)
d_pred_quad <- predict(model_quad, data.frame(speed=v_range))
lines(v_range, d_pred_quad, col="blue", lwd=2)

# (d) Estimate stopping distance for V = 45 and 80 mph
new_speeds <- data.frame(speed = c(45, 80))
preds <- predict(model_quad, new_speeds)
print(paste("Predicted Distance at 45 mph:", round(preds[1], 2), "ft"))
print(paste("Predicted Distance at 80 mph:", round(preds[2], 2), "ft"))

# (e) Fit Linear Model and Overlay
model_lin <- lm(dist ~ speed)
abline(model_lin, col="red", lwd=2, lty=2)

legend("topleft", legend=c("Quadratic Fit", "Linear Fit"), 
       col=c("blue", "red"), lwd=2, lty=c(1, 2))

# Comparison based on Fit (Residual Sum of Squares)
rss_lin <- sum(residuals(model_lin)^2)
rss_quad <- sum(residuals(model_quad)^2)
print(paste("RSS Linear:", round(rss_lin, 2)))
print(paste("RSS Quadratic:", round(rss_quad, 2)))
