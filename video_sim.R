
# Simulation of tracking error, i.e., measurement error in position based on video tracking
# -----------------------------------------------------------------------------------------
# By R. Dakin for the Manakin Genomics RCN Meeting
# April 2018

library(dplyr)

# -----------------------------------------------------------------------------------------
# Often we want to use video tracking to obtain and compare velocities and accelerations. Suppose we have two time points, t1 and t2.
# Velocity can be estimated as the change in position / change in time, or: (position2 - position1)/(t2 - t1)
# And acceleration can be estimated as the change in velocity / change in time, or: (velocity2 - velocity1)/(t2 - t1)

# But what is the effect of position measurement error on these estimates?

# Here we simulate some position data that might be obtained from video: 
time <- 1:60 # time in video frames
position.real <- time*20 # This is the bird's actual position, in cm
error <- 3 # Now we add measurement error. 0 = no error. We can experiment with this value. Larger = more measurement error

position <- position.real + rnorm(length(time), 0, error) # This is the measured position = real position + error

# Plot the results:
dev.new(width=5, height=6)
par(mfrow=c(3,1), las=1, mar=c(4,4,0.25,0.25), mgp=c(1.8,0.5,0), bty='l')
plot(position ~ time, pch=16, cex=0.5, ylim=c(0,max(position.real)), xlab='Time (frames)', ylab='Position (cm)')
points(position.real ~ time, type='l', lwd=2, col='grey')
points(position ~ time, pch=16, cex=0.5)
legend('topleft', bty='n', lwd=c(2, NA), col=c('grey','black'), pch=c(NA, 16), legend=c('Real','Measured'))

velocity <- 30/100*(position - lag(position))/(time - lag(time)) # Actual speed in m/2
velocity.real <- 30/100*(position.real - lag(position.real))/(time - lag(time)) # Measured speed

plot(velocity ~ time, type='l', ylim=c(-10,10)+2, col='orange', xlab='Time (frames)', ylab='Velocity (m/s)')
points(velocity.real~ time, type='l', lwd=2, col='grey')
points(velocity ~ time, pch=16, cex=0.5)

acceleration <- (velocity - lag(velocity))/(time - lag(time)) # Actual acceleration in m/s2
acceleration.real <- (velocity.real - lag(velocity.real))/(time - lag(time)) # Measured acceleration

plot(acceleration ~ time, type='l', ylim=c(-10,10), col='red', xlab='Time (frames)', ylab='Acceleration (m/s2)')
points(acceleration.real~ time, type='l', lwd=2, col='grey')
points(acceleration ~ time, pch=16, cex=0.5)

# Re-run the steps above with error = 0 and compare with error = 0.1, 1, 2, 3...

# What's going on? Zoom in on a few points:
par(mfrow=c(3,1), las=1, mar=c(4,4,0.25,0.25), mgp=c(1.8,0.5,0))
plot(position[10:12]~ time[10:12], pch=16, xlab='Time (frames)', ylab='Position (cm)')
points(position.real ~ time, type='l', lwd=2, col='grey')
points(position[10:12] ~ time[10:12], type='l')
legend('topleft', bty='n', lwd=c(2, NA), col=c('grey','black'), pch=c(NA, 16), legend=c('Real','Measured'))
# When the position is underestimated at t1 and overestimated at t2, the slope (velocity) is too high.
# When position is overestimated at t1 and underestimated at t2, the slope (velocity) is too low, etc.
# This problem compounds when using error-prone velocities to estimate acceleration.



# -----------------------------------------------------------------------------------------
# What if velocity is not constant? Here we simulate constant acceleration:

time <- 1:60
position.real <- time^2
position <- position.real + rnorm(length(time), 0, error)

dev.new(width=5, height=6)
par(mfrow=c(3,1), las=1, mar=c(4,4,0.25,0.25), mgp=c(1.8,0.5,0))
plot(position ~ time, pch=16, cex=0.5, ylim=c(0,max(position.real)), xlab='Time (frames)', ylab='Position (cm)')
points(position.real ~ time, type='l', lwd=2, col='grey')
points(position ~ time, type='l', col='brown')

velocity <- 30/100*(position - lag(position))/(time - lag(time))
velocity.real <- 30/100*(position.real - lag(position.real))/(time - lag(time))

plot(velocity ~ time, type='l', ylim=c(-5,15), col='orange', xlab='Time (frames)', ylab='Velocity (m/s)')
points(velocity.real~ time, type='l', lwd=2, col='grey')
points(velocity ~ time, pch=16, cex=0.5)

acceleration <- (velocity - lag(velocity))/(time - lag(time))
acceleration.real <- (velocity.real - lag(velocity.real))/(time - lag(time))

plot(acceleration ~ time, type='l', ylim=c(-10,10), col='red', xlab='Time (frames)', ylab='Acceleration (m/s2)')
points(acceleration.real~ time, type='l', lwd=2, col='grey')
points(acceleration ~ time, pch=16, cex=0.5)

# In real life, the animal may not be in very many frames and it will typically not have a constant acceleration or velocity. This only adds to the challenge. How do we separate real fluctuations from noise?




# -----------
# Summary: Velocities, and especially Accelerations, are more difficult to measure accurately and consistently, because small errors get compounded when we rely on noisy measurements from multiple video frames.
# An important consideration for many field studies (esp. manakins) is that the calibration may be done separately for each individual/display court. This raises the possibility that variation among calibrations can generate spurious variation among individuals/display courts. Therefore, it is important to try to obtain multiple measures of each site (with a new calibration each time), and to ground-truth your calibrations.

# What can we do? Here are some strategies to deal with this:
# 		1. Smooth the position data (e.g., using a rolling average, lowess filter, kalman filter or other smoothing algorithm... note that a smoothing function may be "built-in" for software programs that do automated tracking). Smoothing is a good idea if you want to calculate velocities or accelerations, but it becomes more challenging if there are only a few frames to work with.
#		2. Try to be consistent. Be wary of comparing values that were obtained from different smoothing methods and parameters. Accelerations in particular are highly sensitive to error, making it problematic to compare across studies that use different methods.
# 		3. Ground-truth your calibrations. e.g., Measure a ruler. Drop a high-contrast object and measure its acceleration due to gravity. Do these measurements repeatedly. On average, the acceleration of dropped objects should be 9.8 m/s2. You can report these values +/- range as an indication of acceleration measurement error. I can't necessarily predict how changes to the setup might affect values obtained, but I can always figure it out by experimenting and comparing the same ground-truth measurements on two different setups.
# 		4. Report ground-truth data in your study so that others can interpret the values.
# 		5. Try to take repeated, independent measures of each individual (re-calibrating each time). That will reduce the chance that the calibration error gets confounded with individiual IDs.


