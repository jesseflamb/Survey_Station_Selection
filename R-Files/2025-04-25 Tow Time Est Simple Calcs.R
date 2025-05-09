# Given values
wire_speed <- 42.5  # meters per minute
angle_degrees <- 45
depth <- 100  # meters

# Since the wire is moving at 40 meters per minute at a 45-degree angle, 
# the vertical speed is determined using trigonometry:

# Convert angle to radians
angle_radians <- angle_degrees * (pi / 180)

# Calculate vertical descent rate
vertical_speed <- wire_speed * sin(angle_radians)

# Calculate time to reach bottom
time_minutes <- depth / vertical_speed

# Print result
cat("Estimated time to reach the bottom:", round(time_minutes, 2), "minutes\n")
