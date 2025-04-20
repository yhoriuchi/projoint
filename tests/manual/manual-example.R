# Test example code from the Introduction page of projoint

# Load the projoint package
library(projoint)

# Reshape data for conjoint analysis
# Important: Include the repeated task variable for IRR estimation
data <- reshape_projoint(
  exampleData1, 
  .outcomes = c(paste0("choice", 1:8), "choice1_repeated_flipped")
)

print(data)

# Run conjoint analysis (default: marginal means with IRR correction)
output <- projoint(data)

# Plot the profile-level estimates
plot(output)

# Print the projoint_results object
print(output)

# Summarize the main estimates (tibble format)
summary(output)
