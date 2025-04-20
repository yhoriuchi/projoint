# Test example on the predict page

# 3,1 ---------------------------------------------------------------------

library(projoint)

# 3.2 ---------------------------------------------------------------------

data(out1_arranged)

predicted_irr <- predict_tau(out1_arranged)

print(predicted_irr)

summary(predicted_irr)

plot(predicted_irr)
