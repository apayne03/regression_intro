library(tidyverse)
library(apaTables)

population_data <- read_csv("population_data.csv")
glimpse(population_data)

# get sample
set.seed(1) #ensures we all get the same random sample (don't usually need to use this)
sample_analytic_data <- sample_n(population_data, size = 200)
glimpse(sample_analytic_data)

# sample regression
sample1_lm_results <- lm(performance ~ IQ, data=sample_analytic_data) # lm(performance ~ IQ + 1, data=sample_analytic_data) +1 means "btw I want an intercept, notice we don't put it in typically, but if you wanted no intercept you would put -1
summary(sample1_lm_results)
# intercept tells you the elevation of the line; slope tells you the angle of the line


apa.reg.table(sample1_lm_results)
# fit = r(squared)

# population regression
population_lm_results <- lm(performance ~ IQ, data=population_data)
summary(population_lm_results) # don't do apa reg tables on this one since it doesn't make sense to have a CI for the population

# when there's only one predictor, the beta weight and the zero-order or regular correlation are the same value; not true if there's more than one predictor


# predicted value for a single person
x_axis_range <- data.frame(IQ=c(120))

CI_data <- predict(sample1_lm_results, newdata=x_axis_range, interval = "confidence", level=.95)

CI_data <- as.data.frame(cbind(x_axis_range, CI_data)) # combine x_axis_range with CI_data; puts IQ back in
CI_data 


# predicted value for entre x-axis range (aka regression line)
min_predictor <- min(sample_analytic_data$IQ)
max_predictor <- max(sample_analytic_data$IQ)

x_axis_range <- data.frame(IQ=seq(min_predictor, max_predictor, by = .5))

x_axis_range # will run each of these values through the regression equation

x_axis_range <- data.frame(IQ=seq(min_predictor, max_predictor, by = .5))
CI_data <- predict(sample1_lm_results, newdata=x_axis_range, interval = "confidence", level=.95)
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))
CI_data

PI_data <- predict(sample1_lm_results, newdata=x_axis_range, interval = "prediction", level=.95)
PI_data <- as.data.frame(cbind(x_axis_range, PI_data))

head(CI_data)
head(PI_data)
# note: the PI is wider than the CI


reg_plot <- ggplot(sample_analytic_data, aes(x=IQ, y=performance)) # aes = aestethic, unless I tell you otherwise, take the x value from IQ and y value from performance
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic()
reg_plot <- reg_plot + geom_smooth(method = "lm", se=TRUE)
reg_plot <- reg_plot + geom_smooth(data=PI_data, aes(x=IQ, y=fit, ymin=lwr, ymax=upr), stat="identity")
print(reg_plot)

#reg_plot <- reg_plot + geom_smooth(data=CI_data, aes(x=IQ, y=fit, ymin=lwr, ymax=upr), stat="identity") --> This is another way to calculate CI instead of the "method="lm"; use this way if you do need to access the raw data, can use method shortcut if you don't need to access the raw data