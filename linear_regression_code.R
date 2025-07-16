library(tidyverse)
library(broom)
library(modelr)

# create data
data <- tibble(
  ads=c(10,15,20,25),
  sales=c(100,130,190,233)
)
# fit linear model 
model <-lm(sales~ads,data=data)

#tidy model summary 
model %>% tidy()
model %>% glance()

#predict and calculate residuals
data_with_preds <-data %>% 
  add_predictions(model) %>% 
  mutate(residual=sales-pred)

#calculate MSE
mse <-data_with_preds %>% 
  summarise(mse=mean(residual^2))

print(mse)

# predict new values
new_data <-tibble(ads=c(12,18))
predictions <-predict(model,newdata = new_data)
pr_df <-bind_cols(new_data,predicted_sales=predictions)

#plot
data %>% 
  ggplot(aes(x=ads,y=sales))+
  geom_point(color="blue",size=3)+
  geom_smooth(method="lm",se=FALSE,color="red")+
  geom_point(data=pr_df,aes(x=ads,y=predicted_sales),size=5,color="green")+
  labs(
    title="Linear Regression Example(tidyverse)",
    x="Ad budget",
    y="sales"
  )

#
library(tidyverse)
library(broom)
library(modelr)
library(plotly)

# Load data
read_csv("10_Property_stolen_and_recovered.csv") -> property_df
property_df %>% colnames()

# Fit linear model
model <- lm(Cases_Property_Recovered ~ Cases_Property_Stolen, data = property_df)

# Show model summaries
model %>% tidy()
model %>% glance()

# Plot using ggplot2
df1 <- property_df %>% 
  ggplot(aes(x = Cases_Property_Stolen, y = Cases_Property_Recovered, color = Area_Name)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Linear Regression Example (tidyverse)",
    x = "Cases Property Stolen",
    y = "Cases Property Recovered"
  )

# Display plot
df1
ggplotly(df1)

#------------------------------------------------------------------------------
#
library(tidyverse)
library(broom)
library(modelr)
library(plotly)

# Load data
read_csv("10_Property_stolen_and_recovered.csv") -> property_df
property_df %>% colnames()

# Fit linear model
model <- lm(Value_of_Property_Recovered ~ Value_of_Property_Stolen, data = property_df)

# Show model summaries
model %>% tidy()
model %>% glance()

# Plot using ggplot2
df1 <- property_df %>% 
  mutate(property_stolen_lakh=Value_of_Property_Stolen/100000) %>% 
  mutate(property_recovered_lakh=Value_of_Property_Recovered/100000) %>% 
  ggplot(aes(x = property_stolen_lakh, y = property_recovered_lakh, color = Area_Name)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Linear Regression Example (tidyverse)",
    x = "Cases Property Stolen",
    y = "Cases Property Recovered"
  )

# Display plot
df1
ggplotly(df1)


#--------------------------------------------------------------------------------

library(tidyverse)
library(broom)
library(modelr)
library(plotly)

# Example data

data <- tibble(
  age = c(22, 25, 30, 35, 40, 28, 32, 21),
  bought = c(0, 0, 1, 1, 1, 0, 1, 0)
)

# Fit logistic regression
model <- glm(bought ~ age, data = data, family = binomial)

#summary
tidy(model)

#predict probabailities

data_with_preds <- data %>% 
  mutate(prob = predict(model, type = "response"))

new_data <- tibble(age = c(24, 29, 38))

new_data %>% 
  mutate(probability = predict(model, newdata = ., type = "response"),
         predicted_class = if_else(probability > 0.5, 1, 0)) ->new_data

print(new_data)

# Plot: Age vs Probability of Buying
ggplot(data_with_preds, aes(x = age, y = prob)) +
  geom_point(aes(y = bought), color = "darkblue", size = 4) +
  geom_line(color = "tomato") +
  geom_point(data = new_data,
             aes(x = age, y = predicted_class),
             color = "green", size = 4) +
  labs(title = "Logistic Regression: Probability of Buying",
       x = "Age", y = "Predicted Probability") +
  theme_minimal()



