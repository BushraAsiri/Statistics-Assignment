#load data
data(ChickWeight)
 
 ##weight as the dependent variable and Time as the independent variable
 chick_model<-lm(weight~Time,data=ChickWeight)
 summary_model<-summary(chick_model)
 print(summary_model)
  #1- Find the coefficients of determination
  summary_model$r.squared
  
  # 2- View p_value
  p_value <- summary_model$coefficients["Time", "Pr(>|t|)"]
   
  # Check the significant relationship
     if (p_value < 0.05) {
        print("The model is significant relationship")
       } else {
          print("The model is not significant relationship")
         }
  
 


#load the data
data(women)
View(women)
# Fit simple linear regression model 
women_model<-lm(weight~height,data=women)
#predict the weight using the height value in dataset
predicted_weight<-predict(women_model,newdata=women)
#Add new column to the predict weight
women$predicted_weight<-predicted_weight
print(women)








#load the data
fish_data<-read.csv('~/Downloads/Fish.csv')
fish_data$Species<-as.factor(fish_data$Species)

# Apply multiple linear regression model
Fish_model<-lm(Weight~Species+Length1+Length2+Length3+Height+Width,data=fish_data)

#Define new values
input_values <- data.frame(
  Length1 = c(28.5, 28.4),
  Length2 = c(30.7, 31),
  Length3 = c(36.2, 36.2),
  Height = c(14.2266, 14.2628),
  Width = c(4.9594, 5.1042)
)
#Init a vector to store predictions
predictions<-c()
#  loop each unique fish Species
for (species in unique(fish_data$Species)) {
  
  new_data <- data.frame(
    Species =factor(rep(species,nrow(input_values)),levels=levels(fish_data$Species)),
    Length1=input_values$Length1,
    Length2=input_values$Length2,
    Length3=input_values$Length3,
    Height=input_values$Height,
    Width=input_values$Width
  )
  
  
  # predict the  weights for the current species 
  predicted_weights <- predict(Fish_model, newdata = new_data)
  
  # Append species and predicted weights to 'predictions'
  predictions <- rbind(predictions, data.frame(Species =species, Predicted_Weight = predicted_weights))
}

print(predictions)








