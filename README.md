#Read the Data
cancer_data <- fread("cancer_data.csv")

#Create new data table with average ages per lesion_id, write it to a csv file, and make a graph displaying the data 
lesion_id_analysis <- function(cancer_data) {
  avg_lesion_age <- dcast(cancer_data, lesion_id ~ ., mean, value.var=c("age"))
  setnames(avg_lesion_age, ".", "avg_lesion_tage")
  fwrite(avg_lesion_age, "avg_lesion_age.csv")
  
  count1 <- table(avg_lesion_age)
  barplot(count1, main="Most Frequent Average Ages per Lesion ID", xlab="Age (years)", ylab="Count", sub="This graph shows the frequency of   ages based on unique lesion IDs")
}


#Create new data table with average ages per gender, write it to a csv file, and make a graph displaying the data 
gender_analysis <- function(cancer_data) {
  avg_sex_age <- dcast(cancer_data, sex ~ ., mean, value.var=c("age"))
  setnames(avg_sex_age, ".", "avg_sex_age")
  fwrite(avg_sex_age, "avg_sex_age.csv")
  
  ggplot(data=avg_sex_age, aes(x=sex, y=avg_sex_age, label=avg_sex_age)) + 
+     geom_bar(colour="blue", stat="identity") + 
+     xlab("Gender") + ylab("Average Ages") +
+     ggtitle("Average Ages per Gender") + labs(caption = "This graph shows the average ages grouped by gender for those in the study testing skin cancer patients.") + geom_text(aes(y = avg_sex_age), size = 3)
}


#Create new data table with average ages per body area, write it to a csv file, and make a graph displaying the data 
localization_analysis <- function(cancer_data) {
  avg_area_age <- dcast(cancer_data, localization ~ ., mean, value.var=c("age"))
  setnames(avg_area_age, ".", "avg_area_age")
  fwrite(avg_area_age, "avg_area_age.csv")
  
  ggplot(data=avg_area_age, aes(x=localization, y=avg_area_age, label=avg_area_age)) + 
+     geom_bar(colour="blue", stat="identity") + 
+     xlab("Area of the Body") + ylab("Average Ages") +
+     ggtitle("Average Ages by Body Part") + labs(caption = "This graph shows the average ages for those affected by skin cancer based on the area of the body affected by the cancer.") + geom_text(aes(y = avg_area_age), size = 3)
}
