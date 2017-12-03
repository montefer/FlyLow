#JetBlue data analysis

####################  OBJECTIVES  ####################
                
        # 1) Use dataset to create graphs to determine trends within pricing for deals using ggplot
        # 2) From visible insight from plots/graphs, determine key information for developing NN models in Python
        # 3) Have functions that will return important statistics to the Python program for processing
                
                
                
                
#Code begins here
                
                
library(ggplot2)
setwd("C:/Users/Fernando/JetBlue")

#Store flight to/from data
dir.create("Flights_To_From")
dir.create("Flights_To_From_Plots")


#Import reduced dataset for speed
mar_lf<-read.csv("LowestFares.csv")

#Specify from what airport/region to what airport/region you want to fly
depart<-readline("Where are you leaving from?")
arrive<-readline("Where do you wish to arrive?")

ad_df<-rbind(mar_lf[0,])


i=1
while(i <= nrow(mar_lf)){
  if (mar_lf[i,1]==depart & mar_lf[i,2] ==arrive){
    ad_df<-rbind(ad_df,mar_lf[i,])
    i=i+1
  }
  else{
    i=i+1
  }
    
}


#Dates & Price dataframe (now for specific destination)
#dp_df<-cbind.data.frame(mar_lf[,3],mar_lf[,6])
lf_data<-cbind.data.frame(ad_df[,3],ad_df[,6])

#Take a sample of the data so ggplot can handle it in an efficient manner
#lf_data<-dp_df[sample(nrow(dp_df),1000),]
lf_data[,1]<-as.Date(lf_data[,1],"%m/%d/%Y")

#Save dataframe as a csv file
csv_name<-paste("Flights_To_From/",depart," to ",arrive, ".csv",sep="")
write.csv(lf_data, file=csv_name)

#Weekdays
weekday_lf<-lf_data[which(weekdays(as.Date(lf_data[,1], format = "%m/%d/%Y")) %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]

#Weekends
weekend_lf<-lf_data[which(weekdays(as.Date(lf_data[,1], format = "%m/%d/%Y")) %in% c('Saturday','Sunday')), ]


#Create scatterplots for price across time and dates
plot<-ggplot() +
  geom_point(data=weekday_lf, aes(x=weekday_lf[,1],y=weekday_lf[,2], color=weekday_lf[,2])) +
  geom_point(data=weekend_lf, aes(x=weekend_lf[,1],y=weekend_lf[,2], color=weekend_lf[,2])) +
  geom_smooth(aes(x=lf_data[,1],y=lf_data[,2]), method = "loess") +
  scale_colour_gradient(name="Price", low='blue', high='#de2d26')


#Re-write axis labels, add title
plot<-plot +
  labs(title = paste("Price by Date from",depart,"to",arrive), x = "Date", y = "Price") +
  theme(axis.text.x=element_text(angle=40, size=10, vjust=0.5))


#Save plot  
ggsave(filename = paste("Flights_To_From_Plots/",depart," to ",arrive,".png", sep=""),plot = last_plot())
