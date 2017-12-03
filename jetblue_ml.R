#Self-made basic ML (I really hope this works...)

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
dir.create("Predicted_Flights_To_From_Plots")

#Import data
lows_fare<-read.csv("LowestFares.csv")

#Set Holiday Dates
beg_sb<-format(as.Date("2017-03-10", format = "%Y-%m-%d"))
end_sb<-format(as.Date("2017-03-20", format = "%Y-%m-%d"))
beg_thksgvg<-format(as.Date("2017-11-22", format = "%Y-%m-%d"))
end_thksgvg<-format(as.Date("2017-11-28", format = "%Y-%m-%d"))
beg_sumb<-format(as.Date("2017-05-10", format = "%Y-%m-%d"))
end_sumb<-format(as.Date("2017-07-10", format = "%Y-%m-%d"))
beg_hol_seas<-format(as.Date("2017-12-15", format = "%Y-%m-%d"))
end_hol_seas<-format(as.Date("2017-01-07", format = "%Y-%m-%d"))


####################Function to generate plot#####################

generate_predicted_plot<-function(depart, arrive, date){
  
  date="2017-11-11"
  
  date<-format(as.Date(date), format = "%Y-%m-%d")
  date_range<-c(seq.Date(from = as.Date(as.Date(date)-6),to = as.Date(as.Date(date)+6), by = "1 day"))
  
  ad_df<-rbind(lows_fare[0,])
  
  #Filter
  ad_df<-lows_fare[lows_fare[,"Origin"]==depart,]
  ad_df<-ad_df[ad_df[,"Destination"]==arrive,]
  
  
  #Dates & Price dataframe (now for specific destination)
  lf_data<-cbind.data.frame(ad_df[,3],round(jitter(ad_df[,6],5)))
  
  
  #Turn date-times into just dates
  lf_data[,1]<-(as.Date(lf_data[,1],"%m/%d/%Y")+round(rnorm(length(lf_data[,1]),2)))
  
  #Save dataframe as a csv file
  #csv_name<-paste("Flights_To_From/",depart," to ",arrive, ".csv",sep="")
  #write.csv(lf_data, file=csv_name)
  
  #Weekdays
  weekday_lf<-lf_data[which(weekdays(as.Date(lf_data[,1], format = "%m/%d/%Y")) %in% c('Monday','Tuesday', 'Wednesday', 'Thursday')), ]
  
  #Weekends
  weekend_lf<-lf_data[which(weekdays(as.Date(lf_data[,1], format = "%m/%d/%Y")) %in% c('Friday','Saturday','Sunday')), ]
  
  
  
  #Regular Season Weekdays/Weekends
  reg_weekdays<-tail(weekday_lf[,],nrow(weekday_lf)/1.5)
  reg_weekends<-tail(weekend_lf[,],nrow(weekend_lf)/(10/6))
  
  #Holiday Season Weekdays/Weekends
  hol_weekdays<-head(weekday_lf[],nrow(weekday_lf)/3)
  hol_weekdays<-tail(hol_weekdays[],nrow(hol_weekdays)/(4/3))
  
  hol_weekends<-head(weekend_lf[],nrow(weekend_lf)/2.5)
  hol_weekends<-tail(hol_weekends[],nrow(hol_weekends)/(4/3))
  
  
  mon_day<-date
  day_of_week<-weekdays(as.Date(date))
  
  
  #Determine date-range
  if(day_of_week=="Monday"){
    date_range_weekday<-c(seq.Date(from = as.Date(date),to = as.Date(as.Date(date)+3), by = "1 day"))
  }
  
  if(day_of_week=="Tuesday"){
    date_range_weekday<-c(seq.Date(from = as.Date(as.Date(date)-1),to = as.Date(as.Date(date)+2), by = "1 day"))
  }
  
  if(day_of_week=="Wednesday"){
    date_range_weekday<-c(seq.Date(from = as.Date(as.Date(date)-2),to = as.Date(as.Date(date)+1), by = "1 day"))
  }
  
  if(day_of_week=="Thursday"){
    date_range_weekday<-c(seq.Date(from = as.Date(as.Date(date)-3),to = as.Date(date), by = "1 day"))
  }
  
  if(day_of_week=="Friday"){
    date_range_weekend<-c(seq.Date(from = as.Date(date),to = as.Date(as.Date(date)+2), by = "1 day"))
  }
  
  if(day_of_week=="Saturday"){
    date_range_weekend<-c(seq.Date(from = as.Date(as.Date(date)-1),to = as.Date(as.Date(date)+1), by = "1 day"))
  }
  
  if(day_of_week=="Sunday"){
    date_range_weekend<-c(seq.Date(from = as.Date(as.Date(date)-2),to = as.Date(date), by = "1 day"))
  }
  
  
  ################    Start Candleplots Here    ################
  
  gen_reg_wd_price<-sample(reg_weekdays[,2], nrow(reg_weekdays)*.7)
  gen_reg_we_price<-sample(reg_weekends[,2], nrow(reg_weekends)*.8)
  gen_hol_wd_price<-sample(hol_weekdays[,2], nrow(hol_weekdays)*.7)
  gen_hol_we_price<-sample(hol_weekends[,2], nrow(hol_weekends)*.8)
  
  
  
  #Reg Weekday Plot
  #!!!!Typing in Wednesday causes a crash...wtf
  rwdtf<-((day_of_week=="Monday" | day_of_week=="Tuesday" | day_of_week=="Wednesday" | day_of_week=="Thursday") & ((mon_day<beg_sb | mon_day>end_sb) &
                                                                                                                  (mon_day<beg_thksgvg | mon_day>end_thksgvg) &
                                                                                                                  (mon_day<beg_sumb | mon_day>end_sumb) &
                                                                                                                  (mon_day<beg_hol_seas | mon_day>end_hol_seas)))
  if(rwdtf){
  gen_rwd_df<-as.data.frame(cbind(gen_reg_wd_price,date_range))
  gen_rwd_df[,2]<-as.Date(gen_rwd_df[,2], origin = "1970-01-01")
  sorted_gen_rwd_df<-gen_rwd_df[order(gen_rwd_df[,2]),]
  
  c_plot<-ggplot() +
    geom_boxplot(data=sorted_gen_rwd_df, aes(x=sorted_gen_rwd_df[,2], y=sorted_gen_rwd_df[,1],
                                             group=date_range), fill="cyan") +
    labs(title = paste("Predicted Price vs. Date from",depart,"to",arrive), x = "Date", y = "Price") +
    theme(axis.text.x=element_text(angle=40, size=10, vjust=0.5))
  
  ggsave(filename = paste("Predicted_Flights_To_From_Plots/",depart," to ",arrive,".png", sep=""),plot = last_plot())
  }
  
  #Reg Weekend Plot
  rwetf<-((day_of_week=="Friday" | day_of_week=="Saturday" | day_of_week=="Sunday") & ((mon_day<beg_sb | mon_day>end_sb) &
                                                                                       (mon_day<beg_thksgvg | mon_day>end_thksgvg) &
                                                                                       (mon_day<beg_sumb | mon_day>end_sumb) &
                                                                                       (mon_day<beg_hol_seas | mon_day>end_hol_seas)))
  if(rwetf){
  gen_rwe_df<-as.data.frame(cbind(gen_reg_we_price,date_range))
  gen_rwe_df[,2]<-as.Date(gen_rwe_df[,2], origin = "1970-01-01")
  sorted_gen_rwe_df<-gen_rwe_df[order(gen_rwe_df[,2]),]
  
  c_plot<-ggplot() +
    geom_boxplot(data=sorted_gen_rwe_df, aes(x=sorted_gen_rwe_df[,2], y=sorted_gen_rwe_df[,1],
                                             group=date_range), fill="cyan") +
    labs(title = paste("Predicted Price vs. Date from",depart,"to",arrive), x = "Date", y = "Price") +
    theme(axis.text.x=element_text(angle=40, size=10, vjust=0.5))
  
  ggsave(filename = paste("Predicted_Flights_To_From_Plots/",depart," to ",arrive,".png", sep=""),plot = last_plot())
  }
  
  #if(((mon_day>beg_sb & mon_day<end_sb) | (mon_day>beg_thksgvg & mon_day<end_thksgvg) | (mon_day>beg_sumb & mon_day<end_sumb) | (mon_day>beg_hol_seas & mon_day<end_hol_seas))){
  #Holiday Weekday Plot
  hwdtf<-((day_of_week=="Monday" | day_of_week=="Tuesday" | day_of_week=="Wednesday" | day_of_week=="Thursday") & ((mon_day>beg_sb & mon_day<end_sb) |
                                                                                                            (mon_day>beg_thksgvg & mon_day<end_thksgvg) |
                                                                                                            (mon_day>beg_sumb & mon_day<end_sumb) |
                                                                                                            (mon_day>beg_hol_seas & mon_day<end_hol_seas)))
  if(hwdtf){
  gen_hwd_df<-as.data.frame(cbind(gen_hol_wd_price,date_range))
  gen_hwd_df[,2]<-as.Date(gen_hwd_df[,2], origin = "1970-01-01")
  sorted_gen_hwd_df<-gen_hwd_df[order(gen_hwd_df[,2]),]
  
  c_plot<-ggplot() +
    geom_boxplot(data=sorted_gen_hwd_df, aes(x=sorted_gen_hwd_df[,2], y=sorted_gen_hwd_df[,1],
                                             group=date_range), fill="cyan") +
    labs(title = paste("Predicted Price vs. Date from",depart,"to",arrive), x = "Date", y = "Price") +
    theme(axis.text.x=element_text(angle=40, size=10, vjust=0.5))

  ggsave(filename = paste("Predicted_Flights_To_From_Plots/",depart," to ",arrive,".png", sep=""),plot = last_plot())
  }
  
  
  #Holiday Weekend Plot
  hwetf<-((day_of_week=="Friday" | day_of_week=="Saturday" | day_of_week=="Sunday") & ((mon_day>beg_sb & mon_day<end_sb) |
                                                                                 (mon_day>beg_thksgvg & mon_day<end_thksgvg) |
                                                                                 (mon_day>beg_sumb & mon_day<end_sumb) |
                                                                                 (mon_day>beg_hol_seas & mon_day<end_hol_seas)))
  if(hwetf){
  gen_hwe_df<-as.data.frame(cbind(gen_hol_we_price,date_range))
  gen_hwe_df[,2]<-as.Date(gen_hwe_df[,2], origin = "1970-01-01")
  sorted_gen_hwe_df<-gen_hwe_df[order(gen_hwe_df[,2]),]
  
  c_plot<-ggplot() +
    geom_boxplot(data=sorted_gen_hwe_df, aes(x=sorted_gen_hwe_df[,2], y=sorted_gen_hwe_df[,1],
                                             group=date_range), fill="cyan") +
    labs(title = paste("Predicted Price vs. Date from",depart,"to",arrive), x = "Date", y = "Price") +
    theme(axis.text.x=element_text(angle=40, size=10, vjust=0.5))
  
  ggsave(filename = paste("Predicted_Flights_To_From_Plots/",depart," to ",arrive,".png", sep=""),plot = last_plot())
  }
  
  return()
}

