#load packages
library(data.table)
library(ggplot2)
library(ndot.tools)
library(sjmisc)
#set wd
setwd("/Users/ryanloos/Capstone/")
#load data #53,399 crashes on 20,637 intersections. 885 Severe crashes at 776 intersections #includes ints we might not collect data on
intersections_raw <- fread("raw_data/intersection_data_flat.csv", skip = 1)
#remove boring columns
intersections <- drop_boring_columns(intersections_raw)
#drop additional irrelevant columns and filter out no data collection segments
intersections_small <- intersections[Collect_Data == 'YES' | Collect_Data == 'Yes',c("CRSP2_Unique_ID", "Severe_Crashes", "County_Name",
                                                             "Route_System_Number", "Route_System", "Route_Number", "Cross_Street_Route_System", 
                                                             "City", "Street_Name", "Intersection_Description", "Lat", "Long", 
                                                             "Area_Type", "Context_Zone", "Collect_Data",
                                                             "Int_Type", "Maj1_ADT", "Maj2_ADT", "Min1_ADT", "Min2_ADT", "Min3_ADT", 
                                                             "Major_Entering_ADT", "Minor_Entering_ADT", "Total_Entering_ADT", 
                                                             "Cross_Product", "Design_Type", "Traffic_Control", "Leg_Configuration", 
                                                             "Major_Division_Configuration", "Minor_Division_Configuration", 
                                                             "Major_Surface_Type", "Minor_Surface_Type", "Alignment_Skew", 
                                                             "Railroad_Crossing", "Adjacent_Curve", "Adjacent_Development", 
                                                             "Street Parking", "Lighting", "Previous_Stop", "Major_Speed_Limit", 
                                                             "Minor_Speed_Limit", "Flashers", "Overhead_Signal", 
                                                             "Left_Turn_Phasing_Maj", "Left_Turn_Phasing_Min", "Flashing_Yellow_Arrow", 
                                                             "Right_Turn_On_Red", "Maj1_Lane_Config", "Maj2_Lane_Config", 
                                                             "Min1_Lane_Config", "Min2_Lane_Config", "Min3_Lane_Config", "Max_Lanes_Cross", 
                                                             "Sidewalk", "Refuge_Island", "Ped_Crossing", "Bike_Facility", 
                                                             "Ped_Indicator", "Transit_Adjacent", "School_Crosswalk", "PedBike_Other1", 
                                                             "PedBike_Other2", "Comments", "Legs", "Major Div", "Minor Division", 
                                                             "ADT_1", "ADT_2", "ADT_3", "ADT_4", "ADT_5", 
                                                             "ADT_6", "ADT_7", "ADT_8", "Area_Type_Simplified", 
                                                             "Region", "Collect_Data", "Simplified Traffic Control",
                                                             "Total_Crashes", "Total_RA_Crashes")]
int_for_model <- intersections_small[, c("CRSP2_Unique_ID", "Severe_Crashes",
                                         "Area_Type", "Context_Zone",
                                         "Int_Type", "Total_Entering_ADT", 
                                         "Cross_Product", "Design_Type", "Traffic_Control", "Leg_Configuration", 
                                         "Major_Division_Configuration", "Minor_Division_Configuration", 
                                         "Major_Surface_Type", "Minor_Surface_Type", "Alignment_Skew", 
                                         "Railroad_Crossing", "Adjacent_Curve", "Adjacent_Development", 
                                         "Street Parking", "Lighting", "Previous_Stop", "Major_Speed_Limit", 
                                         "Minor_Speed_Limit", "Flashers", "Overhead_Signal", 
                                         "Left_Turn_Phasing_Maj", "Left_Turn_Phasing_Min", "Maj1_Lane_Config", 
                                         "Maj2_Lane_Config", "Sidewalk", "Refuge_Island", "Ped_Crossing", 
                                         "Bike_Facility", "Ped_Indicator", "Transit_Adjacent", "School_Crosswalk",
                                         "Legs", "Max_Lanes_Cross")]

#review all fields for duplicate entries, set to lower, replace/NA, etc.
#change types
int_for_model[Alignment_Skew == "Unknown", Alignment_Skew := "0"]
int_for_model[Alignment_Skew == "N/A", Alignment_Skew := "0"]
int_for_model[, Alignment_Skew := as.numeric(Alignment_Skew)]

int_for_model[Previous_Stop == "<5", Previous_Stop := "0"]
int_for_model[Previous_Stop == "NV", Previous_Stop := "0"]
int_for_model[Previous_Stop == "Unknown", Previous_Stop := "0"]
int_for_model[Previous_Stop == ">5", Previous_Stop := "1"]
int_for_model[, Previous_Stop := as.factor(Previous_Stop)] #1 is if previous stop is >5mi

int_for_model[, Major_Speed_Limit := as.numeric(Major_Speed_Limit)]
int_for_model[is.na(Major_Speed_Limit), Major_Speed_Limit := 55] #set 128 unknown to 55mph

int_for_model[, Minor_Speed_Limit := as.numeric(Minor_Speed_Limit)]
int_for_model[is.na(Minor_Speed_Limit), Minor_Speed_Limit := 55] #set 162 unknown to 55mph

int_for_model[, Legs := as.numeric(Legs)] #setting NA ints to have 1 leg to keep in analysis, will revisit when we have regression results.
int_for_model[is.na(Legs), Legs := 1]

for(var in colnames(int_for_model)) {
  if (class(int_for_model[[var]]) == "character") {
    int_for_model[[var]] <- tolower(int_for_model[[var]])
  }
}

int_for_model[Area_Type == "nv", Area_Type := NA] #assume 90 "NV" ints are rural
int_for_model[Context_Zone == "nv", Context_Zone := NA] #assume 90 "NV" ints are agriculture (probably same 90 as above)
int_for_model[Design_Type %in% c("unknown", "ddi", "riro", "spi", "3-4"), Design_Type := "other"]
int_for_model[Traffic_Control %in% c("unknown", "not an intersection", "nv"), Traffic_Control := "unknown"]
int_for_model[Leg_Configuration %in% c("unknown", "5-leg", "tt", "y"), Leg_Configuration := "other"]
int_for_model[Major_Division_Configuration %in% c("depressed", "curb", "painted", "barrier"), Major_Division_Configuration := "divided"]
int_for_model[Major_Division_Configuration %in% c("mixed", "unknown"), Major_Division_Configuration := "other"]
int_for_model[Minor_Division_Configuration %in% c("depressed", "curb", "painted", "barrier"), Minor_Division_Configuration := "divided"]
int_for_model[Minor_Division_Configuration %in% c("mixed", "unknown", "n/a"), Minor_Division_Configuration := "other"]
int_for_model[Major_Surface_Type %in% c("both", "unknown"), Major_Surface_Type := "other"]
int_for_model[Minor_Surface_Type %in% c("both", "unknown"), Minor_Surface_Type := "other"]
int_for_model[Railroad_Crossing == "unknown", Railroad_Crossing := "none"] #assuming unknowns (118) are none. Train presence should be easy to identify
int_for_model[Adjacent_Curve %in% c("nv", "unknown", "none"), Adjacent_Curve := "no"] #putting unknowns with none, should be easy to identify
int_for_model[Adjacent_Curve %in% c("horizontal", "vertical", "both"), Adjacent_Curve := "yes"]
int_for_model[Adjacent_Development %in% c("nv", "unknown", "none"), Adjacent_Development := "no"]
int_for_model[Adjacent_Development %in% c("present"), Adjacent_Development := "yes"]
int_for_model[`Street Parking` %in% c("unknown", "none"), `Street Parking` := "no"]
int_for_model[`Street Parking` %in% c("present"), `Street Parking` := "yes"]
int_for_model[Lighting %in% c("unknown", "none"), Lighting := "no"]
int_for_model[Lighting %in% c("present"), Lighting := "yes"]
int_for_model[Flashers %in% c("unknown", "none", "nv"), Flashers := "no"]
int_for_model[Flashers %in% c("overhead", "led", "sign-mounted", "overhead & sign mounted"), Flashers := "yes"]
int_for_model[Overhead_Signal %in% c("overhead - some", "overhead - all present", "pedestal", "span wire"), Overhead_Signal := "yes"]
int_for_model[Overhead_Signal %in% c("n/a", "unknown"), Overhead_Signal := "no"]
int_for_model[Left_Turn_Phasing_Maj %in% c("n/a", "unknown"), Left_Turn_Phasing_Maj := "none"]
int_for_model[Left_Turn_Phasing_Min %in% c("n/a", "unknown"), Left_Turn_Phasing_Min := "none"]
int_for_model[Maj1_Lane_Config %in% c("n/a", "nv"), Maj1_Lane_Config := "other"] #this is ugly. hope we can remove this.
int_for_model[Maj2_Lane_Config %in% c("n/a", "nv"), Maj2_Lane_Config := "other"]
int_for_model[Sidewalk %in% c("none", "nv"), Sidewalk := "no"]
int_for_model[Sidewalk %in% c("all", "some"), Sidewalk := "yes"]
int_for_model[Refuge_Island %in% c("none", "nv", "n/a"), Refuge_Island := "no"]
int_for_model[Refuge_Island %in% c("all", "major", "minor"), Refuge_Island := "yes"]
int_for_model[Ped_Crossing %in% c("none", "nv", "n/a"), Ped_Crossing := "no"]
int_for_model[Ped_Crossing %in% c("markings", "signs & markings", "overhead lights & markings"), Ped_Crossing := "yes"]
int_for_model[Bike_Facility %in% c("none", "nv", "n/a"), Bike_Facility := "no"]
int_for_model[Bike_Facility %in% c("bike lane", "sharrow", "off-street trail", "bike boulevard"), Bike_Facility := "yes"]
int_for_model[Ped_Indicator %in% c("none", "nv", "n/a"), Ped_Indicator := "no"]
int_for_model[Ped_Indicator %in% c("standard", "countdown"), Ped_Indicator := "yes"]
int_for_model[Transit_Adjacent %in% c("none", "nv", "n/a"), Transit_Adjacent := "no"]
int_for_model[Transit_Adjacent %in% c("present"), Transit_Adjacent := "yes"]
int_for_model[School_Crosswalk %in% c("none", "nv", "n/a"), School_Crosswalk := "no"]
int_for_model[School_Crosswalk %in% c("present"), School_Crosswalk := "yes"]

#set to factors once cleaned!
for(var in colnames(int_for_model)) {
  if (class(int_for_model[[var]]) == "character") {
    int_for_model[[var]] <- as.factor(int_for_model[[var]])
  }
}

int_for_model[, CRSP2_Unique_ID := as.character(CRSP2_Unique_ID)]

#remove ints with no volume data
int_for_model <- int_for_model[Total_Entering_ADT>0] #5764 records, 469 ints with severe crashes. 549 severe crashes.
#remove NAs
int_for_model_noNA <- na.omit(int_for_model)

#print summary plots
ndot.tools::summary_plots(int_for_model_noNA, out_file = "figures/summary_plots_ints.pdf")

#export data set
fwrite(int_for_model_noNA, "clean_data/int_for_model.csv")

int_for_model_noNA[, .N, by = Severe_Crashes>0]
int_for_model_noNA[, sum(Severe_Crashes)]
