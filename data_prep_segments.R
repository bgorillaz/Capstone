#load packages
library(data.table)
library(ggplot2)
library(ndot.tools)


#set wd
setwd("/Users/ryanloos/Capstone/")

#load data #41,714 crashes on 3,922 segments 
segments_raw <- fread("raw_data/segment_data_flat.csv", skip = 1)

#remove boring columns
segments <- drop_boring_columns(segments_raw)

#drop additional irrelevant columns and filter out no data collection segments
segments_small <- segments[`Collect DataBeyond This Point?` == 'YES', c("CRSP2_Unique_ID", "District_Number", 
                         "County_ID", "Length_feet", "County_Name", 
                         "Greater/Metro", "County_Number", "Route_System_Number", "Route_System", 
                         "Route_Number", "City", "Street_Name", "Start_Description", "End_Description", 
                         "ADT_year", "ADT_vpd", "Length_miles", "MVMT_5yrMillionVMT", "Num_Curve", "Density_Curve", 
                         "Area_Type", "Urban/Rural", "Initial Filter for All Factors", 
                         "Filter_Region", "Collect DataBeyond This Point?", "Collect Data?", 
                         "Context_Zone", "Surface_Type", "Segment_Cross_Section", "Segment_Design", 
                         "Seg_CrossSection_Design", "Speed_Limit_mph", 
                         "Rumble_Strips", "Mumble_Strips", "Lane_Width", "Shoulder_Type", 
                         "Shoulder_Width", "Median_Type", "Median_Width", "Edge_Risk", 
                         "Edgeline_Striping", "Centerline_Striping", "Parking_Allowed", 
                         "Sidewalks", "Bike_Lanes", "Road_Access_Count", "Commercial_Access_Count", 
                         "Residential_Access_Count", "Farm_Access_Count", "Alley_Other_Access_Count", 
                         "Total_Access_County", "Access_Density",
                         "TotalCrashCount", "SevereCrashCount", "TotalCrashes_LaneDeparture", 
                         "SevereCrashes_LaneDeparture", "FatalCrashes_LaneDeparture", 
                         "TotalCrashes_MultiVehicle", "SevereCrashes_MultiVehicle", "InjuryCrashes_MultiVehicle", 
                         "TotalCrashes_HO_SSO", "SevereCrashes_HO_SSO", "InjuryCrashes_HO_SSO", 
                         "TotalCrash_PED", "SevereCrash_PED", "K", "A", "B", "C", "N", 
                         "KABCN", "CrashCost", "Context Zone", "Speed Limit", "Lane Width", 
                         "Edgeline Striping", "Parking", "ADT0U", "ADT0RS", "ADT0RM", 
                         "AADT", "Access Density", "Cross Section and Design", "Curve Density", 
                         "Edge Risk", "Shoulder Width", "Total Rating", "Buffer Between Opposing Lanes", 
                         "Clear Zone Maintenance", "6'' Wet Reflective in Groove", "Shoulder Paving, Safety Edge", 
                         "Centerline Rumble Strip", "Edge Line Rumble Strip", "Shoulder Rumble Strip", 
                         "Enhanced Edgeline", "Divided Roadway", "Access Management", 
                         "Road Diet Convert to 3-Lane", "Road Diet Convert to 5-Lane", 
                         "Dynamic Speed Sign_Reviewed")]

seg_for_model <- segments_small[, c("CRSP2_Unique_ID", "MVMT_5yrMillionVMT", "ADT_vpd", "Density_Curve", 
                                    "Area_Type","Context_Zone", "Surface_Type", "Segment_Cross_Section", "Segment_Design", 
                                    "Seg_CrossSection_Design", "Speed_Limit_mph", "Access_Density",
                                    "Rumble_Strips", "Mumble_Strips", "Lane_Width", "Shoulder_Type", 
                                    "Shoulder_Width", "Median_Type", "Median_Width", "Edge_Risk", 
                                    "Edgeline_Striping", "Centerline_Striping", "Parking_Allowed", 
                                    "Sidewalks", "Bike_Lanes", "Road_Access_Count", "Commercial_Access_Count", 
                                    "Residential_Access_Count", "Farm_Access_Count", "Alley_Other_Access_Count", 
                                    "TotalCrashCount", "TotalCrashes_LaneDeparture", "TotalCrashes_MultiVehicle",
                                    "TotalCrashes_HO_SSO","TotalCrash_PED", "SevereCrashCount", "Total_Access_County")]

#review all fields for duplicate entries, set to lower, replace/NA, etc.
#change types
seg_for_model[, Speed_Limit_mph := as.numeric(Speed_Limit_mph)] #come back and insert NA as the mode? Don't make factor, want to see how speed changes
seg_for_model[, Lane_Width := as.numeric(Lane_Width)] #come back and insert NA as the mode?
seg_for_model[Shoulder_Width == "Variable", Shoulder_Width := 2]
seg_for_model[, Shoulder_Width := as.numeric(Shoulder_Width)] #lots of NA still

for(var in c("Road_Access_Count", "Commercial_Access_Count", "Total_Access_County",
             "Residential_Access_Count", "Farm_Access_Count", "Alley_Other_Access_Count", 
             "Access_Density", "TotalCrashCount", "TotalCrashes_LaneDeparture", "TotalCrashes_MultiVehicle",
             "TotalCrashes_HO_SSO","TotalCrash_PED")) {
  seg_for_model[[var]] <- as.numeric(seg_for_model[[var]])
}

#clean up each column and convert to best possible data for model.
#clean up character columns
for(var in colnames(seg_for_model)) {
  if (class(seg_for_model[[var]]) == "character") {
    seg_for_model[[var]] <- tolower(seg_for_model[[var]])
  }
}

seg_for_model[seg_for_model == "nv"] <- NA
seg_for_model[, Surface_Type := NULL] #remove as all are paved
seg_for_model[Segment_Cross_Section == "unknown", Segment_Cross_Section := "2-lane"]
seg_for_model[is.na(Segment_Cross_Section), Segment_Cross_Section := "2-lane"]
seg_for_model[Segment_Design == "unknown", Segment_Design := "undivided"]
seg_for_model[is.na(Segment_Design), Segment_Design := "undivided"]
seg_for_model[Rumble_Strips == "unknown", Rumble_Strips := "none"] 
seg_for_model[Mumble_Strips == "unknown", Mumble_Strips := "none"] 
seg_for_model[Shoulder_Type == "unknown", Shoulder_Type := NA] 
seg_for_model[Median_Type == "unknown" | is.na(Median_Type), Median_Type := "undivided"] #converted NAs(19) to undivided
seg_for_model[Median_Type == "none" | Median_Type == "paint", Median_Type := "undivided"] 
seg_for_model[Median_Type == "varies" | Median_Type == "other", Median_Type := "undivided"] #combine varies (4) and other (1) with undivided (majority)
seg_for_model[Median_Width == "none" | Median_Width == "undivided" | Median_Width == "unknown" | Median_Width == "varies", Median_Width := "0"] #added varies (1) to zero
seg_for_model[, Median_Width := as.numeric(Median_Width)]
seg_for_model[is.na(Median_Width), Median_Width := 0]
seg_for_model[Edgeline_Striping == "unknown", Edgeline_Striping := NA]
seg_for_model[Centerline_Striping == "unknown", Centerline_Striping := NA]
seg_for_model[Parking_Allowed == "unknown", Parking_Allowed := NA]
seg_for_model[Sidewalks == "unknown", Sidewalks := NA]
seg_for_model[Bike_Lanes == "unknown", Bike_Lanes := NA]

# lines below are used for examining character/factor variables
# seg_for_model[, unique(Median_Width)]
# seg_for_model[, .N, by = Bike_Lanes]


#convert all chr to factors!
for(var in colnames(seg_for_model)) {
  if (class(seg_for_model[[var]]) == "character") {
    seg_for_model[[var]] <- as.factor(seg_for_model[[var]])
  }
}

seg_for_model[, CRSP2_Unique_ID := as.character(CRSP2_Unique_ID)]

#clean up number columns, potentially convert to factors.
#check the ones we converted above, lane and shoulder widths Check counts.
#access counts need to be changed to zero for all NAs.
seg_for_model <- seg_for_model[!(MVMT_5yrMillionVMT == 0)] #remove zero volume or length segments
seg_for_model <- seg_for_model[is.na(Lane_Width), Lane_Width := 12] #assume that NA(65) are 12 foot lanes
seg_for_model <- seg_for_model[is.na(Speed_Limit_mph), Speed_Limit_mph := 55] #assume that NA(4) are 55mph
#Shoulder width has enough NAs with enough predominate categories that these were not assumed to be a category.
seg_for_model <- seg_for_model[is.na(Road_Access_Count), Road_Access_Count := 0]
seg_for_model <- seg_for_model[is.na(Commercial_Access_Count), Commercial_Access_Count := 0]
seg_for_model <- seg_for_model[is.na(Residential_Access_Count), Residential_Access_Count := 0]
seg_for_model <- seg_for_model[is.na(Farm_Access_Count), Farm_Access_Count := 0]
seg_for_model <- seg_for_model[is.na(Alley_Other_Access_Count), Alley_Other_Access_Count := 0]
seg_for_model <- seg_for_model[is.na(Access_Density), Access_Density := 0] #assume that NA(1) are 0

# lines below are used for examining numeric variables
# summary(seg_for_model$Commercial_Access_Count)
# seg_for_model[, unique(Commercial_Access_Count)]
# seg_for_model[, .N, by = Commercial_Access_Count][order(Commercial_Access_Count)]

#print summary plots
ndot.tools::summary_plots(seg_for_model, out_file = "figures/summary_plots_segments.pdf")

#check how many complete records there are
seg_for_model_noNA <- na.omit(seg_for_model) #drops 532 records.
seg_for_model[, .N, by = SevereCrashCount] #602 severe crash segments out of 2700
seg_for_model_noNA[, .N, by = SevereCrashCount] #560 of 2300 #This should be what the regression model uses?
#390 records have at least 1 NA value

#check columns that have NAs
colnames(seg_for_model)[colSums(is.na(seg_for_model)) > 0]
colnames(seg_for_model_noNA)[colSums(is.na(seg_for_model_noNA)) > 0]

#export data set
fwrite(seg_for_model_noNA, "clean_data/seg_for_model.csv")
