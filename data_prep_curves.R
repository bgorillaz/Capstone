#load packages
library(data.table)
library(ggplot2)
library(ndot.tools)

#set wd
setwd("/Users/ryanloos/Capstone/")

#load data #41,714 crashes on 3,922 segments 
curves_raw <- fread("raw_data/curve_data_flat.csv")
curves_raw <- curves_raw[1:12267]

#remove boring columns
curves <- drop_boring_columns(curves_raw)
curves <- curves[`Collect_Data?` == "Yes"]
curves_small <- curves[, c("District_Number", "County_Name", "County_Number", 
                           "Route_System_Number", "Route_System", "Route_Number", "Corresponding_Segment", 
                           "CRSP2_Unique_Curve_ID", "Street_Name", "Speed_Limit", 
                           "Length_feet", "Radius_feet", "Surface_Type", 
                           "Area_Type", "Warning_Sign_Type", "Speed_Advisory_Sign", 
                           "Advisory_Speed", "Delineation", "ADT_vpd", "Years_Active_in_Study_Period", 
                           "Rumble_Strips", "Mumble", "Lane_Width", "Shoulder_Type", "Outside_Paved_Shoulder_Width", 
                           "Outside_Gravel_Shoulder_Width", "Total_Outside_Shoulder_Width", 
                           "Adjacent_Intersection", "Visual_Trap", "Curve_Lighting", "Isolated_Curve", 
                           "Outside_Edge_Risk", "Redraw_Flag", "MN_Region", "Urban_Rural", "Total_Xsect_width", 
                           "Total_Crashes", "Total_LD_Crashes", "Severe_Crashes")]

curves_for_model <- curves_small[, c("CRSP2_Unique_Curve_ID", "Speed_Limit", 
                                     "Length_feet", "Radius_feet", "Surface_Type", 
                                     "Area_Type", "Warning_Sign_Type", "Speed_Advisory_Sign", 
                                     "Advisory_Speed", "Delineation", "ADT_vpd",
                                     "Rumble_Strips", "Mumble", "Lane_Width", "Shoulder_Type", "Outside_Paved_Shoulder_Width", 
                                     "Outside_Gravel_Shoulder_Width", "Total_Outside_Shoulder_Width", 
                                     "Adjacent_Intersection", "Visual_Trap", "Curve_Lighting", "Isolated_Curve", 
                                     "Outside_Edge_Risk", "Urban_Rural", "Total_Xsect_width", 
                                     "Severe_Crashes")]

#review all fields for duplicate entries, set to lower, replace/NA, etc.
#change types
for(var in colnames(curves_for_model)) {
  if (class(curves_for_model[[var]]) == "character") {
    curves_for_model[[var]] <- tolower(curves_for_model[[var]])
  }
}

num_cols <- c("Speed_Limit", "Length_feet", "Radius_feet", "ADT_vpd", "Lane_Width", "Outside_Paved_Shoulder_Width",
              "Outside_Gravel_Shoulder_Width", "Total_Outside_Shoulder_Width", "Total_Xsect_width", "Total_Crashes",
              "Total_LD_Crashes", "Severe_Crashes")

for(var in num_cols) {
  curves_for_model[[var]] <- as.numeric(curves_for_model[[var]])
}

#clean up each column and convert to best possible data for model.
#clean up character columns
curves_for_model[, Surface_Type := NULL] #all paved
curves_for_model[Area_Type == "small town", Area_Type := "rural"] #classify small town(1) as rural
curves_for_model[Warning_Sign_Type %in% c("nv", "other", "unknown") , Warning_Sign_Type := "none"] #group nv, other, and unknown into none (424+37+274)
curves_for_model[Warning_Sign_Type %in% c("s-turn", "curve warning and s-curve") , Warning_Sign_Type := "s-curve"] #52+3 to generic s-curve
curves_for_model[Warning_Sign_Type %in% c("turn warning") , Warning_Sign_Type := "curve warning"]
curves_for_model[Warning_Sign_Type %in% c("indirect (winding road)", "indirect (s-curve)", "indirect (s-turn)") , Warning_Sign_Type := "indirect"]
curves_for_model[Speed_Advisory_Sign %in% c("nv", "unknown") , Speed_Advisory_Sign := "none"] #426 and 271 to none
curves_for_model[Advisory_Speed %in% c("nv", "unknown", "not applicable", "none") , Advisory_Speed := "0"]
curves_for_model[, Advisory_Speed := as.numeric(Advisory_Speed)]
curves_for_model[Delineation %in% c("nv", "unknown", "not applicable", "none") , Delineation := "0"]
curves_for_model[Delineation %in% c("arrow board and chevrons", "delineators", "arrow board", "chevrons") , Delineation := "1"]
curves_for_model[Rumble_Strips %in% c("nv", "unknown", "none") , Rumble_Strips := "0"]
curves_for_model[Rumble_Strips %in% c("centerline", "both", "shoulder", "edgeline", "present") , Rumble_Strips := "1"]
curves_for_model[Mumble %in% c("nv", "unknown", "none") , Mumble := "0"]
curves_for_model[Mumble %in% c("shoulder", "edgeline") , Mumble := "1"]
curves_for_model[Shoulder_Type %in% c("nv", "unknown") , Shoulder_Type := "none"]
curves_for_model[Adjacent_Intersection %in% c("nv", "unknown") , Adjacent_Intersection := "none"]
curves_for_model[Visual_Trap %in% c("nv", "unknown", "none") , Visual_Trap := "0"]
curves_for_model[Visual_Trap %in% c("present") , Visual_Trap := "1"]
curves_for_model[Curve_Lighting %in% c("nv", "unknown", "none") , Curve_Lighting := "0"]
curves_for_model[Curve_Lighting %in% c("present") , Curve_Lighting := "1"]
curves_for_model[Isolated_Curve %in% c("nv", "unknown") , Isolated_Curve := "no"]
curves_for_model[Outside_Edge_Risk %in% c("nv", "unknown") , Outside_Edge_Risk := "unknown"]
# curves_for_model[Redraw_Flag %in% c("") , Redraw_Flag := "no"] #changed 18 blanks to no

# lines below are used for examining character/factor variables
# curves_for_model[, unique(Area_Type)]
# curves_for_model[, .N, by = Urban_Rural]

#convert all chr to factors!
for(var in colnames(curves_for_model)) {
  if (class(curves_for_model[[var]]) == "character") {
    curves_for_model[[var]] <- as.factor(curves_for_model[[var]])
  }
}

curves_for_model[, CRSP2_Unique_Curve_ID := as.character(CRSP2_Unique_Curve_ID)]

#clean up number columns, potentially convert to factors.
curves_for_model[is.na(Speed_Limit), Speed_Limit := 55] #set 369 NA to 55
curves_for_model <- curves_for_model[!is.na(Length_feet)] #remove two records without curve lengths
curves_for_model[is.na(Lane_Width), Lane_Width := 12] #set 54 NAs to width 12
curves_for_model[is.na(Outside_Paved_Shoulder_Width), Outside_Paved_Shoulder_Width := 0] #set 550 NAs to 0
curves_for_model[is.na(Outside_Gravel_Shoulder_Width), Outside_Gravel_Shoulder_Width := 0] #set 296 NAs to 0
curves_for_model[is.na(Total_Outside_Shoulder_Width), Total_Outside_Shoulder_Width := 0] #set 610 NAs to 0
curves_for_model[is.na(Total_Xsect_width), Total_Xsect_width := 24] #set 54 NAs to 24
curves_for_model[, Severe_Crashes := as.numeric(as.character(Severe_Crashes))]

#these are for testing numeric columns
# curves_for_model[, unique(Radius_feet)]
# curves_for_model[is.na(ADT_vpd)]
# curves_for_model[, .N, by = Total_Xsect_width]
# summary(curves_for_model$Severe_Crashes)

#print summary plots
ndot.tools::summary_plots(curves_for_model, out_file = "figures/summary_plots_curves.pdf")

#check how many complete records there are
curves_for_model_noNA <- na.omit(curves_for_model) #No NAs
curves_for_model_noNA[, .N, by = Severe_Crashes] #157 severe crash curves out of 7449
curves_for_model_noNA[, sum(Severe_Crashes)]

#check columns that have NAs
colnames(curves_for_model)[colSums(is.na(curves_for_model)) > 0]

#export data set
fwrite(curves_for_model_noNA, "clean_data/curv_for_model.csv")
