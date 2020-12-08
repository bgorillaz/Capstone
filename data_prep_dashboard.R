#load packages
library(data.table)

#set dir
setwd("/Users/ryanloos/Capstone/")

#load data
LIME_segs_r <- fread("clean_data/seg_r_LIME_output.csv")
segments_raw <- fread("raw_data/segment_data_flat.csv", skip = 1)

#remove boring columns
segments <- drop_boring_columns(segments_raw)

segments <- segments[, .(CRSP_ID = CRSP2_Unique_ID, County_Name, Start_Description, End_Description)]
fwrite(segments, "clean_data/dashboard_seg_info.csv")

segments_melt <- melt(segments, id.vars = "CRSP_ID")
fwrite(segments_melt, "clean_data/dashboard_seg_melt_info.csv")
# dashboard_DT <- merge(LIME_segs_r, segments, by = "CRSP_ID", all.x = T)

#clean vars
unique(dashboard_DT$feature)
#for those with DV
dashboard_DT <- LIME_segs_r[feature != "sev_obs"]
short_case <- dashboard_DT[, .N, by = case][N == 9, case]
short_DT <- dashboard_DT[case %in% short_case] #remove smallest feature weight
short_DT[, abs_fw := abs(feature_weight)]
min_fw <- short_DT[, .(min_fw = min(abs_fw)), by = case]
short_DT <- merge(short_DT, min_fw, by = "case", all.x = T)
short_DT[, flag := 0]
short_DT[abs_fw == min_fw, flag := 1]
short_DT <- short_DT[flag == 0]

#for those without
long_case <- dashboard_DT[, .N, by = case][N == 10, case]
long_DT <- dashboard_DT[!(case %in% short_case)] #remove smallest feature weight
long_DT[, abs_fw := abs(feature_weight)]
setorder(long_DT, case, -abs_fw)
long_DT[, idx := 1:10, by = case]
long_DT <- long_DT[idx<9]
long_DT[,abs_fw := NULL][,idx := NULL]

short_DT[, abs_fw := NULL][, min_fw := NULL][, flag := NULL]

LIME_segs_r <- rbind(long_DT,short_DT) #check this for 8 records per case
LIME_segs_r[, CRSP_ID := tolower(CRSP_ID)]
segments[, CRSP_ID := tolower(CRSP_ID)]
LIME_segs_r <- merge(LIME_segs_r, segments, by = "CRSP_ID")

fwrite(LIME_segs_r, "clean_data/dashboard_seg_r.csv")

