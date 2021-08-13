#R Session - 3 vCPU / 9 GiB Memory

#1. Setting up the Background

install.packages("lubridate")
install.packages("recommenderlab")
install.packages("reshape2")
rm(list=ls())
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(reshape2)
library(recommenderlab)
library(data.table)
library(readr)
df1 <- read_excel("KINNECT/activity_master.xlsx")
warnings()

#########################################################

#2. Data Pre-Processing: User Activity Data

##2.1. Handle Missing Values

names(df1)<-toupper(names(df1))
print("Before Substitution:")
colSums(is.na(df1))

df1 <- subset(df1, select = -c(NETWORK_ID))
names(df1)[names(df1) == "NETWORK_ID_Y"] <- "NETWORK_ID"


df1$SCREEN_NAME[is.na(df1$SCREEN_NAME) == TRUE] <- "unknown"
df1$FILTER_KEY[is.na(df1$FILTER_KEY) == TRUE] <- "unknown"
#df1$MONTHS[is.na(df1$MONTHS) == TRUE] <- 0
#df1$YEARS[is.na(df1$YEARS) == TRUE] <- 0
#df1$FUNCTIONAL_AREA[is.na(df1$FUNCTIONAL_AREA) == TRUE] <- "unknown"
df1$METRIC_ID[is.na(df1$METRIC_ID) == TRUE] <- 0

print("After Substitution:")
colSums(is.na(df1))

#########################################################

##2.3. Remove Specific Pages

###- Login Pages
###- Edit Metrics
###- Alert

paste("Total no. of rows", nrow(df1))
paste("Total no. of rows - LogIn only", nrow(df1[df1$ACTIVITY %in% "LOGIN",]))
paste("Total no. of rows - excluding LogIn should be", nrow(df1)-nrow(df1[df1$ACTIVITY %in% "LOGIN",]))


'%!in%' <- function(x,y)!('%in%'(x,y))

df2 <- df1[df1$ACTIVITY %!in% "LOGIN",]
df2 <- df2[df2$ACTIVITY %!in% "ALERT",]
df2 <- df2[df2$ACTIVITY %!in% "EDIT_METRIC",]

table(df2$ACTIVITY)

paste("Total no. of rows - excluding LogIn", nrow(df2))

activity_df1 <- df2



#########################################################


##4.1. User - Unique Landing Page - No. of Visits
#fav_df1 <- combined_df1


colnames(activity_df1)[colnames(activity_df1)=="NETWORK_ID"] <- "Network_ID"

fav_df1 <- activity_df1


#keeps <- c("Network_ID", "ACTIVITY", "ACTIVITY_DATE", "COMMENTS", "SCREEN_NAME", "FILTER_KEY", "METRIC_ID")
keeps <- c("Network_ID", "SCREEN_NAME", "FILTER_KEY", "METRIC_ID")
fav_df1 <- fav_df1[,keeps]

fav_df1$Visits <- 1

fav_df1$FILTER_KEY <- gsub("~.*","",fav_df1$FILTER_KEY)


fav_df2 <- aggregate(fav_df1, by=list(fav_df1$Network_ID, fav_df1$SCREEN_NAME, fav_df1$FILTER_KEY, fav_df1$METRIC_ID), FUN =length)

#keeps2 <- c("Group.1", "Group.2", "Group.3", "Group.4", "Group.5", "Visits")
keeps2 <- c("Group.1", "Group.2", "Group.3", "Group.4", "Visits")

fav_df2 <- fav_df2[ ,keeps2]

colnames(fav_df2)[colnames(fav_df2)=="Group.1"] <- "Network_ID"
#colnames(fav_df2)[colnames(fav_df2)=="Group.2"] <- "ACTIVITY"
colnames(fav_df2)[colnames(fav_df2)=="Group.2"] <- "METRIC_ID"
colnames(fav_df2)[colnames(fav_df2)=="Group.3"] <- "SCREEN_NAME"
colnames(fav_df2)[colnames(fav_df2)=="Group.4"] <- "FILTER_KEY"
#colnames(fav_df2)[colnames(fav_df2)=="Group.5"] <- "METRIC_ID"

fav_df3<-fav_df2[!(fav_df2$FILTER_KEY=="PRODUCT^NaN" | fav_df2$FILTER_KEY=="PRODUCT^undefined" | fav_df2$FILTER_KEY=="PRODUCT^" | fav_df2$FILTER_KEY=="PRODUCT^0"),]


fav_df3$FILTER_KEY <- gsub("[[:punct:]]", " ", fav_df3$FILTER_KEY)
fav_df3$FILTER_KEY <- gsub(" ", "_", fav_df3$FILTER_KEY)
fav_df3$SCREEN_NAME <- gsub(" ", "_", fav_df3$SCREEN_NAME)

table(fav_df3$SCREEN_NAME)
#table(fav_df3$SCREEN_NAME, fav_df3$ACTIVITY)

##Removing Activity


#table(fav_df3$ACTIVITY)
table(fav_df3$SCREEN_NAME)
table(fav_df3$FILTER_KEY)
table(fav_df3$SCREEN_NAME)

fav_df3 <- fav_df3["unknown" != fav_df3$FILTER_KEY,]


fav_df4 <- unite_(fav_df3, sep = "~", "User_Landing", c("METRIC_ID", "SCREEN_NAME", "FILTER_KEY"))
fav_df4 <- fav_df4[,c("Network_ID", "User_Landing", "Visits")]


paste(length(unique(fav_df4$Network_ID)),"unique users visit",length(unique(fav_df4$User_Landing)),"unique landing pages")

head(fav_df4[order(fav_df4$Network_ID,-fav_df4$Visits),])

#########################################################

#6. Build User - Rating Matrix


##6.1. Data Pre-Processing

user_id_df <- as.data.frame(unique(fav_df4$Network_ID))
page_id_df <- as.data.frame(unique(fav_df4$User_Landing))

user_id_df$User_ID <- 1:nrow(user_id_df)
page_id_df$Page_ID <- 1:nrow(page_id_df)

colnames(user_id_df)[colnames(user_id_df)=="unique(fav_df4$Network_ID)"] <- "Network_ID"
colnames(page_id_df)[colnames(page_id_df)=="unique(fav_df4$User_Landing)"] <- "User_Landing"

user_id_df$Network_ID <- as.character(user_id_df$Network_ID)
page_id_df$User_Landing <- as.character(page_id_df$User_Landing)

work_df1 <- merge(x=fav_df4,y=user_id_df,by="Network_ID",all.x=TRUE)
work_df2 <- merge(x=work_df1,y=page_id_df,by="User_Landing",all.x=TRUE)



main_mapping_df <- work_df2
main_mapping_df
#View(main_mapping_df)
#View(user_df1)
paste("Total no. of unique landings",length(unique(main_mapping_df$User_Landing)))


#########################################################

##6.2. Rating Matrix

ratings_df <- work_df2[,c("User_ID", "Page_ID", "Visits")]

ratingmat <- dcast(ratings_df, User_ID~Page_ID, value.var = "Visits", na.rm=FALSE)

ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds
head(ratings_df)

#########################################################

#7. Behavior based Recommendations

##7.1.1. Determine location of top recommendations

ratingmat <- as(ratingmat, "realRatingMatrix")

ratingmat_norm <- normalize(ratingmat)

recommender_model <- Recommender(ratingmat_norm, method = "UBCF", param=list(method="Cosine",nn=30))
recom <- predict(recommender_model, ratingmat[2], n=10) #Obtain top 10 recommendations for 1st user in dataset
recom_list <- as(recom, "list")
recom_list

##7.1.2. Determine top recommendations

unique_pages <- as.data.frame(unique(work_df2$Page_ID))

recom_result <- matrix(0,6)


for (i in c(1:6)){
  recom_result[i] <- unique_pages[as.integer(recom_list[[1]][i]),]
}

recom_result

#########################################################

##7.2. For all users



for(i in c(1:nrow(ratingmat))){
  
  recom <- predict(recommender_model, ratingmat[i], n=10) #Obtain top 10 recommendations for 1st user in dataset
  recom_list[i] <- as(recom, "list")
  
}
#recom_list

unique_pages <- as.data.frame(unique(work_df2$Page_ID))

recom_result <- matrix(0,nrow(ratingmat),6)

for(h in c(1:nrow(ratingmat))){
  for (i in c(1:6)){
    recom_result[h,i] <- unique_pages[as.integer(recom_list[[h]][i]),]
  }
}



recom_result_df <- cbind(as.data.frame(recom_result),c(1:nrow(ratingmat)))


colnames(recom_result_df)[colnames(recom_result_df)=="c(1:nrow(ratingmat))"] <- "User_ID"

recomm1_df <- recom_result_df[,c("V1", "User_ID")]
recomm2_df <- recom_result_df[,c("V2", "User_ID")]
recomm3_df <- recom_result_df[,c("V3", "User_ID")]

recomm4_df <- recom_result_df[,c("V4", "User_ID")]
recomm5_df <- recom_result_df[,c("V5", "User_ID")]
recomm6_df <- recom_result_df[,c("V6", "User_ID")]

colnames(recomm1_df)[colnames(recomm1_df)=="V1"] <- "Page_ID"
colnames(recomm2_df)[colnames(recomm2_df)=="V2"] <- "Page_ID"
colnames(recomm3_df)[colnames(recomm3_df)=="V3"] <- "Page_ID"

colnames(recomm4_df)[colnames(recomm4_df)=="V4"] <- "Page_ID"
colnames(recomm5_df)[colnames(recomm5_df)=="V5"] <- "Page_ID"
colnames(recomm6_df)[colnames(recomm6_df)=="V6"] <- "Page_ID"


###7.2.3. Output Data Conditioning



user_mapping_df <- unique(main_mapping_df[,c("User_ID", "Network_ID")])
page_mapping_df <- unique(main_mapping_df[,c("Page_ID", "User_Landing")])


recomm1_df <- merge(x=recomm1_df,y=user_mapping_df,by="User_ID",all.y=TRUE)
recomm1_df <- merge(x=recomm1_df,y=page_mapping_df,by="Page_ID",all.x=TRUE)
recomm1_df$Recomm_No <- 1

recomm2_df <- merge(x=recomm2_df,y=user_mapping_df,by="User_ID",all.y=TRUE)
recomm2_df <- merge(x=recomm2_df,y=page_mapping_df,by="Page_ID",all.x=TRUE)
recomm2_df$Recomm_No <- 2

recomm3_df <- merge(x=recomm3_df,y=user_mapping_df,by="User_ID",all.y=TRUE)
recomm3_df <- merge(x=recomm3_df,y=page_mapping_df,by="Page_ID",all.x=TRUE)
recomm3_df$Recomm_No <- 3

recomm4_df <- merge(x=recomm4_df,y=user_mapping_df,by="User_ID",all.y=TRUE)
recomm4_df <- merge(x=recomm4_df,y=page_mapping_df,by="Page_ID",all.x=TRUE)
recomm4_df$Recomm_No <- 4

recomm5_df <- merge(x=recomm5_df,y=user_mapping_df,by="User_ID",all.y=TRUE)
recomm5_df <- merge(x=recomm5_df,y=page_mapping_df,by="Page_ID",all.x=TRUE)
recomm5_df$Recomm_No <- 5

recomm6_df <- merge(x=recomm6_df,y=user_mapping_df,by="User_ID",all.y=TRUE)
recomm6_df <- merge(x=recomm6_df,y=page_mapping_df,by="Page_ID",all.x=TRUE)
recomm6_df$Recomm_No <- 6


recomm_df <- rbind(recomm1_df[,3:5], recomm2_df[,3:5], recomm3_df[,3:5], recomm4_df[,3:5], recomm5_df[,3:5], recomm6_df[,3:5])

head(recomm_df)


###7.2.4. Output Data Final - Recommendation


final_recomm_df <- separate(recomm_df, User_Landing, c("Metric_ID", "Screen_Name", "Filter_Key"), sep = "~", remove = TRUE)

final_recomm_df$Product_Name <- "NA" 
final_recomm_df$Product_Name <- final_recomm_df$Filter_Key 

# final_recomm_df$Filter_Key <- gsub("Name", "1", final_recomm_df$Filter_Key)

colnames(final_recomm_df)[colnames(final_recomm_df)=="Filter_Key"] <- "Product_ID"

final_recomm_df$Product_ID <- gsub("_.*","",final_recomm_df$Product_ID)


#########################################################

#9. Mapping File

final_recomm_map_df <- final_recomm_df
prod_map_df <- read_csv("KINNECT/Screen_Product_Mapping.csv")

final_recomm_map_df1 <- merge(x=final_recomm_map_df,y=prod_map_df,by="Screen_Name",all.x=TRUE)

user_id_map_df <- aggregate(df1, by=list(df1$USER_ID, df1$NETWORK_ID), FUN =length)
user_id_map_df <- user_id_map_df[,1:2]

colnames(user_id_map_df)[colnames(user_id_map_df)=="Group.1"] <- "User_ID"
colnames(user_id_map_df)[colnames(user_id_map_df)=="Group.2"] <- "Network_ID"

final_recomm_map_df2 <- merge(x=final_recomm_map_df1,y=user_id_map_df,by="Network_ID",all.x=TRUE)

head(final_recomm_map_df2)
head(df1)

final_recomm_map_df3 <- final_recomm_map_df2[order(final_recomm_map_df2$User_ID, final_recomm_map_df2$Recomm_No),]

head(final_recomm_map_df3)


#########################################################

#10. Quick Links


##10.1. Frequent Visits

#table(combined_df1$ACTIVITY)

keeps_quick <- c("Network_ID", "USER_ID", "ACTIVITY_DATE", "SCREEN_NAME", "FILTER_KEY", "METRIC_ID")
#quick_df1 <- combined_df1[,keeps_quick]
quick_df1 <- activity_df1[,keeps_quick]

quick_df1$FILTER_KEY <- gsub("~.*","",quick_df1$FILTER_KEY)

quick_df2 <- quick_df1[!(quick_df1$FILTER_KEY=="PRODUCT^NaN" | quick_df1$FILTER_KEY=="PRODUCT^undefined" | quick_df1$FILTER_KEY=="PRODUCT^" | quick_df1$FILTER_KEY=="PRODUCT^0"),]

quick_df2$FILTER_KEY <- gsub("[[:punct:]]", " ", quick_df2$FILTER_KEY)
# quick_df2$FILTER_KEY <- gsub("PRODUCT 1", "Name", quick_df2$FILTER_KEY)
quick_df2$FILTER_KEY <- gsub(" ", "_", quick_df2$FILTER_KEY)
quick_df2$SCREEN_NAME <- gsub(" ", "_", quick_df2$SCREEN_NAME)



quick_df2 <- quick_df2["unknown" != quick_df2$FILTER_KEY,]


quick_df3 <- unite_(quick_df2, sep = "~", "User_Landing", c("METRIC_ID", "SCREEN_NAME", "FILTER_KEY"))
quick_df3$Visits <- 1
quick_df3 <- aggregate(quick_df3, by=list(quick_df3$Network_ID, quick_df3$USER_ID, quick_df3$User_Landing), FUN =length)

keeps_quick <- c("Group.1", "Group.2", "Group.3", "Visits")

quick_df3 <- quick_df3[,keeps_quick]


colnames(quick_df3)[colnames(quick_df3)=="Group.1"] <- "Network_ID"
colnames(quick_df3)[colnames(quick_df3)=="Group.2"] <- "User_ID"
colnames(quick_df3)[colnames(quick_df3)=="Group.3"] <- "User_Landing"
colnames(quick_df3)[colnames(quick_df3)=="Group.4"] <- "Visits"

quick_df3 <- as.data.table(quick_df3)
quick_df3 <- quick_df3[order(User_ID, -Visits),]
quick_df3

min_User_ID <- min(quick_df3$User_ID)
max_User_ID <- max(quick_df3$User_ID)

quick_df3 <- data.table(quick_df3, key="User_ID")
quick_df3 <- quick_df3[, head(.SD, 3), by=User_ID]

quick_df3

final_quick_df <- separate(quick_df3, User_Landing, c("Metric_ID", "Screen_Name", "Filter_Key"), sep = "~", remove = TRUE)

final_quick_df$Product_Name <- "NA" 
final_quick_df$Product_Name <- final_quick_df$Filter_Key 

# final_quick_df$Filter_Key <- gsub("Name", "1", final_quick_df$Filter_Key)

colnames(final_quick_df)[colnames(final_quick_df)=="Filter_Key"] <- "Product_ID"

final_quick_df$Product_ID <- gsub("_.*","",final_quick_df$Product_ID)
head(final_quick_df)

final_quick_map_df <- merge(x=final_quick_df,y=prod_map_df,by="Screen_Name",all.x=TRUE)

#stand_col_order <- c("Network_ID", "Screen_Name", "Metric_ID", "Product_ID", "Recomm_No", "Product_Name", "Screen_ID", "User_ID")
stand_col_order <- c("Network_ID", "Screen_Name", "Metric_ID", "Product_ID", "Recomm_No", "Product_Name", "Screen_Display_Name", "Application_Name", "Menu_ID", "User_ID")
final_quick_map_df <- as.data.frame(final_quick_map_df)
final_quick_map_df$Recomm_No <- 1

final_quick_map_df <- final_quick_map_df[,stand_col_order]

final_quick_map_df <- final_quick_map_df[order(final_quick_map_df$User_ID, final_quick_map_df$Recomm_No),]

##10.2. Validate

paste("Total No. of Unique users in Recommendations:", length(unique(final_recomm_map_df3$Network_ID)))
paste("Total No. of Unique users in Quick Links:", length(unique(final_quick_map_df$Network_ID)))

#View(aggregate(final_quick_map_df, by=list(final_quick_map_df$Network_ID), FUN =length))


final_quick_map_df$Recomm_No <- ave(final_quick_map_df$Network_ID, final_quick_map_df$User_ID, FUN = seq_along)
head(final_quick_map_df)


#########################################################

#11. Combine Files

final_recomm_map_df3$Type <- "R"
final_quick_map_df$Type <- "Q"


final_KINNECT_df <- rbind(final_recomm_map_df3, final_quick_map_df)

final_KINNECT_df <- final_KINNECT_df[order(final_KINNECT_df$User_ID, final_KINNECT_df$Type, final_KINNECT_df$Recomm_No),]

head(final_KINNECT_df)

write_csv(final_KINNECT_df, "KINNECT/final_output_KINNECT.csv")

paste("Output is successfully generated")
