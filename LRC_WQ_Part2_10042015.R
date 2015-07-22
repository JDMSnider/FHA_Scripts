function(Basin, prv_list) {
  
require(Hmisc)
require (ggplot2)
require(gridExtra)
require(wq)
require(reshape)

Date <- Sys.Date()

#this is part2 of FHA water quality anaylsis and requires "site_df" and "df_l" objects from Part 1. 
#make sure this work directory is set to the same workspace as the outputs from part 1. 
#where watershed falls into two provinces, the script most refer to output files from both provinces, and run rbind to join the two datasetes from each province for both of the above elements.

i = 1
while (i <= length(prv_list)){
  prv = prv_list[i]
  if (i == 1) {  
    df_l_file <- paste(Basin, "_", prv, "_WQ_DF_L_out.csv", sep = "")
    df_l = read.csv(df_l_file)
    site_df_file <- paste(Basin, "_", prv, "_WQ_Site_DF_out.csv",sep = "")
    site_df <- read.csv(site_df_file)
  } else {
    df_l_file_2 <- paste(Basin, "_", prv, "_WQ_DF_L_out.csv", sep = "")
    df_l_2 = read.csv(df_l_file_2)
    site_df_file_2 <- paste(Basin, "_", prv, "_WQ_Site_DF_out.csv", sep = "")
    site_df2 <- read.csv(site_df_file_2)
    df_l <- rbind(df_l, df_l_2)
    site_df <- rbind(site_df, site_df2)
  }
  i = i + 1
}

fig1 <- paste(Basin, "_Exceedance_Guidelines_", Date, ".png", sep = "")
line1 <- "Proportion of Water Quality Measurements Exceeeding Guidelines"
f1 <- ggplot(data= site_df, aes(factor(year), P1)) + geom_boxplot(aes(fill = factor(ref)), outlier.size=1) + facet_wrap(~wscsda)
f2 <- f1 + xlab("year") + ylab("Proportion of Exceedance") + theme(axis.text.x = element_text(angle = 90, size = 7)) + ggtitle(line1) + scale_fill_discrete(name="Sources")
ggsave( filename= fig1, plot = f2, dpi=128, width = 10, height = 8 )

line2 <- "Proportion of Water Quality Measurements Exceeeding 75th Percentile"
fig2 <- paste(Basin, "_Exceedance_P75_", Date, ".png", sep = "")
f3 <- ggplot(data= site_df, aes(factor(year), P2)) + geom_boxplot(aes(fill = factor(ref)), outlier.size=1) + facet_wrap(~wscsda)
f4 <- f3 + xlab("year") + ylab("Proportion of Exceedance") + theme(axis.text.x = element_text(angle = 90, size = 7)) + ggtitle(line2)+ scale_fill_discrete(name="Sources")
ggsave( filename= fig2, plot = f4, dpi=128, width = 10, height = 8 )

line3 <- "Proportion of Water Quality Measurements Exceeeding 90th Percentile"
fig3 <- paste(Basin, "_Exceedance_P90_", Date, ".png", sep = "")
f5 <- ggplot(data= site_df, aes(factor(year), P3)) + geom_boxplot(aes(fill = factor(ref)), outlier.size=1) + facet_wrap(~wscsda)
f6 <- f5 + xlab("year") + ylab("Proportion of Exceedance") + theme(axis.text.x = element_text(angle = 90, size = 7)) + ggtitle(line3)+ scale_fill_discrete(name="Sources")
ggsave( filename= fig3, plot = f6, dpi=128, width = 10, height = 8 )

line4 <- "Weighted Average of Exceedances of 75th Percentile, 90th Percentile and Guideline"
fig4 <- paste(Basin, "_Weighted_Average_Exceedance_", Date, ".png", sep = "")
f7 <- ggplot(data = site_df, aes(factor(year), FS)) + geom_boxplot(aes(fill = factor(ref)), outlier.size=1) + facet_wrap(~wscsda, ncol = 2)
f8 <- f7 + xlab("year") + ylab("Proportion of Exceedance") + theme(axis.text.x = element_text(angle = 90, size = 7)) + ggtitle(line4)+ scale_fill_discrete(name="Sources") + geom_hline(aes(yintercept = 0.069 , linetype = "Very Good"), show_guide = TRUE) + geom_hline(aes(yintercept = 0.139 , linetype = "Good"), show_guide = TRUE) + geom_hline(aes(yintercept = 0.239 , linetype = "Fair"), show_guide = TRUE) + geom_hline(aes(yintercept = 0.49 , linetype = "Poor"), show_guide = TRUE) + geom_hline(aes(yintercept = 1.00 , linetype = "Very Poor"), show_guide = TRUE) + scale_linetype_discrete(name = "Water Quality Categories")
ggsave( filename= fig4, plot = f8, dpi=128, width = 10, height = 8 )

summary_file <- paste(Basin, "_WQ_Summary_Raw_", Date, ".csv", sep = "")
summary_header <-  "Source, WSCSDA, Year, Number of Contaminants Measured,Total Number of Sites, Number of Measurements, Total Number of Guidelines Exceedances, Proportion of Guideline Exceedance,  Total Number of 90th Percentile Exceedances, Proportion of 90th Percentile Exceedance, Total Number of 75th Percentile Exceedances, Proportion of 75th Percentile Exceedance, Weighted Average Exceedance"
write(summary_header, summary_file)

out_df <- data.frame(matrix(nrow = 1, ncol = 13))
colnames(out_df) <- c("Source", "WSCSDA", "Year", "Contaminants", "Sites", "Measurements", "Guidelines", "Prop.Guidelines", "N90th", "P90th", "N75th", "P75th", "Average")

score_file <- paste(Basin, "_WQ_Score_for_Map_", Date, ".csv", sep = "")
score_header <-  "Site, Source, WSCSDA, S_Year, E_Year, N_year, N_Mes, MED_WQ, STD_WQ"
write(score_header, score_file)

score_df = unique(site_df[, c("site", "wscsda", "ref")])
score_func = function(x){
  s <- as.character(x["site"])
  s_yr = min(site_df[site_df$site == s, c("year")])
  E_yr = max(site_df[site_df$site == s, c("year")])
  N_yr = length(unique(site_df[site_df$site == s, c("year")]))
  t_yr = E_yr - 4

  if (t_yr >= 2009 | E_yr >= 2009){
    t_df = site_df[site_df$site == s & site_df$year >= 2009,]
    n_ms = nrow(t_df)

    if (n_ms > 0) {
      wq_m = median(t_df$FS)
      wq_sd = sd(t_df$FS)
    }else {
      wq_m = NA
      wq_sd = NA
    }
  }else {
    n_ms = 0
    wq_m = NA
    wq_sd = NA
  }
  wsc = x["wscsda"]
  ref = x["ref"]
  out_val = paste(s,wsc, ref, s_yr, E_yr, N_yr, n_ms, wq_m, wq_sd, sep = ",")
  write(out_val, score_file, append = TRUE)
}

apply(score_df, 1, score_func)

years <- levels(factor(df_l$year))
yr <- as.numeric(max(years))
if (yr == 2014) {
  limit <- 6
} else {
  limit <- 5
}
while (yr > (as.numeric(max(years)) - limit)){
  print(yr)
  
  sum_df <- subset(df_l, year == yr)
  
  for (j in levels(factor(sum_df$wscsda))){
    sum_df2 <- subset(sum_df, wscsda == j)
  
    for (k in levels(factor(sum_df2$ref))){
      sum_df3 <- subset(sum_df2, ref == k)
      m3 <- nrow(sum_df3)
      v3 <- length(levels(factor(sum_df3$variable)))
      s3 <- length(levels(factor(sum_df3$site)))
      g3 <- sum(sum_df3$G1, na.rm = TRUE)
      pg3 <- g3/m3
      t23 <- sum(sum_df3$T2)
      pt23 <- t23/m3
      t13 <- sum(sum_df3$T1)
      pt13 <- t13/m3
      FS3 <- ((pg3 * 3) + pt13 + (pt23 * 2)) / 6
      sum_out3 <- paste(k, j, yr, v3, s3, m3, g3, pg3, t23, pt23, t13, pt13, FS3, sep = ",")
      write(sum_out3, summary_file, append = TRUE) 
      out_df <- rbind(out_df, c(k, j, yr, v3, s3, m3, g3, pg3, t23, pt23, t13, pt13, FS3))
    }
  }

  map_df <- subset(site_df, year == yr)
  map_df <- aggregate(FS ~ site + year, map_df, FUN = "median")
  
  out_file <-paste("WQ_Exceedance_", Basin, "_", yr, "_for_MAP_", Date, ".csv", sep = "")
  write.csv(map_df, out_file)
  
  yr <- yr - 1
}

file_list<-list.files(pattern="for_MAP")
formap1<-read.csv(file_list[1])
formap2<-read.csv(file_list[2])
formap3<-read.csv(file_list[3])
formap4<-read.csv(file_list[4])
formap5<-read.csv(file_list[5])
mergeformap<-rbind(formap1, formap2, formap3, formap4, formap5)
mergedfile<-paste("WQ_Exceedance_", Basin, "_all_Years_for_MAP_", Date, ".csv", sep="")
write.csv(mergeformap, mergedfile)
            
out_df <- out_df[2:nrow(out_df),]

result_df <- data.frame(unique(out_df[, c("WSCSDA")]))
colnames(result_df) <- "WSCSDA"

for (i in levels(factor(df_l$wscsda))) {

  temp_df <- subset(out_df, WSCSDA == i)

  temp_df$Multiplier <- as.numeric(temp_df$Sites) * as.numeric(temp_df$Contaminants)
  temp_df$Weighted <- as.numeric(temp_df$Average) * as.numeric(temp_df$Multiplier)
  out_val <- sum(temp_df$Weighted)/sum(temp_df$Multiplier)
  result_df$Average[result_df$WSCSDA == i] <- out_val
  
  temp_df2 <- subset(df_l, wscsda == i)
  years <- levels(factor(temp_df2$year))
  mx_yr <- as.numeric(max(years))
  l_yr <- mx_yr - 4
  yrs <- length(years[years > (mx_yr - limit)])
  result_df$Years[result_df$WSCSDA == i] <- yrs
  
  n_sites <- length(unique(temp_df2$site[temp_df2$year >= l_yr]))
  result_df$Sites[result_df$WSCSDA == i] <- n_sites
  
  par_df <- data.frame(unique(temp_df2[, c("site", "year")]))
  p_func <- function(x){
    s <- x[1]
    y <- x[2]
    s_df <- df_l$variable[df_l$site == s & df_l$year == y]
    params <- as.numeric(length(levels(factor(s_df))))
   return(params)
  }
  
  par_df$params <- apply(par_df,1, p_func)
  par_df2 <- subset(par_df, year > (mx_yr - limit))
  result_df$Prop.Params[result_df$WSCSDA == i] <- nrow(par_df2[par_df2$params >= 10,])/nrow(par_df2)
  
  
}

output_file <- paste(Basin, "_WQ_Weighted_Average_", Date, ".csv", sep = "")
write.csv(result_df, output_file)

param_file <- paste(Basin, "_WQ_Parameters_", Date, ".csv", sep = "")
param_header <-  "WSCSDA, First Year of Available Monitoring, Most Recent Year of Monitoring, Number of Sites in First Year of Monitoring, Number of Sites in last Five Years, Total Number of Samples, Number of Samples with at least 10 parameters, Percentage of Samples with more than 10 parameters, Number of Years Sampled in Last Ten Years"
write(param_header, param_file)

samp <- function(x) {
  s <- as.character(x[1])
  d <- x[2]
  temp_df <- subset(df_l, site == s & date == d)
  out_val <- length(levels(factor(temp_df$variable)))
  return(out_val)
}

wscsda <-levels(droplevels(factor(df_l$wscsda)))

for (i in wscsda) {
  
  df1 <- subset(df_l, wscsda == i)
  f <- min(df1$year)
  r <- length(levels(factor(df1$site[df1$year == f])))
  y <- max(df1$year)
  df2 <- subset(df1, year >= 2009)

  df3 <- unique(df2[, c("site", "date")])
  
  df3$param <- apply(df3, 1, samp)
  p = length(levels(factor(df3$site)))
  m = length(df3$param)
  n = length(df3$param[df3$param >=10])
  q = n/m
  
  l <- length(levels(factor(df1$year[df1$year > 2003 ])))
  out <- paste(i, f, y, r, p, m, n,q,l, sep = ",")
  write(out, param_file, append = TRUE)
}


}