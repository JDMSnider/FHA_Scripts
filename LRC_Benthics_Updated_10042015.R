hbi_val <- function(input_file, basin){
require(lubridate)
require(ggplot2)
require(reshape)
require(gridExtra)
require(wq)
require(gdata)
require(Hmisc)

#requires data with columns that include site, date(formated as mm/dd/yyyy), wscsda, and ref attributes; plus taxa names listed as rows in column titled "family" or "variable", and count/abundace values listed by row in column titled "count" of "value".

#please cross-reference taxa values with sensitivity index to ensure as many as possible families are included in HBI calculation. See "Missing_Tax" below.

#make sure the ann_ts subsets for the MK test are for the right parameter (HBI) and not years or refs (part#3)
#Section1
Date <- Sys.Date()

sens_file <- "L:/FHA_Documents/HBI_Tolerances/Tolerances_FHA_07042015.csv"
sens_df <- read.csv(sens_file)
sens_df$Tolerance <- gsub("NA", NA, sens_df$Tolerance)
sens_df$Tolerance <- as.numeric(sens_df$Tolerance)
sens_df <- na.omit(sens_df)


in_df <- read.csv(input_file)
colnames(in_df) <- tolower(colnames(in_df)) 
colnames(in_df) <- gsub("family", "variable", colnames(in_df))
colnames(in_df) <- gsub("count", "value", colnames(in_df))

in_df$date = as.Date(in_df$date, format='%m/%d/%Y')

  cap_func <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
    sep="", collapse=" ")
  }
  colnames(in_df) <- gsub("wscssda", "wscsda", colnames(in_df))
                          
  in_df$variable <- as.character(in_df$variable)
  in_df$variable <- sapply(in_df$variable, cap_func)
  in_df$variable <- as.factor(in_df$variable)
  
  in_df$site <- as.character(in_df$site)
  in_df$site <- gsub("^\\s+|\\s+$", "", in_df$site)
  in_df$value <- gsub("NA", NA, in_df$value)
  in_df$value <- as.numeric(in_df$value)
  in_df <- na.omit(in_df)
  
  missing_taxa = setdiff(levels(factor(in_df$variable)),(levels(sens_df$Family)))
  print(missing_taxa)

  if ("sample" %in% colnames(in_df)){
    site_df <- unique(in_df[, c("site", "date", "sample")])
    
    b <- 1
    while (b <= nrow(site_df)){
      s1 <- as.character(site_df[b,1])
      t1 <- site_df[b,2]
      p1 <- as.character(site_df[b,3])
      df3 <- subset(in_df, site == s1 & date == t1 & sample == p1)
      df3 <- na.omit(df3)
      
      df3 <-merge(df3, sens_df, by.x = "variable", by.y = "Family", all)
      df3$out <- df3$value * df3$Tolerance
      HBI <- sum(df3$out) / sum(df3$value)
      
      site_df$HBI[b] <- HBI
      site_df[b,c("HBI")] <- HBI
      
      b <- b + 1
    }
  }else{
    site_df <- unique(in_df[, c("site", "date")])
  
    b <- 1
    while (b <= nrow(site_df)){
      s1 <- as.character(site_df[b,1])
      t1 <- site_df[b,2]
      if (is.na(t1) == TRUE){
        print (s1, t1)
      }
      df3 <- subset(in_df, site == s1 & date == t1)
      df3 <- na.omit(df3)
      
      df3 <-merge(df3, sens_df, by.x = "variable", by.y = "Family", all)
      df3$out <- df3$value * df3$Tolerance
      HBI <- sum(df3$out) / sum(df3$value)
      
      site_df$HBI[b] <- HBI
      site_df[b,c("HBI")] <- HBI
      
      b <- b + 1
    }
  }
   #Section 2
id_df <- unique(in_df[,c("site", "date", "wscsda", "ref")])
in_data <- merge(site_df, id_df, id.var = c("site", "date"))

if ("year" %in% colnames(in_data) == FALSE) {
  in_data$year <- as.numeric(format(in_data$date, "%Y"))
}

in_data <- na.omit(in_data)
colnames(in_data) <- gsub("wscssda", "wscsda", colnames(in_data))

out_file <- paste("HBI_Values_", basin, "_", Date, ".csv", sep = "")
write.csv(in_data, out_file)

#Section 2

colnames(in_data) <- tolower(colnames(in_data))
median_df <- aggregate(in_data$hbi, by =list(in_data$site, in_data$year, in_data$ref, in_data$wscsda), FUN = median)
colnames(median_df) <- c("site", "year", "ref", "wscsda",  "hbi")
count_df <- aggregate(data = in_data, hbi ~ site + year, function(x) length(unique(x)))
colnames(count_df) <- gsub("hbi", "n_samples", colnames(count_df))

median_df <- merge(median_df, count_df, id.var = c("site", "year"), all.x = TRUE)

map_file <-  paste("Benthic_Median_Site_Scores_", basin, "_for_Map_", Date, ".csv", sep = "")
write.csv(median_df, map_file)
  

colnames(in_data) <- tolower(colnames(in_data))

sum_file <- paste("HBI_Summary_Scores_", basin, "_", Date, ".csv")
sum_df <- aggregate(hbi ~ year + wscsda + ref, data = in_data,  FUN = "median")
  
count <- function(x){
  yr <- x[1]
  ws <- x[2]
  print(paste(yr, ws))
  c_df <- subset(in_data, year == yr & wscsda == ws)
  n <- length(levels(factor(c_df$site)))
  print(n)
  x[3] <- n
}

sum_df1 <- apply(sum_df, 1, count)
sum_df$n <- sum_df1

write.csv(sum_df, sum_file)

#Section 3
regfile <- paste(basin, "_", "Benthic_Annual_Stats_", Date, ".csv", sep = "")

annual_header <-  "Source, WSCSDA, Start_Year, End_Year, n, Intercept, Intercept_STE, Intercept-T, Intercept-p, Slope, Slope_STE, Slope-T, Slope-p, Adj.RSqr, F-Stat, F-pvalue, Theil-Sen Slope, Mann-Ken Score, Mann-Ken p-value"
write(annual_header, regfile)
y_lab <- "Median Annual Hilsenhoff Biotic Index"


in_data$wscsda <-substr(in_data$wscsda, 1, 3)

if (class(in_data$wscsda) == "factor" ){
 wscsda <-levels(droplevels(in_data$wscsda))
}else {
 wscsda <-levels(factor(in_data$wscsda))
}

data_sources <- levels(factor(in_data$ref))
sources <- length(data_sources)

basin_number <- length(wscsda)

x <- 0

while (x <= basin_number){
 if (x == 0){
   
   n <- length(levels(factor(in_data$site)))
  
   
   years <- levels(factor(in_data$year))
   n_years <- length(years)
   
   if (n_years >= 3){
     start_year <- as.numeric(years[1])
     end_year <- as.numeric(years[n_years])
     
     annual_data <- aggregate(hbi ~ year + ref, data = in_data, FUN = "median")
     
     x_year <- start_year
     ann_ts <- ts(annual_data, start = start_year, end = end_year, frequency = 1)
     unit <- basin 
     line1 <- paste("Basin ", basin, sep ="")
     
     regplot <- paste(basin, "_Benthic_", unit, "_Annual_Regression.png", sep = "")
     print(regplot
           )
     reg <- lm(hbi~ year, data = ann_ts)
     
     reg_sum <- summary(reg)
     c <- reg_sum$coefficients
     rs <- reg_sum$adj.r.squared
     f <- reg_sum$fstatistic
     if(!is.null(f)){
       rp <- unname(pf(f[1],f[2],f[3],lower.tail=F))
     } else {
       rp <- "NA"
     }
     
     mk <- mannKen(ann_ts[,3])
     mks <- mk$sen.slope
     mkk <- mk$S
     mkp <- mk$p.value
     
     ann_out <- paste("All", unit, start_year, end_year, n, c[1,1], c[1,2], c[1,3], c[1,4], c[2,1], c[2,2], c[2,3], c[2,4], rs, unname(f[1]), rp, mks, mkk, mkp, sep = ",")
     write(ann_out, regfile, append = TRUE)
     
     
     line2 <- paste("Intercept =", format(c[1,1], digits = 5), ";", "Intercept STE =", format(c[1,2], digits = 5),";",  "Intercept T-Statistic", format(c[1,3], digits = 5),";", "Intercept p-value =", format(c[1,4], digits = 5))
     line3 <- paste("Slope =", format(c[2,1], digits = 5), ";", "Slope STE =", format(c[2,2], digits = 5),";",  "Slope T-Statistic", format(c[2,3], digits = 5),";", "Slope p-value =", format(c[2,4], digits = 5))
     line4 <- paste("F-Statistic:", format(f[1], digits = 5), "; p-value:", format(rp, digits = 4))
     line5 <- paste("Theil-Sen Slope = ", format(mks, digits = 4), ";", "Mann-Kendal Score = ", format(mkk, digits = 5), ";", "Mann-Kendall p-value = ", format(mkp, digits = 5))
     line6 <- paste("Number of sites = ", n, sep = " ")
     footer <-paste(line2, "\n", line3, "\n", line4, "\n", line5,"\n", line6)
     
     
     if (mkp < 0.05) {
       t <- ggplot(data = annual_data, aes(factor(year), hbi)) + geom_point(aes(colour=factor(ref))) + geom_smooth(method = "lm", aes(group = 1)) + xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 4))  + scale_colour_discrete(name="Sources")
     } else{ 
       t <- ggplot(data = annual_data, aes(factor(year), hbi)) + geom_point(aes(colour=factor(ref))) +  xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 6)) + scale_colour_discrete(name="Sources")
     }
     
     s <- t + theme(plot.margin = unit(c(0.5,0.5,2,0), "cm")) + ggtitle(line1) + theme(plot.title = element_text(size = 8)) 
     v <- arrangeGrob(s, sub = textGrob(footer, y = 1.5, gp=gpar(fontsize=8)))
     ggsave(filename= regplot, plot = v, dpi= 128, width = 8, height = 6)
     
     
   
   }
   
   boxplot <- paste(basin, "_Benthic_Boxplot.png", sep = "")
   b <- ggplot(data = in_data, aes(factor(year), hbi)) + geom_boxplot(aes(fill = factor(ref))) +  xlab("Year") + ylab("Hilsenhoff Biotic Index") + theme(axis.text.x = element_text(angle = 90, size = 6)) + facet_wrap(~wscsda) + scale_fill_discrete(name="Sources")
   ggsave(filename= boxplot, plot = b, dpi= 128, width = 8, height = 6)
  
   scatterplot <- paste(basin, "_Benthic_Scatterplot.png", sep = "")  
   d <- ggplot(data = in_data, aes(factor(year), hbi)) + geom_point(aes(colour = factor(ref))) + facet_wrap(~wscsda) +  xlab("Year") + ylab("Hilsenhoff Biotic Index Value") + theme(axis.text.x = element_text(angle = 90, size = 6)) + scale_colour_discrete(name="Sources")
   ggsave(filename= scatterplot, plot = d, dpi= 128, width = 8, height = 6)
   
   
   q <- 1
   
   while (q <= sources){
     
     print(paste("q =", q, sep = ""))
     
     sr <- data_sources[q]
     print(paste("source =", sr))
     
     reg_data <- subset(in_data, ref == sr)
     
     years1 <- levels(factor(reg_data$year))
     n1_years <- length(years1)
     
     if (n1_years >= 3){
       
       start_year1 <- as.numeric(years1[1])
       end_year1 <- as.numeric(years1[n1_years])
       

       n1 <- length(levels(factor(reg_data$site)))
       
       
       annual_data1 <- aggregate(hbi ~ year, data = reg_data, FUN = "median")
       ann_ts1 <- ts(annual_data1, start = start_year1, end = end_year1, frequency = 1)
       unit <- basin 
       line1 <- paste("Basin ", basin, sep ="")
       
       regplot1 <- paste(basin, "_Benthic_", unit, "_", sr, "_Annual_Regression.png", sep = "")
       print(regplot1)
       
       reg1 <- lm(hbi~ year, data = ann_ts1)
       
       reg_sum1 <- summary(reg1)
       c1 <- reg_sum1$coefficients
       rs1 <- reg_sum1$adj.r.squared
       f1 <- reg_sum1$fstatistic
       if(!is.null(f1)){
         rp1 <- unname(pf(f1[1],f1[2],f1[3],lower.tail=F))
       } else {
         rp1 <- "NA"
       }
       
       mk1 <- mannKen(ann_ts1[,2])
       mks1 <- mk1$sen.slope
       mkk1 <- mk1$S
       mkp1 <- mk1$p.value
       
       ann_out1 <- paste(sr, unit, start_year1, end_year1, n1, c1[1,1], c1[1,2], c1[1,3], c1[1,4], c1[2,1], c1[2,2], c1[2,3], c1[2,4], rs1, unname(f1[1]), rp1, mks1, mkk1, mkp1, sep = ",")
       write(ann_out1, regfile, append = TRUE)
       
       
       line2 <- paste("Intercept =", format(c1[1,1], digits = 5), ";", "Intercept STE =", format(c1[1,2], digits = 5),";",  "Intercept T-Statistic", format(c1[1,3], digits = 5),";", "Intercept p-value =", format(c1[1,4], digits = 5))
       line3 <- paste("Slope =", format(c1[2,1], digits = 5), ";", "Slope STE =", format(c1[2,2], digits = 5),";",  "Slope T-Statistic", format(c1[2,3], digits = 5),";", "Slope p-value =", format(c1[2,4], digits = 5))
       line4 <- paste("F-Statistic:", format(f1[1], digits = 5), "; p-value:", format(rp1, digits = 4))
       line5 <- paste("Theil-Sen Slope = ", format(mks1, digits = 4), ";", "Mann-Kendal Score = ", format(mkk1, digits = 5), ";", "Mann-Kendall p-value = ", format(mkp1, digits = 5))
       line6 <- paste("Number of sites = ", n1, sep = " ")
       line7 <- paste("Data Source:", sr, sep = "")
       footer <-paste(line2, "\n", line3, "\n", line4, "\n", line5,"\n", line6, ",", line7 )
       
       
       if (mkp1 < 0.05) {
         t1 <- ggplot(data = annual_data1, aes(factor(year), hbi)) + geom_point() + geom_smooth(method = "lm", aes(group = 1)) + xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 4)) 
       } else{ 
         t1 <- ggplot(data = annual_data1, aes(factor(year), hbi)) + geom_point() + xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 6))	
       }
       
       s1 <- t1 + theme(plot.margin = unit(c(0.5,0.5,2,0), "cm")) + ggtitle(line1) + theme(plot.title = element_text(size = 8)) 
       v1 <- arrangeGrob(s1, sub = textGrob(footer, y = 1.5, gp=gpar(fontsize=8)))
       ggsave(filename= regplot1, plot = v1, dpi= 128, width = 8, height = 6)
       
     }
     q <- q + 1
     }
    x <- x + 1
     
 } else {
   i <- wscsda[x]
   print(i)
   reg_data1 <- subset(in_data, wscsda == i)
   
   years1 <- levels(factor(reg_data1$year))
   n1_years <- length(years1)
   
   if (n1_years >= 3){
     
     start_year1 <- as.numeric(years1[1])
     end_year1 <- as.numeric(years1[n1_years])
     
     
     n1 <- length(levels(factor(reg_data1$site)))
     
     
     annual_data1 <- aggregate(hbi ~ year + ref, data = reg_data1, FUN = "median")
     ann_ts1 <- ts(annual_data1, start = start_year1, end = end_year1, frequency = 1)
     unit <- i
     line1 <- paste("Sub-basin ", i, sep ="")
     
     
     regplot2 <- paste(basin, "_Benthic_", unit, "_Annual_Regression.png", sep = "")
     print(regplot2)
     reg1 <- lm(hbi~ year, data = ann_ts1)
     
     reg_sum1 <- summary(reg1)
     c1 <- reg_sum1$coefficients
     rs1 <- reg_sum1$adj.r.squared
     f1 <- reg_sum1$fstatistic
     if(!is.null(f1)){
       rp1 <- unname(pf(f1[1],f1[2],f1[3],lower.tail=F))
     } else {
       rp1 <- "NA"
     }
     
     mk1 <- mannKen(ann_ts1[,3])
     mks1 <- mk1$sen.slope
     mkk1 <- mk1$S
     mkp1 <- mk1$p.value
     
     ann_out1 <- paste("All", unit, start_year1, end_year1, n1, c1[1,1], c1[1,2], c1[1,3], c1[1,4], c1[2,1], c1[2,2], c1[2,3], c1[2,4], rs1, unname(f1[1]), rp1, mks1, mkk1, mkp1, sep = ",")
     write(ann_out1, regfile, append = TRUE)
     
     
     line2 <- paste("Intercept =", format(c1[1,1], digits = 5), ";", "Intercept STE =", format(c1[1,2], digits = 5),";",  "Intercept T-Statistic", format(c1[1,3], digits = 5),";", "Intercept p-value =", format(c1[1,4], digits = 5))
     line3 <- paste("Slope =", format(c1[2,1], digits = 5), ";", "Slope STE =", format(c1[2,2], digits = 5),";",  "Slope T-Statistic", format(c1[2,3], digits = 5),";", "Slope p-value =", format(c1[2,4], digits = 5))
     line4 <- paste("F-Statistic:", format(f1[1], digits = 5), "; p-value:", format(rp1, digits = 4))
     line5 <- paste("Theil-Sen Slope = ", format(mks1, digits = 4), ";", "Mann-Kendal Score = ", format(mkk1, digits = 5), ";", "Mann-Kendall p-value = ", format(mkp1, digits = 5))
     line6 <- paste("Number of sites = ", n1, sep = " ")
     
     footer <-paste(line2, "\n", line3, "\n", line4, "\n", line5,"\n", line6)
     
     
     if (mkp1 < 0.05) {
       t2 <- ggplot(data = annual_data1, aes(factor(year), hbi)) + geom_point(aes(colour=factor(ref))) + geom_smooth(method = "lm", aes(group = 1)) + xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 4))  + scale_colour_discrete(name="Sources")
     } else{ 
       t2 <- ggplot(data = annual_data1, aes(factor(year), hbi)) + geom_point(aes(colour=factor(ref))) +  xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 6)) + scale_colour_discrete(name="Sources")
     }
     
     s2 <- t2 + theme(plot.margin = unit(c(0.5,0.5,2,0), "cm")) + ggtitle(line1) + theme(plot.title = element_text(size = 8)) 
     v2 <- arrangeGrob(s2, sub = textGrob(footer, y = 1.5, gp=gpar(fontsize=8)))
     ggsave(filename= regplot2, plot = v2, dpi= 128, width = 8, height = 6)
   }
   
   q <- 1
   
   while (q <= sources){
     
     print(paste("q =", q, sep = ""))
     
     sr <- data_sources[q]
     print(paste("source =", sr))
     
     reg_data3 <- subset(reg_data, ref == sr)
     
     years3 <- levels(factor(reg_data3$year))
     n3_years <- length(years3)
     
     if (n3_years >= 3){
       
       start_year3 <- as.numeric(years3[1])
       end_year3 <- as.numeric(years3[n3_years])
       

       n3 <- length(levels(factor(reg_data3$site)))

       
       annual_data3 <- aggregate(hbi ~ year, data = reg_data3, FUN = "median")
       ann_ts3 <- ts(annual_data3, start = start_year3, end = end_year3, frequency = 1)
       unit <- basin 
       line1 <- paste("Basin ", basin, sep ="")
       
       
       regplot3 <- paste(basin, "_Benthic_", unit, "_", sr, "_Annual_Regression.png", sep = "")
       print(regplot3)
       reg3 <- lm(hbi~ year, data = ann_ts3)
       
       reg_sum3 <- summary(reg3)
       c3 <- reg_sum3$coefficients
       rs3 <- reg_sum3$adj.r.squared
       f3 <- reg_sum3$fstatistic
       if(!is.null(f3)){
         rp3 <- unname(pf(f3[1],f3[2],f3[3],lower.tail=F))
       } else {
         rp3 <- "NA"
       }
       
       mk3 <- mannKen(ann_ts3[,2])
       mks3 <- mk3$sen.slope
       mkk3 <- mk3$S
       mkp3 <- mk3$p.value
       
       ann_out3 <- paste(sr, unit, start_year3, end_year3, n3, c3[1,1], c3[1,2], c3[1,3], c3[1,4], c3[2,1], c3[2,2], c3[2,3], c3[2,4], rs3, unname(f3[1]), rp3, mks3, mkk3, mkp3, sep = ",")
       write(ann_out3, regfile, append = TRUE)
       
       
       line2 <- paste("Intercept =", format(c3[1,1], digits = 5), ";", "Intercept STE =", format(c3[1,2], digits = 5),";",  "Intercept T-Statistic", format(c3[1,3], digits = 5),";", "Intercept p-value =", format(c3[1,4], digits = 5))
       line3 <- paste("Slope =", format(c3[2,1], digits = 5), ";", "Slope STE =", format(c3[2,2], digits = 5),";",  "Slope T-Statistic", format(c3[2,3], digits = 5),";", "Slope p-value =", format(c3[2,4], digits = 5))
       line4 <- paste("F-Statistic:", format(f3[1], digits = 5), "; p-value:", format(rp3, digits = 4))
       line5 <- paste("Theil-Sen Slope = ", format(mks3, digits = 4), ";", "Mann-Kendal Score = ", format(mkk3, digits = 5), ";", "Mann-Kendall p-value = ", format(mkp3, digits = 5))
       line6 <- paste("Number of sites = ", n3, sep = " ")
       
       footer <-paste(line2, "\n", line3, "\n", line4, "\n", line5,"\n", line6, ",", line7 )
       
       
       if (mkp3 < 0.05) {
         t3 <- ggplot(data = annual_data3, aes(factor(year), hbi)) + geom_point() + geom_smooth(method = "lm", aes(group = 1)) + xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 4)) 
       } else{ 
         t3 <- ggplot(data = annual_data3, aes(factor(year), hbi)) + geom_point() + xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 6)) 
       }
       
       s3 <- t3 + theme(plot.margin = unit(c(0.5,0.5,2,0), "cm")) + ggtitle(line1) + theme(plot.title = element_text(size = 8)) 
       v3 <- arrangeGrob(s3, sub = textGrob(footer, y = 1.5, gp=gpar(fontsize=8)))
       ggsave(filename= regplot3, plot = v3, dpi= 128, width = 8, height = 6)
     }
     q <- q + 1
   }
   x <- x + 1
   }

}                 
}
