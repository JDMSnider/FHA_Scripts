fish_stats <- function(file_name, Basin, prv, start_year){
  
  require (ggplot2)
  require(gridExtra)
  require(wq)
  require(reshape2)

  
  #Script developed by WWF-Canada to be used in combination with hydrometric, water quality and benthic macro-invertebrates analyses to attribute overall health scores to basins.
  #For questions or more information please contact James Snider jsnider@wwfcanada.org or Catherine Paquette cpaquette@wwfcanada.org
  
  #Script développé par WWF-Canada afin d'être utilisé en combinaison avec des analyses hydrometriques, de qualité de l'eau et de macro invertébrés benthiques afin d'attribué des scores globaux de santé aux bassins versants
  #Pour questions ou plus d'informations s-v-p contacter James Snider jsnider@wwfcanada.org ou Catherine Paquette cpaquette@wwfcanada.org
  
  #This script analyses trends of native fish species richness. It takes a csv file with site, date, year, species, individual count, reference/source and watershed attributes and creates regressions based both on median number 
  #of unique native fish species per site and total number of unique native species per site, at a watershed level
  
  #Ce script analyse pour des tendances de richesse chez les espèces de poissons indigènes. Il utilise un fichier csv avec les attributs de site, date, année, espèce, nombre/compte d'individus, reference/source de donnée et bassin versant afin
  #de calculer des régressions du nombre moyen et nombre total d'espèces uniques de poissons indigènes par site, au niveau du bassin versant
  
  #file_name is input file, must always have same format (in this order) Site, Date (mm/dd/yyyy), year, species (indigenous), count, ref (data source), wscsda (watershed, we use the ones identified by the Water Survey of Canada)
  #file_name est le fichier d'entrée et doit avoir le format suivant (dans cet ordre) Site = site d'échantillonage, date (format mois/jour/année), year = année, species = espèce (espe`ces indigènes, la langue n'importe pas), count = compte d'espèce, ref = source de données, wscsda = bassin versant (notre analyse est faite au niveau des bassins versant identifier par Relevés hydrologiques du Canada)
  
  sys_date <- Sys.Date()
  
  
  read_data <- read.csv(file_name)
  read_data <- na.omit(read_data)
   
  colnames(read_data) <- tolower(colnames(read_data))
  
  read_data$date <- as.Date(read_data$date, format = "%m/%d/%Y") 
  read_data$site <- as.character(read_data$site)
 
  #Part 1A
  #To calculate the richness (number of unique species per site per year)
  #Pour calculer la richesse (nombre d'espèces uniques par site par an)
  richness_func <-  function(x) {
    s <- as.character(x[1])
    y <- x[2]
    temp_df <- subset(read_data, site == s & year == y)
    out_val <- length(levels(factor(temp_df$species)))
    return(out_val)
  }
  
  site_df <-unique(read_data[, c("site", "year", "ref", "wscsda")])
  site_df$richness <- apply(site_df, 1, richness_func)
  
  year_df <- unique(site_df[,c("year","ref", "wscsda")])
  
  #Part 1B
  #To calculate the median number of species per site per watershed
  #Pour calculer le nombre médian d'espèces par site par bassin versant
  median_func <- function(x) {
    y <- x[1]
    temp_df <- subset(site_df, year == y)
    med_val <- median(temp_df$richness)
    return(med_val)
  }
  
  year_df$med_rich <- apply(year_df, 1, median_func)
  
  
  total_df <-unique(read_data[, c("year", "ref", "wscsda")])
  
  #Part 1C
  #To calculate the total number of unique species per site per watershed
  #Pour calculer le nombre total d'espèces uniques par site par bassin versant
  total_func <- function(x){
  y <- x[1]
  temp_df <- subset(read_data, year == y)
  out_val <- length(levels(factor(temp_df$species)))
  return(out_val)
  }
  
  total_df$richness <- apply(total_df, 1,total_func)
  
  
  
  #Part 2A
  #Starts to calculate regressions etc. on median number of fish per site, Mann-Kendall non-parametric test for directional trends
  #Commence a calculer les régression etc. sur valeurs médianes de poissons par site, test non-parametric Mann-Kendall pour tendances directionelles 
  regfile <- paste(Basin, "_Fish_Median_Richness_Annual_Stats_", sys_date, ".csv", sep = "")
  
  annual_header <-  "Source, WSCSDA, Start_Year, Intercept, Intercept_STE, Intercept-T, Intercept-p, Slope, Slope_STE, Slope-T, Slope-p, Adj.RSqr, F-Stat, F-pvalue, Theil-Sen Slope, Mann-Ken Score, Mann-Ken p-value"
  write(annual_header, regfile)
  
  data_sources <- levels(factor(year_df$ref))
  sources <- length(data_sources)
  
  y_lab <- "Median Number of Fish Species Observed Per Site"
  
  y <- 1
  
  while (y <= sources){
    
    print(y)
    src <- data_sources[y]
    print(src)
    in_data <- subset(year_df, ref == src)
    
    if (class(in_data$wscsda) == "factor" ){
      wscsda <-levels(droplevels(in_data$wscsda))
    }else {
      wscsda <-levels(factor(in_data$wscsda))
    }
    
    if ("0" %in% wscsda){
      in_data <- subset(in_data, wscsda != "0")
    }
    
    wscsda <-levels(droplevels(in_data$wscsda))
    
    basin_number <- length(wscsda)
    
    x <- 0
    
    while (x <= basin_number){
      
      if (x == 0){
        
        years <- levels(factor(in_data$year))
        n_years <- length(years)
        
        if (n_years >= 5){
          start_year <- as.numeric(years[1])
          end_year <- as.numeric(years[n_years])
          
          x_year <- start_year
          ann_ts <- ts(in_data, start = start_year, end = end_year, frequency = 1)
          unit <- Basin 
          line1 <- paste("Basin ", Basin, sep ="")
          
          regplot <- paste(Basin, "_Fish_", unit, "_", src, "_Median_Richness_Regression_", sys_date, ".png", sep = "")
          
          reg <- lm(med_rich~ year, data = ann_ts)
          
          reg_sum <- summary(reg)
          c <- reg_sum$coefficients
          rs <- reg_sum$adj.r.squared
          f <- reg_sum$fstatistic
          if(!is.null(f)){
            rp <- unname(pf(f[1],f[2],f[3],lower.tail=F))
          } else {
            rp <- "NA"
          }
          
          mk <- mannKen(ann_ts[,4])
          mks <- mk$sen.slope
          mkk <- mk$S
          mkp <- mk$p.value
          
          ann_out <- paste(src, unit, start_year, c[1,1], c[1,2], c[1,3], c[1,4], c[2,1], c[2,2], c[2,3], c[2,4], rs, unname(f[1]), rp, mks, mkk, mkp, sep = ",")
          write(ann_out, regfile, append = TRUE)
          
          
          line2 <- paste("Intercept =", format(c[1,1], digits = 5), ";", "Intercept STE =", format(c[1,2], digits = 5),";",  "Intercept T-Statistic", format(c[1,3], digits = 5),";", "Intercept p-value =", format(c[1,4], digits = 5))
          line3 <- paste("Slope =", format(c[2,1], digits = 5), ";", "Slope STE =", format(c[2,2], digits = 5),";",  "Slope T-Statistic", format(c[2,3], digits = 5),";", "Slope p-value =", format(c[2,4], digits = 5))
          line4 <- paste("F-Statistic:", format(f[1], digits = 5), "; p-value:", format(rp, digits = 4))
          line5 <- paste("Theil-Sen Slope = ", format(mks, digits = 4), ";", "Mann-Kendal Score = ", format(mkk, digits = 5), ";", "Mann-Kendall p-value = ", format(mkp, digits = 5))
          line7 <- paste("Data Source:", src, sep = "")
          footer <-paste(line2, "\n", line3, "\n", line4, "\n", line5,"\n", line7 )
          
          
          if (rp < 0.05) {
            t <- ggplot(data = in_data, aes(factor(year), med_rich)) + geom_point() + geom_smooth(method = "lm", aes(group = 1)) + xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 4))  
          } else{ 
            t <- ggplot(data = in_data, aes(factor(year), med_rich)) + geom_point()+ xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 6))   	
          }
          
          s <- t + theme(plot.margin = unit(c(0.5,0.5,2,0), "cm")) + ggtitle(line1) + theme(plot.title = element_text(size = 8)) 
          v <- arrangeGrob(s, sub = textGrob(footer, y = 1.5, gp=gpar(fontsize=8)))
          ggsave(filename= regplot, plot = v, dpi= 128, width = 8, height = 6)
        } else {
          y <- y + 1
        }
        
      } else {
        i <- wscsda[x]
        
        reg_data <- subset(in_data, wscsda == i)
        
        years1 <- levels(factor(reg_data$year))
        n1_years <- length(years1)
        
        if (n1_years >= 5){
          
          start_year1 <- as.numeric(years1[1])
          end_year1 <- as.numeric(years1[n1_years])
          
          ann_ts1 <- ts(reg_data, start = start_year1, end = end_year1, frequency = 1)
          unit <- i
          line1 <- paste("Sub-Basin ", i, sep ="")
          
          
          regplot1 <- paste(Basin, "_Fish_",unit, "_", src, "_Median_Richness_Regression_",  sys_date, ".png", sep = "")
          
          reg1 <- lm(med_rich~ year, data = ann_ts1)
          
          reg_sum1 <- summary(reg1)
          c1 <- reg_sum1$coefficients
          rs1 <- reg_sum1$adj.r.squared
          f1 <- reg_sum1$fstatistic
          if(!is.null(f1)){
            rp1 <- unname(pf(f1[1],f1[2],f1[3],lower.tail=F))
          } else {
            rp1 <- "NA"
          }
          
          mk1 <- mannKen(ann_ts1[,4])
          mks1 <- mk1$sen.slope
          mkk1 <- mk1$S
          mkp1 <- mk1$p.value
          
          ann_out1 <- paste(src, unit, start_year, c1[1,1], c1[1,2], c1[1,3], c1[1,4], c1[2,1], c1[2,2], c1[2,3], c1[2,4], rs1, unname(f1[1]), rp1, mks1, mkk1, mkp1, sep = ",")
          write(ann_out1, regfile, append = TRUE)
          
          
          line2 <- paste("Intercept =", format(c1[1,1], digits = 5), ";", "Intercept STE =", format(c1[1,2], digits = 5),";",  "Intercept T-Statistic", format(c1[1,3], digits = 5),";", "Intercept p-value =", format(c1[1,4], digits = 5))
          line3 <- paste("Slope =", format(c1[2,1], digits = 5), ";", "Slope STE =", format(c1[2,2], digits = 5),";",  "Slope T-Statistic", format(c1[2,3], digits = 5),";", "Slope p-value =", format(c1[2,4], digits = 5))
          line4 <- paste("F-Statistic:", format(f1[1], digits = 5), "; p-value:", format(rp1, digits = 4))
          line5 <- paste("Theil-Sen Slope = ", format(mks1, digits = 4), ";", "Mann-Kendal Score = ", format(mkk1, digits = 5), ";", "Mann-Kendall p-value = ", format(mkp1, digits = 5))
          line7 <- paste("Data Source:", src, sep = "")
          footer <-paste(line2, "\n", line3, "\n", line4, "\n", line5,"\n", line7 )
          
          
          if (rp1 < 0.05) {
            t1 <- ggplot(data = reg_data, aes(factor(year), med_rich)) + geom_point() + geom_smooth(method = "lm", aes(group = 1)) + xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 4))	
          } else{ 
            t1 <- ggplot(data = reg_data, aes(factor(year), med_rich)) + geom_point() + xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 6))	 	
          }
          
          s1 <- t1 + theme(plot.margin = unit(c(0.5,0.5,2,0), "cm")) + ggtitle(line1) + theme(plot.title = element_text(size = 8)) 
          v1 <- arrangeGrob(s1, sub = textGrob(footer, y = 1.5, gp=gpar(fontsize=8)))
          ggsave(filename= regplot1, plot = v1, dpi= 128, width = 8, height = 6)
          
        }
      }
      if (start_year > x_year){
        x_year <- start_year
      }
      x <- x + 1
      
    }
    
    if (y ==1) {
      source_list <- src
    } else {
      source_list <- paste(source_list, src, sep = ";")
    }
    y <- y + 1
  }
  
  if (sources >1){
    global_data <- subset(in_data, year >= x_year)
    start_year <- global_data$year[1]
    ann_ts <- ts(global_data, start = start_year, end = 2012, frequency = 1)
    unit <- Basin 
    line1 <- paste("Basin ", Basin, sep ="")
    
    regplot <- paste(Basin, "_Fish_", unit, "_", src, "_Median_Richness_Regression", sys_date, ".png", sep = "")
    
    reg <- lm(med_rich~ year, data = ann_ts)
    
    reg_sum <- summary(reg)
    c <- reg_sum$coefficients
    rs <- reg_sum$adj.r.squared
    f <- reg_sum$fstatistic
    if(!is.null(f)){
      rp <- unname(pf(f[1],f[2],f[3],lower.tail=F))
    } else {
      rp <- "NA"
    }
    
    mk <- mannKen(ann_ts[,4])
    mks <- mk$sen.slope
    mkk <- mk$S
    mkp <- mk$p.value
    
    ann_out <- paste(source_list, unit, start_year, c[1,1], c[1,2], c[1,3], c[1,4], c[2,1], c[2,2], c[2,3], c[2,4], rs, unname(f[1]), rp, mks, mkk, mkp, sep = ",")
    write(ann_out, regfile, append = TRUE)
    
    
    line2 <- paste("Intercept =", format(c[1,1], digits = 5), ";", "Intercept STE =", format(c[1,2], digits = 5),";",  "Intercept T-Statistic", format(c[1,3], digits = 5),";", "Intercept p-value =", format(c[1,4], digits = 5))
    line3 <- paste("Slope =", format(c[2,1], digits = 5), ";", "Slope STE =", format(c[2,2], digits = 5),";",  "Slope T-Statistic", format(c[2,3], digits = 5),";", "Slope p-value =", format(c[2,4], digits = 5))
    line4 <- paste("F-Statistic:", format(f[1], digits = 5), "; p-value:", format(rp, digits = 4))
    line5 <- paste("Theil-Sen Slope = ", format(mks, digits = 4), ";", "Mann-Kendal Score = ", format(mkk, digits = 5), ";", "Mann-Kendall p-value = ", format(mkp, digits = 5))
    line7 <- paste("Data Sources:", source_list, sep = "")
    footer <-paste(line2, "\n", line3, "\n", line4, "\n", line5,"\n", ",", line7 )
  } 
   
    
  #Part 2B
  #Starts to calculate regressions etc. on total fish numbers
  #Commence à calculer les régressions etc. sur les nombre de poissons totaux 
  
    regfile <- paste(Basin, "_Fish_Total_Richness_Annual_Stats_", sys_date, ".csv", sep = "")
    
    annual_header <-  "Source, WSCSDA, Start_Year, Intercept, Intercept_STE, Intercept-T, Intercept-p, Slope, Slope_STE, Slope-T, Slope-p, Adj.RSqr, F-Stat, F-pvalue, Theil-Sen Slope, Mann-Ken Score, Mann-Ken p-value"
    write(annual_header, regfile)
    
    data_sources <- levels(factor(total_df$ref))
    sources <- length(data_sources)
    
    y_lab <- "Total Number of Fish Species Observed Per Site"
    
    y <- 1
    
    while (y <= sources){
      
      print(y)
      src <- data_sources[y]
      print(src)
      in_data <- subset(total_df, ref == src)
      
      if (class(in_data$wscsda) == "factor" ){
        wscsda <-levels(droplevels(in_data$wscsda))
      }else {
        wscsda <-levels(factor(in_data$wscsda))
      }
      
      if ("0" %in% wscsda){
        in_data <- subset(in_data, wscsda != "0")
      }
      
      wscsda <-levels(droplevels(in_data$wscsda))
      
      basin_number <- length(wscsda)
      
      x <- 0
      
      while (x <= basin_number){
        
        if (x == 0){
          
          years <- levels(factor(in_data$year))
          n_years <- length(years)
          
          if (n_years >= 5){
            start_year <- as.numeric(years[1])
            end_year <- as.numeric(years[n_years])
            
            x_year <- start_year
            ann_ts <- ts(in_data, start = start_year, end = end_year, frequency = 1)
            unit <- Basin 
            line1 <- paste("Basin ", Basin, sep ="")
            
            regplot <- paste(Basin, "_Fish_", unit, "_", src, "_Total_Richness_Regression_", sys_date, ".png", sep = "")
            
            reg <- lm(richness~ year, data = ann_ts)
            
            reg_sum <- summary(reg)
            c <- reg_sum$coefficients
            rs <- reg_sum$adj.r.squared
            f <- reg_sum$fstatistic
            if(!is.null(f)){
              rp <- unname(pf(f[1],f[2],f[3],lower.tail=F))
            } else {
              rp <- "NA"
            }
            
            mk <- mannKen(ann_ts[,4])
            mks <- mk$sen.slope
            mkk <- mk$S
            mkp <- mk$p.value
            
            ann_out <- paste(src, unit, start_year, c[1,1], c[1,2], c[1,3], c[1,4], c[2,1], c[2,2], c[2,3], c[2,4], rs, unname(f[1]), rp, mks, mkk, mkp, sep = ",")
            write(ann_out, regfile, append = TRUE)
            
            
            line2 <- paste("Intercept =", format(c[1,1], digits = 5), ";", "Intercept STE =", format(c[1,2], digits = 5),";",  "Intercept T-Statistic", format(c[1,3], digits = 5),";", "Intercept p-value =", format(c[1,4], digits = 5))
            line3 <- paste("Slope =", format(c[2,1], digits = 5), ";", "Slope STE =", format(c[2,2], digits = 5),";",  "Slope T-Statistic", format(c[2,3], digits = 5),";", "Slope p-value =", format(c[2,4], digits = 5))
            line4 <- paste("F-Statistic:", format(f[1], digits = 5), "; p-value:", format(rp, digits = 4))
            line5 <- paste("Theil-Sen Slope = ", format(mks, digits = 4), ";", "Mann-Kendal Score = ", format(mkk, digits = 5), ";", "Mann-Kendall p-value = ", format(mkp, digits = 5))
            line7 <- paste("Data Source:", src, sep = "")
            footer <-paste(line2, "\n", line3, "\n", line4, "\n", line5,"\n", line7 )
            
            
            if (rp < 0.05) {
              t <- ggplot(data = in_data, aes(factor(year), richness)) + geom_point() + geom_smooth(method = "lm", aes(group = 1)) + xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 4))  
            } else{ 
              t <- ggplot(data = in_data, aes(factor(year), richness)) + geom_point()+ xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 6))     
            }
            
            s <- t + theme(plot.margin = unit(c(0.5,0.5,2,0), "cm")) + ggtitle(line1) + theme(plot.title = element_text(size = 8)) 
            v <- arrangeGrob(s, sub = textGrob(footer, y = 1.5, gp=gpar(fontsize=8)))
            ggsave(filename= regplot, plot = v, dpi= 128, width = 8, height = 6)
          } else {
            y <- y + 1
          }
          
        } else {
          i <- wscsda[x]
          
          reg_data <- subset(in_data, wscsda == i)
          
          years1 <- levels(factor(reg_data$year))
          n1_years <- length(years1)
          
          if (n1_years >= 5){
            
            start_year1 <- as.numeric(years1[1])
            end_year1 <- as.numeric(years1[n1_years])
            
            ann_ts1 <- ts(reg_data, start = start_year1, end = end_year1, frequency = 1)
            unit <- i
            line1 <- paste("Sub-Basin ", i, sep ="")
            
            
            regplot1 <- paste(Basin, "_Fish_",unit, "_", src, "_Total_Richness_Regression_",  sys_date, ".png", sep = "")
            
            reg1 <- lm(richness~ year, data = ann_ts1)
            
            reg_sum1 <- summary(reg1)
            c1 <- reg_sum1$coefficients
            rs1 <- reg_sum1$adj.r.squared
            f1 <- reg_sum1$fstatistic
            if(!is.null(f1)){
              rp1 <- unname(pf(f1[1],f1[2],f1[3],lower.tail=F))
            } else {
              rp1 <- "NA"
            }
            
            mk1 <- mannKen(ann_ts1[,4])
            mks1 <- mk1$sen.slope
            mkk1 <- mk1$S
            mkp1 <- mk1$p.value
            
            ann_out1 <- paste(src, unit, start_year, c1[1,1], c1[1,2], c1[1,3], c1[1,4], c1[2,1], c1[2,2], c1[2,3], c1[2,4], rs1, unname(f1[1]), rp1, mks1, mkk1, mkp1, sep = ",")
            write(ann_out1, regfile, append = TRUE)
            
            
            line2 <- paste("Intercept =", format(c1[1,1], digits = 5), ";", "Intercept STE =", format(c1[1,2], digits = 5),";",  "Intercept T-Statistic", format(c1[1,3], digits = 5),";", "Intercept p-value =", format(c1[1,4], digits = 5))
            line3 <- paste("Slope =", format(c1[2,1], digits = 5), ";", "Slope STE =", format(c1[2,2], digits = 5),";",  "Slope T-Statistic", format(c1[2,3], digits = 5),";", "Slope p-value =", format(c1[2,4], digits = 5))
            line4 <- paste("F-Statistic:", format(f1[1], digits = 5), "; p-value:", format(rp1, digits = 4))
            line5 <- paste("Theil-Sen Slope = ", format(mks1, digits = 4), ";", "Mann-Kendal Score = ", format(mkk1, digits = 5), ";", "Mann-Kendall p-value = ", format(mkp1, digits = 5))
            line7 <- paste("Data Source:", src, sep = "")
            footer <-paste(line2, "\n", line3, "\n", line4, "\n", line5,"\n", line7 )
            
            
            if (rp1 < 0.05) {
              t1 <- ggplot(data = reg_data, aes(factor(year), richness)) + geom_point() + geom_smooth(method = "lm", aes(group = 1)) + xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 4))	
            } else{ 
              t1 <- ggplot(data = reg_data, aes(factor(year), richness)) + geom_point() + xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 6))	 	
            }
            
            s1 <- t1 + theme(plot.margin = unit(c(0.5,0.5,2,0), "cm")) + ggtitle(line1) + theme(plot.title = element_text(size = 8)) 
            v1 <- arrangeGrob(s1, sub = textGrob(footer, y = 1.5, gp=gpar(fontsize=8)))
            ggsave(filename= regplot1, plot = v1, dpi= 128, width = 8, height = 6)
            
          }
        }
        if (start_year > x_year){
          x_year <- start_year
        }
        x <- x + 1
        
      }
      
      if (y ==1) {
        source_list <- src
      } else {
        source_list <- paste(source_list, src, sep = ";")
      }
      y <- y + 1
    }
    
    if (sources >1){
      global_data <- subset(in_data, year >= x_year)
      start_year <- global_data$year[1]
      ann_ts <- ts(global_data, start = start_year, end = 2012, frequency = 1)
      unit <- Basin 
      line1 <- paste("Basin ", Basin, sep ="")
      
      regplot <- paste(Basin, "_Fish_", unit, "_", src, "_Total_Richness_Regression", sys_date, ".png", sep = "")
      
      reg <- lm(richness~ year, data = ann_ts)
      
      reg_sum <- summary(reg)
      c <- reg_sum$coefficients
      rs <- reg_sum$adj.r.squared
      f <- reg_sum$fstatistic
      if(!is.null(f)){
        rp <- unname(pf(f[1],f[2],f[3],lower.tail=F))
      } else {
        rp <- "NA"
      }
      
      mk <- mannKen(ann_ts[,4])
      mks <- mk$sen.slope
      mkk <- mk$S
      mkp <- mk$p.value
      
      ann_out <- paste(source_list, unit, start_year, c[1,1], c[1,2], c[1,3], c[1,4], c[2,1], c[2,2], c[2,3], c[2,4], rs, unname(f[1]), rp, mks, mkk, mkp, sep = ",")
      write(ann_out, regfile, append = TRUE)
      
      
      line2 <- paste("Intercept =", format(c[1,1], digits = 5), ";", "Intercept STE =", format(c[1,2], digits = 5),";",  "Intercept T-Statistic", format(c[1,3], digits = 5),";", "Intercept p-value =", format(c[1,4], digits = 5))
      line3 <- paste("Slope =", format(c[2,1], digits = 5), ";", "Slope STE =", format(c[2,2], digits = 5),";",  "Slope T-Statistic", format(c[2,3], digits = 5),";", "Slope p-value =", format(c[2,4], digits = 5))
      line4 <- paste("F-Statistic:", format(f[1], digits = 5), "; p-value:", format(rp, digits = 4))
      line5 <- paste("Theil-Sen Slope = ", format(mks, digits = 4), ";", "Mann-Kendal Score = ", format(mkk, digits = 5), ";", "Mann-Kendall p-value = ", format(mkp, digits = 5))
      line7 <- paste("Data Sources:", source_list, sep = "")
      footer <-paste(line2, "\n", line3, "\n", line4, "\n", line5,"\n", ",", line7 )
    
    
    #Part3
    #Boxplots to show number of species per site, per year
    #Graphiques a boîtes montrant le nombre d'espèces par site, par an
    if (rp < 0.05) {
      t <- ggplot(data = read_data, aes(factor(year), count)) + geom_point() + geom_smooth(method = "lm", aes(group = 1)) + xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 4))  
    } else{ 
      t <- ggplot(data = read_data, aes(factor(year), count)) + geom_point() + xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 6))	 	
    }
    
    s <- t + theme(plot.margin = unit(c(0.5,0.5,2,0), "cm")) + ggtitle(line1) + theme(plot.title = element_text(size = 8)) 
    v <- arrangeGrob(s, sub = textGrob(footer, y = 1.5, gp=gpar(fontsize=8)))
    ggsave(filename= regplot, plot = v, dpi= 200, width = 8, height = 6)
    
    
    n <- ggplot(data = site_df, aes(factor(year), richness)) + geom_boxplot(aes(fill = ref)) + facet_wrap(~wscsda, ncol = 2)
    m <- n  + xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 6)) + labs(fill = "Data Source")
    boxplot <- paste(Basin, "_Fish_", unit, "_", "_Summary_Boxplot.png", sep = "")
    ggsave(filename= boxplot, plot = m, dpi= 200, width = 8, height = 6)
  } else {
    
    
    n <- ggplot(data = site_df, aes(factor(year), richness)) + geom_boxplot() + facet_wrap(~wscsda)
    m <- n  + xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 8)) 
    boxplot <- paste(Basin, "_Fish_", unit, "_", "_Summary_Boxplot.png", sep = "")
    ggsave(filename= boxplot, plot = m, dpi= 200, width = 8, height = 6)
  }
}


  