wq_stats <- function(in_file, Basin, prv) {
  require(Hmisc)
  require (ggplot2)
  require(gridExtra)
  require(wq)
  require(reshape2)

  
  #this version includes water quality guidelines for pollutant of emerging concern, from CCME, Quebec, and Alberta
  #input datasets must include attributes "site", "date", "year", "ref", and "wscsda". 
  #all additional attributes should relate to parameter names, such as "nitrogen" and should fall under the column heading "variable" with an associated value in the "value" column.
  #if the above structure is not met, run "melt" with id.var = c("site", "date", "year", "ref", "wscsda).
  #make sure "date" column is formatted as.Date not character, if not run as.Date.
  
  #code is currently is setup primarily for running watersheds in a single province. however it is possible to adapt. 
  #make sure "Basin name is added to basin dictionary below.
  #also, add all provincial/CCME water quality guidelines that are required for the province of interest.
  
  Date <- Sys.Date()
  df <- read.csv(in_file)
  df$date = as.Date(df$date, format='%d/%m/%Y')
  
  basins = c("01" = "St. John River Basin", "01EF" = "LaHave River Basin", "01EJ" = "Sackville River Basin", "02" = "Ottawa River Basin", "02N" = "St. Maurice Basin", "07" = "Athabasca River Basin", "07-1" = "Peace River Basin", "08" = "Fraser River Basin", "08-1" = "Skeena River Basin", "02G" = "Thames River Basin", "05" = "South Saskatchewan Basin", "05-1" = "North Saskatchewan Basin", "02-HC" = "Humber River Basin", "10" = "Liard River Basin", "LStLN"="Lower St.Lawrence, North", "LStLS"="Lower St.Lawrence, South", "MStL"="Main St.Lawrence Reach", "NEStL"="Northeast St.Lawrence and Estuary", "Saguenay"="Saguenay", "UStLN"="Upper St.Lawrence, North", "UStLS"="Upper St.Lawrence, South", "StL"="St.Lawrence (Pearse)", "North Shore and Gaspe"="North Shore and Gaspe Peninsula", "LM"="Lower Mackenzie", "PA"="Peace Athabasca", "GL"="Great Lakes", "Skeena"="Skeena", "MC"="Maritime Coastal", "6"  = "Peace-Athabasca")
  bname <- as.character(basins[Basin])
  print(bname)
  
  colnames(df) <- tolower(colnames(df))    
  
  df$value <- gsub("NA", NA, df$value)
  df$value <- as.numeric(df$value)
  df <- na.omit(df)
  
  df$variable <- as.character(df$variable)
  df$variable <- tolower(df$variable)
  df$variable <- as.factor(df$variable)
  
  levels(df$variable)[levels(df$variable) == "aluminum"] <- "aluminium"
  levels(df$variable)[levels(df$variable) == "phosphorous"] <- "phosphorus"
  levels(df$variable)[levels(df$variable) == "total.arsenic"] <- "arsenic"
  levels(df$variable)[levels(df$variable) == "dissolved oxygen"] <- "dissolved.oxygen"
  levels(df$variable)[levels(df$variable) == "nickle"] <- "nickel"
  
  levels(df$variable) <- gsub(".total", "", levels(df$variable))
  
  colnames(df) <- gsub("wscssda", "wscsda", colnames(df))
  
  wscsda <-levels(factor(df$wscsda))
  
  if ("0" %in% wscsda){
    df <- subset(df, wscsda != "0")
  } else if ("" %in% wscsda){
    df <- subset(df, wscsda != "")
    
  }
  
  wscsda <-levels(droplevels(factor(df$wscsda)))

  
  print(prv)
  if (prv == "AB"){
    guides <- c("aluminium" = c(5,100), "ammonia" = 0.019, "arsenic" = 5, "cadmium" = 1 , "chloride" = 120, "copper" = 7, "lead" = 1, "iron" = 300, "mercury" = 0.026, "nickel" = 25, "nitrate"= 13, "nitrite" = 0.198, "nitrogen" = 1, "phosphorus" = 0.05, "ph" = c(6.5, 9), "uranium" = 15, "zinc" = 30, "dissolved.oxygen" = 5,"aldrin"=0.004,"chlordane"=0.006, "dieldrin"=0.004, "mirex"=0.001,"polychlorinated.biphenyls"=0.001,"bisphenol.a"=20,"bis.2.ethylhexyl.phthalate
"=16,"di.n.butyl.phthalate
"=19,"dimethyl.phthalate
"=73,"atrazine"=1.8,"bentazone"=510,"bromoxynil"=5,"dicamba"=10,"glyphosate"=65,"x2.4.d
"=4,"mcpa"=2.6,"acenaphtene"=5.8,"acridine"=4.4,"anthracene"=0.012,"benz.a.anthracene"=0.018,"benzo.a.pyrene"=0.015,"fluorene"=3,"fluoranthene"=0.04,"naphtalene"=1.1,"phenanthrene
"=0.4,"pyrene"=0.025,"aldrin"=0.015,"chlordane"=0.029,"polychlorinated.biphenyls
"=0.001,"toxaphene"=0.005,"bisphenol.a
"=20,"bis.2.ethylhexyl.phthalate
"=16,"di.n.butyl.phthalate
"=19,"atrazine"=1.8,"bentazone"=510,"bromoxynil
"=5,"dicamba"=10,"glyphosate
"=800,"x2.4.d
"=4.2,"mcpa"=2.6)
    units <- c("aluminium" = "ug/L", "ammonia" = "mg/L", "arsenic" = "ug/L", "cadmium" = "ug/L", "chloride" = "mg/L", "copper" = "ug/L", "lead" = "ug/L", "iron" = "ug/L", "mercury" = "ug/L", "nickel" = "ug/L", "nitrate"= "mg/L", "nitrite" = "mg/L", "nitrogen" = "mg/L", "phosphorus" = "mg/L", "ph" = " ", "uranium" = "ug/L", "zinc" = "ug/L", "dissolved.oxygen" = "mg/L","turbidity" = "NTU","temperature"= "Celsius","aldrin"="ug/L",chlordane="ug/L","dieldrin"="ug/L","mirex"="ug/L","polychlorinated.biphenyls"="ug/L","bisphenol.a"="ug/L","bis.2.ethylhexyl.phthalate
"="ug/L","di.n.butyl.phthalate
"="ug/L","dimethyl.phthalate
"="ug/L","atrazine"="ug/L","bentazone"="ug/L","bromoxynil"="ug/L","dicamba"="ug/L","glyphosate"="ug/L","x2.4.d
"="ug/L","mcpa"="ug/L","acenaphtene"="ug/L","acridine"="ug/L","anthracene"="ug/L","benz.a.anthracene"="ug/L","benzo.a.pyrene"="ug/L","fluorene"="ug/L","fluoranthene"="ug/L","naphtalene"="ug/L","phenanthrene
"="ug/L","pyrene"="ug/L","aldrin"="ug/L","chlordane"="ug/L","polychlorinated.biphenyls
"="ug/L","toxaphene"="ug/L","bisphenol.a
"="ug/L","bis.2.ethylhexyl.phthalate
"="ug/L","di.n.butyl.phthalate
"="ug/L","atrazine"="ug/L","bentazone"="ug/L","bromoxynil
"="ug/L","dicamba"="ug/L","glyphosate"="ug/L","x2.4.d
"="ug/L","mcpa"="ug/L")
  } else if (prv == "BC"){
    guides <- c("ammonia" = 0.019, "arsenic" = 5, "cadmium" = 1, "chloride" = 120, "copper" = 2, "lead" = 1, "iron" = 300, "mercury" = 0.026, "nickel" = 25, "nitrate"= 12.76, "nitrite" = 0.088, "nitrogen" = 0.7, "phosphorus" = 0.025, "ph" = c(6.5, 8.5), "uranium" = 10, "zinc" = 30, "dissolved.oxygen" = 6.5)
    units <- c("aluminium" = "ug/L", "ammonia" = "mg/L", "arsenic" = "ug/L", "cadmium" = "ug/L", "chloride" = "mg/L", "copper" = "ug/L", "lead" = "ug/L", "iron" = "ug/L", "mercury" = "ug/L", "nickel" = "ug/L", "nitrate"= "mg/L", "nitrite" = "mg/L", "nitrogen" = "mg/L", "phosphorus" = "mg/L", "ph" = " ", "uranium" = "ug/L", "zinc" = "ug/L", "dissolved.oxygen" = "mg/L","turbidity" = "NTU","temperature", "Celsius")
  } else if (prv == "NB"){
    guides <- c("ammonia" = 0.019, "arsenic" = 5, "cadmium" = 1, "chloride" = 120, "copper" = 2, "lead" = 1, "iron" = 300, "mercury" = 0.026, "nickel" = 25, "nitrate"= 12.97, "nitrite" = 0.198, "phosphorus" = 0.03, "ph" = c(6.5, 9), "uranium" = 15, "zinc" = 30, "dissolved.oxygen" = 6.5, "turbidity" = 10)
    units <- c("aluminium" = "ug/L", "ammonia" = "mg/L", "arsenic" = "ug/L", "cadmium" = "ug/L", "chloride" = "mg/L", "copper" = "ug/L", "lead" = "ug/L", "iron" = "ug/L", "mercury" = "ug/L", "nickel" = "ug/L", "nitrate"= "mg/L", "nitrite" = "mg/L", "nitrogen" = "mg/L", "phosphorus" = "mg/L", "ph" = " ", "uranium" = "ug/L", "zinc" = "ug/L", "dissolved.oxygen" = "mg/L", "turbidity" = "NTU","temperature", "Celsius")
  }else if (prv == "NS"){
    guides <- c("ammonia" = 0.019, "arsenic" = 5, "cadmium" = 1, "chloride" = 120, "copper" = 2, "lead" = 1, "iron" = 0.3, "mercury" = 0.026, "nickel" = 25, "nitrate"= 13.28, "nitrite" = 0.198, "phosphorus" = 0.02, "ph" = c(6.5, 9), "uranium" = 15, "zinc" = 30, "dissolved.oxygen" = 6.5)
    units <- c("aluminium" = "ug/L", "ammonia" = "mg/L", "arsenic" = "ug/L", "cadmium" = "ug/L", "chloride" = "mg/L", "copper" = "ug/L", "lead" = "ug/L", "iron" = "ug/L", "mercury" = "ug/L", "nickel" = "ug/L", "nitrate"= "mg/L", "nitrite" = "mg/L", "nitrogen" = "mg/L", "phosphorus" = "mg/L", "ph" = " ", "uranium" = "ug/L", "zinc" = "ug/L", "dissolved.oxygen" = "mg/L", "turbidity" = "NTU","temperature", "Celsius") 
  }else if (prv == "NWT"){
    guides <- c("ammonia" = 0.019, "arsenic" = 5, "cadmium" = 1, "chloride" = 120, "copper" = 2, "lead" = 1, "iron" = 0.3, "mercury" = 0.026, "nickel" = 25, "nitrate"= 13.28, "nitrite" = 0.198, "phosphorus" = 0.02, "ph" = c(6.5, 9), "uranium" = 15, "zinc" = 30, "dissolved.oxygen" = 5)
    units <- c("aluminium" = "ug/L", "ammonia" = "mg/L", "arsenic" = "ug/L", "cadmium" = "ug/L", "chloride" = "mg/L", "copper" = "ug/L", "lead" = "ug/L", "iron" = "ug/L", "mercury" = "ug/L", "nickel" = "ug/L", "nitrate"= "mg/L", "nitrite" = "mg/L", "nitrogen" = "mg/L", "phosphorus" = "mg/L", "ph" = " ", "uranium" = "ug/L", "zinc" = "ug/L", "dissolved.oxygen" = "mg/L", "turbidity" = "NTU","temperature", "Celsius") 
  }else if (prv == "ON"){
    guides <- c("ammonia" = 0.020, "arsenic" = 5, "cadmium" = 1, "chloride" = 120, "copper" = 2, "lead" = 1, "iron" = 300, "
mercury" = 0.02, "nickel" = 25, "nitrate"= 12.97, "nitrite" = 0.198, "phosphorus" = 0.03, "ph" = c(6.5, 8.5), "uranium" = 15, "
zinc" = 30, "dissolved.oxygen" = 6.5, "dicamba"=200, "bis.2.ethylhexyl.phtalate"=66, "di.n.butyl.phtalate"=19, "atrazine"=1.8, "
bromoxynil"=5, "glyphosate"=800, "x2.4.d"=4, "mcpa"=2.6, "acenaphtene"=5.8, "acridine"=4.4, "anthracene"=0.012, "benz.a.anthracene"=0.018, "
benzo.a.pyrene"=0.015, "fluorene"=3, "fluoranthene"=0.04, "naphtalene"=1.1, "phenanthrene"=0.4, "pyrene"=0.025, "quinoline"=3.4)
    units <- c("aluminium" = "ug/L", "ammonia" = "mg/L", "arsenic" = "ug/L", "cadmium" = "ug/L", "chloride" = "mg/L", "copper
" = "ug/L", "lead" = "ug/L", "iron" = "ug/L", "mercury" = "ug/L", "nickel" = "ug/L", "nitrate"= "mg/L", "nitrite
" = "mg/L", "nitrogen" = "mg/L", "phosphorus" = "mg/L", "ph" = " ", "uranium" = "ug/L", "zinc" = "ug/L", "dissolved.oxygen
" = "mg/L", "turbidity" = "NTU","temperature", "Celsius", "dicamba"="ug/L","bis.2.ethylhexyl.phtalate"="ug/L", "di.n.butyl.phtalate
" = "ug/L", "atrazine"="ug/L", "bromoxynil"="ug/L", "glyphosate"="ug/L", "x2.4.d"="ug/L", "mcpa"="ug/L", "acenaphtene"="mg/L", "acridine
" = "ug/L", "anthracene"="ug/L", "benz.a.anthracene"="ug/L", "benzo.a.pyrene"="ug/L", "fluorene"="ug/L", "fluoranthene"="ug/L", "naphtalene
" = "ug/L", "phenanthrene"="ug/L", "pyrene"="ug/L", "quinoline"="ug/L") 
  }else if (prv == "SK"){
    guides <- c("ammonia" = 0.019, "arsenic" = 5, "cadmium" = 1, "chloride" = 120, "copper" = 2, "lead" = 1, "iron" = 300, "mercury" = 0.02, "nickel" = 25, "nitrate"= 13, "nitrite" = 0.198, "nitrogen" = 1, "phosphorus" = 0.05, "ph" = c(6.5, 9), "uranium" = 15, "zinc" = 30, "dissolved.oxygen" = 6.5)
    units <- c("aluminium" = "ug/L", "ammonia" = "mg/L", "arsenic" = "ug/L", "cadmium" = "ug/L", "chloride" = "mg/L", "copper" = "ug/L", "lead" = "ug/L", "iron" = "ug/L", "mercury" = "ug/L", "nickel" = "ug/L", "nitrate"= "mg/L", "nitrite" = "mg/L", "nitrogen" = "mg/L", "phosphorus" = "mg/L", "ph" = " ", "uranium" = "ug/L", "zinc" = "ug/L", "dissolved.oxygen" = "mg/L", "turbidity" = "NTU","temperature", "Celsius") 
  }else if (prv == "QC"){
    guides <- c("ammonia" = 0.019, "arsenic" = 5, "cadmium" = 1, "chloride" = 640, "copper" = 2, "lead" = 1, "iron
" = 300, "mercury" = 0.026, "nickel" = 25, "nitrate"= 12.97, "nitrite" = 13.2, "phosphorus" = 0.03, "ph" = c(6.5, 9), "uranium
" = 15, "zinc" = 30, "dissolved.oxygen" = 5.5, "turbidity" = 10, "aldrin"=0.015, "chlordane"=0.029, "DDT"=0.029, "dieldrin
"=0.056, "hexachlorocyclohexane"=0.07, "U"=0.006, "toxaphene"=0.005, "bisphenol.a"=20, "benzyl.butyl.phtalate
"=67, "bis.2.ethylhexyl.phthalate"=16, "di.n.butyl.phthalate"=19, "dimethyl.phthalate"=73, "dionyl.phtalate"=140, "atrazine"=1.8, "bentazone
"=510, "bromoxynil"=5, "dicamba"=10, "dimethenamid"=5.6, "glyphosate"=65,"x2.4.d"=4.2, "mcpa"=2.6, "acenaphtene
"=38, "benz.a.anthracene"=0.018, "benzo.a.pyrene"=0.015, "fluorene"=0.12, "fluoranthene"=1.6, "naphtalene"=11, "phenanthrene
"=1.4, "pyrene"=0.025, "acridine"=4.4, "anthracene"=0.012)
    units <- c("aluminium" = "ug/L", "ammonia" = "mg/L", "arsenic" = "ug/L", "cadmium" = "ug/L", "chloride" = "mg/L", "copper
" = "ug/L", "lead" = "ug/L", "iron" = "ug/L", "mercury" = "ug/L", "nickel" = "ug/L", "nitrate"= "mg/L", "nitrite" = "mg/L", "nitrogen
" = "mg/L", "phosphorus" = "mg/L", "ph" = " ", "uranium" = "ug/L", "zinc" = "ug/L", "dissolved.oxygen" = "mg/L","turbidity
" = "NTU","temperature"= "Celsius", "aldrin"="ug/L","chlordane"="ug/L","DDT"="ug/L", "dieldrin"="ug/L","hexachlorocyclohexane
"="ug/L", "U"="ug/L", "toxaphene"="ug/L", "mirex"="ug/L","bisphenol.a"="ug/L","benzyl.butyl.phtalate"="ug/L", "bis.2.ethylhexyl.phthalate
"="ug/L", "di.n.butyl.phthalate"="ug/L", "dimethyl.phthalate"="ug/L", "dionyl.phtalate"="ug/L", "atrazine"="ug/L", "bentazone"="ug/L", "bromoxynil
"="ug/L", "dicamba"="ug/L", "glyphosate"="ug/L", "x2.4.d"="ug/L","mcpa"="ug/L","acenaphtene"="ug/L","acridine"="ug/L","anthracene
"="ug/L","benz.a.anthracene"="ug/L","benzo.a.pyrene"="ug/L","fluorene"="ug/L","fluoranthene"="ug/L","naphtalene"="ug/L","phenanthrene
"="ug/L","pyrene"="ug/L", "acridine"="ug/L", "anthracene"="ug/L")
  }else if (prv == "YK"){
    guides <- c("ammonia" = 0.019, "arsenic" = 5, "cadmium" = 1, "chloride" = 640, "copper" = 2, "lead" = 1, "iron" = 300, "mercury" = 0.026, "nickel" = 25, "nitrate"= 12.97, "nitrite" = 0.088, "nitrogen" = 0.7, "phosphorus" = 0.025, "ph" = c(6.5, 9), "uranium" = 15, "zinc" = 30, "dissolved.oxygen" = 8)
    units <- c("aluminium" = "ug/L", "ammonia" = "mg/L", "arsenic" = "ug/L", "cadmium" = "ug/L", "chloride" = "mg/L", "copper" = "ug/L", "lead" = "ug/L", "iron" = "ug/L", "mercury" = "ug/L", "nickel" = "ug/L", "nitrate"= "mg/L", "nitrite" = "mg/L", "nitrogen" = "mg/L", "phosphorus" = "mg/L", "ph" = " ", "uranium" = "ug/L", "zinc" = "ug/L", "dissolved.oxygen" = "mg/L", "turbidity" = "NTU", "temperature", "Celsius") 
  } 
  
  
  print(as.character(guides))
  
  regfile <- paste(Basin, "_", prv, "_", "WQ_Stats_", Date, ".csv", sep = "")
  annual_header <-  "Source, Indicator, WSCSDA, Start Year, End Year, Number of Years, Number of Sites, Intercept, Intercept_STE, Intercept-T, Intercept-p, Slope, Slope_STE, Slope-T, Slope-p, Adj.RSqr, F-Stat, F-pvalue, Theil-Sen Slope, Mann-Ken Score, Mann-Ken p-value"
  write(annual_header, regfile)
  
  ad_file <- paste(Basin, "_", prv, "_", "WQ_Adherence_", Date, ".csv", sep = "")
  ad_header <-  "Source, Indicator, WSCSDA, Start Year, End Year, Number of Years, Total Number of Sites, Number of Measurements, Provincial Guideline, Number of Guidelines Exceedances, 90th Percentile, Number of 90th Percentile Exceedances,75th Percentile, Number of 75th Percentile Exceedances"
  write(ad_header, ad_file)
  
  threshfile <- paste(Basin, "_", prv, "_", "WQ_Thresh_", Date, ".csv", sep = "")
  thresh_head <- paste("Contaminant", "Guideline", "75th Percentile", "90th Percentile", sep = ",")
  write(thresh_head,threshfile)
  
  chems <- levels(factor(df$variable))
  
  j <- 1
  while (j <= length(chems)){
    print(j)
    chem <- chems[j]
    print(chem)
    df2 <- subset(df, variable == chem)
    
    df2$site <- as.factor(df2$site)
    
    df2$value <- as.numeric(df2$value)
    df2 <- na.omit(df2)
    
    if (length(df2$value) == 0 ) {
      j <- j + 1
    } else {
      
      if (chem == "ph"){
        guide1 = as.numeric(guides["ph1"])
        guide2 = as.numeric(guides["ph2"])
        guide = paste(guide1, "-", guide2, sep = "")
        thresh1a <- as.numeric(quantile(df2$value, 0.25))
        thresh1b <- as.numeric(quantile(df2$value, 0.75))
        thresh2a <- as.numeric(quantile(df2$value, 0.10))
        thresh2b <- as.numeric(quantile(df2$value, 0.90))
        
        thresh <- paste(thresh1a, "-", thresh1b, sep = "")
        thresh2 <- paste(thresh2a, "-", thresh2b, sep = "")
      
      } else if (chem == "dissolved.oxygen"){
        guide <- as.numeric(guides[chem])
        thresh <- as.numeric(quantile(df2$value, 0.25))
        thresh2 <- as.numeric(quantile(df2$value, 0.10))
      
      } else if (chem == "aluminium"){        
        guide1 = as.numeric(guides["aluminium1"])
        guide2 = as.numeric(guides["aluminium2"])
        guide = paste(guide1, "-", guide2, sep = "")
        thresh <- as.numeric(quantile(df2$value, 0.75))
        thresh2 <- as.numeric(quantile(df2$value, 0.90))
        
      } else if (chem %in% c("cadmium", "copper", "lead", "nickel", "zinc" )){
        thresh <- as.numeric(quantile(df2$value, 0.75))
        thresh2 <- as.numeric(quantile(df2$value, 0.90))  
        guide <- as.numeric(guides[chem])
        
      } else {
        guide <- as.numeric(guides[chem])
        thresh <- as.numeric(quantile(df2$value, 0.75))
        thresh2 <- as.numeric(quantile(df2$value, 0.90))
      }  
      
      line_out <- paste(chem, guide, thresh, thresh2, sep = ",")
      print(line_out)
      write(line_out, threshfile, append = TRUE)
      un <- as.character(units[chem])
      
      if (j ==1){
        threshdf <- data.frame(chem, guide, thresh, thresh2, stringsAsFactors=FALSE)
        
      } else {
        
        row <- c(chem, guide, thresh, thresh2)
        threshdf <- rbind(threshdf, row)
      } 
      
    }
  j <- j + 1
  }
  
  colnames(threshdf) <- c("Chem", "Guide", "P75", "P90") 
  row.names(threshdf) <- NULL
  
  df$ref <- as.factor(df$ref)
  df$site <- as.character(df$site)
  df$variable <- as.character(df$variable)
  
  df_out <- data.frame(variable = character(), site = character(), year = integer(),  value = numeric(),  ref = factor(),  wscsda = factor(), date = as.Date(character()), Guide = numeric(), P75 = character(), P90 = character(), G1 = numeric(), T1 = numeric(), T2 = numeric())
    
  df_l <- merge(df, threshdf, by.x = "variable", by.y = "Chem", all)
  
  df_l1 <- subset(df_l, variable == "dissolved.oxygen")
  df_l1$G1 <- ifelse(as.numeric(df_l1$value) <= as.numeric(df_l1$Guide), 1, 0)
  df_l1$T1 <- ifelse(as.numeric(df_l1$value) <= as.numeric(df_l1$P75), 1, 0)
  df_l1$T2 <- ifelse(as.numeric(df_l1$value) <= as.numeric(df_l1$P90), 1, 0)
  
  if (nrow(df_l1) > 0) {
    df_out <- rbind(df_out, df_l1)
  }
  
  df_l2 <- subset(df_l, variable == "ph")
  df_l2$G1 <- ifelse(as.numeric(df_l2$value) <= as.numeric(strsplit(df_l2$Guide, "-")[[1]][1]), 1, 0)
  df_l2$G1 <- ifelse(as.numeric(df_l2$value) >= as.numeric(strsplit(df_l2$Guide, "-")[[1]][2]), 1, 0)
  df_l2$T1 <- ifelse(as.numeric(df_l2$value) <= as.numeric(strsplit(df_l2$P75, "-")[[1]][1]), 1, 0)
  df_l2$T1 <- ifelse(as.numeric(df_l2$value) >= as.numeric(strsplit(df_l2$P75, "-")[[1]][2]), 1, 0)
  df_l2$T2 <- ifelse(as.numeric(df_l2$value) <= as.numeric(strsplit(df_l2$P90, "-")[[1]][1]), 1, 0)
  df_l2$T2 <- ifelse(as.numeric(df_l2$value) >= as.numeric(strsplit(df_l2$P90, "-")[[1]][2]), 1, 0)
  
  if (nrow(df_l2) > 0) {
    df_out <- rbind(df_out, df_l2)
  }
  
  df_l3 <- subset(df_l, variable == "aluminium") 
  t <- unique(df_l3[,c("site", "date")])
  s <- t$site
  d <- t$date
  a <- subset(df, site %in% s & date %in% d & variable == "ph")
  if (nrow(a) > 0 ) {
    
    Al_CCME <- function(x){ 
      ph_val <- as.numeric(x["value"])
      if (ph_val < 6.5) {
        Guide <- 5  
      } else {
        Guide <- 100
      }
      return(Guide)
    }
    
    
    if (prv == "BC" | prv == "AB") {
      BC_al <- function(x){
        ph_val <- as.numeric(x["value"])
        if (ph_val < 6.5) {
          Guide <- (exp(1.209 - (2.426*ph_val) + (0.286 * (ph_val^2)))) * 1000
        } else {
          Guide <- 100
        } 
        return(Guide)
      }
      
      a$Guide <- apply(a, 1, BC_al)
    
    } else if (prv == "QC"){
        
        b <- subset(a, ref %in% grep("BQMA", levels(a$ref), value = TRUE))
        if(nrow(b) > 0) {
          b$Guide <- 87
        }
        
        c <- subset(a, ref %in% grep("BQMA", levels(a$ref), value = TRUE, invert = TRUE))
        if (nrow(c) > 0) {
          c$Guide <- apply(c, 1, Al_CCME)
        }
  
        if (nrow(b) > 0 & nrow(c) > 0) {
          a <- rbind(b, c)
        } else if (nrow(b) > 0) {
          a <- b
        } else if (nrow(c) > 0) {
          a <- c
        }  
        
    }else {
      a$Guide <- apply(a, 1, Al_CCME)
    }

    a <- a[, c("site", "date", "Guide")]
    df_l3 <- df_l3[,!names(df_l3) %in% c("Guide")]
    df_l3 <- merge(df_l3, a, by = c("site", "date"))

  }else {
    df_l3$Guide <- NULL
  }
  
  df_l3$G1 <- ifelse(as.numeric(df_l3$value) >= as.numeric(df_l3$Guide), 1, 0)
  df_l3$T1 <- ifelse(as.numeric(df_l3$value) >= as.numeric(df_l3$P75), 1, 0)
  df_l3$T2 <- ifelse(as.numeric(df_l3$value) >= as.numeric(df_l3$P90), 1, 0)
  
  if (nrow(df_l3) > 0) {
    df_out <- rbind(df_out, df_l3)
  }
  
  df_l4 <-subset(df_l, variable == "cadmium")
  t <- unique(df_l4[,c("site", "date")])
  s <- unique(t$site)
  d <- unique(t$date)
  a <- subset(df, site %in% s & date %in% d & variable == "hardness")
  
  b <- t[t$site %nin% a$site | t$date %nin% a$date, ]
  b$Guide <- 1
    
  cad_func <- function(x) {
    if (x[1] < 5.3) {
      Guide <- 0.11
    } else if (x[1] <= 360){
      Guide <- 10^((1.016*log10(x[1])) - 1.71)
    } else {
      Guide <- 7.7
    }
    return(Guide)
  }
  
  if (prv == "AB"){
    a$Guide<- sapply(a$value, cad_func)
  }else if (prv == "BC") {
    a$Guide <- 10^((0.86*log10(a$value)) - 3.2)
  } else if (prv == "MB"){
    a$Guide <- exp((0.7409*log(a$value)) - 4.719) * (1.101672 - (log(a$value) * 0.041838))    
  } else { 
    a$Guide<- sapply(a$value, cad_func)  
  }
  
  a <- a[, c("site", "date", "Guide")]
  a <- rbind(a, b)
  
  df_l4 <- df_l4[,!names(df_l4) %in% c("Guide")]
  df_l4 <- merge(df_l4, a, by = c("site", "date")) 
  
  df_l4$G1 <- ifelse(as.numeric(df_l4$value) >= as.numeric(df_l4$Guide), 1, 0)
  df_l4$T1 <- ifelse(as.numeric(df_l4$value) >= as.numeric(df_l4$P75), 1, 0)
  df_l4$T2 <- ifelse(as.numeric(df_l4$value) >= as.numeric(df_l4$P90), 1, 0)
  
  if (nrow(df_l4) > 0) {
    df_out <- rbind(df_out, df_l4)
  }
  
  df_l5 <-subset(df_l, variable == "copper")
  t <- unique(df_l5[,c("site", "date")])
  s <- unique(t$site)
  d <- unique(t$date)
  a <- subset(df, site %in% s & date %in% d & variable == "hardness")
  
  b <- t[t$site %nin% a$site | t$date %nin% a$date, ] 
  b$Guide <- 2
  
    if (prv == "AB"){
      
      a$Guide <- exp((0.979123*log(a$value)) - 8.64497) * 1000
      
    } else if (prv == "BC") {  
      a$Guide <- (0.094*a$value)+2 
      
    } else if (prv == "MB"){
      
        if (length(grep("EC", levels(a$ref))) > 1) {
          c <- subset(a, value < 90)
          if(nrow(c) > 0) {
            c$Guide <- 2
          }
          d <- subset(a, value >= 90)
          if (nrow(d) > 0) {
            d$Guide <- 0.2 * exp((0.8545*log(d$value)) - 1.465)
          }
          if (nrow(c) > 0 & nrow(d) > 0){
            a <- rbind(c, d)
          } else if (nrow(c) > 0){
            a <- c
          } else if (nrow(d) > 0){
            a <- d
          }
          
      } else {
        a$Guide <- 0.96 * exp((0.8545*log(a$value)) -1.702)
      }
        
    }else {
      cu_ccme <- function(x) {
        if(x[1] < 90) {
          Guide <- 2
        } else {
        Guide <- 0.2 * exp((0.8545*log(X[1])) - 1.465)
        }
      }
    
      Guide <- sapply(a$value, cu_ccme)
    }
    
  a <- a[, c("site", "date", "Guide")]
  a <- rbind(a, b)
  
  df_l5 <- df_l5[,!names(df_l5) %in% c("Guide")]
  df_l5 <- merge(df_l5, a, by = c("site", "date"))

  df_l5$G1 <- ifelse(as.numeric(df_l5$value) >= as.numeric(df_l5$Guide), 1, 0)
  df_l5$T1 <- ifelse(as.numeric(df_l5$value) >= as.numeric(df_l5$P75), 1, 0)
  df_l5$T2 <- ifelse(as.numeric(df_l5$value) >= as.numeric(df_l5$P90), 1, 0)
  
  if (nrow(df_l5) > 0) {
    df_out <- rbind(df_out, df_l5)
  }
  
  df_l6 <-subset(df_l, variable == "lead")
  t <- unique(df_l6[,c("site", "date")])
  s <- t$site
  d <- t$date
  a <- subset(df, site %in% s & date %in% d & variable == "hardness")
  
  b <- t[t$site %nin% a$site | t$date %nin% a$date, ]
  b$Guide <- 1
    
  if (prv == "YK"){
    
    ld_yk <- function(x) {
      if(x[1] < 50) {
        Guide <- 1
      } else {
        Guide <- exp((1.273*log(X[1])) - 4.705)
      }
    }
    a$Guide <- sapply(a$value, ld_yk)
    
  } else if (prv == "BC") {
    ld_bc <- function(x) {
      if(x[1] <= 8) {
        Guide <- 3
      } else {
        Guide <- exp((1.273*log(x[1])) - 1.460)
      }
    }
    a$Guide <- sapply(a$value, ld_bc)

  } else if (prv == "MB") {
    a$Guide <- (exp((1.273 * log(a$value)) - 4.705)) * (log(a$value)*0.145712)
  } else if (prv %in% c("SK", "NS", "NWT", "NFLD")){
    a$Guide <- exp((1.273 * log(a$value)) - 4.705)
  } else {
    ld_ccme <- function(x){
      if(x[1] <= 60) {
        Guide <- 1
      } else if (x[1] <= 180){
        Guide <- exp((1.273*log(x[1])) - 4.705)
      } else {
        Guide <- 7
      }      
    }
    a$Guide <- sapply(a$value, ld_ccme)
  }
  
  a <- a[, c("site", "date", "Guide")]
  a <- rbind(a, b)
  
  df_l6 <- df_l6[,!names(df_l6) %in% c("Guide")]
  df_l6 <- merge(df_l6, a, by = c("site", "date"))

  df_l6$G1 <- ifelse(as.numeric(df_l6$value) >= as.numeric(df_l6$Guide), 1, 0)
  df_l6$T1 <- ifelse(as.numeric(df_l6$value) >= as.numeric(df_l6$P75), 1, 0)
  df_l6$T2 <- ifelse(as.numeric(df_l6$value) >= as.numeric(df_l6$P90), 1, 0)
  
  if (nrow(df_l6) > 0) {
    df_out <- rbind(df_out, df_l6)
  }
  
  df_l7 <-subset(df_l, variable == "nickel") 
  t <- unique(df_l7[,c("site", "date")])
  s <- t$site
  d <- t$date
  a <- subset(df, site %in% s & date %in% d & variable == "hardness")
  
  b <- t[t$site %nin% a$site | t$date %nin% a$date, ]
  b$Guide <- 25
  
  ni_ccme <- function(x) {
    if(x[1] <= 60) {
      Guide <- 25
    } else if (x[1] <= 180) {
      Guide <- exp((0.76*log(x[1])) + 1.06)
    } else {
      Guide <- 150
    }
  }
  
  if (prv %in% c("NFLD", "NS", "ON", "SK")){ 
    a$Guide <- exp((0.76*log(a$value)) + 1.06)
  } else if (prv == "BC") {
    
    ni_bc <- function(x){
      if(x[1] <= 60){
        Guide <- 25
      } else if (x[1] <= 120) {
        Guide <- 65
      } else if (x[1] <= 180) {
        Guide <- 110
      } else {
        Guide <- 150
      } 
    }
    a$Guide <- sapply(a$value, ni_bc)
  
  } else if (prv == "AB") {
    
    a$Guide <- exp(0.846*log(a$value) + 2.255) 
    
  } else if (prv == "MB"){
    #needs to be fixed     
      c <- subset(a, ref %in% grep("MB", levels(a$ref), value = TRUE))
      if(nrow(c) > 0) {
        c$Guide <- (exp((0.8460*log(c$value)) + 0.0584)) * 0.997
      }
      d <- subset(a, ref %in% grep("MB", levels(a$ref), value = TRUE, invert = TRUE))
      if (nrow(d) > 0) {
        d$Guide <- exp((0.76*log(d$value)) + 1.06)
      }
      
  } else if (prv == "QC") {
    c <- subset(a, ref %in% grep("BQMA", levels(a$ref), value = TRUE))
    if(nrow(c) > 0) {
      c$Guide <- (exp((0.8460*log(e$value)) + 0.0584)) / 1000
    }
    fd<- subset(a, ref %in% grep("BQMA", levels(a$ref), value = TRUE, invert = TRUE))
    if (nrow(d) > 0) {
      d$Guide <- apply(d, 1, ni_ccme)
    }
    
    if (nrow(c) > 0 & nrow(d) > 0) {
      a <- rbind(c, d)
    } else if (nrow(c) > 0) {
      a <- c
    } else if (nrow(d) > 0) {
      a <- d
    }
    
  }else {
  a$Guide <- sapply(a$value, ni_ccme) 
  }
  
  a <- a[, c("site", "date", "Guide")]
  a <- rbind(a, b)
  
  df_l7 <- df_l7[,!names(df_l7) %in% c("Guide")]
  df_l7 <- merge(df_l7, a, by = c("site", "date"))

  df_l7$G1 <- ifelse(as.numeric(df_l7$value) >= as.numeric(df_l7$Guide), 1, 0)
  df_l7$T1 <- ifelse(as.numeric(df_l7$value) >= as.numeric(df_l7$P75), 1, 0)
  df_l7$T2 <- ifelse(as.numeric(df_l7$value) >= as.numeric(df_l7$P90), 1, 0)
  
  if (nrow(df_l7) > 0) {
    df_out <- rbind(df_out, df_l7)
  }
  
  df_l8 <-subset(df_l, variable == "zinc")
  
  t <- unique(df_l8[,c("site", "date")])
  s <- t$site
  d <- t$date
  a <- subset(df, site %in% s & date %in% d & variable == "hardness")
  
  b <- t[t$site %nin% a$site | t$date %nin% a$date, ]
  b$Guide <- 30
    
  zn_ccme <- function(x){
    if(x["value"] <= 90) {
      Guide <- 5.7
    } else {
      Guide <- 7.5 + (0.75 * (x["value"] - 90))
    }
    return(Guide)
  }
  
  if (prv == "BC") {
    a$Guide <- 7.5 + (0.75 * (a$value - 90))
    
  } else if (prv == "QC") {
    c <- subset(a, ref %in% grep("BQMA", levels(a$ref), value = TRUE))
    if(nrow(c) > 0) {
      c$Guide <- (exp((0.8473*log(c$value)) + 0.884)) / 1000
    }
    d <- subset(a, ref %in% grep("BQMA", levels(a$ref), value = TRUE, invert = TRUE))
    if (nrow(d) > 0) {
      d$Guide <- apply(d, 1, zn_ccme)
    }

    if (nrow(c) > 0 & nrow(d) > 0) {
      a <- rbind(c, d)
    } else if (nrow(c) > 0) {
      a <- c
    } else if (nrow(d) > 0) {
      a <- d
    }

  } else { 
    a$Guide <- apply(a,1, zn_ccme)
  }
  
  a <- a[, c("site", "date", "Guide")]
  a <- rbind(a, b)
  
  df_l8 <- df_l8[,!names(df_l8) %in% c("Guide")]
  df_l8 <- merge(df_l8, a, by = c("site", "date"))
  
  df_l8$G1 <- ifelse(as.numeric(df_l8$value) >= as.numeric(df_l8$Guide), 1, 0)
  df_l8$T1 <- ifelse(as.numeric(df_l8$value) >= as.numeric(df_l8$P75), 1, 0)
  df_l8$T2 <- ifelse(as.numeric(df_l8$value) >= as.numeric(df_l8$P90), 1, 0)
  
  if (nrow(df_l8) > 0) {
    df_out <- rbind(df_out, df_l8)
  }
  
  df_l9<-subset(df_l, variable == "ammonia")
  t <- unique(df_l9[,c("site", "date")])
  s <- as.character(t$site)
  d <- as.Date(t$date, format='%Y-%m-%d')
  a1 <- subset(df, site %in% s & date %in% d & variable == "temperature")
  colnames(a1) <- gsub("value", "temperature", colnames(a1))
  a2 <- subset(df, site %in% s & date %in% d & variable == "ph")
  colnames(a2) <- gsub("value", "ph", colnames(a2))
  a <-merge(a1, a2, by = c("site", "date", "year", "wscsda", "ref"))
  
  a$pka <- 0.0901821 + (2729.92/(a$temperature + 273.15))  
  a$f <- 1/(10^(a$pka - a$ph) + 1)
  a$Guide <- 0.019 / a$f
  a <- a[, c("site", "date", "Guide")]
  
  b <- t[t$site %nin% a$site | t$date %nin% a$date, ]
  b$Guide <- 0.019
  
  a <- rbind(a, b)
  
  df_l9 <- df_l9[,!names(df_l9) %in% c("Guide")]
  df_l9 <- merge(df_l9, a, by = c("site", "date"))
    
  df_l9$G1 <- ifelse(as.numeric(df_l9$value) >= as.numeric(df_l9$Guide), 1, 0)
  df_l9$T1 <- ifelse(as.numeric(df_l9$value) >= as.numeric(df_l9$P75), 1, 0)
  df_l9$T2 <- ifelse(as.numeric(df_l9$value) >= as.numeric(df_l9$P90), 1, 0)
  
  if (nrow(df_l9) > 0) {
    df_out <- rbind(df_out, df_l9)
  }
  
  df_l10 <-subset(df_l, variable == "nitrite")
  
  t <- unique(df_l10[,c("site", "date")])
  s <- unique(t$site)
  d <- unique(t$date)
  a <- subset(df, site %in% s & date %in% d & variable == "chloride")
  
  if (prv == "BC") {
    n_func <- function(x) {
      if(x < 0.02){
        guide <- 0.066
      } else if (x >= 2 & x < 4) {
        guide <- 0.132
      } else if (x >= 4 & x < 6) {
        guide <-0.198
      } else if (x >= 6 & x < 8) {
        guide <- 0.264
      } else if (x >= 8 & x < 10) {
        guide <- 0.33
      } else {
        guide <- 0.66
      }
        return(guide)
      }
    
    a$Guide <- sapply(a$value, n_func)
  
  }else {
    a$Guide <- 0.197
  }
  
  a <- a[, c("site", "date", "Guide")]
  
  b <- t[t$site %nin% a$site | t$date %nin% a$date, ]
  b$Guide <- 0.197
  df_l10_m<- rbind(a, b)
  
      
  df_l10 <- df_l10[,!names(df_l10) %in% c("Guide")]
  df_l10 <- merge(df_l10, df_l10_m, by = c("site", "date"))
        
  df_l10$G1 <- ifelse(as.numeric(df_l10$value) >= as.numeric(df_l10$Guide), 1, 0)
  df_l10$T1 <- ifelse(as.numeric(df_l10$value) >= as.numeric(df_l10$P75), 1, 0)
  df_l10$T2 <- ifelse(as.numeric(df_l10$value) >= as.numeric(df_l10$P90), 1, 0)
  
  if (nrow(df_l10) > 0 ) {
    df_out <- rbind(df_out, df_l10)
  }
  

  df_l11 <-subset(df_l, variable == "uranium")
  t <- unique(df_l11[,c("site", "date")])
  s <- t$site
  d <- t$date
  a <- subset(df, site %in% s & date %in% d & variable == "hardness")
    
  if (prv == "QC") {
  
    qc_un <- function(x){
      hard <- x["value"]
      if (hard <= 100) {
        Guide <-  14
      } else {
        Guide <- 100
      }
      return(Guide)
    }
    
    c <- subset(a, ref %in% grep("BQMA", levels(a$ref), value = TRUE))
    if(nrow(c) > 0) {
      c$Guide <- apply(b, 1, qc_un)
    }
    
    d <- subset(a, ref %in% grep("BQMA", levels(a$ref), value = TRUE, invert = TRUE))
    if (nrow(cd) > 0) {
      d$Guide <- 15
    }
    
    if (nrow(c) > 0 & nrow(d) > 0) {
      a <- rbind(c, d)
    } else if (nrow(c) > 0) {
      a <- c
    } else if (nrow(d) > 0) {
      a <- d
    }
  }else {
    a$Guide <- 0.15
  }
  
  
  a <- a[, c("site", "date", "Guide")]
  
  b <- t[t$site %nin% a$site | t$date %nin% a$date, ]
  b$Guide <- 0.15
  
  df_l11_m <- rbind(a,b) 
  
  df_l11 <- df_l11[,!names(df_l11) %in% c("Guide")]
  df_l11 <- merge(df_l11, df_l11_m, by = c("site", "date"))
    
  df_l11$G1 <- ifelse(as.numeric(df_l11$value) >= as.numeric(df_l11$Guide), 1, 0)
  df_l11$T1 <- ifelse(as.numeric(df_l11$value) >= as.numeric(df_l11$P75), 1, 0)
  df_l11$T2 <- ifelse(as.numeric(df_l11$value) >= as.numeric(df_l11$P90), 1, 0)
  
  if (nrow(df_l11) > 0 ) {
    df_out <- rbind(df_out, df_l11)
  }
  
  df_l12 <-subset(df_l, variable == "arsenic")
  if (prv == "QC") {
    b <- subset(a, ref %in% grep("BQMA", levels(a$ref), value = TRUE))
    if(nrow(b) > 0) {
      b$Guide <- 150
    }
    c <- subset(a, ref %in% grep("BQMA", levels(a$ref), value = TRUE, invert = TRUE))
    if (nrow(c) > 0) {
      c$Guide <- 5
    }

    if (nrow(b) > 0 & nrow(c) > 0) {
      a <- rbind(b, c)
    } else if (nrow(b) > 0) {
      a <- b
    } else if (nrow(c) > 0) {
      a <- c
    }
    
    a <- a[, c("site", "date", "Guide")]
    df_l12 <- df_l12[,!names(df_l12) %in% c("Guide")]
    df_l12 <- merge(df_l12, a, by = c("site", "date"))
    
  } else {
    df_l12$Guide <- 5
  }
  
  df_l12$G1 <- ifelse(as.numeric(df_l12$value) >= as.numeric(df_l12$Guide), 1, 0)
  df_l12$T1 <- ifelse(as.numeric(df_l12$value) >= as.numeric(df_l12$P75), 1, 0)
  df_l12$T2 <- ifelse(as.numeric(df_l12$value) >= as.numeric(df_l12$P90), 1, 0)

  if (nrow(df_l12) > 0 ) {
    df_out <- rbind(df_out, df_l12)
  }

  df_l13 <- subset(df_l, variable %nin% c("dissolved.oxygen", "ph", "aluminium","cadmium", "copper", "lead", "nickel", "zinc", "ammonia", "nitrite", "uranium", "arsenic"))
  df_l13$G1 <- ifelse(as.numeric(df_l13$value) >= as.numeric(df_l13$Guide), 1, 0)
  df_l13$T1 <- ifelse(as.numeric(df_l13$value) >= as.numeric(df_l13$P75), 1, 0)
  df_l13$T2 <- ifelse(as.numeric(df_l13$value) >= as.numeric(df_l13$P90), 1, 0)
  
  if (nrow(df_l13) >0 ) {
    df_out <- rbind(df_out, df_l13)
  }
  
  df_l <- subset(df_out, variable %nin% c("hardness", "temperature"))
  
  df_l$year <- as.numeric(format(df_l$date, "%Y"))
  df_l <- na.omit(df_l)
  
  df_l_file <- paste(Basin, "_", prv, "_WQ_DF_L_out.csv", sep = "")
  write.csv(df_l, df_l_file)
    
  site_df <- unique(df_l[, c("site", "date")])
  
  b <- 1
  while (b <= nrow(site_df)){
    s1 <- as.character(site_df[b,1])
    t1 <- as.Date(site_df[b,2])
    df3 <- subset(df_l, site == s1 & date == t1)
    
    n <- nrow(df3)
    site_df$n[b] <- n
    
    df4 <- subset(df3, G1 == 1)
    g <- nrow(df4)
    site_df$Guide1[b] <- g
    
    df5 <- subset(df3, T1 == 1)
    p <- nrow(df5)
    site_df$Thresh1[b] <- p
    
    df6 <- subset(df3, T2 == 1)
    r <- nrow(df6)
    site_df$Thresh[b] <- r
    b <- b + 1
  }
  
  site_df$date <- as.Date(site_df$date)
  site_df$year <-  as.numeric(format(site_df$date, format = "%Y"))
  site_df$month <-  as.numeric(format(site_df$date, format = "%m"))
  
  site_df$P1 <- site_df$Guide1 /  site_df$n
  site_df$P2 <- site_df$Thresh1 / site_df$n
  site_df$P3 <- site_df$Thresh / site_df$n
  site_df$FS <- ((site_df$P1 * 3) + site_df$P2 + (site_df$P3 * 2)) / 6
  
  for (w in 1:nrow(site_df)){
    site <- as.character(site_df[w,c("site")])
    date <- as.character(site_df[w,c("date")])
    wsc <- as.character(unique(df[df$site == site, c("wscsda")]))
    site_df$wscsda[w] <- wsc
    ref <- as.character(unique(df[df$site == site & df$date == date, c("ref")]))
    site_df$ref[w] <- ref
  }
  
  site_df_file <- paste(Basin, "_", prv, "_WQ_Site_DF_out.csv", sep = "")
  write.csv(site_df, site_df_file)
  
  regfile1 <- paste(Basin, "_", prv,  "_WQ_Exceedance_Regression_", Date, ".csv", sep = "")
  reg_header <- "Source, WSCSDA, Start Year, End Year, Number of Years, Number of Sites, Intercept, Intercept_STE, Intercept-T, Intercept-p, Slope, Slope_STE, Slope-T, Slope-p, Adj.RSqr, F-Stat, F-pvalue, Theil-Sen Slope, Mann-Ken Score, Mann-Ken p-value"
  write(reg_header, regfile1)
  
  if ("0" %in% wscsda){
    site_df <- subset(site_df, wscsda != "0")
  } else if ("" %in% wscsda){
    site_df <- subset(site_df, wscsda != "")
  }
  
  wscsda <-levels(droplevels(factor(site_df$wscsda)))
  site_df$site <- as.factor(site_df$site)
  basin_number <- length(wscsda)
  
  z <- 0
  
  while (z <= basin_number){
    print(paste("z = ", z, sep = ""))
    if (z == 0){
      
      unit <- Basin 
      bname <- as.character(basins[Basin])
      
      years <- levels(factor(site_df$year))
      n_years <- length(years)
      
      start_year <- as.numeric(years[1])
      end_year <- as.numeric(years[n_years])
      
      n_mes <- length(site_df$value)
      
      data_sources <- levels(factor(site_df$ref))
      sources <- length(data_sources)
      
      if (sources >1) {
        n <- length(levels(droplevels(site_df$site)))
      }else {
        n <- length(levels(factor(site_df$site)))
      }
      
      
      if (n_years > 3){
        annual_data <- aggregate(FS ~ year, data = site_df , FUN = "median")
        
        ann_ts <- ts(annual_data, start = start_year, end = end_year, frequency = 1)
        
        reg <- lm(FS~ year, data = ann_ts)
        
        reg_sum <- summary(reg)
        c <- reg_sum$coefficients
        rs <- reg_sum$adj.r.squared
        f <- reg_sum$fstatistic
        
        if(!is.null(f)){
          rp <- unname(pf(f[1],f[2],f[3],lower.tail=F))
          mk <- mannKen(ann_ts[,2])
          mks <- mk$sen.slope
          mkk <- mk$S
          mkp <- mk$p.value
          
          ann_out <- paste("All", unit, start_year, end_year, n_years, n, c[1,1], c[1,2], c[1,3], c[1,4], c[2,1], c[2,2], c[2,3], c[2,4], rs, unname(f[1]), rp, mks, mkk, mkp, sep = ",")
          write(ann_out, regfile1, append = TRUE)
          
        }else{
          print("f test is null)")
          ann_out <- paste("All", unit, start_year, end_year, n_years, n, sep = ",")
          write(ann_out, regfile1, append = TRUE)
        } 
        
      } else {
        print("insufficient number of years of monitoring")
        ann_out <- paste("All", unit, start_year, end_year, n_years, n, sep = ",")
        write(ann_out, regfile1, append = TRUE)
      }
      
      q <- 1
      
      while (q <= sources){
        
        print(paste("q =", q, sep = ""))
        
        sr <- data_sources[q]
        print(paste("source =", sr))
        
        wsc_data <- subset(site_df, ref == sr)
        
        years1 <- levels(factor(wsc_data$year))
        n1_years <- length(years1)
        
        start_year1 <- as.numeric(years1[1])
        end_year1 <- as.numeric(years1[n1_years])
        
        d_sources <- levels(wsc_data$ref)
        n_sources <- length(d_sources)
        
        if (n_sources >1) {
          n1 <- length(levels(droplevels(wsc_data$site)))
        }else {
          n1 <- length(levels(factor(wsc_data$site)))
        }
        
        if (n1_years > 3){
          
          annual_data1 <- aggregate(FS ~ year, data = wsc_data , FUN = "median")
          ann_ts1 <- ts(annual_data1, start = start_year1, end = end_year1, frequency = 1)
          
          reg1 <- lm(FS~ year, data = ann_ts)
          
          reg_sum1 <- summary(reg1)
          c1 <- reg_sum1$coefficients
          rs1 <- reg_sum1$adj.r.squared
          f1 <- reg_sum1$fstatistic
          if(!is.null(f1)){
            rp1 <- unname(pf(f1[1],f1[2],f1[3],lower.tail=F))
            mk1 <- mannKen(ann_ts1[,2])
            mks1 <- mk1$sen.slope
            mkk1 <- mk1$S
            mkp1 <- mk1$p.value
            
            ann_out1 <- paste(sr, unit, start_year1, end_year1, n1_years, n1, c1[1,1], c1[1,2], c1[1,3], c1[1,4], c1[2,1], c1[2,2], c1[2,3], c1[2,4], rs1, unname(f1[1]), rp1, mks1, mkk1, mkp1, sep = ",")
            write(ann_out1, regfile1, append = TRUE)
            
          }else{
            print("f test is null)")
            ann_out1 <- paste(sr, unit, start_year1, end_year1, n1_years, n1, sep = ",")
            write(ann_out1, regfile1, append = TRUE)
          }
        } else {
          print("insufficient number of years of monitoring")
          ann_out1 <- paste(sr, unit, start_year1, end_year1, n1_years, n1, sep = ",")
          write(ann_out1, regfile, append = TRUE)
        }
        q <- q + 1
      }
      z<- z + 1   
      
    } else {
      
      i <- wscsda[z]
      unit <- i
      print(paste("unit =", i, sep = ""))
      
      reg_data <- subset(site_df, wscsda == i)
      
      d1_sources <- levels(factor(reg_data$ref))
      n1_sources <- as.numeric(length(d1_sources))
      
      years2 <- levels(factor(reg_data$year))
      n2_years <- length(years2)
      
      start_year2 <- as.numeric(years2[1])
      end_year2 <- as.numeric(years2[n2_years])
      
      
      if (n1_sources > 1){
        n2 <- length(levels(droplevels(reg_data$site)))
      }else {
        n2 <- length(levels(factor(reg_data$site)))
      }
      
      
      if (n2_years > 3){
        
        annual_data2 <- aggregate(FS ~ year, data = reg_data, FUN = "median")
        
        ann_ts2 <- ts(annual_data2, start = start_year2, end = end_year2, frequency = 1)
        
        reg2 <- lm(FS~ year, data = ann_ts)
        
        reg_sum2 <- summary(reg2)
        c2 <- reg_sum2$coefficients
        rs2 <- reg_sum2$adj.r.squared
        f2 <- reg_sum2$fstatistic
        if(!is.null(f2)){
          rp2 <- unname(pf(f2[1],f2[2],f2[3],lower.tail=F))
          mk2 <- mannKen(ann_ts2[,2])
          mks2 <- mk2$sen.slope
          mkk2 <- mk2$S
          mkp2 <- mk2$p.value
          
          ann_out2 <- paste("All",  unit, start_year2, end_year2, n2_years, n2, c2[1,1], c2[1,2], c2[1,3], c2[1,4], c2[2,1], c2[2,2], c2[2,3], c2[2,4], rs2, unname(f2[1]), rp2, mks2, mkk2, mkp2, sep = ",")
          write(ann_out2, regfile1, append = TRUE)
        }else{
          print("f test is null)")
          ann_out2 <- paste("All",  unit, start_year2, end_year2, n2_years, n2, sep = "'")
          write(ann_out2, regfile1, append = TRUE)
        }
      } else {
        print("insufficient number of years of monitoring")
        ann_out2 <- paste("All",  unit, start_year2, end_year2, n2_years, n2, sep = ",")
        write(ann_out2, regfile1, append = TRUE)
      }
      
      
      q <- 1
      while (q <= n1_sources){
        
        sr1 <- d1_sources[q]
        print(paste("sources 1 =", sr1, sep = ""))
        
        wsc_data_1 <- subset(reg_data , ref == sr1)
        
        years3 <- levels(factor(wsc_data_1$year))
        n3_years <- length(years3)
        
        start_year3 <- as.numeric(years3[1])
        end_year3 <- as.numeric(years3[n3_years])
        
        if (n1_sources > 1){
          n3<- length(levels(droplevels(wsc_data_1$site)))
        }else {
          n3 <- length(levels(factor(wsc_data_1$site)))
        }
        
        
        if (n3_years > 3){
          
          annual_data3 <- aggregate(FS ~ year, data = wsc_data_1 , FUN = "median")
          
          ann_ts3 <- ts(annual_data3, start = start_year3, end = end_year3, frequency = 1)
          
          reg3 <- lm(FS~ year, data = ann_ts)
          
          reg_sum3 <- summary(reg3)
          c3 <- reg_sum3$coefficients
          rs3 <- reg_sum3$adj.r.squared
          f3 <- reg_sum3$fstatistic
          if(!is.null(f3)){
            rp3 <- unname(pf(f3[1],f3[2],f3[3],lower.tail=F))
            mk3 <- mannKen(ann_ts3[,2])
            mks3 <- mk3$sen.slope
            mkk3 <- mk3$S
            mkp3 <- mk3$p.value
            
            ann_out3 <- paste(sr1, unit, start_year3, end_year3, n3_years, n3, c3[1,1], c3[1,2], c3[1,3], c3[1,4], c3[2,1], c3[2,2], c3[2,3], c3[2,4], rs3, unname(f3[1]), rp3, mks3, mkk3, mkp3, sep = ",")
            write(ann_out3, regfile1, append = TRUE)
          }else{
            print("f test is null)")
            ann_out3 <- paste(sr1, unit, start_year3, end_year3, n3_years, n3, sep = "'")
            write(ann_out3, regfile1, append = TRUE)
          } 
        } else {
          print("insufficient number of years of monitoring")
          ann_out3 <- paste(sr1, unit, start_year3, end_year3, n3_years, n3, sep = ",")
          write(ann_out3, regfile1, append = TRUE)
        }
        
        q <- q + 1
      }
      z <- z+ 1
    }
  }
  
  r <- 1
  while (r <= length(chems)){
    print(r)
    chem <- chems[r]
    print(chem)
    
    un <- as.character(units[chem])
    
    if (chem == "ph"){
      thresh2 <- threshdf$P90[threshdf$Chem == chem]
      thresh <- threshdf$P75[threshdf$Chem == chem]
      thresh2a <- as.numeric(strsplit(thresh2, "-")[[1]][1])
      thresh2b <- as.numeric(strsplit(thresh2, "-")[[1]][2])
      thresh1a <- as.numeric(strsplit(thresh, "-")[[1]][1])
      thresh1b <- as.numeric(strsplit(thresh, "-")[[1]][2])
    }else if (chem == "aluminium"){
      
      thresh2 <- as.numeric(threshdf$P90[threshdf$Chem == chem])
      thresh <- as.numeric(threshdf$P75[threshdf$Chem == chem])
      
      
    }  else {
      
      thresh2 <- as.numeric(threshdf$P90[threshdf$Chem == chem])
      thresh <- as.numeric(threshdf$P75[threshdf$Chem == chem])
    }
    
    df2 <- subset(df, variable == chem)
    
    df2$site <- as.factor(df2$site)
    
    df2$value <- as.numeric(df2$value)
    df2 <- na.omit(df2)
    wscsda <-levels(droplevels(factor(df2$wscsda)))
    wscsda <- sort(wscsda)
    
    df2$wscsda <- factor(df2$wscsda, levels = wscsda)
    
    basin_number <- length(wscsda)
    
    x <- 0
    
    while (x <= basin_number){
      print(paste("x = ", x, sep = ""))
      if (x == 0){
        
        unit <- Basin 
        bname <- as.character(basins[Basin])
        
        years <- levels(factor(df2$year))
        n_years <- length(years)
        
        start_year <- as.numeric(years[1])
        end_year <- as.numeric(years[n_years])
        
        n_mes <- length(df2$value)
        
        data_sources <- levels(df2$ref)
        sources <- length(data_sources)
        
        
        n <- length(levels(factor(df2$site)))
        
        line1 <- paste(bname, "\n", "Indicator = ", capitalize(chem), sep ="")
        regplot <- paste(Basin, "_", prv, "_", capitalize(chem), "_Threshold_Plot_", Date, ".png", sep = "")
        
        y_lab <- paste(capitalize(chem), un, sep = " ")
        
        if (chem == "ph"){
          t <- ggplot(data = df2, aes(factor(year), value)) + geom_boxplot(aes(fill = factor(ref)), outlier.size=1)  + facet_wrap(~wscsda)  + scale_fill_discrete(name="Sources")
          s <- t + xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 4)) 
          v <- s + geom_hline(aes(yintercept = thresh2a, linetype = "90th Percentile - Lower"), show_guide = TRUE) + geom_hline(aes(yintercept = thresh1a, linetype = "75th Percentile - Lower"), show_guide = TRUE)  + geom_hline(aes(yintercept = thresh2b, linetype = "90th Percentile - Upper"), show_guide = TRUE) + geom_hline(aes(yintercept = thresh1b, linetype = "75th Percentile - Upper"), show_guide = TRUE) + geom_hline(aes(yintercept = guide1, linetype = "Guideline Limit - Lower"), show_guide = TRUE) + geom_hline(aes(yintercept = guide2, linetype = "Guideline Limit - Upper"), show_guide = TRUE)
        
        } else if (chem == "dissolved.oxygen") {
          t <- ggplot(data = df2, aes(factor(year), value)) + geom_boxplot(aes(fill = factor(ref)), outlier.size=1)  + facet_wrap(~wscsda) + scale_y_log10() + scale_fill_discrete(name="Sources")
          s <- t + xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 4))  
          v <- s + geom_hline(aes(yintercept = thresh2, linetype = "90th Percentile - Lower"), show_guide = TRUE) + geom_hline(aes(yintercept = thresh, linetype = "75th Percentile - Lower"), show_guide = TRUE) + geom_hline(aes(yintercept = guide, linetype = "Guideline Limit"), show_guide = TRUE)
        
        } else if (chem == "aluminium") {
          t <- ggplot(data = df2, aes(factor(year), value)) + geom_boxplot(aes(fill = factor(ref)), outlier.size=1) + scale_y_log10()  + facet_wrap(~wscsda)  + scale_fill_discrete(name="Sources")
          s <- t + xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 4)) 
          v <- s  + geom_hline(aes(yintercept = thresh2, linetype = "90th Percentile"), show_guide = TRUE) + geom_hline(aes(yintercept = thresh, linetype = "75th Percentile"), show_guide = TRUE) + geom_hline(aes(yintercept = guide, linetype = "Guideline Limit"), show_guide = TRUE)
          
        }else{
          t <- ggplot(data = df2, aes(factor(year), value)) + geom_boxplot(aes(fill = factor(ref)), outlier.size=1)  + facet_wrap(~wscsda) + scale_y_log10() + scale_fill_discrete(name="Sources")
          s <- t + xlab("Year") + ylab(y_lab) + theme(axis.text.x = element_text(angle = 90, size = 4))  
          v <- s + geom_hline(aes(yintercept = thresh2, linetype = "90th Percentile"), show_guide = TRUE) + geom_hline(aes(yintercept = thresh, linetype = "75th Percentile"), show_guide = TRUE) + geom_hline(aes(yintercept = guide, linetype = "Guideline Limit"), show_guide = TRUE)
        }
        
        u <- v + scale_linetype_discrete(name = "Thresholds")  + xlab("Year") + ylab(y_lab) 
        q <- u + theme(plot.margin = unit(c(0.5,0.5,2,0), "cm"))+ ggtitle(line1) + theme(plot.title = element_text(size = 8)) 
        
        ggsave(filename= regplot, plot = q, dpi= 128, width = 8, height = 6)
        
        if (n_years > 3){
          annual_data <- aggregate(value ~ year, data = df2 , FUN = "median")
          
          ann_ts <- ts(annual_data, start = start_year, end = end_year, frequency = 1)
          
          reg <- lm(value~ year, data = ann_ts)
          
          reg_sum <- summary(reg)
          c <- reg_sum$coefficients
          rs <- reg_sum$adj.r.squared
          f <- reg_sum$fstatistic
          
          if(!is.null(f)){
            rp <- unname(pf(f[1],f[2],f[3],lower.tail=F))
            mk <- mannKen(ann_ts[,2])
            mks <- mk$sen.slope
            mkk <- mk$S
            mkp <- mk$p.value
            
            ann_out <- paste("All", capitalize(chem), unit, start_year, end_year, n_years, n, c[1,1], c[1,2], c[1,3], c[1,4], c[2,1], c[2,2], c[2,3], c[2,4], rs, unname(f[1]), rp, mks, mkk, mkp, sep = ",")
            write(ann_out, regfile, append = TRUE)
            
          }else{
            print("f test is null)")
            ann_out <- paste("All", capitalize(chem), unit, start_year, end_year, n_years, n, sep = ",")
            write(ann_out, regfile, append = TRUE)
          } 
          
        } else {
          print("insufficient number of years of monitoring")
          ann_out <- paste("All", capitalize(chem), unit, start_year, end_year, n_years, n, sep = ",")
          write(ann_out, regfile, append = TRUE)
        }
        
        
        y <- 1
        
        while (y <= sources){
          
          print(paste("y =", y, sep = ""))
          
          sr <- data_sources[y]
          print(paste("source =", sr))
          
          wsc_data <- subset(df2, ref == sr)
          
          years1 <- levels(factor(wsc_data$year))
          n1_years <- length(years1)
          
          start_year1 <- as.numeric(years1[1])
          end_year1 <- as.numeric(years1[n1_years])
          
          d_sources <- levels(wsc_data$ref)
          n_sources <- length(d_sources)
          
          if (y ==1){
            n1 <- length(levels(factor(wsc_data$site)))
          }else {
            n1 <- length(levels(droplevels(wsc_data$site)))
          }
          
          if (n1_years > 3){
            
            annual_data1 <- aggregate(value ~ year, data = wsc_data , FUN = "median")
            ann_ts1 <- ts(annual_data1, start = start_year1, end = end_year1, frequency = 1)
            
            reg1 <- lm(value~ year, data = ann_ts)
            
            reg_sum1 <- summary(reg1)
            c1 <- reg_sum1$coefficients
            rs1 <- reg_sum1$adj.r.squared
            f1 <- reg_sum1$fstatistic
            if(!is.null(f1)){
              rp1 <- unname(pf(f1[1],f1[2],f1[3],lower.tail=F))
              mk1 <- mannKen(ann_ts1[,2])
              mks1 <- mk1$sen.slope
              mkk1 <- mk1$S
              mkp1 <- mk1$p.value
              
              ann_out1 <- paste(sr, capitalize(chem), unit, start_year1, end_year1, n1_years, n1, c1[1,1], c1[1,2], c1[1,3], c1[1,4], c1[2,1], c1[2,2], c1[2,3], c1[2,4], rs1, unname(f1[1]), rp1, mks1, mkk1, mkp1, sep = ",")
              write(ann_out1, regfile, append = TRUE)
              
            }else{
              print("f test is null)")
              ann_out1 <- paste(sr, capitalize(chem), unit, start_year1, end_year1, n1_years, n1, sep = ",")
              write(ann_out1, regfile, append = TRUE)
            }
          } else {
            print("insufficient number of years of monitoring")
            ann_out1 <- paste(sr, capitalize(chem), unit, start_year1, end_year1, n1_years, n1, sep = ",")
            write(ann_out1, regfile, append = TRUE)
          }
          
          print(ann_out1)
          
          
          dfyear1 <- subset(wsc_data, year == end_year1)
          
          n_mes1 <- length(dfyear1$value)
          m1 <- length(levels(factor(dfyear1$site)))
          
          wsc_thresh1 <- subset(dfyear1, value >= thresh)
          wsc_t_mes1 <- length(wsc_thresh1$value)
          
          wsc_thresh2 <- subset(dfyear1, value >= thresh2)
          wsc_t_mes2 <- length(wsc_thresh2$value)
          
          wsc_guide <- subset(dfyear1, value >= guide)
          wsc_t_guide <- length(wsc_guide$value)
          
          ad_out1 <- paste(sr, capitalize(chem), unit, start_year1, end_year1, n1_years, m1, n_mes1, guide, wsc_t_guide, thresh2, wsc_t_mes2, thresh, wsc_t_mes1, sep = ",")
          write(ad_out1, ad_file, append = TRUE)
          print(ad_out1)
          
          y <- y + 1
        }
        
        x <- x + 1    
        
      } else {
        
        i <- wscsda[x]
        unit <- i
        print(paste("unit =", i, sep = ""))
        
        reg_data <- subset(df2, wscsda == i)
        
        d1_sources <- levels(reg_data$ref)
        n1_sources <- as.numeric(length(d1_sources))
        
        years2 <- levels(factor(reg_data$year))
        n2_years <- length(years2)
        
        start_year2 <- as.numeric(years2[1])
        end_year2 <- as.numeric(years2[n2_years])
        
        
        if (n1_sources > 1){
          n2 <- length(levels(droplevels(reg_data$site)))
        }else {
          n2 <- length(levels(factor(reg_data$site)))
        }
        
        
        if (n2_years > 3){
          
          annual_data2 <- aggregate(value ~ year, data = reg_data, FUN = "median")
          
          ann_ts2 <- ts(annual_data2, start = start_year2, end = end_year2, frequency = 1)
          
          reg2 <- lm(value~ year, data = ann_ts)
          
          reg_sum2 <- summary(reg2)
          c2 <- reg_sum2$coefficients
          rs2 <- reg_sum2$adj.r.squared
          f2 <- reg_sum2$fstatistic
          if(!is.null(f2)){
            rp2 <- unname(pf(f2[1],f2[2],f2[3],lower.tail=F))
            mk2 <- mannKen(ann_ts2[,2])
            mks2 <- mk2$sen.slope
            mkk2 <- mk2$S
            mkp2 <- mk2$p.value
            
            ann_out2 <- paste("All", capitalize(chem), unit, start_year2, end_year2, n2_years, n2, c2[1,1], c2[1,2], c2[1,3], c2[1,4], c2[2,1], c2[2,2], c2[2,3], c2[2,4], rs2, unname(f2[1]), rp2, mks2, mkk2, mkp2, sep = ",")
            write(ann_out2, regfile, append = TRUE)
          }else{
            print("f test is null)")
            ann_out2 <- paste("All", capitalize(chem), unit, start_year2, end_year2, n2_years, n2, sep = "'")
            write(ann_out2, regfile, append = TRUE)
          }
        } else {
          print("insufficient number of years of monitoring")
          ann_out2 <- paste("All", chem, unit, start_year2, end_year2, n2_years, n2, sep = ",")
          write(ann_out2, regfile, append = TRUE)
        }
        
        
        y <- 1
        while (y <= n1_sources){
          
          sr1 <- d_sources[y]
          print(paste("sources 1 =", sr1, sep = ""))
          
          wsc_data_1 <- subset(reg_data , ref == sr1)
          
          years3 <- levels(factor(wsc_data_1$year))
          n3_years <- length(years3)
          
          start_year3 <- as.numeric(years3[1])
          end_year3 <- as.numeric(years3[n3_years])
          
          if (n1_sources > 1){
            n3<- length(levels(droplevels(wsc_data_1$site)))
          }else {
            n3 <- length(levels(factor(wsc_data_1$site)))
          }
          
          
          if (n3_years > 3){
            
            annual_data3 <- aggregate(value ~ year, data = wsc_data_1 , FUN = "median")
            
            ann_ts3 <- ts(annual_data3, start = start_year3, end = end_year3, frequency = 1)
            
            reg3 <- lm(value~ year, data = ann_ts)
            
            reg_sum3 <- summary(reg3)
            c3 <- reg_sum3$coefficients
            rs3 <- reg_sum3$adj.r.squared
            f3 <- reg_sum3$fstatistic
            if(!is.null(f3)){
              rp3 <- unname(pf(f3[1],f3[2],f3[3],lower.tail=F))
              mk3 <- mannKen(ann_ts3[,2])
              mks3 <- mk3$sen.slope
              mkk3 <- mk3$S
              mkp3 <- mk3$p.value
              
              ann_out3 <- paste(sr1, capitalize(chem), unit, start_year3, end_year3, n3_years, n3, c3[1,1], c3[1,2], c3[1,3], c3[1,4], c3[2,1], c3[2,2], c3[2,3], c3[2,4], rs3, unname(f3[1]), rp3, mks3, mkk3, mkp3, sep = ",")
              write(ann_out3, regfile, append = TRUE)
            }else{
              print("f test is null)")
              ann_out3 <- paste(sr1, capitalize(chem), unit, start_year3, end_year3, n3_years, n3, sep = "'")
              write(ann_out3, regfile, append = TRUE)
            } 
          } else {
            print("insufficient number of years of monitoring")
            ann_out3 <- paste(sr1, capitalize(chem), unit, start_year3, end_year3, n3_years, n3, sep = ",")
            write(ann_out3, regfile, append = TRUE)
          }
          print(ann_out3)
          
          wsc_year_1 <- subset(wsc_data_1, year == end_year3)
          n_mes3 <- length(wsc_year_1$value)
          m3 <- length(levels(factor(wsc_year_1$site)))
          
          wsc1_thresh1 <- subset(wsc_year_1, value >= thresh)
          wsc1_t_mes1 <- length(wsc1_thresh1$value)
          
          wsc1_thresh2 <- subset(wsc_year_1, value >= thresh2)
          wsc1_t_mes2 <- length(wsc1_thresh2$value)
          
          wsc1_guide <- subset(wsc_year_1, value >= guide)
          wsc1_t_guide <- length(wsc1_guide$value)
          
          ad_out3 <- paste(sr1, capitalize(chem), unit,start_year3, end_year3, n3_years, m3, n_mes3, guide, wsc1_t_guide, thresh2, wsc1_t_mes2, thresh, wsc1_t_mes1, sep = ",")
          write(ad_out3, ad_file, append = TRUE)
          print(ad_out3)
          y <- y + 1
        }
        x <- x + 1
      }
    }
  r <- r + 1
}

}





    
   