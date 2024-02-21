# The following code is an example of reading the pipe-delimited Survey of Income and Program Participation (SIPP) 
# 	data into an R dataframe in preparation for analysis. Specifically, this code loads in both the primary data file 
#   and the calendar-year replicate weights file (as opposed to the longitudinal replicate weights). These files are 
#   separate downloads on the SIPP website.
# SIPP data are in person-month format, meaning each record represents one month for a specific person.
#   Unique persons are identified using SSUID+PNUM. Unique households are identified using SSUID+ERESIDENCEID. For 
#   additional guidance on using SIPP data, please see the SIPP Users' Guide at <https://www.census.gov/programs-surveys/sipp/guidance/users-guide.html>
# This code was written in R 4.1.0, and requires the "data.table", "dplyr", and "bit64" packages. 
# Note the 'select' statement in the first use of fread(). Most machines do not have enough memory to read
# 	the entire SIPP file into memory. Use a 'select' statement to read in only the columns you are interested in using. 
#   If you still encounter an out-of-memory error, you must select less columns or less observations.
# Run this code from the same directory as the extracted data.
# Please contact the SIPP Coordination and Outreach Staff at census.sipp@census.gov if you have any questions.

# set your working directory, if necessary
# setwd("")

#Load the "data.table", "dplyr", and "bit64" libraries
require("data.table")
require("bit64")
require("dplyr")
require("mase")
require("survey")
require("readxl")
require("matrixStats")

setwd("data")

#Read in the Primary Data files. 
for(year in as.character(2022)){#2020:2022)){
#for(year in as.character(2020:2022)){ #if we want to combine multiple years
    varname <- paste0("pu", year)
    filename <- paste0(varname,".csv")
    
    assign(varname, 
           fread(filename, sep = "|", select = c(
  
                  #Common case identification variables
                  'SSUID','PNUM','MONTHCODE','ERESIDENCEID','ERELRPE','SPANEL','SWAVE',
  
                  #The base weight
                  'WPFINWGT',
  
                  #Common demographics variables, including age at time of interview (TAGE)
                  #and monthly age during the reference period (TAGE_EHC)
                  'ESEX','TAGE','TAGE_EHC','ERACE','EORIGIN','EEDUC',
  
                  #Example additional variables for analysis
                  'TPTOTINC',
                  'TLIVQTR', 
                  'TPRVLVQRT',
                  'TEHC_ST', #monthly state of residence 
                  'TST_INTV', #state of residence for the interview address
                  'THNETWORTH',  #household-level net worth
                  'THEQ_HOME',  #equity in own home
                  'TOCHKVAL' #value of individually-owned checking accounts
                 )
            )
        )
}
pu <- pu2022

#process for annual household estimates
pu <-  pu %>%
          filter(ERELRPE %in% c(1,2)) %>% #get householders only so that analysis is at household level
          filter(MONTHCODE==12) %>%  #take only last month of year in order to make annual estimate
          filter(TLIVQTR != 3) %>%   #exclude group quarters 

#dictionary variable for mapping TEHC_ST variable to state names
state_dict <- cbind(TEHC_ST=(1:56)[-c(3,7,14,43,52)], NAME=sort(c(state.name, "District of Columbia")))
state_dict <- rbind(state_dict, c(60, "Puerto Rico and "), c(61, "Foreign Country"))

#Read in the replicate-weight data. 
dw <- c("rw2022.csv")
rw <- fread(dw, sep = "|")
names(rw) <- toupper(names(rw))

#Merge primary data and replicate weights on SSUID, PNUM, MONTHCODE, SPANEL, and SWAVE
sipp.df <- inner_join(pu, rw, by = c("SSUID","PNUM","MONTHCODE", "SPANEL", "SWAVE"))

#How the SIPP user guide suggests making the calculation
sipp.svy = svrepdesign( data = sipp.df,
                        weights = ~WPFINWGT,
                        repweights = "REPWGT[1-9]+",
                        type = "Fay", 
                        rho = 0.5)

svyquantile(x=~THNETWORTH , design=sipp.svy, quantiles=0.5 , na.rm = TRUE )

#We can instead do the following with dplyr to get state-by-state results more quickly
#household_var <- "THNETWORTH"
household_var <- "THEQ_HOME"

#Read in Excel file of official tables to compare net worth to
truth <- read_excel("State_Wealth_tables_dy2021.xlsx")
truth <- truth[-c(1:5), ] #drop headers 
truth <- truth[,c(1,7)] #pick column that corresponds to current variable
colnames(truth) <- c("State", "Reported")


#try unweighted and weighted medians
state_medians <- pu %>%
                  group_by(TEHC_ST) %>% #group by state (but need to check whether TST_INTV more appropriate here)
                  summarize(wgt_median=weightedMedian(get(household_var), WPFINWGT))
#map to state names
state_medians$TEHC_ST <- state_dict[which(state_dict[, 1] %in% as.character(state_medians$TEHC_ST)) , 2]

#alternative approach for calculating median using ECDF from Lohr (2019) Ch. 7
#gives better agreement with official tables, but doesn't give uncertainty
state_medians_ecdf <- pu %>%
                        group_by(TEHC_ST, !!sym(household_var)) %>% 
                        mutate(numer=sum(WPFINWGT)) %>%
                        group_by(TEHC_ST) %>%
                        mutate(ECDF=numer/sum(WPFINWGT)) %>%
                        arrange(TEHC_ST, -!!sym(household_var)) %>%
                        select(!!sym(household_var), TEHC_ST, ECDF, everything()) %>%
                        mutate(ECDF=cumsum(ECDF)) 

#get the median for each state
median.df <- tibble()
for(i in 1:nrow(state_dict)){
  state_id <- as.numeric(state_dict[i,1])
  state_name <- state_dict[i,2]
  val <-as.numeric( filter(tst,TEHC_ST==state_id)[which.min((filter(tst, TEHC_ST==state_id))$tmp), household_var])
  median.df <- rbind(median.df, c(state_name, val))
}
names(median.df) <- c("State", "ECDF_median")

median.df <- right_join(truth, median_tst, by=c("State"="TEHC_ST"))  %>%
                  left_join(median.df, by=c("State"="State"))
median.df

