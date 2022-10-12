pacman::p_load(shiny,lubridate, stringi, jpeg, timeperiodsR, 
               tidyverse, data.table, here, magrittr, 
               ggh4x, kableExtra, usmap, ggridges)

####
#analyticsPath = "//vac21appanr200.va.gov/Repository/Analytics/"
analyticsPath = paste0("C:/Users/", Sys.getenv("username"),
                       "/Department of Veterans Affairs/Operations Triage Group (OTG) - Analytics - Documents")
dataPath = "//vac21appanr200.va.gov/Repository/Analytics/DataRepo/SP Data Extracts/"

d = fread(paste0(dataPath, 'Incident (New).csv'), stringsAsFactors = TRUE,strip.white=TRUE)
d_new = fread(paste0(dataPath, 'Incident (Renew).csv'), stringsAsFactors = TRUE,strip.white=TRUE)
# s = fread(paste0(analyticsPath,'/Publication/incident.csv'), stringsAsFactors = TRUE,strip.white=TRUE)
s = fread(paste0(dataPath,'incidentOTG.csv'), stringsAsFactors = TRUE,strip.white=TRUE)

# rename X.U.FEFF. columns
colnames(d) = str_replace_all(colnames(d), 'X.U.FEFF.', '')
colnames(d_new) = str_replace_all(colnames(d_new), 'X.U.FEFF.', '')
colnames(s) = str_replace_all(colnames(s), 'X.U.FEFF.', '')

make_nice_cols <- function(x){
  x = str_replace_all(x, ' {2,}', ' ')
  x = str_replace_all(x, ' ', '_')
  str_replace_all(x, '[\\)\\(\\.\\/\\?\\,]', '')
}

old_text_fields<-c("Facilities Affected TEXT",
                   "Other Facilities Affected TEXT",
                   "Primary Culpable System TEXT",
                   "Primary System/Application Affected TEXT",
                   "Additional System(s)/Application(s) Affected TEXT")

new_text_fields<-c("Facilities Affected",
                   "Other Facilities Affected",
                   "Primary Culpable System",
                   "Primary System/Application Affected",
                   "Additional System(s)/Application(s) Affected")

d_new %<>% rename(`Incident Ticket`=`Incident Number`,
                  `How Issue Was Reported`=`How Issue was Reported`,
                  `HPI/CPI Request Date/Time (ET)`=`HPI/CPI Request Date/Time`,
                  `Incident Resolved by`=`Incident Resolved By`,
                  `Issue Start Date/Time is estimated`=`Issue Start Date/Time is Estimated`
                  ) %>%
  mutate(across(all_of(c(old_text_fields,new_text_fields)), ~replace(., .=="",NA))) %>%
  mutate(`Primary Culpable System`=coalesce(`Primary Culpable System`,`Primary Culpable System TEXT`),
         `Facilities Affected`=coalesce(`Facilities Affected`,`Facilities Affected TEXT`),
         `Other Facilities Affected`=coalesce(`Other Facilities Affected`,`Other Facilities Affected TEXT`),
         `Primary System/Application Affected`=coalesce(`Primary System/Application Affected`,`Primary System/Application Affected TEXT`),
         `Additional System(s)/Application(s) Affected`=coalesce(`Additional System(s)/Application(s) Affected`,`Additional System(s)/Application(s) Affected TEXT`)) %>%
  mutate(across(all_of(new_text_fields), ~replace(., is.na(.),"")))

d %<>% mutate(`Mitigation Action`=NA, `Mitigation Action Category`=NA) %>%
  bind_rows(d_new) %>% 
  mutate(`Incident Impact`=as.character(`Incident Impact`),
         `Incident Urgency`=as.character(`Incident Urgency`),
         `Incident Impact`= recode(`Incident Impact`,
                                  "High - Impacts Region"="2 - High - Impacts Regional",
                                  "Critical - Impacts Nation"="1 - Critical - Impacts National"),
         `Incident Priority`=recode(`Incident Priority`,
                                  "2. High"="2 - High",
                                  "1. Critical"="1 - Critical"),
         `No Call Indicator`=as.character(`No Call Indicator`),
         `SolarWinds Alert`=as.character(`SolarWinds Alert`),
         `SolarWinds Alert`=ifelse(is.na(`SolarWinds Alert`), 'No', as.character(`SolarWinds Alert`)),
         `No Call Indicator`=ifelse(is.na(`No Call Indicator`), 'No', as.character(`No Call Indicator`))
         
         ) %>%
  group_by(`Incident Ticket`) %>% 
  mutate(r_id=row_number()) %>% 
  filter(r_id==max(r_id)) %>%
  select(-r_id) %>%
  rename_all(make_nice_cols)

#d %<>% rename_all(make_nice_cols)
s %<>% rename_all(make_nice_cols)

#Identify date cols
dateCols = grep('Date|Incident_Promoted_to_Major_Incident', colnames(d), value =T)

startOfCurrentMonth = floor_date(Sys.Date(), 'month')
startOfLastMonth = floor_date(Sys.Date(), 'month') - months(1)
startOfNextMonth = ceiling_date(Sys.Date(), unit='month')
startOf12Ago = floor_date(Sys.Date(), 'month') - months(12)

prevQStart = previous_quarter()$start
prevQEnd = previous_quarter()$end

s_all = s %>%
  mutate_at(vars(opened_at, resolved_at, closed_at, promoted_on), list(~lubridate::as_datetime(as.character(.), format = "%m/%d/%Y %H:%M", tz="EST"))) %>% 
  mutate(automation = (grepl('integration', caller_id, ignore.case = TRUE) &
                         (grepl('soi|appdynamics|splunk|dynatrace|liveaction|solarwinds', caller_id, ignore.case = TRUE)))) %>% 
  mutate_at(vars(opened_at, resolved_at, closed_at, promoted_on), 
            list(~lubridate::as_datetime(as.character(.), format = "%Y-%m-%d %H:%M:%S", tz="EST"))) %>%
  mutate(prom_datetime = ifelse(is.na(promoted_on), 
                                opened_at %>% as.character() %>% as_datetime(), promoted_on) %>% as_datetime(tz="EST"))

d_all = d %>%
  mutate(nChar = nchar(as.character(Incident_Ticket))) %>% 
  rowwise() %>% 
  mutate(number = ifelse(nChar == 10, paste0("INC0", str_split(as.character(Incident_Ticket), "INC")[[1]][2]),
                         as.character(Incident_Ticket))) %>% 
  ungroup() %>% 
  mutate(Incident_Pillar = fct_relabel(Incident_Pillar, ~gsub(",.*", "", .x))) %>% #drop the char. after comma for levels in pillars
  mutate_at(vars(all_of(dateCols)), 
            list(~lubridate::as_datetime(as.character(.), format = "%m/%d/%Y %H:%M", tz="EST"))) %>%
  mutate(
    ttm = (Incident_Close_DateTime_ET - Issue_Start_DateTime_ET), # total user impact
    ttd = (First_Reported_to_ESD_DateTime_ET - Issue_Start_DateTime_ET), # detection
    ttrs2 = (Incident_Close_DateTime_ET - First_Reported_to_ESD_DateTime_ET), # incident duration from esd to close
    ttta = (HPICPI_Request_DateTime_ET - First_Reported_to_ESD_DateTime_ET), # escalation to triage
    ttir = (DateTime_ET_Resolution_Identified - HPICPI_Request_DateTime_ET), # triage
    ttir_close = (Incident_Close_DateTime_ET - HPICPI_Request_DateTime_ET), # triage
    tte = (DateTime_ET_Incident_First_Escalated_to_P1P2 - First_Reported_to_ESD_DateTime_ET), # prioritization
    ttp = (Incident_Promoted_to_Major_Incident - DateTime_ET_Incident_First_Escalated_to_P1P2), # promotion
    ttn = (HPICPI_Request_DateTime_ET - Incident_Promoted_to_Major_Incident), # notification
    ttr = (DateTime_ET_Resolution_Identified - Issue_Start_DateTime_ET), # time to resolution
    ttrs = (DateTime_ET_Resolution_Identified - First_Reported_to_ESD_DateTime_ET) # time to resolution from first reported to esd
    #ttre = (DRI_Engaged_DateTime_ET - HPICPI_Request_DateTime_ET), # engagement
    #ttf = (Incident_Close_DateTime_ET - DRI_Engaged_DateTime_ET), # resolution
  ) %>% 
  mutate_at(vars(starts_with('tt')), 
            ~lubridate::as.period(.) %>% as.numeric() %>% (function(x) x/3600)) %>% 
  rename(date_time = Issue_Start_DateTime_ET) %>% 
  mutate(date = lubridate::as_date(date_time), 
         hpicpi_date = as_date(HPICPI_Request_DateTime_ET),
         month_n = substr(date, 1, 7) %>% factor %>% as.numeric(), 
         month = lubridate::month(date), 
         month_name = month.abb[month],
         year = lubridate::year(date), 
         yearMonth = paste0(year, '-', ifelse(nchar(month)==2, month, paste0('0',month))),
         month_name_year = paste0(month_name, "-", year), 
         weekday = lubridate::wday(HPICPI_Request_DateTime_ET, label = TRUE),
         weekend = ifelse(weekday %in% c("Sat", "Sun"), 1, 0)) %>% 
  left_join(s_all %>% select(number, incident_state, prom_datetime, problem_idu_problem_caused_by_change, automation),
            by = 'number') %>% 
  mutate(prom_date = lubridate::as_date(prom_datetime), 
         prom_month_n = substr(prom_date, 1, 7) %>% factor %>% as.numeric(), 
         prom_month = lubridate::month(prom_date), 
         prom_month_name = month.abb[prom_month],
         prom_year = lubridate::year(prom_date), 
         prom_yearMonth = paste0(prom_year, '-', ifelse(nchar(prom_month)==2, prom_month, paste0('0',prom_month))),
         prom_month_name_year = paste0(prom_month_name, "-", prom_year)) 

weekTable = data.frame(date = seq.Date("2019-07-14" %>% as_date(),
                                       # d %>% pull(s_date) %>% first(), 
                                       Sys.Date(), by = 'day')) %>% 
  mutate(weekday = lubridate::wday(date, label = TRUE)) %>% 
  arrange(desc(date))

sunTable = weekTable %>% filter(weekday == "Sun") %>%  
  mutate(x = 1, weekNum = cumsum(x) - 1, 
         weekName = paste0(date, " to ", date + lubridate::days(6)))

weekTable %<>% left_join(sunTable %>% select(-x), by = c("date", "weekday")) %>% 
  arrange(desc(date)) %>% 
  fill(weekNum, weekName, .direction = "up")

d_all %<>% left_join(weekTable %>% select(date, weekNum, weekName), by = c('prom_date' = 'date')) %>% 
  mutate(Primary_Culpable_System = as.character(Primary_Culpable_System),
         Primary_SystemApplication_Affected = as.character(Primary_SystemApplication_Affected),
         Primary_Culpable_System = ifelse(Primary_Culpable_System == "Telecommunications Services", "Telecom Services", Primary_Culpable_System),
         Primary_SystemApplication_Affected = ifelse(Primary_SystemApplication_Affected == "Telecommunications Services", "Telecom Services", Primary_SystemApplication_Affected),
         curWeek = ifelse(weekNum == 0, 1, 0))

d_all$holiday = map_dbl(d_all$hpicpi_date, 
                        function(x) ifelse(!is.na(x), tis::isHoliday(x, goodFriday = TRUE, inaug = TRUE, board = TRUE) %>% as.numeric, 0))

d_all %<>% mutate(
  time = gsub(".* ", "", date_time), 
  hour = lubridate::hour(HPICPI_Request_DateTime_ET),
  fiscal_year = case_when(date >= '2021-10-01' & date < '2022-10-01' ~ '2022',
                          date >= '2020-10-01' & date < '2021-10-01' ~ '2021', 
                          date >= '2019-10-01' & date < '2020-10-01' ~ '2020', 
                          TRUE ~ '2019'), 
  WHEN = ifelse(weekend == 1 | holiday == 1 | hour >= 20 | hour <= 8, 1, 0),
  vclFlag = as.factor(ifelse(
    (Primary_Culpable_System == 'VCL') | (grepl("VCL",Primary_SystemApplication_Affected)) |
      (grepl("VCL",Additional_SystemsApplications_Affected)) | (grepl("VCL",Incident_Short_Description)) |
      (grepl("Veterans Crisis Line",Incident_Short_Description)), 1, 0))) %>%
  filter(SolarWinds_Alert == 'No' | No_Call_Indicator == 'No') %>%
  mutate(Incident_Priority = factor(Incident_Priority)) %>% 
  mutate(nChar = nchar(as.character(Incident_Ticket))) %>% 
  rowwise() %>% 
  mutate(number = ifelse(nChar == 10, paste0("INC0", str_split(as.character(Incident_Ticket), "INC")[[1]][2]),
                         as.character(Incident_Ticket))) %>% 
  ungroup()

ttCols = colnames(d_all)[grepl('^tt', colnames(d_all))] 
#d_all[d_all$Primary_Culpable_System == "Power Outage (Electrical)","Primary_Culpable_System"] = "Power (UPS)"
d_all[d_all$Primary_Culpable_System == "Telecom Services","Primary_Culpable_System"] = "Voice"
d_all[d_all$Primary_Culpable_System == "Network Services","Primary_Culpable_System"] = "Data"
d_all[d_all$Primary_Culpable_System == "Network Services (Wireless)","Primary_Culpable_System"] = "Data (Wireless)"

d_all[d_all$Primary_SystemApplication_Affected == "Telecom Services","Primary_SystemApplication_Affected"] = "Voice"
d_all[d_all$Primary_SystemApplication_Affected == "Network Services","Primary_SystemApplication_Affected"] = "Data"
d_all[d_all$Primary_SystemApplication_Affected == "Network Services (Wireless)","Primary_SystemApplication_Affected"] = "Data (Wireless)"


######altering data

d_all %<>% 
  mutate(Primary_Culpable_System = as.character(Primary_Culpable_System), 
         Primary_Culpable_System = ifelse(Incident_Ticket == 'INC21305388' & Primary_Culpable_System == 'JHIE', 'Cerner database', Primary_Culpable_System), 
         Primary_Culpable_System = ifelse(Incident_Ticket == 'INC21332802' & Primary_Culpable_System == 'EHRM', 'DHA/DOD database', Primary_Culpable_System),
         Primary_Culpable_System = ifelse(Incident_Ticket == 'INC21445459' & Primary_Culpable_System == 'JLV', 'DHA/DOD database', Primary_Culpable_System))

###
#add new column to show incidents as chronological number

d_all<- d_all %>% mutate(inc_number =as.numeric(gsub("[^0-9]", "", Incident_Ticket)))






#we run a loop through d_all to correct all the isncident with invalid date_time field

#psuedo_date_time gets a valid date_time by getting the date_time from the chronologically next incident


d_all$pseudo_date_time <- 0
for( i in 1:nrow(d_all)){
  if(is.na(d_all$date_time[i])){
    print(paste0("one",i))
    
    tryCatch({
      myNum <-d_all$inc_number[i]
      gd<- d_all %>%  filter(inc_number>myNum) %>%
        filter(!(is.na(date_time))) %>%
        filter(inc_number==min(inc_number)) %>%
        select(inc_number,date_time)
      
      
      print(gd)
      d_all$pseudo_date_time[i] <- as.character( gd$date_time)
    },
    error = function(e) {
      #what should be done in case of exeption?
      myNum <-d_all$inc_number[i-1]
      gd<- d_all %>%  filter(inc_number>myNum) %>%
        filter(!(is.na(date_time))) %>%
        filter(inc_number==min(inc_number)) %>%
        select(inc_number,date_time)
      
      
      print(gd)
      d_all$pseudo_date_time[i] <- as.character( gd$date_time)
    })
  }
}

#INC21445459 has JLV from us, DHA from them

######end altering data



verifiedOutliersTicket = c("INC19615282", "INC19504051", "INC19237435","INC16233953","INC8982561", "INC8563764" , "INC18858014", 
                           "INC8458580" , "INC17444119",  "INC18689200" ,"INC15453761",
                           "INC11174173", "INC11170806" ,"INC11674562", "INC13533622",
                           "INC13546353", "INC12401693", "INC14167027", "INC19799787",
                           "INC6838362", "INC7814952", "INC20200196","INC20143657","INC6578180","INC20562842","INC20608640")

verifiedOutliers_df = data.frame(Incident_Ticket = verifiedOutliersTicket, 
                                 Rationale = c(rep(NA,23), 'INC6578180:This incident involved latency (Doctors are experiencing latency when loading templates/notes via VistA. 
They had multiple short reconvenes to discuss this matter and continued to troubleshoot. After a few weeks, 
they came to conclusion that the latency was no longer impacting patient care and reached an acceptable threshold. 
They continued to troubleshoot the issue it as a Medium Priority Incident.', 'INC20562842: The incident on Tuesday 11/30 at 8:30pm EST was caused when the (PITC) production network was migrated to
the new Palo Alto firewall. When this occurred it caused one of the database servers in the PITC to become inaccessible.
That was fixed by 8:15am the next morning, however, this did not resolve all the issues. It appears for the next two weeks
they troubleshooted as a low priority ticket (first reported to ESD on 12/7). 
It was escalated to an HPI on 12/14 and took two days to discover the permanent fix, 
which was opening an emergency change CHG0240904 to rebuild the database and move it from one subnet to another','INC20608640	The latency issue had been happening all month of December due to faulty circuits in ATT. A temporary fix was
put in, however, it did not hold up. They continued to troubleshoot through a PIR 3 ticket but ended up opening a
new incident (INC20756451) on 12/22. It was finally resolved yesterday 12/28'))
verifiedOutliers_df %>% write.csv('verifiedOutliers.csv', row.names = F)

omitOutliersTicket = c('INC20248529')

newOutliers =  d_all %>%
  filter(ttr > 240) %>%
  mutate(legitOutlierFlag = ifelse(Incident_Ticket %in% verifiedOutliersTicket,1,0))%>%
  filter(legitOutlierFlag==0)%>%
  select(Incident_Ticket, date, ttr,legitOutlierFlag)


if (nrow(newOutliers) !=0){
  warning('The above incidents need to be verified as true outliers')
  print(paste0(newOutliers$Incident_Ticket))
}

d_all$month_name_year = factor(d_all$month_name_year, levels = unique(d_all$month_name_year))

colorX = c(
  "#4773AA","#F9C642","#4AA564","#0f2e50","#ed7d31","#d9d9d9","#d9d9d3","#ffffff",
  "#000000","#eb7f29","#004795","#fac922","#fff1d2","#0071bb","#003e73","#112e51",
  "#02bfe7","#00a6d2","#046b99","#9bdaf1","#e1f3f8","#e31c3d","#cd2026","#981b1e",
  "#e59393","#f9dede","#5b616b","#323a45","#757575","#aeb0b5","#eeeeee","#d6d7d9",
  "#f1f1f1","#494440","#e4e2e0","#dce4ef","#fdb81e","#988530","#f9c642","#fad980",
  "#fff1d2","#2e8540","#195c27","#4aa564","#94bfa2","#e7f4e4","#205493","#4773aa",
  "#8ba6ca","#dce4ef","#3f57a6","#21827f","#a23737","#bd5727","#3e8520","#8e704f",
  "#6f7a41","#357ab2","#003e73"
)
colorX_desc = c(
  "OTG - Blue", "OTG - Yellow", "OTG - Green","OTG - Deep Blue","OTG - Orange",
  "OTG - Grey tabs","OTG - Grey tabs2","white","black","orange", "link-default", "warning-message",
  "gibill-accent","primary","primary-darker", "primary-darkest","primary-alt",
  "primary-alt-dark", "primary-alt-darkest","primary-alt-light","primary-alt-lightest",
  "secondary","secondary-dark", "secondary-darkest","secondary-light","secondary-lightest",
  "gray", "gray-dark","gray-medium","gray-light", "gray-light-alt", "gray-lighter",
  "gray-lightest","gray-warm-dark", "gray-warm-light","gray-cool-light","gold",
  "gold-darker","gold-light", "gold-lighter", "gold-lightest","green","green-darker",
  "green-light","green-lighter","green-lightest", "cool-blue","cool-blue-light",
  "cool-blue-lighter","cool-blue-lightest", "hub-health-care","hub-education",
  "hub-disability", "hub-careers","hub-pension","hub-housing","hub-life-insurance",
  "hub-burials","hub-family-member"
)
ColorVA <- data.table(
  Color = colorX,
  Description = colorX_desc
)

cio_theme = theme(
  axis.title.x = element_text(size = 12),
  axis.text.x = element_text(size = 12, angle = 90),
  axis.title.y = element_text(size = 12)
)
cio_x =  labs(x = "Month Issue Date")

#d_all %>% write.csv(paste0('d_all_', Sys.Date(), '.csv'), row.names = F)

#sum(d_all$Facilities_Affected == "", na.rm = FALSE)
d_all %>%
  filter(Incident_State == 'Resolved' & Facilities_Affected == "" & Other_Facilities_Affected =="") %>%
  arrange(desc(date))%>%
  select(Incident_Ticket, date, Incident_State, Facilities_Affected,Other_Facilities_Affected) %>%
  write.csv('Missing_Facilities_Affected.csv', row.names = F)

#####################################################################
# This is the list of incidents that have less than 11 char. in their ticket number
d_all %>%
  filter(nchar(as.character(Incident_Ticket)) != 11 & date >= '2021-01-01')%>%
  select(Incident_Ticket)
#####################################################################
# This is the list of all incidents whose ticket number isn't in snow
d_all %>%
  filter(!Incident_Ticket %in% s$number & Incident_State == "Resolved") %>%
  select(date, Incident_Ticket, Incident_State)%>%
  arrange(desc(date))


#####################################################################
# This is a data frame of incidents with missing HPICPI_Request_DateTime_ET entries
d_all %>%
  filter(is.na(hpicpi_date)) %>% 
  select(date, Incident_Ticket, Incident_State, HPICPI_Request_DateTime_ET) %>%
  arrange(desc(date))


##################################################################
# This is the list of all incidents with negative tt's

d_all %>%
  filter_at(vars(ttCols), any_vars(.<0 | is.na(.))) %>%
  filter(Incident_State == "Resolved") %>%
  filter(Incident_Ticket != 'INC20487682') %>% # Incident INC20487682 is an anomaly per Brian: "it is correct as you see it: the resolution was identified a couple of hours prior to the call being initiated. 
  # It was stated as so by one of the stakeholders on the call and Ryan Vandelinder reported it accurately."
  select(Incident_Ticket, Incident_State, date, ttCols) %>%
  arrange(desc(date)) %>%
  write.csv('problematicTTs.csv', row.names = F)

#################################################################
# This is the list of incidents with missing values
# for the required fields for weekly rundown.

d_all %>%
  filter_at(vars(Incident_Category, Primary_Culpable_System, Primary_SystemApplication_Affected,Additional_SystemsApplications_Affected), any_vars(.=="")) %>%
  filter(Incident_State == "Resolved") %>%
  filter(Incident_Ticket != 'INC20487682') %>% # Incident INC20487682 is an anomaly per Brian: "it is correct as you see it: the resolution was identified a couple of hours prior to the call being initiated. 
  # It was stated as so by one of the stakeholders on the call and Ryan Vandelinder reported it accurately."
  select(Incident_Ticket, date, Incident_Category, Primary_Culpable_System, Primary_SystemApplication_Affected, Additional_SystemsApplications_Affected) %>%
  arrange(desc(date)) %>%
  write.csv('missingFields.csv', row.names = F)

##################################################################
# Finding the last date of record that matches between SP and SNOW

dateCols = c('date',dateCols[!grepl("Issue", dateCols)])
sp_snow_all = s_all %>%
  arrange(opened_at) %>% 
  select(number, opened_at, resolved_at, closed_at, state) %>%
  merge(d_all %>%
          filter(date >= startOfLastMonth-1) %>%
          arrange(date) %>% 
          select(Incident_Ticket, date, Incident_State, Primary_Culpable_System, Primary_SystemApplication_Affected, Additional_SystemsApplications_Affected, ttd, ttta, ttir,ttr),
        by.x = 'number', by.y = 'Incident_Ticket', all =TRUE
  ) 


sp_snow_recent = sp_snow_all %>%
  filter(opened_at >= startOfLastMonth) %>% 
  filter(state %in% c('Resolved','Closed')) 

# opened_at is from SNOW. opened_at doesn't correspond to any fields in SP. 
# But the best field that can be used is Issue_start_Date(date) from SP.
lastDateWithoutNa = sp_snow_recent %>%
  filter_at(vars(date, ttr),any_vars(is.na(.))) %>%
  summarise(lastValidDate = min(as.Date(opened_at,format = "%Y-%m-%d")-1)) %>% 
  suppressWarnings()

lastDateWithValidTtr = sp_snow_recent %>%
  filter(ttr<0) %>%
  summarise(lastValidDate = min(date)-1) %>% 
  suppressWarnings()

#Check for the date of incidents that are closed/resolved in snow but show as in progress in sp
lastStatusMatchDate = sp_snow_recent %>%
  filter(Incident_State == "In Progress" & (state == 'Closed' | state == 'Resolved')) %>%
  summarise(lastValidDate = min(date)-1) %>% 
  suppressWarnips

ngs()

lastMatchDate= rbind(lastDateWithoutNa,lastDateWithValidTtr,lastStatusMatchDate) %>%
  summarise(lastMatchDate=min(lastValidDate, na.rm = T))

if (is.na(as.character(lastMatchDate$lastMatchDate))){
  lastMatchDate$lastMatchDate = max(sp_snow_recent$date)
}

sp_snow_recent %>%
  select(number:Incident_State, ttr)
###############################################
#Weekly Rundown


startDate ='2022-02-07'
endDate = '2022-02-13'
sp_snow_recent %>%
  filter(date >= startDate & date <= endDate)
#################################################

# d_all %>% 
# filter(date >= '2021-04-01' &  How_Issue_Was_Reported == "Automated") %>%
#   select(Incident_Ticket, date,  How_Issue_Was_Reported)


# startDate = '2021-11-01'
# 
# d_all %>% 
#   filter(date >= startDate) %>%
#   select(Incident_Ticket, date, Incident_State, Incident_Priority, Incident_Pillar, Primary_Culpable_System, ttr) %>%
#   arrange(desc(Incident_State)) %>%
#   write.csv('recentIncidents.csv', row.names = F)

#de %>% filter(Primary_Culpable_System == 'VDIF-EP') %>% select(Incident_Ticket, Caused.by.Change)
install.packages("flexdashboard")
install.packages("package:kableExtra")
library("flexdashboard")
library("flextable")
library(purrr)
library(dplyr)
library(flextable)
flexdashboard::flex_dashboard()
rmarkdown::draft("dashboard.rmd")
template = "flex_dashboard_bslib"
package = "flex_dashboard_bslib"

colnames(s_all)

t=s_all%>% 
 #filter(opened_at >= "2021-10-05")%>% 
  filter(incident_state == ""|category == ""|location == "")%>%
  select(number,promoted_on, cmdb_ci,state,category,location)%>%
myft2 <- flextable(head(t), 
                   col_keys = c("number", "cmdb_ci", "state", "category", "location" ))
myft2
myft2 <- italic(myft2, j = 3)
myft2 <- color(myft2, ~ number > 3.5, ~ number, color = "red")
myft2 <- bold(myft2, ~ number > 3.5, ~ number, bold = TRUE)
myft2
myft2 <- add_header_row(
  x = myft2, values = c("Missing Data & Outliers"),
  colwidths = c(5))
myft2 <- align(myft2, i = 1, part = "header", align = "center")
myft2

t1=s_all %>% 
  filter(opened_at >= "2022-09-29")%>% 
  filter(cmdb_ci == "")%>%
  select(number,promoted_on,cmdb_ci)%>%print()
myft2 <- flextable(head(t1), 
                   col_keys = c("number", "cmdb_ci" ))
myft2 <- color(myft2, color = "orange", part = "header")
myft2
myft2 <- italic(myft2, j = 3)
myft2 <- color(myft2,color = "red")
myft2
myft2 <- add_header_row(
  x = myft2, values = c("Missing Data (cmdb_ci)"),
  colwidths = c(2))
myft2 <- align(myft2, i = 1, part = "header", align = "center")
myft2

t3=s_all %>%
  filter(opened_at >= "2022-09-29")%>% 
  filter(state == "Canceled" |state=="In Progress")%>%
  select(number,state,priority,cmdb_ci,opened_at,closed_at)%>%print()
myft3 <- flextable(head(t3), 
                   col_keys = c("number", "state", "opened_at", "closed_at" ))
myft3 <- color(myft3, color = "orange", part = "header")
myft3 <- padding(myft3, j = 1, padding.right = 100)
myft3 <- fontsize(myft3, size = 10, j = 2)
myft3
myft3 <- color(myft3, i= ~ state > 3.5, color = "red")
myft3 <- bold(myft3, i= ~ state > 3.5, bold = TRUE)
myft3
myft3 <- add_header_row(
  x = myft3, values = c("Canceled & In Progress Incidents"),
  colwidths = c(4))
myft3 <- align(myft3, i = 1, part = "header", align = "center")
myft3
t2= s_all%>%
  filter(opened_at >= "2021-09-29")%>% 
  filter(state == "Resolved" |state == "Closed" )%>%
  select(number,state,cmdb_ci,category,opened_at)%>%print()
myft2 <- flextable(head(t2), 
                   col_keys = c("number","state", "cmdb_ci", "category","opened_at" ))
myft2
myft2 <- add_header_row(
  x = myft2, values = c("Resolved & Closed Incidents for Week(42)"),
  colwidths = c(5))
myft2 <- align(myft2, i = 1, part = "header", align = "center")
myft2

s_all%>% 
  filter(opened_at >= "2020-09-29")%>% 
  #filter(state == "Resolved")%>%
  filter(cmdb_ci != "")%>%
  select(number, cmdb_ci)%>%
  group_by(format(cmdb_ci), .drop = FALSE)%>%summarise(N=n())%>% arrange(desc(N))%>%head(20)%>%print()%>%
  ggplot(aes(x=reorder(  `format(cmdb_ci)` ,N),y=N)) + 
  geom_bar(stat="identity",  fill="steelblue") +
  geom_text(aes(label=N), vjust=-0.25, color="Black", size=3.5)+
  ylab("Number of Incidents") + 
  xlab("cmdb_ci") + 
  theme_minimal() +
  ggtitle("Number of cmdb_ci  ") +
  coord_flip()
s_all %>% 
  filter(opened_at >= "2022-09-29")%>% 
  filter(state == "Resolved")%>%
  select(number, category)%>%
  group_by(format(category), .drop = FALSE)%>%summarise(N=n())%>% arrange(desc(N))%>% print()%>%
  ggplot(aes(x=reorder(  `format(category)` ,N),y=N)) + 
  geom_bar(stat="identity",  fill="steelblue") +
  geom_text(aes(label=N), vjust=-0.25, color="Black", size=3.5)+
  ylab("Number of Incidents") + 
  xlab("category ") + 
  theme_minimal() +
  ggtitle("Number of Incidents by Category  ") +
  coord_flip()

s_all %>% 
  filter(opened_at >= "2022-09-29")%>% 
  filter(state == "Resolved")%>%
  select(number, priority)%>%
  group_by(format(priority), .drop = FALSE)%>%summarise(N=n())%>% arrange(desc(N))%>% print()%>%
  ggplot(aes(x=reorder(  `format(priority)` ,N),y=N)) + 
  geom_bar(stat="identity",  fill="steelblue") +
  geom_text(aes(label=N), vjust=-0.25, color="Black", size=3.5)+
  ylab("Number of Incidents") + 
  xlab("Incident_Pillar ") + 
  theme_minimal() +
  ggtitle("Number of Incidents  ") +
  coord_flip()
s_all%>% 
  filter(opened_at >= "2022-09-29")%>% 
  filter(problem_idu_problem_caused_by_change!= "")%>%
  select(number,problem_idu_problem_caused_by_change)%>%
  group_by(format(problem_idu_problem_caused_by_change), .drop = FALSE)%>% summarise(N=n())%>% arrange(desc(N))%>%print()%>%
  ggplot(aes(x=reorder( `format(problem_idu_problem_caused_by_change)` ,N),y=N)) + 
  geom_bar(stat="identity",  fill="steelblue") +
  geom_text(aes(label=N), vjust=-0.25, color="Black", size=3.5)+
  ylab("Number of Incidents") + 
  xlab("problem_id.u_problem_caused_by_change ") + 
  theme_minimal() +
  ggtitle("Number of Incidents  ") +
  coord_flip()
s_all%>% 
  filter(opened_at >= "2022-09-29")%>% 
  filter(caused_by!="")%>%
  select(number,caused_by)%>% 
  group_by(format(caused_by), .drop = FALSE) %>% summarise(N=n())%>%head(15)%>% arrange(desc(N))%>%
  ggplot(aes(x=reorder( `format(caused_by)` ,N),y=N)) + 
  geom_bar(stat="identity",  fill="steelblue") +
  geom_text(aes(label=N), vjust=-0.25, color="Black", size=3.5)+
  ylab("Number of Incidents") + 
  xlab("problem_id.u_problem_caused_by_change ") + 
  theme_minimal() +
  ggtitle("Number of Incidents  ")





