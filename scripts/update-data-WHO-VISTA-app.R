# Entry point run by GitHub Actions (or manually: Rscript scripts/update-data - WHO VISTA app.R)

library(httr)
library(jsonlite)
library(sqldf)
#setwd("C:/Users/nedelecy/OneDrive - World Health Organization/Documents/__WIISE/_WIISEMART/ANALYSES/STANDARD VISUALS/DASHBOARD/Shiny app/GitHub")
#dir.create("data", showWarnings = FALSE)
nrow <- 0
yr_plan <- 2026 # 2026 Hardcoded - to update. Also to update in global.R for the app

for(dataset in c("REF_COUNTRIES", 
                 "MT_AD_INTRO_LONG", "MT_RI_INTRO_DTP_BOOSTER", "AD_VACCINE_INTRODUCTIONS", "V_RI_INTRO_YEAR_LONG", 
                 "MT_AD_COV_NATIONAL_LONG",
                 "MT_AD_INC_NATIONAL_LONG", "MT_AD_INC_RATE_NATIONAL_LONG",
                 "MT_AD_IND_LONG", 
                 # Ref tables to be run after because of dependancies on the data tables
                 "REF_INDICATOR_CATEGORIES", "REF_INDICATORS", "REF_COVERAGE_CODES", "REF_COVERAGE_CATEGORIES", "REF_DISEASES", "REF_VACCINES"))
{
  # ---- 1. Load data ---------------------------------------------------------------
  
  if(dataset %in% c("REF_COUNTRIES"))
  {
    REF_COUNTRIES <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/REF_COUNTRIES?$format=streaming&$select=CODE,NAMEWORKEN,WHOREGIONC,WHOMEMBER,WHO_LEGAL_STATUS_TITLE,GRP_ATRISK_YF,GRP_MENING_BELT,GRP_ATRISK_JE", sep = ""))$content))$value
    df <- REF_COUNTRIES
  }
  
  
  
  if(dataset %in% c("MT_AD_INTRO_LONG"))
  {
    MT_AD_INTRO_LONG <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/MT_AD_INTRO_LONG?$format=streaming&$select=ISO_3_CODE,YEAR,ANTIGEN,INTRO&$filter=YEAR%20ge%202013&excludeSysColumns=0", sep = ""))$content))$value
    df <- MT_AD_INTRO_LONG
  }
  if(dataset %in% c("MT_RI_INTRO_DTP_BOOSTER"))
  {
    MT_RI_INTRO_DTP_BOOSTER <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/MT_RI_INTRO_DTP_BOOSTER?$format=streaming&$select=COUNTRY,YEAR,INTRO_TYPE_EDIT&$filter=YEAR%20ge%202013", sep = ""))$content))$value
    df <- MT_RI_INTRO_DTP_BOOSTER
  }
  if(dataset %in% c("AD_VACCINE_INTRODUCTIONS"))
  {
    AD_VACCINE_INTRODUCTIONS <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/AD_VACCINE_INTRODUCTIONS?$format=streaming&$select=COUNTRY,YEAR,VACCINECODE,NATIONWIDE,PARTIALLY,HIGHRISKGROUP,SPECIAL_SCHEDULE,WOMEN_SCHEDULE,OFFICIAL_INTRO&$filter=YEAR%20ge%202013&excludeSysColumns=1", sep = ""))$content))$value
    AD_VACCINE_INTRODUCTIONS <- AD_VACCINE_INTRODUCTIONS[AD_VACCINE_INTRODUCTIONS$COUNTRY %in% REF_COUNTRIES$CODE[REF_COUNTRIES$WHO_LEGAL_STATUS_TITLE %in% c("Member State")],c("COUNTRY", "YEAR", "VACCINECODE", "NATIONWIDE", "PARTIALLY", "HIGHRISKGROUP", "SPECIAL_SCHEDULE", "WOMEN_SCHEDULE", "OFFICIAL_INTRO")]
    colnames(AD_VACCINE_INTRODUCTIONS) <- c("COUNTRY", "YEAR", "VACCINECODE", "NATIONWIDE", "PARTIALLY", "HIGHRISKGROUP", "SPECIAL_SCHEDULE", "WOMEN_SCHEDULE", "OFFICIAL_INTRO")
    data_latest <- sqldf(paste("SELECT temp.COUNTRY, 
                                        temp.VACCINECODE,  
                                        temp.[YEAR], 
                                        AD_VACCINE_INTRODUCTIONS.NATIONWIDE, 
                                        AD_VACCINE_INTRODUCTIONS.PARTIALLY, 
                                        AD_VACCINE_INTRODUCTIONS.HIGHRISKGROUP, 
                                        AD_VACCINE_INTRODUCTIONS.SPECIAL_SCHEDULE, 
                                        AD_VACCINE_INTRODUCTIONS.WOMEN_SCHEDULE, 
                                        AD_VACCINE_INTRODUCTIONS.OFFICIAL_INTRO 
                                FROM 
                                (
                                  SELECT COUNTRY, VACCINECODE, MAX([YEAR]) AS [YEAR]
                                  FROM AD_VACCINE_INTRODUCTIONS
                                  WHERE [YEAR] <= ",yr_plan, " 
                                  GROUP BY COUNTRY, VACCINECODE
                                ) temp
                                LEFT OUTER JOIN AD_VACCINE_INTRODUCTIONS ON temp.COUNTRY = AD_VACCINE_INTRODUCTIONS.COUNTRY AND temp.[YEAR] = AD_VACCINE_INTRODUCTIONS.[YEAR] AND temp.VACCINECODE = AD_VACCINE_INTRODUCTIONS.VACCINECODE",
                                sep = ""))
    
    data_latest$INTRO <- # Conditions specific for RSV vaccine
                          ifelse(data_latest$VACCINECODE %in% c("RSV_STRAT") & is.na(data_latest$SPECIAL_SCHEDULE) == F & data_latest$SPECIAL_SCHEDULE %in% c("INFANT") & (data_latest$NATIONWIDE %in% c("yes", "Yes") | data_latest$PARTIALLY %in% c("yes", "Yes")), "Yes (I)", 
                          ifelse(data_latest$VACCINECODE %in% c("RSV_STRAT") & is.na(data_latest$SPECIAL_SCHEDULE) == F & data_latest$SPECIAL_SCHEDULE %in% c("MATERNAL") & (data_latest$NATIONWIDE %in% c("yes", "Yes") | data_latest$PARTIALLY %in% c("yes", "Yes")), "Yes (M)", 
                          ifelse(data_latest$VACCINECODE %in% c("RSV_STRAT") & is.na(data_latest$SPECIAL_SCHEDULE) == F & data_latest$SPECIAL_SCHEDULE %in% c("BOTH") & (data_latest$NATIONWIDE %in% c("yes", "Yes") | data_latest$PARTIALLY %in% c("yes", "Yes")), "Yes (Both)", 
                          ifelse(data_latest$VACCINECODE %in% c("RSV_STRAT") & is.na(data_latest$SPECIAL_SCHEDULE) == F & data_latest$SPECIAL_SCHEDULE %in% c("INFANT","MATERNAL","BOTH") & (data_latest$NATIONWIDE %in% c("planned") | data_latest$PARTIALLY %in% c("planned")),  "No", #"Planned" changed to "No" so they Planned status do NOT appear on the To-date maps 
                          #ifelse(data_latest$VACCINECODE %in% c("RSV_STRAT") & is.na(data_latest$SPECIAL_SCHEDULE) == F & data_latest$SPECIAL_SCHEDULE %in% c("ADULT"), "No", 
                          ifelse(data_latest$VACCINECODE %in% c("RSV_STRAT"), "No", 
                                         
                          # Conditions for all other vaccines
                          ifelse(data_latest$NATIONWIDE %in% c("yes", "Yes"), "Yes",
                          ifelse(data_latest$PARTIALLY %in% c("yes", "Yes"), "Yes (P)",
                          ifelse(data_latest$HIGHRISKGROUP %in% c("yes", "Yes"), "Yes (R)",
                          ifelse(data_latest$NATIONWIDE %in% c("yes(A)"), "Yes (A)",
                          ifelse(data_latest$PARTIALLY %in% c("high risk"), "High risk area",
                          ifelse(data_latest$HIGHRISKGROUP %in% c("yes(O)"), "Yes (O)",
                          ifelse(data_latest$NATIONWIDE %in% c("yes (VA)"), "Yes (VA)",
                          ifelse(data_latest$NATIONWIDE %in% c("planned") | data_latest$PARTIALLY %in% c("planned") | data_latest$HIGHRISKGROUP %in% c("planned"), "No", #"Planned" changed to "No" so they Planned status do NOT appear on the To-date maps 
                                 "No")))))))))))))
    
    
    df <- data_latest
  }
  if(dataset %in% c("V_RI_INTRO_YEAR_LONG"))
  {
    V_RI_INTRO_YEAR_LONG <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/V_RI_INTRO_YEAR_LONG?$format=streaming&$select=COUNTRY,ANTIGEN,YEAR_INTRO_NATIONAL,YEAR_INTRO_PARTIAL,YEAR_INTRO_RISK_GROUPS,YEAR_INTRO_RISK_AREA", sep = ""))$content))$value
    df <- V_RI_INTRO_YEAR_LONG
  }
  
  if(dataset %in% c("MT_AD_COV_NATIONAL_LONG"))
  {
    MT_AD_COV_NATIONAL_LONG <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/MT_AD_COV_NATIONAL_LONG?$format=streaming&$select=ISO_3_CODE,YEAR,COVERAGE_CATEGORY,ANTIGEN,DOSES,TARGET_NUMBER,COVERAGE&$filter=YEAR%20ge%202013", sep = ""))$content))$value
    colnames(MT_AD_COV_NATIONAL_LONG) <- c("COUNTRY","YEAR","COVERAGE_CATEGORY","COVERAGE_CODE","DOSES","TARGET_NUMBER","COVERAGE")
    df <- MT_AD_COV_NATIONAL_LONG
  }
  
  if(dataset %in% c("MT_AD_INC_NATIONAL_LONG"))
  {
    MT_AD_INC_NATIONAL_LONG <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/MT_AD_INC_NATIONAL_LONG?$format=streaming&$select=ISO_3_CODE,YEAR,DISEASE,CASES&$filter=YEAR%20ge%202013", sep = ""))$content))$value
    colnames(MT_AD_INC_NATIONAL_LONG) <- c("COUNTRY","YEAR","DISEASE","CASES")
    df <- MT_AD_INC_NATIONAL_LONG
  }
  if(dataset %in% c("MT_AD_INC_RATE_NATIONAL_LONG"))
  {
    MT_AD_INC_RATE_NATIONAL_LONG <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/MT_AD_INC_RATE_LONG?$format=streaming&$select=CODE,YEAR,DISEASE,DENOMINATOR,INCIDENCE_RATE&$filter=YEAR%20ge%202013%20and%20GROUP%20in%20(%27COUNTRIES%27)", sep = ""))$content))$value
    colnames(MT_AD_INC_RATE_NATIONAL_LONG) <- c("COUNTRY","YEAR","DISEASE","DENOMINATOR","INCIDENCE_RATE")
    df <- MT_AD_INC_RATE_NATIONAL_LONG
  }
  
  if(dataset %in% c("MT_AD_IND_LONG"))
  {
    MT_AD_IND_LONG <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/MT_AD_IND_LONG?$format=streaming&$select=ISO_3_CODE,YEAR,INDCATCODE,INDCODE,VALUE&$filter=YEAR%20ge%202013", sep = ""))$content))$value
    colnames(MT_AD_IND_LONG) <- c("COUNTRY","YEAR","INDICATOR_CATEGORY","INDICATOR","VALUE")
    df <- MT_AD_IND_LONG
  }
  
  
  
  
  
  if(dataset %in% c("REF_INDICATOR_CATEGORIES"))
  {
    REF_INDICATOR_CATEGORIES <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/REF_AD_INDICATORCATS?$format=streaming&$select=CODE,DESCRIPTION,CATSORT", sep = ""))$content))$value
    colnames(REF_INDICATOR_CATEGORIES) <- c("CODE", "DESCRIPTION","CATEGORY_SORT")
    #REF_INDICATOR_CATEGORIES <- REF_INDICATOR_CATEGORIES[REF_INDICATOR_CATEGORIES$CODE %in% unique(MT_AD_IND_LONG$INDICATOR_CATEGORY),c("CODE", "DESCRIPTION")]
    REF_INDICATOR_CATEGORIES <- REF_INDICATOR_CATEGORIES[order(REF_INDICATOR_CATEGORIES$DESCRIPTION),]
    df <- REF_INDICATOR_CATEGORIES
  }
  if(dataset %in% c("REF_INDICATORS"))
  {
    REF_INDICATORS <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/REF_AD_INDICATORS?$format=streaming&$select=CODE,INDPHRASESHORTEN,INDCATCODE", sep = ""))$content))$value
    colnames(REF_INDICATORS) <- c("CODE", "DESCRIPTION","CATEGORY_SORT")
    #REF_INDICATORS <- REF_INDICATORS[REF_INDICATORS$CODE %in% unique(MT_AD_IND_LONG$INDICATOR),c("CODE", "DESCRIPTION")]
    REF_INDICATORS <- REF_INDICATORS[order(REF_INDICATORS$DESCRIPTION),]
    
    REF_INDICATORS$TITLE_MAP <- REF_INDICATORS$DESCRIPTION
    REF_INDICATORS$TITLE_MAP[REF_INDICATORS$CODE == "DEMAND_STRATEGIES_COMM"]                 <- "Did the country implement public communications strategies to address under-vaccination\nwhich was informed by results of demand-related assessments?"
    REF_INDICATORS$TITLE_MAP[REF_INDICATORS$CODE == "DEMAND_STRATEGIES_CE"]                   <- "Did the country implement community engagement strategies to address under-vaccination\nwhich was informed by results of demand-related assessments?"
    REF_INDICATORS$TITLE_MAP[REF_INDICATORS$CODE == "DEMAND_STRATEGIES_INTERVENTIONS"]        <- "Did the country implement behaviorally informed interventions strategies to address\nunder-vaccination which was informed by results of demand-related assessments?"
    REF_INDICATORS$TITLE_MAP[REF_INDICATORS$CODE == "HESIT_COMMUNICATION_PLAN"]               <- "Is there a risk communication plan in place to respond to vaccine related events?"
    REF_INDICATORS$TITLE_MAP[REF_INDICATORS$CODE == "DEMAND_STRATEGIES_DIGITAL"]              <- "Did the country implement digital or social listening strategies to address under-vaccination\nwhich was informed by results of demand-related assessments?"
    REF_INDICATORS$TITLE_MAP[REF_INDICATORS$CODE == "DEMAND_ASSESSMENT_MEASURES"]             <- "Did this assessment include any survey of Behavioural and Social Drivers (BeSD) of\nVaccination using the globally validated tools, including priority indicators?"
    REF_INDICATORS$TITLE_MAP[REF_INDICATORS$CODE == "DEMAND_ASSESSMENT"]                      <- "Did the country conduct any assessment of the demand-related reasons for under-vaccination?"
    REF_INDICATORS$TITLE_MAP[REF_INDICATORS$CODE == "DEMAND_STRATEGIES_SERVICE"]              <- "Did the country implement service quality intervention strategies to address under-vaccination\nwhich was informed by results of demand-related assessments?"
    REF_INDICATORS$TITLE_MAP[REF_INDICATORS$CODE == "DEMAND_STRATEGIES_OTHER"]                <- "Did the country implement other strategies to address under-vaccination which was informed\nby results of demand-related assessments?"
    REF_INDICATORS$TITLE_MAP[REF_INDICATORS$CODE == "DEMAND_STRATEGIES_UPTAKE_IMPLEMENTED"]   <- "Did the country implement any strategies to address under-vaccination which was informed\nby results of demand-related assessments?"
    
    df <- REF_INDICATORS
  }
  if(dataset %in% c("REF_COVERAGE_CODES"))
  {
    REF_COVERAGE_CODES <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/REF_COVERAGE_CODES?$format=streaming&$select=CODE,DESCRIPTION", sep = ""))$content))$value
    colnames(REF_COVERAGE_CODES) <- c("CODE", "DESCRIPTION")
    REF_COVERAGE_CODES$DESCRIPTION[REF_COVERAGE_CODES$CODE == "HEPB_BD"] <- "HepB, birth dose"
    #REF_COVERAGE_CODES <- REF_COVERAGE_CODES[REF_COVERAGE_CODES$CODE %in% unique(MT_AD_COV_NATIONAL_LONG$COVERAGE_CODE),c("CODE", "DESCRIPTION")]
    REF_COVERAGE_CODES <- REF_COVERAGE_CODES[order(REF_COVERAGE_CODES$DESCRIPTION),]
    df <- REF_COVERAGE_CODES
  }
  if(dataset %in% c("REF_COVERAGE_CATEGORIES"))
  {
    REF_COVERAGE_CATEGORIES <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/REF_COVERAGECATEGORIES?$format=streaming&$select=CODE,DESCRIPTION", sep = ""))$content))$value
    colnames(REF_COVERAGE_CATEGORIES) <- c("CODE", "DESCRIPTION")
    #REF_COVERAGE_CATEGORIES <- REF_COVERAGE_CATEGORIES[REF_COVERAGE_CATEGORIES$CODE %in% unique(MT_AD_COV_NATIONAL_LONG$COVERAGE_CATEGORY),c("CODE", "DESCRIPTION")]
    REF_COVERAGE_CATEGORIES <- REF_COVERAGE_CATEGORIES[order(REF_COVERAGE_CATEGORIES$DESCRIPTION),]
    df <- REF_COVERAGE_CATEGORIES
  }
  if(dataset %in% c("REF_DISEASES"))
  {
    REF_DISEASES <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/REF_AD_DISEASES?$format=streaming&$select=CODE,DESCRIPTION", sep = ""))$content))$value
    colnames(REF_DISEASES) <- c("CODE", "DESCRIPTION")
    #REF_DISEASES <- REF_DISEASES[REF_DISEASES$CODE %in% unique(MT_AD_INC_NATIONAL_LONG$DISEASE),c("CODE", "DESCRIPTION")]
    REF_DISEASES <- REF_DISEASES[order(REF_DISEASES$DESCRIPTION),]
    df <- REF_DISEASES
  }
  if(dataset %in% c("REF_VACCINES"))
  {
    REF_VACCINES <- rbind(data.frame(ORDER = 1, VACCINE = "COVID19", VACCINE_NAME = "COVID-19 vaccine", TO_DATE = "No"),
                          data.frame(ORDER = 2, VACCINE = "DTPBOOSTER", VACCINE_NAME = "DTP Booster dose(s)", TO_DATE = "No"),
                          data.frame(ORDER = 3, VACCINE = "HEPA", VACCINE_NAME = "Hepatitis A vaccine", TO_DATE = "Yes"),
                          data.frame(ORDER = 4, VACCINE = "HEPB", VACCINE_NAME = "Hepatitis B vaccine", TO_DATE = "Yes"),
                          data.frame(ORDER = 5, VACCINE = "HepB_BD", VACCINE_NAME = "Hepatitis B Birth dose", TO_DATE = "Yes"),
                          data.frame(ORDER = 6, VACCINE = "HEPB + HepB_BD", VACCINE_NAME = "Hepatitis B Birth dose vaccination strategies", TO_DATE = "Yes"),
                          data.frame(ORDER = 7, VACCINE = "HIB", VACCINE_NAME = "Hib (Haemophilus influenzae type B) vaccine", TO_DATE = "Yes"),
                          data.frame(ORDER = 8, VACCINE = "HPV", VACCINE_NAME = "Human papillomavirus (HPV) vaccine", TO_DATE = "Yes"),
                          data.frame(ORDER = 9, VACCINE = "IPV2", VACCINE_NAME = "Inactivated polio vaccine (IPV) 2nd dose", TO_DATE = "Yes"),
                          data.frame(ORDER = 10, VACCINE = "JAPENC", VACCINE_NAME = "Japanese Encephalitis vaccine", TO_DATE = "Yes"),
                          data.frame(ORDER = 11, VACCINE = "MALARIA", VACCINE_NAME = "Malaria vaccine", TO_DATE = "Yes"),
                          data.frame(ORDER = 12, VACCINE = "MCV2", VACCINE_NAME = "Measles-containing vaccine 2nd dose", TO_DATE = "Yes"),
                          data.frame(ORDER = 13, VACCINE = "MMCV", VACCINE_NAME = "Meningococcal meningitis vaccines (any strain)", TO_DATE = "Yes"),
                          data.frame(ORDER = 14, VACCINE = "MMCV (MENING_BELT)", VACCINE_NAME = "Meningococcal A conjugate vaccine in the Meningitis Belt", TO_DATE = "Yes"),
                          data.frame(ORDER = 15, VACCINE = "MUMPS", VACCINE_NAME = "Mumps vaccine", TO_DATE = "Yes"),
                          data.frame(ORDER = 16, VACCINE = "AP", VACCINE_NAME = "Pertussis-containing vaccine, acellular (aP)", TO_DATE = "Yes"),
                          data.frame(ORDER = 17, VACCINE = "PNEUMO_CONJ", VACCINE_NAME = "Pneumococcal conjugate vaccine (PCV)", TO_DATE = "Yes"),
                          #data.frame(ORDER = 18, VACCINE = "PNEUMO_PS", VACCINE_NAME = "Pneumococcal polysaccharide vaccine (PPV)", TO_DATE = "Yes"),
                          data.frame(ORDER = 19, VACCINE = "ROTAVIRUS", VACCINE_NAME = "Rotavirus vaccine", TO_DATE = "Yes"),
                          data.frame(ORDER = 20, VACCINE = "RSV_STRAT", VACCINE_NAME = "RSV infant vaccination strategies", TO_DATE = "Yes"),
                          data.frame(ORDER = 21, VACCINE = "RUBELLA", VACCINE_NAME = "Rubella vaccine", TO_DATE = "Yes"),
                          data.frame(ORDER = 22, VACCINE = "INFLUENZA", VACCINE_NAME = "Seasonal influenza vaccine", TO_DATE = "No"),
                          data.frame(ORDER = 23, VACCINE = "TYPHOID_CONJ", VACCINE_NAME = "Typhoid conjugate vaccine", TO_DATE = "Yes"),
                          data.frame(ORDER = 24, VACCINE = "VARICELLA", VACCINE_NAME = "Varicella vaccine", TO_DATE = "Yes"),
                          data.frame(ORDER = 25, VACCINE = "YF", VACCINE_NAME = "Yellow fever vaccine", TO_DATE = "Yes"))
    
    #REF_VACCINES <- REF_VACCINES[order(REF_VACCINES$VACCINE_NAME),]
    REF_VACCINES <- REF_VACCINES[order(REF_VACCINES$ORDER),]
    df <- REF_VACCINES
  }
  
  # ---- 2. Validate (fail the job rather than overwrite good data with bad) ----
  stopifnot("API returned no rows" = nrow(df) > 0
            # , "Missing column 'id'" = "id" %in% names(df)
  )
  
  # ---- 3. Save atomically ------------------------------------------------------
  tmp <- tempfile(fileext = ".rds")
  saveRDS(df, tmp, compress = "xz")
  file.copy(tmp, paste("data/WHO VISTA app/",dataset,".rds", sep = ""), overwrite = TRUE)

  
  message(dataset, " - Saved ", nrow(df), " rows at ", format(Sys.time(), tz = "UTC"), " UTC")
  
  nrow <- nrow + nrow(df)
}
  
  # ---- 4. Save a small metadata file the Shiny app can display ----------------
  saveRDS(list(updated_at = Sys.time(), n_rows = nrow), "data/WHO VISTA app/meta.rds")
