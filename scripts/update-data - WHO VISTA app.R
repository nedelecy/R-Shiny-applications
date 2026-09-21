# Entry point run by GitHub Actions (or manually: Rscript scripts/update-data - WHO VISTA app.R)

library(httr)
library(jsonlite)

#setwd("C:/Users/nedelecy/OneDrive - World Health Organization/Documents/__WIISE/_WIISEMART/ANALYSES/STANDARD VISUALS/DASHBOARD/Shiny app/GitHub")
#dir.create("data", showWarnings = FALSE)

for(dataset in c("REF_COUNTRIES", "REF_INDICATOR_CATEGORIES", #"REF_INDICATORS", 
                 "REF_COVERAGE_CODES", "REF_COVERAGE_CATEGORIES", "REF_DISEASES",
                 "MT_AD_INTRO_LONG", "MT_RI_INTRO_DTP_BOOSTER", "AD_VACCINE_INTRODUCTIONS"))
{
  # ---- 1. Load data ---------------------------------------------------------------
  if(dataset %in% c("REF_COUNTRIES"))
  {
    df <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/REF_COUNTRIES?$format=streaming&$select=CODE,NAMEWORKEN,WHOREGIONC,WHOMEMBER,WHO_LEGAL_STATUS_TITLE,GRP_ATRISK_YF,GRP_MENING_BELT,GRP_ATRISK_JE", sep = ""))$content))$value
  }
  if(dataset %in% c("REF_INDICATOR_CATEGORIES"))
  {
    df <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/REF_AD_INDICATORCATS?$format=streaming&$select=CODE,DESCRIPTION,CATSORT", sep = ""))$content))$value
    colnames(df) <- c("CODE", "DESCRIPTION","CATEGORY_SORT")
  }
#  if(dataset %in% c("REF_INDICATORS"))
#  {
#    df <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/REF_AD_INDICATORS?$format=streaming&$select=CODE,INDPHRASESHORTEN,INDCATCODE&$filter=CODE%20in%20(",indicators_url,")", sep = ""))$content))$value
#    colnames(df) <- c("CODE", "DESCRIPTION","CATEGORY_SORT")
#  }
  if(dataset %in% c("REF_COVERAGE_CODES"))
  {
    df <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/REF_COVERAGE_CODES?$format=streaming&$select=CODE,DESCRIPTION", sep = ""))$content))$value
    colnames(df) <- c("CODE", "DESCRIPTION")
  }
  if(dataset %in% c("REF_COVERAGE_CATEGORIES"))
  {
    df <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/REF_COVERAGECATEGORIES?$format=streaming&$select=CODE,DESCRIPTION", sep = ""))$content))$value
    colnames(df) <- c("CODE", "DESCRIPTION")
  }
  if(dataset %in% c("REF_DISEASES"))
  {
    df <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/REF_AD_DISEASES?$format=streaming&$select=CODE,DESCRIPTION", sep = ""))$content))$value
    colnames(df) <- c("CODE", "DESCRIPTION")
  }
  if(dataset %in% c("MT_AD_INTRO_LONG"))
  {
    df <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/MT_AD_INTRO_LONG?$format=streaming&$select=ISO_3_CODE,YEAR,ANTIGEN,INTRO&$filter=YEAR%20ge%202013&excludeSysColumns=0", sep = ""))$content))$value
  }
  if(dataset %in% c("MT_RI_INTRO_DTP_BOOSTER"))
  {
    df <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/MT_RI_INTRO_DTP_BOOSTER?$format=streaming&$select=COUNTRY,YEAR,INTRO_TYPE_EDIT&$filter=YEAR%20ge%202013", sep = ""))$content))$value
  }
  if(dataset %in% c("AD_VACCINE_INTRODUCTIONS"))
  {
    df <- fromJSON(rawToChar(GET(paste("https://xmart-api-public.who.int/WIISE/AD_VACCINE_INTRODUCTIONS?$format=streaming&$select=COUNTRY,YEAR,VACCINECODE,NATIONWIDE,PARTIALLY,HIGHRISKGROUP,SPECIAL_SCHEDULE,WOMEN_SCHEDULE,OFFICIAL_INTRO&$filter=YEAR%20ge%202013&excludeSysColumns=1", sep = ""))$content))$value
  }
  
  


  # ---- 2. Validate (fail the job rather than overwrite good data with bad) ----
  stopifnot("API returned no rows" = nrow(df) > 0
            # , "Missing column 'id'" = "id" %in% names(df)
            )
  
  # ---- 3. Save atomically ------------------------------------------------------
  tmp <- tempfile(fileext = ".rds")
  saveRDS(df, tmp, compress = "xz")
  file.copy(tmp, paste("data/WHO VISTA app/",dataset,".rds", sep = ""), overwrite = TRUE)
  
  # ---- 4. Save a small metadata file the Shiny app can display ----------------
  saveRDS(list(updated_at = Sys.time(), n_rows = nrow(df)), "data/WHO VISTA app/meta.rds")
  
  message("Saved ", nrow(df), " rows at ", format(Sys.time(), tz = "UTC"), " UTC")
  
}
