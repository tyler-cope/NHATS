# Clean Workspace
rm(list = ls()) 

# --- Load Libraries ---
library(tidyverse)   # Data wrangling
library(haven)       # Read SAS files
library(janitor)     # Clean variable names
library(skimr)       # Data inspection


# --- Set Working Directory ---
setwd("C:/Users/tlc74/OneDrive - Duke University/1) Tyler/PhD/Data Cleaning Course/Data_cleaning_class/NHATS Data")

############################################
# 📋 Helper Definitions
############################################

# Missing data codes used across NHATS
special_missing_levels <- c("-1", "-7", "-8", "-9")
special_missing_labels <- c("Inapplicable", "Refused", "Don’t know", "Missing")

# Common Yes/No structure across NHATS
yes_no_levels <- c("1", "2", special_missing_levels)
yes_no_labels <- c("Yes", "No", special_missing_labels)

# Convert special missing codes to numeric for case_when logic
special_missing_numeric <- as.numeric(special_missing_levels)

# 🛠 Helper: Collapse special missing levels into a single “Missing” factor level
safe_collapse <- function(x, missing_levels = special_missing_labels) {
  if (!is.factor(x)) x <- factor(x)
  valid <- intersect(levels(x), missing_levels)
  if (length(valid) > 0) {
    x |> fct_collapse(Missing = valid) |> fct_explicit_na("Missing")
  } else {
    x |> fct_explicit_na("Missing")
  }
}

############################################
# 🔷 Round 1 – Clean
############################################
R1_SP <- read_sas("NHATS_Round_1_SP_File.sas7bdat")

R1_SP_clean <- R1_SP |>
  select(
    spid, is1resptype, r1dresid, r1d2intvrage, r1dgender, el1higstschl,
    lf1doccpctgy, lf1occupaton, hp1ownrentot, lf1workfpay, hp1sec8pubsn,
    ip1cmedicaid, ew1progneed1, ew1progneed2, ew1progneed3, el1hlthchild,
    ia1totinc, hh1dhshldnum, rl1dracehisp
  ) |>
  filter(is1resptype == 1 & r1dresid == 1) |>   # Only sample-person responders living at home
  rename(
    Responder1 = is1resptype,
    Residential1 = r1dresid,
    Age1 = r1d2intvrage,
    Gender1 = r1dgender,
    Education1 = el1higstschl,
    Occupation1 = lf1doccpctgy,
    EverWorked1 = lf1occupaton,
    HomeOwnership1 = hp1ownrentot,
    HouseholdSize1 = hh1dhshldnum,
    HouseholdIncome1 = ia1totinc,
    RetirementStatus1 = lf1workfpay,
    Section81 = hp1sec8pubsn,
    Medicaid1 = ip1cmedicaid,
    FoodAssist11 = ew1progneed1,
    FoodAssist21 = ew1progneed2,
    FoodAssist31 = ew1progneed3,
    ChildhoodHealth1 = el1hlthchild,
    RaceEthnicity1 = rl1dracehisp
  ) |>
  mutate(
    # --- Simplify occupation codes into 6 buckets ---
    Occupation1 = case_when(
      Occupation1 %in% 1:11 ~ "1",       # Management / Professional
      Occupation1 %in% 12:15 ~ "2",      # Service
      Occupation1 %in% 16:17 ~ "3",      # Sales / Office
      Occupation1 %in% 18:20 ~ "4",      # Construction / Farming
      Occupation1 %in% 21:23 ~ "5",      # Production
      EverWorked1 %in% c(2,3) ~ "6",     # Never worked / Homemaker
      TRUE ~ NA_character_
    ),
    
    # --- Convert to labeled factors ---
    Responder1 = factor(Responder1, c("1","2"), c("Sample_Person","Proxy")),
    Residential1 = factor(Residential1, c("1","2","3","4","5", special_missing_levels),
                          c("Home/apartment","Retirement community","Assisted living","Nursing home","Other institution", special_missing_labels)),
    Age1 = factor(Age1, c("1","2","3","4","5","6", special_missing_levels),
                  c("65–69","70–74","75–79","80–84","85–89","90+", special_missing_labels)),
    Gender1 = factor(Gender1, c("1","2"), c("Male","Female")),
    Education1 = factor(Education1, c("1","2","3","4","5","6","7","8","9", special_missing_levels),
                        c("No schooling","1–8th grade","9–12 (no diploma)","HS grad",
                          "Vocational","Some college","Associate","Bachelor","Master/PhD", special_missing_labels)),
    Occupation1 = factor(Occupation1, c("1","2","3","4","5","6"),
                         c("Management/Professional","Service","Sales/Office",
                           "Construction/Farming","Production","Homemaker")),
    HomeOwnership1 = factor(HomeOwnership1, c("1","2","3", special_missing_levels),
                            c("Own","Rent","Other", special_missing_labels)),
    RetirementStatus1 = factor(RetirementStatus1, c("1","2","3", special_missing_levels),
                               c("Yes","No","Retired", special_missing_labels)),
    Section81 = factor(Section81, yes_no_levels, yes_no_labels),
    Medicaid1 = factor(Medicaid1, yes_no_levels, yes_no_labels),
    FoodAssist11 = factor(FoodAssist11, yes_no_levels, yes_no_labels),
    FoodAssist21 = factor(FoodAssist21, yes_no_levels, yes_no_labels),
    FoodAssist31 = factor(FoodAssist31, yes_no_levels, yes_no_labels),
    ChildhoodHealth1 = factor(ChildhoodHealth1, c("1","2","3","4","5", special_missing_levels),
                              c("Excellent","Very good","Good","Fair","Poor", special_missing_labels)),
    RaceEthnicity1 = factor(RaceEthnicity1)
  ) |>
  mutate(Section81 = fct_recode(Section81, "No" = "Inapplicable"))

############################################
# 🔷 Round 5 – Clean
############################################
R5_SP <- read_sas("NHATS_Round_5_SP_File_v2.sas7bdat")

R5_SP_clean <- R5_SP |>
  select(
    spid, is5resptype, r5dresid, r5dcontnew, r5d2intvrage, r5dgender, el5higstschl,
    lf5doccpctgy, lf5occupaton, hp5ownrentot, lf5workfpay, hp5sec8pubsn,
    ip5cmedicaid, ew5progneed1, ew5progneed2, ew5progneed3, el5hlthchild,
    hh5dmarstat, ia5totinc, hh5dhshldnum, fl5newsample, rl5dracehisp
  ) |>
  filter(is5resptype == 1 & r5dresid == 1) |>
  rename(
    Responder5 = is5resptype,
    Residential5 = r5dresid,
    NewPerson = r5dcontnew,
    Age = r5d2intvrage,
    Gender = r5dgender,
    Education = el5higstschl,
    Occupation = lf5doccpctgy,
    EverWorked = lf5occupaton,
    HomeOwnership = hp5ownrentot,
    HouseholdSize = hh5dhshldnum,
    HouseholdIncome = ia5totinc,
    RetirementStatus = lf5workfpay,
    Section8 = hp5sec8pubsn,
    Medicaid = ip5cmedicaid,
    FoodAssist1 = ew5progneed1,
    FoodAssist2 = ew5progneed2,
    FoodAssist3 = ew5progneed3,
    ChildhoodHealth = el5hlthchild,
    MaritalStatus = hh5dmarstat,
    Newsample = fl5newsample,
    RaceEthnicity = rl5dracehisp
  ) |>
  mutate(
    # Collapse occupation codes into categories (includes “retired/not working”)
    Occupation = case_when(
      Occupation %in% 1:11 ~ "1",
      Occupation %in% 12:15 ~ "2",
      Occupation %in% 16:17 ~ "3",
      Occupation %in% 18:20 ~ "4",
      Occupation %in% 21:23 ~ "5",
      EverWorked %in% c(2,3) ~ "6",
      Occupation == -1 ~ "7",
      TRUE ~ NA_character_
    ),
    
    # Factorize all variables
    Responder5 = factor(Responder5, c("1","2"), c("Sample_Person","Proxy")),
    Residential5 = factor(Residential5, c("1","2","3","4","5", special_missing_levels),
                          c("Home/apartment","Retirement community","Assisted living","Nursing home","Other institution", special_missing_labels)),
    NewPerson = factor(NewPerson, yes_no_levels, yes_no_labels),
    Age = factor(Age, c("1","2","3","4","5","6", special_missing_levels),
                 c("65–69","70–74","75–79","80–84","85–89","90+", special_missing_labels)),
    Gender = factor(Gender, c("1","2"), c("Male","Female")),
    Education = factor(Education, c("1","2","3","4","5","6","7","8","9", special_missing_levels),
                       c("No schooling","1–8th grade","9–12 (no diploma)","HS grad",
                         "Vocational","Some college","Associate","Bachelor","Master/PhD", special_missing_labels)),
    Occupation = factor(Occupation, c("1","2","3","4","5","6","7"),
                        c("Management/Professional","Service","Sales/Office","Construction/Farming","Production","Homemaker","Not working/retired")),
    HomeOwnership = factor(HomeOwnership, c("1","2","3", special_missing_levels),
                           c("Own","Rent","Other", special_missing_labels)),
    RetirementStatus = factor(RetirementStatus, c("1","2","3", special_missing_levels),
                              c("Yes","No","Retired", special_missing_labels)),
    Section8 = factor(Section8, yes_no_levels, yes_no_labels),
    Medicaid = factor(Medicaid, yes_no_levels, yes_no_labels),
    FoodAssist1 = factor(FoodAssist1, yes_no_levels, yes_no_labels),
    FoodAssist2 = factor(FoodAssist2, yes_no_levels, yes_no_labels),
    FoodAssist3 = factor(FoodAssist3, yes_no_levels, yes_no_labels),
    ChildhoodHealth = factor(ChildhoodHealth, c("1","2","3","4","5", special_missing_levels),
                             c("Excellent","Very good","Good","Fair","Poor", special_missing_labels)),
    MaritalStatus = factor(MaritalStatus, c("1","2","3","4","5","6", special_missing_levels),
                           c("Married","Living with Partner","Separated","Divorced","Widowed","Never married", special_missing_labels)),
    Newsample = factor(Newsample, yes_no_levels, yes_no_labels),
    RaceEthnicity = factor(RaceEthnicity)
  ) |>
  mutate(Section8 = fct_recode(Section8, "No" = "Inapplicable"))

############################################
# 🔷 Merge Round 1 + 5 (Corrected Naming)
############################################

# Use the clean datasets you created
R5_SP_filtered <- R5_SP_clean
R1_SP_filtered <- R1_SP_clean

# Merge Round 5 with Round 1
R5_merged <- R5_SP_filtered %>%
  left_join(R1_SP_filtered, by = "spid") %>%
  
  # ✅ Fill from R1 if R5 is special missing (-1, -7, -8, -9)
  mutate(
    Responder_final       = if_else(Responder5 %in% special_missing_labels, Responder1, Responder5),
    Residential_final     = if_else(Residential5 %in% special_missing_labels, Residential1, Residential5),
    Age_final             = if_else(Age %in% special_missing_labels, Age1, Age),
    Gender_final          = if_else(Gender %in% special_missing_labels, Gender1, Gender),
    Education_final       = if_else(Education %in% special_missing_labels, Education1, Education),
    Occupation_final      = if_else(Occupation %in% special_missing_labels, Occupation1, Occupation),
    HomeOwnership_final   = if_else(HomeOwnership %in% special_missing_labels, HomeOwnership1, HomeOwnership),
    HouseholdSize_final   = if_else(HouseholdSize %in% special_missing_labels, HouseholdSize1, HouseholdSize),   # ✅ FIXED CAPITALIZATION
    HouseholdIncome_final = if_else(HouseholdIncome %in% special_missing_labels, HouseholdIncome1, HouseholdIncome),
    RetirementStatus_final = if_else(RetirementStatus %in% special_missing_labels, RetirementStatus1, RetirementStatus),
    Section8_final        = if_else(Section8 %in% special_missing_labels, Section81, Section8),
    Medicaid_final        = if_else(Medicaid %in% special_missing_labels, Medicaid1, Medicaid),
    FoodAssist1_final     = if_else(FoodAssist1 %in% special_missing_labels, FoodAssist11, FoodAssist1),
    FoodAssist2_final     = if_else(FoodAssist2 %in% special_missing_labels, FoodAssist21, FoodAssist2),
    FoodAssist3_final     = if_else(FoodAssist3 %in% special_missing_labels, FoodAssist31, FoodAssist3),
    ChildhoodHealth_final = if_else(ChildhoodHealth %in% special_missing_labels, ChildhoodHealth1, ChildhoodHealth),
    RaceEthnicity_final   = if_else(RaceEthnicity %in% special_missing_labels, RaceEthnicity1, RaceEthnicity),
    
    # ✅ Round 5 only variables
    NewPerson_final       = NewPerson,
    MaritalStatus_final   = MaritalStatus,
    Newsample_final       = Newsample
  )

# ✅ Collapse missing levels consistently into one “Missing” level
R5_merged <- R5_merged %>%
  mutate(across(ends_with("_final"), safe_collapse, .names = "{.col}"))

# ✅ Keep only final columns for downstream merges
R5_merged <- R5_merged %>%
  select(spid, ends_with("_final"))


############################################
# 🔷 Round 6 – Clean  
############################################
R6_SP <- read_sas("NHATS_Round_6_SP_File_V2.sas7bdat")

R6_SP_clean <- R6_SP |>
  select(spid, is6resptype, r6dresid,
         ew6mealskip1, ew6nopayhous, ew6nopayutil, ew6nopaymed) |>
  filter(is6resptype == 1 & r6dresid == 1) |>
  rename(
    SkippedMeals = ew6mealskip1,
    UnableToPayRent = ew6nopayhous,
    UnableToPayUtilities = ew6nopayutil,
    UnableToPayMedical = ew6nopaymed
  ) |>
  mutate(
    across(c(SkippedMeals, UnableToPayRent, UnableToPayUtilities, UnableToPayMedical),
           ~ factor(.x, yes_no_levels, yes_no_labels)),
    
    # ✅ Improved logic with explicit Missing category
    FinancialStrainFlag = case_when(
      # If ANY of the four is YES, classify as Any Strain
      SkippedMeals == "Yes" | UnableToPayRent == "Yes" |
        UnableToPayUtilities == "Yes" | UnableToPayMedical == "Yes" ~ "Any Strain",
      
      # If ALL FOUR are explicitly NO, classify as No Strain
      SkippedMeals == "No" & UnableToPayRent == "No" &
        UnableToPayUtilities == "No" & UnableToPayMedical == "No" ~ "No Strain",
      
      # If we get here, at least one answer is missing/inapplicable/refused
      TRUE ~ "Missing"
    ) |> factor(c("No Strain", "Any Strain", "Missing"))
  )

############################################
# 🔷 Round 7 – Clean 
############################################

R7_SP <- read_sas("NHATS_Round_7_SP_File.sas7bdat")

R7_SP_clean <- R7_SP |>
  select(spid, is7resptype, r7dresid,
         hc7disescn9, cg7dwrdimmrc, cg7dwrddlyrc,
         cg7todaydat1, cg7todaydat2, cg7todaydat3, cg7todaydat4,
         cg7presidna1, cg7presidna3, cg7vpname1, cg7vpname3,
         cg7dclkdraw) |>
  filter(r7dresid == 1) |>   # Only include respondents living at home
  rename(
    DementiaDx = hc7disescn9,           # Clinician-verified dementia diagnosis
    MemoryImmediate = cg7dwrdimmrc,     # Immediate recall
    MemoryDelayed = cg7dwrddlyrc,       # Delayed recall
    ExecutiveDraw = cg7dclkdraw         # Clock draw
  ) |>
  mutate(
    # 🧠 MEMORY SCORE: Immediate + Delayed Recall
    # If either is missing, we assign 0 points for that part (as NHATS coding convention).
    memory_score = if_else(MemoryImmediate >= 0, MemoryImmediate, 0) +
      if_else(MemoryDelayed >= 0, MemoryDelayed, 0),
    memory_impaired = memory_score <= 3,  # Binary: <=3 points means impaired
    
    # 🧭 ORIENTATION SCORE: Recode correct/incorrect items (1=correct, 2=incorrect)
    across(c(cg7todaydat1, cg7todaydat2, cg7todaydat3, cg7todaydat4,
             cg7presidna1, cg7presidna3, cg7vpname1, cg7vpname3),
           ~ case_when(.x == 2 ~ 1,     # Incorrect = 1 point (impaired)
                       .x == 1 ~ 0,     # Correct = 0 points
                       TRUE ~ NA_real_), 
           .names = "{.col}_rec"),
    
    orientation_score = rowSums(across(ends_with("_rec")), na.rm = TRUE),
    orientation_impaired = orientation_score >= 5,   # ≥5 wrong answers = impaired
    
    # 🕰 EXECUTIVE FUNCTION: Clock draw task
    exec_impaired = ExecutiveDraw %in% c(0, 1),  # 0/1 scores indicate impairment
    
    # 🏷 Count how many domains (memory/orientation/executive) are impaired
    impaired_domains = rowSums(across(c(memory_impaired, orientation_impaired, exec_impaired)), 
                               na.rm = TRUE),
    
    # ✅ Improved Dementia Classification with Explicit Missingness Handling
    dementia_class = case_when(
      # 1️⃣ **Probable Dementia**: If the NHATS clinician diagnosis says so, trust it
      DementiaDx == 1 ~ "Probable Dementia",
      
      # 2️⃣ **Missing**: If DementiaDx is missing/refused/inapplicable (-1, -7, -8, -9)
      DementiaDx %in% special_missing_numeric ~ "Missing",
      
      # 3️⃣ **Possible Dementia**: No formal diagnosis, but ≥2 impaired domains
      DementiaDx != 1 & impaired_domains >= 2 ~ "Possible Dementia",
      
      # 4️⃣ **No Dementia**: Explicit diagnosis of “No dementia” (code 2),
      # and all three domains are fully observed (none missing) AND <2 impaired domains
      DementiaDx == 2 & impaired_domains < 2 &
        !is.na(memory_impaired) & !is.na(orientation_impaired) & !is.na(exec_impaired) ~ "No Dementia",
      
      # 5️⃣ **Missing**: Anything else (e.g., partial data, can’t confidently classify)
      TRUE ~ "Missing"
    ) |> factor(c("No Dementia", "Possible Dementia", "Probable Dementia", "Missing"))
  )

############################################
# 🔷 Final Merge: R1/5 + R6 + R7
############################################

# 1️⃣ FullData keeps everyone from R6, even if FinancialStrainFlag or dementia_class = "Missing"
FullData <- R6_SP_clean |>
  left_join(R5_merged, by = "spid") |>
  left_join(R7_SP_clean, by = "spid")

# 2️⃣ CleanData removes people missing either the exposure (FinancialStrainFlag) or outcome (dementia_class)
CleanData <- FullData |>
  filter(
    !is.na(FinancialStrainFlag),       # drop rows that are NA (true missing)
    !is.na(dementia_class),            # drop rows that are NA (true missing)
    FinancialStrainFlag != "Missing",  # drop coded "Missing" exposures
    dementia_class != "Missing"        # drop coded "Missing" outcomes
  )
#######################################
#Who was excluded
#######################################
#table(FullData$FinancialStrainFlag, useNA = "ifany")
#table(FullData$dementia_class, useNA = "ifany")

#table(CleanData$FinancialStrainFlag, useNA = "ifany")
#table(CleanData$dementia_class, useNA = "ifany")