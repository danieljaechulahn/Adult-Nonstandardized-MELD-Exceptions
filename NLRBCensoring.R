prepolicycohort <- candidatelist %>% filter(POLICY_COHORT == "Pre-Policy")

prepolicycohort <- prepolicycohort %>%
  mutate(
    DEATH_DT = case_when(
      !is.na(PERS_OPTN_DEATH_DT) ~ PERS_OPTN_DEATH_DT,
      is.na(PERS_OPTN_DEATH_DT) & !is.na(PERS_SSA_DEATH_DT) ~ PERS_SSA_DEATH_DT,
      is.na(PERS_OPTN_DEATH_DT) & is.na(PERS_SSA_DEATH_DT) & !is.na(CAN_DEATH_DT) ~ CAN_DEATH_DT))



prepolicycohort$PRE_TX_DEATH <- ifelse(prepolicycohort$CAN_REM_CD == 8 & prepolicycohort$DEATH_DT <= as.POSIXct("2019-05-14") | 
              (prepolicycohort$CAN_REM_CD == 5 & !is.na(prepolicycohort$DEATH_DT) & as.numeric(difftime(prepolicycohort$DEATH_DT, prepolicycohort$CAN_LISTING_DT, units = "days")) <= 180) & prepolicycohort$DEATH_DT <= as.POSIXct("2019-05-14") |
              (prepolicycohort$CAN_REM_CD == 6 & !is.na(prepolicycohort$DEATH_DT) & as.numeric(difftime(prepolicycohort$DEATH_DT, prepolicycohort$CAN_LISTING_DT, units = "days")) <= 180) & prepolicycohort$DEATH_DT <= as.POSIXct("2019-05-14") |
              (prepolicycohort$CAN_REM_CD == 7 & !is.na(prepolicycohort$DEATH_DT) & as.numeric(difftime(prepolicycohort$DEATH_DT, prepolicycohort$CAN_LISTING_DT, units = "days")) <= 180) & prepolicycohort$DEATH_DT <= as.POSIXct("2019-05-14") |
              (prepolicycohort$CAN_REM_CD == 9 & !is.na(prepolicycohort$DEATH_DT) & as.numeric(difftime(prepolicycohort$DEATH_DT, prepolicycohort$CAN_LISTING_DT, units = "days")) <= 180) & prepolicycohort$DEATH_DT <= as.POSIXct("2019-05-14") |
              (prepolicycohort$CAN_REM_CD == 10 & !is.na(prepolicycohort$DEATH_DT) & as.numeric(difftime(prepolicycohort$DEATH_DT, prepolicycohort$CAN_LISTING_DT, units = "days")) <= 180) & prepolicycohort$DEATH_DT <= as.POSIXct("2019-05-14") |
              (prepolicycohort$CAN_REM_CD == 11 & !is.na(prepolicycohort$DEATH_DT) & as.numeric(difftime(prepolicycohort$DEATH_DT, prepolicycohort$CAN_LISTING_DT, units = "days")) <= 180) & prepolicycohort$DEATH_DT <= as.POSIXct("2019-05-14") |
              (prepolicycohort$CAN_REM_CD == 12 & !is.na(prepolicycohort$DEATH_DT) & as.numeric(difftime(prepolicycohort$DEATH_DT, prepolicycohort$CAN_LISTING_DT, units = "days")) <= 180) & prepolicycohort$DEATH_DT <= as.POSIXct("2019-05-14") |
              (prepolicycohort$CAN_REM_CD == 13 & !is.na(prepolicycohort$DEATH_DT) & as.numeric(difftime(prepolicycohort$DEATH_DT, prepolicycohort$CAN_LISTING_DT, units = "days")) <= 180) & prepolicycohort$DEATH_DT <= as.POSIXct("2019-05-14") |
              (prepolicycohort$CAN_REM_CD == 16 & !is.na(prepolicycohort$DEATH_DT) & as.numeric(difftime(prepolicycohort$DEATH_DT, prepolicycohort$CAN_LISTING_DT, units = "days")) <= 180) & prepolicycohort$DEATH_DT <= as.POSIXct("2019-05-14") |
              (prepolicycohort$CAN_REM_CD == 17 & !is.na(prepolicycohort$DEATH_DT) & as.numeric(difftime(prepolicycohort$DEATH_DT, prepolicycohort$CAN_LISTING_DT, units = "days")) <= 180) & prepolicycohort$DEATH_DT <= as.POSIXct("2019-05-14") |
              (prepolicycohort$CAN_REM_CD == 20 & !is.na(prepolicycohort$DEATH_DT) & as.numeric(difftime(prepolicycohort$DEATH_DT, prepolicycohort$CAN_LISTING_DT, units = "days")) <= 180) & prepolicycohort$DEATH_DT <= as.POSIXct("2019-05-14") |
              (prepolicycohort$CAN_REM_CD == 24 & !is.na(prepolicycohort$DEATH_DT) & as.numeric(difftime(prepolicycohort$DEATH_DT, prepolicycohort$CAN_LISTING_DT, units = "days")) <= 180) & prepolicycohort$DEATH_DT <= as.POSIXct("2019-05-14"), 1, 0)

prepolicycohort <- prepolicycohort %>% mutate(PRE_TX_DEATH = ifelse(is.na(PRE_TX_DEATH), 0, PRE_TX_DEATH))

#p1$MULTI_LISTING_DT <- as.Date(p1$MULTI_LISTING_DT, origin = "1970-01-01")

prepolicycohort$DEATH_DT <- as.Date(prepolicycohort$DEATH_DT, origin = "1970-01-01")
prepolicycohort$REC_TX_DT <- as.Date(prepolicycohort$REC_TX_DT, origin = "1970-01-01")

prepolicycohort <- prepolicycohort %>% mutate(
  CAN_LAST_DT = case_when(
    CAN_REM_DT <= as.POSIXct("2019-05-14") ~ CAN_REM_DT,
    CAN_REM_DT > as.POSIXct("2019-05-14") ~ as.POSIXct("2019-05-14"),
    TRUE ~ CAN_REM_DT),
  CENSOR_CD = case_when(
    CAN_REM_DT <= as.POSIXct("2019-05-14") ~ 1,
    CAN_REM_DT > as.POSIXct("2019-05-14") ~ 2,
    TRUE ~ 3),
  CAN_REM_CD = case_when(
    CENSOR_CD == 1 ~ CAN_REM_CD,
    CENSOR_CD == 2 ~ NA,
    CENSOR_CD == 3 ~ CAN_REM_CD),
  MULTI_ORGAN_LISTING_DT = case_when(
    MULTI_ORGAN_LISTING_DT <= as.POSIXct("2019-05-14") ~ MULTI_ORGAN_LISTING_DT,
    MULTI_ORGAN_LISTING_DT > as.POSIXct("2019-05-14") ~ as.POSIXct("2019-05-14"),
    TRUE ~ MULTI_ORGAN_LISTING_DT),
  MULTI_ORGAN_CENSOR_CD = case_when(
    MULTI_ORGAN_LISTING_DT <= as.POSIXct("2019-05-14") ~ 1,
    MULTI_ORGAN_LISTING_DT > as.POSIXct("2019-05-14") ~ 2,
    TRUE ~ 3),
  MULTI_ORGAN = case_when(
    MULTI_ORGAN_CENSOR_CD == 1 ~ MULTI_ORGAN,
    MULTI_ORGAN_CENSOR_CD == 2 ~ 0,
    MULTI_ORGAN_CENSOR_CD == 3 ~ MULTI_ORGAN
  )
)

prepolicycohort <- prepolicycohort %>% mutate(REMOVAL_DATE = case_when(CAN_REM_CD == 8 & DEATH_DT <= as.POSIXct("2019-05-14") |
                   (CAN_REM_CD == 5 & !is.na(DEATH_DT) & as.numeric(difftime(DEATH_DT, CAN_LISTING_DT, units = "days")) <= 180) & DEATH_DT <= as.POSIXct("2019-05-14") |
                   (CAN_REM_CD == 6 & !is.na(DEATH_DT) & as.numeric(difftime(DEATH_DT, CAN_LISTING_DT, units = "days")) <= 180) & DEATH_DT <= as.POSIXct("2019-05-14") |
                   (CAN_REM_CD == 7 & !is.na(DEATH_DT) & as.numeric(difftime(DEATH_DT, CAN_LISTING_DT, units = "days")) <= 180) & DEATH_DT <= as.POSIXct("2019-05-14") |
                   (CAN_REM_CD == 9 & !is.na(DEATH_DT) & as.numeric(difftime(DEATH_DT, CAN_LISTING_DT, units = "days")) <= 180) & DEATH_DT <= as.POSIXct("2019-05-14") |
                   (CAN_REM_CD == 10 & !is.na(DEATH_DT) & as.numeric(difftime(DEATH_DT, CAN_LISTING_DT, units = "days")) <= 180) & DEATH_DT <= as.POSIXct("2019-05-14") |
                   (CAN_REM_CD == 12 & !is.na(DEATH_DT) & as.numeric(difftime(DEATH_DT, CAN_LISTING_DT, units = "days")) <= 180) & DEATH_DT <= as.POSIXct("2019-05-14") |
                   (CAN_REM_CD == 13 & !is.na(DEATH_DT) & as.numeric(difftime(DEATH_DT, CAN_LISTING_DT, units = "days")) <= 180) & DEATH_DT <= as.POSIXct("2019-05-14") |
                   (CAN_REM_CD == 16 & !is.na(DEATH_DT) & as.numeric(difftime(DEATH_DT, CAN_LISTING_DT, units = "days")) <= 180) & DEATH_DT <= as.POSIXct("2019-05-14") |
                   (CAN_REM_CD == 17 & !is.na(DEATH_DT) & as.numeric(difftime(DEATH_DT, CAN_LISTING_DT, units = "days")) <= 180) & DEATH_DT <= as.POSIXct("2019-05-14") |
                   (CAN_REM_CD == 20 & !is.na(DEATH_DT) & as.numeric(difftime(DEATH_DT, CAN_LISTING_DT, units = "days")) <= 180) & DEATH_DT <= as.POSIXct("2019-05-14") |
                   (CAN_REM_CD == 24 & !is.na(DEATH_DT) & as.numeric(difftime(DEATH_DT, CAN_LISTING_DT, units = "days")) <= 180) & DEATH_DT <= as.POSIXct("2019-05-14") ~ DEATH_DT,
                   TRUE ~ CAN_LAST_DT))

prepolicycohort$REMOVAL_DATE <- as.Date(prepolicycohort$REMOVAL_DATE, origin = "1970-01-01")

prepolicycohort <- prepolicycohort %>%
  mutate(
    LASTDATE = case_when(
      is.na(REMOVAL_DATE) ~ as.POSIXct("2019-05-14"),
      TRUE ~ REMOVAL_DATE
    )
  )

prepolicycohort <- prepolicycohort %>%
  mutate(
    REMOVAL_CAUSE = case_when(
      MULTI_ORGAN == 2 ~ 0,
      MULTI_ORGAN != 2 & PRE_TX_DEATH == 1 ~ 1,
      MULTI_ORGAN != 2 & (CAN_REM_CD == 4 | CAN_REM_CD == 14 | CAN_REM_CD == 21) ~ 2,
      MULTI_ORGAN != 2 & is.na(CAN_REM_CD) ~ 3,
      TRUE ~ 4
    )
  )

prepolicycohort <- prepolicycohort %>%
  mutate(
    TRANSPLANT = case_when(
      MULTI_ORGAN != 2 & (CAN_REM_CD == 4 | CAN_REM_CD == 14 | CAN_REM_CD == 21) ~ 1,
      TRUE ~ 0)
  )

prepolicycohort <- prepolicycohort %>%
  mutate(
    FINAL_REMOVAL_DATE = case_when(
      REMOVAL_CAUSE == 0 ~ MULTI_ORGAN_LISTING_DT,
      REMOVAL_CAUSE == 1 ~ DEATH_DT,
      REMOVAL_CAUSE == 2 ~ CAN_LAST_DT,
      REMOVAL_CAUSE == 3 ~ as.POSIXct("2019-05-14"),
      REMOVAL_CAUSE == 4 ~ CAN_LAST_DT
    )
  )

prepolicycohort <- prepolicycohort %>%
  mutate(TIME_TO_REMOVAL = as.numeric(difftime(FINAL_REMOVAL_DATE, CAN_LISTING_DT, units = "days")),
         TIME_TO_REMOVAL_YEARS = TIME_TO_REMOVAL / 365.25) %>%
  dplyr::select(-c("CAN_LAST_DT", "CENSOR_CD", "MULTI_ORGAN_CENSOR_CD"))

postpolicycohort <- candidatelist %>% filter(POLICY_COHORT == "Post-Policy")

nlrbcensoredcohort <- rbind(prepolicycohort, postpolicycohort)

institution <- institution2024q1 %>% 
  rename(CAN_LISTING_CTR_CD = CTR_CD)

nlrbcensoredcohort <- merge(x = nlrbcensoredcohort, y = institution, by = "CAN_LISTING_CTR_CD", all.x = TRUE) %>%
  filter(CTR_TY == "TX1")




nlrbcensoredcohort <- merge(x = nlrbcensoredcohort, y = stathist_liin2024q1, by = "PX_ID", all.x = TRUE)

nlrbcensoredcohort <- nlrbcensoredcohort %>% 
  rename(CAN_INIT_ACT_STAT_DT = CAN_INIT_ACT_STAT_DT.x, 
         CAN_INIT_ACT_STAT_CD = CAN_INIT_ACT_STAT_CD.x, CAN_LAST_STAT = CAN_LAST_STAT.x,
         CAN_LAST_ACT_STAT_DT = CAN_LAST_ACT_STAT_DT.x, CAN_GENDER = CAN_GENDER.x,
         CAN_INIT_SRTR_LAB_MELD = CAN_INIT_SRTR_LAB_MELD.x,
         CAN_REM_DT = CAN_REM_DT.x, 
         CAN_LISTING_DT = CAN_LISTING_DT.x, 
         CAN_REM_CD = CAN_REM_CD.x)

nlrbcensoredcohort <- dplyr::select(nlrbcensoredcohort, "PX_ID", "PERS_ID", "POLICY_COHORT", "REC_TX_DT", "CAN_REM_DT", "CAN_REM_CD", 
                            "CAN_REM_COD", "CAN_LISTING_DT", "CANHX_BEGIN_DT", "CANHX_END_DT", "CAN_DEATH_DT",
                            "PERS_OPTN_DEATH_DT", "PERS_SSA_DEATH_DT", "CANHX_EXC_FLG", "CAN_MED_COND", 
                            "CANHX_EXC_SCORE", "CANHX_EXC_DIAG_OTHER", "CANHX_OPTN_LAB_MELD", 
                            "CANHX_SRTR_LAB_MELD", "CANHX_STAT_CD", "CANHX_MELD_DIFF_REASON_CD", "CANHX_EXC_DIAG_HCC1",
                            "CANHX_EXC_DIAG_HCC2", "CANHX_EXC_DIAG_HCC_NOPOLICY", "CAN_INIT_ACT_STAT_DT", 
                            "CAN_AGE_AT_LISTING", "CAN_INIT_ACT_STAT_CD", "CAN_INIT_SRTR_LAB_MELD",
                            "CAN_LAST_STAT", "CAN_LAST_ACT_STAT_DT", "CAN_LISTING_CTR_CD", "TRANSPLANT",
                            "CAN_DGN", "CAN_DGN_OSTXT", "CAN_GENDER", "CAN_ABO", "CAN_RACE", "CAN_ETHNICITY_SRTR", "REGION",
                            "CAN_FUNCTN_STAT", "CAN_AGE_IN_MONTHS_AT_LISTING", "CAN_PRIMARY_PAY", "CAN_BMI",
                            "DEATH_DT", "PRE_TX_DEATH", "REMOVAL_DATE", "LASTDATE", "REMOVAL_CAUSE",
                            "FINAL_REMOVAL_DATE", "TIME_TO_REMOVAL", "TIME_TO_REMOVAL_YEARS", 
                            "CANHX_BILI", "CANHX_SERUM_CREAT", "CANHX_DIAL_PRIOR_WEEK",
                            "CANHX_INR", "CANHX_SERUM_SODIUM")

nlrbcensoredcohort <- nlrbcensoredcohort %>% group_by(PX_ID) %>% arrange(CANHX_BEGIN_DT) %>% mutate(CANHX_MELD_DIFF_REASON_CD = ifelse(is.na(CANHX_MELD_DIFF_REASON_CD), 0, CANHX_MELD_DIFF_REASON_CD))
                                                                                    

#this code lets us censor patients when they obtain Status 1

status1 <- nlrbcensoredcohort %>%
  group_by(PX_ID) %>%
  arrange(PX_ID, CANHX_BEGIN_DT) %>%
  filter(any(CANHX_STAT_CD == 6011)) %>%
  mutate(STATUS1 = as.numeric(row_number() == min(row_number()[CANHX_STAT_CD == 6011]))) %>%
  filter(row_number() <= which.max(STATUS1) | all(STATUS1 == 0)) %>%
  filter(row_number() <= n()-1) %>%
  mutate(PRE_TX_DEATH = ifelse(PRE_TX_DEATH == 1, 0, PRE_TX_DEATH)) %>%
  ungroup()
status1$STATUS1 <- NULL

nonstatus1 <- nlrbcensoredcohort %>%
  group_by(PX_ID) %>%
  arrange(PX_ID, CANHX_BEGIN_DT) %>%
  filter(!any(CANHX_STAT_CD == 6011)) %>%
  ungroup()

nlrbcensoredcohort <- rbind(status1, nonstatus1) %>% group_by(PX_ID) %>% arrange(PX_ID, CANHX_BEGIN_DT)

nlrbcensoredcohort <- nlrbcensoredcohort %>%
  mutate(
    EXCEPTION = case_when(
      ((CANHX_MELD_DIFF_REASON_CD == 3 | CANHX_MELD_DIFF_REASON_CD == 7) & CANHX_EXC_FLG == 1) ~ 1,
      TRUE ~ 0),
    ALLOCATION_SCORE = case_when(
      CANHX_STAT_CD == 6999 & ((CANHX_OPTN_LAB_MELD - 6200) > 40) ~ 40,
      CANHX_STAT_CD == 6999 & ((CANHX_OPTN_LAB_MELD - 6200) <= 40) ~ CANHX_OPTN_LAB_MELD - 6200,
      is.na(CANHX_OPTN_LAB_MELD) & !is.na(CANHX_SRTR_LAB_MELD) & ((CANHX_SRTR_LAB_MELD - 6200) <= 40) ~ CANHX_SRTR_LAB_MELD - 6200,
      TRUE ~ CANHX_STAT_CD - 6200),
    LAB_SCORE = case_when(
      (!is.na(CANHX_OPTN_LAB_MELD)) & (CANHX_OPTN_LAB_MELD - 6200 > 40) ~ 40,
      is.na(CANHX_OPTN_LAB_MELD) ~ CANHX_SRTR_LAB_MELD - 6200,
      TRUE ~ CANHX_OPTN_LAB_MELD - 6200))


nlrbcensoredcohorttimeseries <- nlrbcensoredcohort %>% group_by(PX_ID) %>% arrange(PX_ID, CANHX_BEGIN_DT) %>%
  mutate(TIME1 = as.numeric(difftime(CANHX_BEGIN_DT, CAN_LISTING_DT, units = "days")),
         TIME2 = as.numeric(difftime(CANHX_END_DT, CAN_LISTING_DT, units = "days")) + 1,
         TIME2 = case_when(
           is.na(TIME2) ~ as.numeric(difftime(FINAL_REMOVAL_DATE, CAN_LISTING_DT, units = "days")),
           TRUE ~ TIME2),
         sex = ifelse(CAN_GENDER == "M", "Male", "Female"),
         sex = factor(sex, levels = c("Male", "Female")),
         bmi = case_when(
           CAN_BMI < 18 ~ "Underweight",
           CAN_BMI >= 18 & CAN_BMI < 25 ~ "Normal",
           CAN_BMI >= 25 & CAN_BMI < 30 ~ "Overweight",
           CAN_BMI >= 30 ~ "Obese",
           TRUE ~ "Unknown"),
         bmi = factor(bmi, levels = c("Normal", "Underweight", "Overweight", "Obese", "Unknown")),
         yearoflisting = case_when(
           CAN_LISTING_DT >= as.POSIXct("2016-06-01") & CAN_LISTING_DT <= as.POSIXct("2016-12-31") ~ "2016",
           CAN_LISTING_DT >= as.POSIXct("2017-01-01") & CAN_LISTING_DT <= as.POSIXct("2017-12-31") ~ "2017",
           CAN_LISTING_DT >= as.POSIXct("2018-01-01") & CAN_LISTING_DT <= as.POSIXct("2018-12-31") ~ "2018",
           CAN_LISTING_DT >= as.POSIXct("2019-01-01") & CAN_LISTING_DT <= as.POSIXct("2019-12-31") ~ "2019",
           CAN_LISTING_DT >= as.POSIXct("2020-01-01") & CAN_LISTING_DT <= as.POSIXct("2020-12-31") ~ "2020",
           CAN_LISTING_DT >= as.POSIXct("2021-01-01") & CAN_LISTING_DT <= as.POSIXct("2021-12-31") ~ "2021",
           CAN_LISTING_DT >= as.POSIXct("2022-01-01") & CAN_LISTING_DT <= as.POSIXct("2022-04-30") ~ "2022"),
         yearoflisting = factor(yearoflisting, levels = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")),
         race = case_when(
           is.na(CAN_RACE) & CAN_ETHNICITY_SRTR == "LATINO" ~ "Hispanic/Latino",
           CAN_RACE == 8 & CAN_ETHNICITY_SRTR == "NLATIN" ~ "White",
           CAN_RACE == 16 & CAN_ETHNICITY_SRTR == "NLATIN" ~ "Black",
           CAN_RACE == 64 & CAN_ETHNICITY_SRTR == "NLATIN" ~ "Asian",
           TRUE ~ "Other"),
         race = factor(race, levels = c("White", "Black", "Hispanic/Latino", "Asian", "Other")),
         age = CAN_AGE_AT_LISTING,
         functional = case_when(
           (CAN_FUNCTN_STAT == 2010 | CAN_FUNCTN_STAT == 2020 | CAN_FUNCTN_STAT == 2030 | CAN_FUNCTN_STAT == 2040) ~ "Low",
           (CAN_FUNCTN_STAT == 2050 | CAN_FUNCTN_STAT == 2060 | CAN_FUNCTN_STAT == 2070) ~ "Intermediate",
           (CAN_FUNCTN_STAT == 2080 | CAN_FUNCTN_STAT == 2090 | CAN_FUNCTN_STAT == 2100) ~ "High",
           TRUE ~ "Unknown"),
         functional = factor(functional, levels = c("High", "Intermediate", "Low", "Unknown")),
         payor = case_when(
           CAN_PRIMARY_PAY %in% c(2,3,4,5,6,7,13) ~ "Public",
           CAN_PRIMARY_PAY == 1 ~ "Private",
           TRUE ~ "Other"),
         payor = factor(payor, levels = c("Private", "Public", "Other")))

nlrbcensoredcohorttimeseries1 <- nlrbcensoredcohorttimeseries %>% ungroup() %>% mutate(
  DELETE = case_when(
    (TIME1 > TIME2) & is.na(CANHX_END_DT) ~ 1,
    TRUE ~ 0),
  SCORE_DIFFERENCE = case_when(
    EXCEPTION == 1 ~ ALLOCATION_SCORE - LAB_SCORE,
    TRUE ~ 0
  )) %>% subset(DELETE == 0) %>% group_by(PX_ID) %>% arrange(PX_ID, CANHX_BEGIN_DT)

deaths <- nlrbcensoredcohorttimeseries1 %>% filter(any(PRE_TX_DEATH == 1)) %>% 
  filter(row_number() == n()) %>% mutate(NEW_DEATH = 1)
nodeaths <- nlrbcensoredcohorttimeseries1 %>% filter(any(PRE_TX_DEATH == 1)) %>% 
  filter(row_number() != n()) %>% mutate(NEW_DEATH = 0)
nodeaths1 <- nlrbcensoredcohorttimeseries1 %>% filter(!any(PRE_TX_DEATH == 1)) %>% mutate(NEW_DEATH = 0)

nlrbcensoredcohorttimeseries1 <- rbind(deaths, nodeaths, nodeaths1) %>% 
  group_by(PX_ID) %>% arrange(PX_ID, CANHX_BEGIN_DT)

transplants <- nlrbcensoredcohorttimeseries1 %>% filter(any(TRANSPLANT == 1)) %>% 
  filter(row_number() == n()) %>% mutate(TX = 1)
notransplants <- nlrbcensoredcohorttimeseries1 %>% filter(any(TRANSPLANT == 1)) %>% 
  filter(row_number() != n()) %>% mutate(TX = 0)
notransplants1 <- nlrbcensoredcohorttimeseries1 %>% filter(!any(TRANSPLANT == 1)) %>% mutate(TX = 0)

nlrbcensoredcohorttimeseries1 <- rbind(transplants, notransplants, notransplants1) %>%
  group_by(PX_ID) %>% arrange(PX_ID, CANHX_BEGIN_DT) 


nlrbcensoredcohorttimeseries1 <- nlrbcensoredcohorttimeseries1 %>% mutate(REC_EXCEPTION = ifelse((any(EXCEPTION == 1)), 1, 0)) %>% mutate(REC_TX = ifelse((any(TX == 1)), 1, 0)) %>%
  mutate(EXC_WITHIN_WK = case_when(
    any(EXCEPTION == 1 & TIME1 < 8) ~ 1,
    TRUE ~ 0)) 

nlrbcensoredcohorttimeseries1 <- nlrbcensoredcohorttimeseries1 %>% group_by(PX_ID) %>% arrange(PX_ID, CANHX_BEGIN_DT) %>% mutate(Count = n())

exceptionwithinweek <- nlrbcensoredcohorttimeseries1 %>% filter(!any(EXCEPTION == 1 & TIME1 < 8)) %>%
  filter(row_number() == 1)

nlrbcensoredcohorttimeseries2 <- nlrbcensoredcohorttimeseries1 %>% ungroup() %>% group_by(PX_ID) %>% arrange(PX_ID, desc(EXCEPTION), TIME1)

notexceptionwithinweek <- nlrbcensoredcohorttimeseries2 %>% filter(any(EXCEPTION == 1 & TIME1 < 8)) %>% 
  filter(row_number() == 1) 

nlrbfinaldataset <- rbind(exceptionwithinweek, notexceptionwithinweek)
