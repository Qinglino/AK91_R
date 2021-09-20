input <- read_dta("./Build/Input/NEW7080.dta")

df<- input %>% 
  select(c("v1", "v2", "v4", "v5", "v6", "v9", "v10", 
           "v11", "v12", "v13", "v16", "v18", "v19",
           "v20", "v21", "v24", "v25", "v27")) %>% 
  rename(AGE = v1) %>% 
  rename(AGEQ = v2) %>% 
  rename(EDUC = v4) %>% 
  rename(ENOCENT = v5) %>% 
  rename(ESOCENT = v6) %>% 
  rename(LWKLYWGE = v9) %>% 
  rename(MARRIED = v10) %>% 
  rename(MIDATL = v11) %>% 
  rename(MT = v12) %>% 
  rename(NEWENG = v13) %>% 
  rename(CENSUS = v16) %>% 
  rename(QOB = v18) %>% 
  rename(RACE = v19) %>% 
  rename(SMSA = v20) %>% 
  rename(SOATL = v21) %>% 
  rename(WNOCENT = v24) %>% 
  rename(WSOCENT = v25) %>% 
  rename(YOB = v27) %>% 
  mutate(COHORT = 20.29) %>% 
  mutate(COHORT = case_when(
    (YOB>=30)&(YOB<=39) ~ 30.39,
    (YOB>=40)&(YOB<=49) ~ 40.49,
    TRUE ~ as.numeric(COHORT))) %>% 
  mutate(AGEQ = if_else(CENSUS == 80, AGEQ-1900, AGEQ)) %>% 
  mutate(AGEQSQ = AGEQ * AGEQ) %>% 
  mutate(YR20 = if_else(YOB %% 10 == 0, 1, 0)) %>% 
  mutate(YR21 = if_else(YOB %% 10 == 1, 1, 0)) %>%
  mutate(YR22 = if_else(YOB %% 10 == 2, 1, 0)) %>%
  mutate(YR23 = if_else(YOB %% 10 == 3, 1, 0)) %>%
  mutate(YR24 = if_else(YOB %% 10 == 4, 1, 0)) %>%
  mutate(YR25 = if_else(YOB %% 10 == 5, 1, 0)) %>%
  mutate(YR26 = if_else(YOB %% 10 == 6, 1, 0)) %>%
  mutate(YR27 = if_else(YOB %% 10 == 7, 1, 0)) %>%
  mutate(YR28 = if_else(YOB %% 10 == 8, 1, 0)) %>%
  mutate(YR29 = if_else(YOB %% 10 == 9, 1, 0)) %>%
  mutate(QTR1 = if_else(QOB == 1, 1, 0)) %>%
  mutate(QTR2 = if_else(QOB == 2, 1, 0)) %>%
  mutate(QTR3 = if_else(QOB == 3, 1, 0)) %>%
  mutate(QTR4 = if_else(QOB == 4, 1, 0))

# Generate YOB*QOB dummies
for (y in 20:29) {
  for (q in 1:4) {
    colname <- paste("QTR", q, y, sep = "")
    colvalue <- df[, which(colnames(df) == paste("QTR", q, sep = ""))] * 
      df[, which(colnames(df) == paste("YR", y, sep = ""))]
    colnames(colvalue) <- colname
    df <- bind_cols(df, colvalue)
  }
}

rm(colvalue)
rm(colname)
save(df, file = "./Analysis/Input/AK91_cleaned.RData", ascii = TRUE)
# Data saving is optional, df will be still accessible in this session