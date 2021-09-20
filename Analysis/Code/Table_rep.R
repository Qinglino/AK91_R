yr <- paste("YR", 20:28, sep = "")
controls <- c("RACE", "MARRIED", "SMSA", "NEWENG", "MIDATL",
              "ENOCENT", "WNOCENT", "SOATL", "ESOCENT", "WSOCENT", "MT")
instruments <- c(paste("QTR1", 20:29, sep = ""), paste("QTR2", 20:29, sep = ""),
                 paste("QTR3", 20:29, sep = ""), paste("YR", 20:28, sep = ""))

regression <- function(table_name, cohort){
  # Here I provide two ways of constructing regression
  reg1 <- lm(LWKLYWGE ~ ., #regress WAGE on whatever else
             data = filter(df, COHORT == cohort)[, c("LWKLYWGE", "EDUC", yr)])
  
  reg3 <- lm(LWKLYWGE ~ ., 
             data = filter(df, COHORT == cohort)[, c("LWKLYWGE", "EDUC", yr, "AGEQ", "AGEQSQ")])
                           
  reg5 <- lm(LWKLYWGE ~ ., 
             data = filter(df, COHORT == cohort)[, c("LWKLYWGE", "EDUC", controls, yr)])
  
  reg7 <- lm(LWKLYWGE ~ ., 
             data = filter(df, COHORT == cohort)[, c("LWKLYWGE", "EDUC", controls, yr, "AGEQ", "AGEQSQ")])
  
  reg2 <- paste("LWKLYWGE", "~", sep = "") %>% #construct regression equation
    paste(paste(yr, collapse = "+")) %>% # by concatenating (very tedious)
    paste("+EDUC|") %>% 
    paste(paste(instruments, collapse = "+")) %>% 
    ivreg(data = filter(df, COHORT == cohort))
  
  reg4 <- paste("LWKLYWGE", "~", sep = "") %>% 
    paste(paste(yr, collapse = "+")) %>% 
    paste("+AGEQ+AGEQSQ+EDUC|") %>% 
    paste(paste(instruments, collapse = "+")) %>% 
    ivreg(data = filter(df, COHORT == cohort))
  
  reg6 <- paste("LWKLYWGE", "~", sep = "") %>% 
    paste(paste(yr, collapse = "+")) %>% 
    paste(paste(controls, collapse = "+"), sep = "+") %>% 
    paste("+EDUC|") %>% 
    paste(paste(instruments, collapse = "+")) %>%
    paste(paste(controls, collapse = "+"), sep = "+") %>%
    ivreg(data = filter(df, COHORT == cohort))
  
  reg8 <- paste("LWKLYWGE", "~", sep = "") %>% 
    paste(paste(yr, collapse = "+")) %>% 
    paste(paste(controls, collapse = "+"), sep = "+") %>% 
    paste("+AGEQ+AGEQSQ+EDUC|") %>% 
    paste(paste(instruments, collapse = "+")) %>% 
    ivreg(data = filter(df, COHORT == cohort))
  
  # Prepare for output
  filename <- paste("./Analysis/Output/", table_name, ".tex",
                    sep = "", collapse = "")
  table.title <- case_when(
    cohort == 20.29 ~"OLS and TSLS Estimates of Return to Education for Men Born 
    1920-1929: 1970 Census",
    cohort == 30.39 ~"OLS and TSLS Estimates of Return to Education for Men Born 
    1930-1939: 1980 Census",
    cohort == 40.49 ~"OLS and TSLS Estimates of Return to Education for Men Born 
    1940-1949: 1980 Census")
  
  stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, 
            digits = 4,
            title = table.title,
            keep = c("EDUC", "RACE", "SMSA", "MARRIED", "AGEQ", "AGEQSQ"),
            keep.stat = c("n"),
            out = filename
            )
  }

regression("TableIV", 20.29)
regression("TableV", 30.39)
regression("TableVI", 40.49)
