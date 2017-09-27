library(dplyr)
library(tableone)
library(Matching)

binarize <- function(x, cat)
    return(as.numeric(x == cat))

rhc <- readRDS("rhc.rds")
df <- model.matrix(~ cat1 - 1, rhc) %>%
    tbl_df()

names(df) <- c('ARF', 'CHF', 'Cirr', 'colcan', 'Coma', 'COPD',
               'lungcan', 'MOSF', 'sepsis')

df <- mutate(df,
             age = rhc$age,
             meanbp1 = rhc$meanbp1,
             treatment = binarize(rhc$swang1, 'RHC'),
             died = binarize(rhc$death, 'Yes'))

outcome <- "died"
treatment <- "treatment"
xvars <- setdiff(names(df), c(outcome, treatment))

table1 <- CreateTableOne(xvars, treatment, df, test = FALSE)
print(table1, smd = TRUE)

greedy_match <- Match(Tr = pull(df, treatment), M = 1, X = df[, xvars])
matched_df <- df[unlist(greedy_match[c("index.treated", "index.control")]),]
matched_table1 <- CreateTableOne(xvars, treatment, matched_df, test = FALSE)

