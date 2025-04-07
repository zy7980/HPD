rm(list=ls())
library(rio)
library(openxlsx)
library(tidyverse)
library(compareGroups)
library(survival)
library(survminer)
library(patchwork)
library(grid)
library(forestploter)
library(ggrastr)
setwd("E:/data")

#################################
### imput data
dataA <- read.xlsx("dataA.xlsx",sheet = "Sheet1")

dataB <- dataA %>% mutate(time_PFS = `time-PFS`/30,
                          time_OS = `time-os`/30,
                          PFS_status = `PFS-status`,
                          OS_status = `os-status`,
                          treat = factor(treatment.mode, levels = c("LNP","NMT","MTL","MTH"), labels = c(1,2,3,4)),
                          treat_1 = ifelse(treat==1,1,ifelse(treat==2,2,3))##LNP,NMT,MT
)

###############################Cox——LNP,NMT,MT
dataB <- dataB %>%
  mutate(treat_1 = factor(treat_1, levels = c(1, 2, 3)))

# 
run_cox_analysis <- function(group1, group2, data) {
  # 
  data_subset <- data %>%
    filter(treat_1 %in% c(group1, group2)) %>%
    mutate(treat_1 = relevel(treat_1, ref = as.character(group1))) # 
  
  # 
  cox_model <- coxph(Surv(time_OS, OS_status) ~ treat_1, data = data_subset)
  
  # 
  summary(cox_model)
}

comparisons <- list(c(1, 2), c(1, 3), c(2, 3))

# 
for (comparison in comparisons) {
  cat("Comparison between", comparison[1], "and", comparison[2], ":\n")
  print(run_cox_analysis(comparison[1], comparison[2], dataB))
  cat("\n")
}

###############################log rank p——LNP,NMT,MT
# 
dataB <- dataB %>%
  mutate(treat_1 = factor(treat_1, levels = c(1, 2, 3)))

# 
run_logrank_test <- function(group1, group2, data) {
  # 
  data_subset <- data %>% filter(treat_1 %in% c(group1, group2))
  
  # 
  surv_diff <- survdiff(Surv(time_OS, OS_status) ~ treat_1, data = data_subset)
  
  # 
  p_value <- 1 - pchisq(surv_diff$chisq, df = length(surv_diff$n) - 1)
  
  # 
  return(p_value)
}

# 
comparisons <- list(c(1, 2), c(1, 3), c(2, 3))

# 
for (comparison in comparisons) {
  cat("Log-rank P-value for groups", comparison[1], "and", comparison[2], ":\n")
  p_value <- run_logrank_test(comparison[1], comparison[2], dataB)
  print(p_value)
  cat("\n")
}

###############################KM——LNP,NMT,MT
fit_OS <- survfit(Surv(time_OS,OS_status)~treat_1, data = dataB)
print(fit_OS)
##KM_LNP,NMT,MT
OS_KM_3 <- ggsurvplot(fit_OS,
                      data = dataB,
                      #conf.int = TRUE,
                      #pval = " P <0.001",pval.coord=c(1.3,0.025),
                      pval = TRUE, 
                      #surv.median.line = "hv",
                      risk.table = TRUE,
                      risk.table.height =.3,
                      xlab = "Time (months)",
                      ylab = "Overall survival",
                      legend = "top",
                      legend.title = "",
                      legend.labs = c("LNT","NMT","MT"),
                      break.x.by = 3, 
                      palette = c("steelblue1", "tan2","firebrick3")
)%>%rasterize(layers='Point',dpi=300,scale=0.8)

OS_KM_3