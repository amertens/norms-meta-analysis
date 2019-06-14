

rm(list=ls())
library(tidyverse)
library(metafor)
library(readxl)
library(here)
theme_set(theme_bw())

source(paste0(here(),"/norms_meta_analysis_functions.R"))



#-------------------------------------------
#Clean SN 0nly
#-------------------------------------------

d.SN <- read_excel(paste0(here(), "/data/Meta Analysis Clean Coding Sheet.xlsx"), sheet = "SN Only")

#fill in empty rows
d.SN <- d.SN %>% fill("Folder","Paper title","Reason for not including",
                      "Paper Year","Authors","Category",
                      "Behavioral Intention(s)","N (number surveyed)")

#clean up for meta-analysis
colnames(d.SN)
d.SN <- d.SN %>% select(Category, `Paper title`, `Effect Size of Subjective Norms`, `P-value`, `Significant?`, `Standard Deviation`, `Standard Error`,`T statistic`, `Additional Notes`) %>%
  rename(study = `Paper title`,
         cat = Category,
         est = `Effect Size of Subjective Norms`,
         pval = `P-value`,
         pcat = `Significant?`,
         sd = `Standard Deviation`,
         se = `Standard Error`,
         tstat = `T statistic`,
         Notes = `Additional Notes`)

#Clean category variable
unique(d.SN$cat)
d.SN <- d.SN %>% mutate(
  cat = case_when(cat %in% c("Green consumerism", "Green Consumerism") ~ "Green consumerism",
                  cat %in% c("Household conservation", "Household Conservation") ~ "Household conservation",
                  TRUE ~ cat))

#Drop unstandardized estimates
table(d.SN$Notes)
d.SN <- d.SN %>% filter(is.na(Notes) | Notes != "unstandardized")

#-------------------------------------------
#Clean SN and PN 0nly
#-------------------------------------------

d.PN <- read_excel(paste0(here(), "/data/Meta Analysis Clean Coding Sheet.xlsx"), sheet = "SN and PN")

#fill in empty rows
d.PN <- d.PN %>% fill("Folder","Paper title","Reason for not including",
                      "Paper Year","Authors","Category",
                      "N (number surveyed)")

#clean up for meta-analysis
colnames(d.PN)
d.PN <- d.PN %>% select(Category, `Paper title`,`Type of Norm`, `Effect Size`, `Significant?`, `Standard Deviation`, `Standard Error`,`T-Value`, `Notes`) %>%
  rename(study = `Paper title`,
         cat = Category,
         normcat = `Type of Norm`,
         est = `Effect Size`,
         pcat = `Significant?`,
         sd = `Standard Deviation`,
         se = `Standard Error`,
         tstat = `T-Value`,
         Notes = `Notes`) %>%
  mutate(pval=NA)

#Clean category variable
unique(d.PN$cat)
d.PN <- d.PN %>% mutate(
  cat = case_when(cat %in% c("Green consumerism", "Green Consumerism") ~ "Green consumerism",
                  cat %in% c("Household conservation", "Household Conservation","Household Conservation (?)") ~ "Household conservation",
                  cat %in% c("Everyday Public Conservation", "everyday public conservation", "Everyday public conservation") ~ "Everyday Public Conservation",
                  TRUE ~ cat))

#Drop unstandardized estimates
table(d.PN$Notes)
d.PN <- d.PN %>% filter(is.na(Notes) | Notes != "unstandardized")

#-------------------------------------------
#Clean SN and DN 0nly
#-------------------------------------------

d.DN <- read_excel(paste0(here(), "/data/Meta Analysis Clean Coding Sheet.xlsx"), sheet = "SN and DN")

#fill in empty rows
d.DN <- d.DN %>% fill("Folder","Paper title","Reason for not including",
                      "Paper Year","Authors","Category",
                      "N (number surveyed)")

#clean up for meta-analysis
colnames(d.DN)
d.DN <- d.DN %>% select(Category, `Paper title`, `Type of Norm`, `Effect Size`, `Significant?`, `Standard Deviation`, `Standard Error`) %>%
  rename(study = `Paper title`,
         cat = Category,
         normcat = `Type of Norm`,
         est = `Effect Size`,
         pcat = `Significant?`,
         sd = `Standard Deviation`,
         se = `Standard Error`) %>%
  mutate(tstat=NA, pval=NA) 

#Clean category variable
unique(d.DN$cat)
d.DN <- d.DN %>% mutate(
  cat = case_when(cat %in% c("Green consumerism", "Green Consumerism") ~ "Green consumerism",
                  cat %in% c("Household conservation", "Household Conservation") ~ "Household conservation",
                  TRUE ~ cat))


#-------------------------------------------
#Clean SN, PN, DN 0nly
#-------------------------------------------

d.all <- read_excel(paste0(here(), "/data/Meta Analysis Clean Coding Sheet.xlsx"), sheet = "SN, PN and DN")

#fill in empty rows
d.all <- d.all %>% fill("Folder","Paper title","Reason for not including",
                        "Paper Year","Authors","Category",
                        "N (number surveyed)")

#clean up for meta-analysis
colnames(d.all)
d.all <- d.all %>% select(Category, `Paper title`,`Type of norm`, `Effect Size`, `Significant?`, `Standard Deviation`, `Standard Error`,`T-Value`, `Notes`) %>%
  rename(study = `Paper title`,
         cat = Category,
         normcat = `Type of norm`,
         est = `Effect Size`,
         pcat = `Significant?`,
         sd = `Standard Deviation`,
         se = `Standard Error`,
         tstat = `T-Value`,
         Notes = `Notes`) %>%
  mutate(pval=NA)

#Clean category variable
unique(d.all$cat)
d.all <- d.all %>% mutate(
  cat = case_when(cat %in% c("Green consumerism", "Green Consumerism") ~ "Green consumerism",
                  cat %in% c("Household conservation", "Household Conservation") ~ "Household conservation",
                  TRUE ~ cat))

#Drop unstandardized estimates
table(d.all$Notes)
d.all <- d.all %>% filter(is.na(Notes) | Notes != "unstandardized")

table(d.SN$cat)
table(d.PN$cat)
table(d.DN$cat)
table(d.all$cat)



#-------------------------------------------
# Conduct meta-analyses
#-------------------------------------------

resSN <- meta_clean(d.SN, strat=F, pool_method="FE")

resPN <- meta_clean(d.PN, pool_method="FE")

resDN <- meta_clean(d.DN, pool_method="FE")

resAll <- meta_clean(d.all, pool_method="FE")

#-------------------------------------------
# Plot meta-analysis forest plots
#------------------------------------------

p.SN <- forest_plot(resSN$plotdf, strat=F)
#p.SN

p.PN <- forest_plot(resPN$plotdf, title="Subjective + Personal Norms")
p.DN <- forest_plot(resDN$plotdf, title="Subjective + Descriptive Norms")
p.all <- forest_plot(resAll$plotdf, title="Subjective, Personal, and Descriptive Norms")

h=10
w=14
ggsave(p.SN, file=paste0(here(),"/figures/forest_subject_norms_FE.jpg"), height=h, width=w)
ggsave(p.PN, file=paste0(here(),"/figures/forest_subject_personal_norms_FE.jpg"), height=h, width=w)
ggsave(p.DN, file=paste0(here(),"/figures/forest_subject_descriptive_norms_FE.jpg"), height=h, width=w)
ggsave(p.all, file=paste0(here(),"/figures/forest_subject_personal_descriptive_norms_FE.jpg"), height=h, width=w)




#-------------------------------------------
# Plot pooled estimate comparison
#------------------------------------------

df <- bind_rows(data.frame(resSN$plotdf, type="Subjective only"),
                data.frame(resDN$plotdf, type="Subjective + Descriptive"),
                data.frame(resPN$plotdf, type="Subjective + Personal"),
                data.frame(resAll$plotdf, type="Subjective, Descriptive, + Personal")) %>% 
  filter(pooled=="pooled")
df$normcat2 <- paste0(df$normcat,": ",df$cat)
df$normcat2 <- gsub("NA: ","", df$normcat2)
head(df)



p <- ggplot(df, aes(x=normcat2)) + 
  geom_point(aes(y=est,  color=cat), size = 4) +
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub, color=cat)) +
  facet_wrap(~type, scales = "free_x") +
  labs(x = "", y = "Standardized coefficient") +
  geom_hline(yintercept = 0) +
  geom_text(aes(y=est+0.15, label=pooled_label)) +
  scale_colour_manual(values=tableau10, name = "Pooled", drop=F) +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size=14),
    axis.title = element_text(size=12),
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10, angle = 45, hjust = 0.5, vjust=0.5)
  ) +
  ggtitle("") + 
  guides(color=FALSE, shape=FALSE) 
p

ggsave(p, file=paste0(here(),"/figures/pooled_estimate_comparisons_FE.jpg"), height=18, width=14)
