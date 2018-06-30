# code to process 2 files
# protocol_search and task_report
# within reports folder
# into an ACD report
# NOTE: use project acd_ctsu2

#### load libraries
library(ggplot2)
library(tidyverse)
library(magrittr)
library(readxl)
library(writexl)
library(tidyr)
library(purrr)
library(stringr)
library(here)
library(janitor)
library(googlesheets)
library(lubridate)

# read in protocol search
ps <-
  read_excel(here::here("reports", "protocol_search.xlsx"), skip = 2)

# clean names
ps <- clean_names(ps) %>%
  select(protocol_no:additional_sponsor)

# read in task report, clean and select
tr <-
  read_excel(here::here("reports", "task_report.xlsx"), skip = 3) %>%
  clean_names() %>%
  select(
    task_list,
    task_name,
    na,
    owner_type,
    owner_name,
    target_date,
    days_overdue,
    completed_date,
    protocol_no,
    institution,
    mgmt_group
  )
#save a version with NA for missing dates
tr_na <- tr

# clean up who is owner
tr_owner <- tr %>% select(protocol_no, owner_name, task_name) %>%
  filter(task_name == 'Created CTRF & \"Notify ORSP\"') %>%
  select(-task_name)

# add owner to ps
ps <- left_join(ps, tr_owner)

# fix dates for tasks that are not applicable to study
# give them an artificial date in year 2200
tr$completed_date[tr$na == "Y"] <- as.Date("2200-01-01")

#select for only Pre-Award tasks
tr <- tr %>%
  filter(str_detect(task_list, "Pre-"))

tr_na <- tr_na %>% 
  filter(str_detect(task_list, "Pre-"))

# now a version for Post-Award Tasks
# tr <- tr %>%
#  filter(str_detect(task_list, "Post-"))


# now make columns = tasks
# using spread
tr_spread <- tr %>%
  select(protocol_no, task_name, completed_date) %>%
  filter(!is.na(completed_date)) %>%
  unique() %>%
  spread(task_name, completed_date)

tr_na_spread <- tr_na %>%
  select(protocol_no, task_name, completed_date) %>%
  filter(!is.na(completed_date)) %>%
  unique() %>%
  spread(task_name, completed_date)

# fix one colname
colnames(tr_spread)[6] <- "Created CTRF & Notified ORSP"

colnames(tr_na_spread)[6] <- "Created CTRF & Notified ORSP"

#merge ps and tr, put names in order
pstr <- left_join(ps, tr_spread, by = "protocol_no")
pstr <- pstr %>%
  arrange(`UFA Completed`) %>%
  mutate_if(is.POSIXct, as.Date) %>%
  mutate_if(is.Date, funs(format(., format = "%m/%d/%Y"))) %>% 
  select(
    protocol_no,
    additional_protocol_numbers,
    department,
    pi_name,
    principal_sponsor,
    current_status,
    current_status_date,
    owner_name,
    title,
    `Intake Form Completed`,
    `UFA Completed`,
    `Sponsor Reach out`,
    `Feasibility w/Documents received`,
    `Feasibility approved/CRAO Notified`,
    `Planning Meeting Request Sent`,
    `Planning Meeting Completed`,
    `Created CTRF & Notified ORSP`,
    `Billing Calendar Received`,
    `Care Designations completed`,
    `Internal Budget Finished`,
    `PI approved Budget`,
    `Budget Negotiations`,
    `Final Budget`,
    `PAF routed`,
    `IRB Application Submitted`,
    `OnCore Budget Build`,
    `Calendar Released`,
    `PAN Released`,
    `Open to Accrual`
  )

#merge ps and tr_na_spreqd, put names in order
pstr_na <- left_join(ps, tr_na_spread, by = "protocol_no")
pstr_na <- pstr_na %>%
  arrange(`UFA Completed`) %>%
  mutate_if(is.POSIXct, as.Date) %>%
  select(
    protocol_no,
    additional_protocol_numbers,
    department,
    pi_name,
    principal_sponsor,
    current_status,
    current_status_date,
    owner_name,
    title,
    `Intake Form Completed`,
    `UFA Completed`,
    `Sponsor Reach out`,
    `Feasibility w/Documents received`,
    `Feasibility approved/CRAO Notified`,
    `Planning Meeting Request Sent`,
    `Planning Meeting Completed`,
    `Created CTRF & Notified ORSP`,
    `Billing Calendar Received`,
    `Care Designations completed`,
    `Internal Budget Finished`,
    `PI approved Budget`,
    `Budget Negotiations`,
    `Final Budget`,
    `PAF routed`,
    `IRB Application Submitted`,
    `OnCore Budget Build`,
    `Calendar Released`,
    `PAN Released`,
    `Open to Accrual`
  )

#set up report names with date
reportname <- paste0("ACD_CTSU_Report_", Sys.Date(), ".xlsx")
reportcurrent <- paste0("ACD_CTSU_Report_", "Current", ".xlsx")

# write assembled report - dated and current
write_xlsx(pstr, here::here("reports", reportname))
write_xlsx(pstr, here::here("reports", reportcurrent))

#version with NAs
# write_xlsx(pstr_na, here::here("reports", 
#       paste0("ACD_CTSU_Report_", "Current_na", ".xlsx")))

#write to google drive

#read in protocol search from googlesheets acdctsu google drive site
#load data from googlesheets using key
# my_sheets <- gs_ls()
# sheet <- gs_key("1jhjXKyKUlEI4nWcmRNrV9fthfxEp_UUvXhfdBPIkGsw")
# gs_ws_ls(sheet)
# ps <- sheet %>% gs_read(ws ="Protocol Search", range = "A3:K287")
# read in assembled report
# in future from google drive

# read in file to generate table of times to each event
# test <-
#   read_excel( here::here("reports",
#   paste0("ACD_CTSU_Report_", "Current_na", ".xlsx")), 
#   sheet = "Sheet1")

#### to do::insert net days calculations at far right
# test <- test %>%
#   mutate(
#     docs_to_feas = difftime(`Feasibility approved/CRAO Notified`,
#       `Feasibility w/Documents received`, units = 'days'),
#     feas_to_plan = difftime(`Planning Meeting Completed`,
#       `Feasibility approved/CRAO Notified`, units = 'days'),
#     plan_to_PIApp = difftime(`PI approved Budget`,
#       `Planning Meeting Completed`, units = 'days'),
#     PIApp_to_finalbudg = difftime(`Final Budget`,
#       `PI approved Budget`, units = 'days'),
#     feas_to_IRBsub = difftime(`IRB Application Submitted`,
#       `Feasibility approved/CRAO Notified`, units = 'days'),
#     paf_to_pan = difftime(`PAN Released`,
#       `PAF routed`, units = 'days')
# )
# 
# #check test 
# # test %>% select(feas_to_plan, `Planning Meeting Completed`,
# #                 `Feasibility approved/CRAO Notified`) %>% 
# #   arrange(feas_to_plan) %>% 
# #   print(n=nrow(test))
# # looks OK, only 4 negative values
# 
# #### to do:: write new file with calculations
# # to Google drive. current version with no date, and a
# # dated version with SysDate added to title
# 
# #### select columns, create summary table
# test %<>% 
#   select(c(department, title, pi_name, 30:35))
# 
# names(test) <- c(
#   "dept",
#   "title",
#   "PI",
#   "docs_to_feas_approv",
#   "feas_approv_to_plan_mtg",
#   "mtg_to_PIapprov_budget",
#   "PIapprov_budg_to_finalbudg",
#   "feas_approv_to_IRB_submit",
#   "PAFtoPAN"
# )
# 
# ctsu_data <- test %>%
#   gather(step, days, 4:9) %>%
#   filter(!is.na(days)) %>%
#   filter(!is.na(dept))
# 
# step_levels = c(
#   "docs_to_feas_approv",
#   "feas_approv_to_plan_mtg",
#   "mtg_to_PIapprov_budget",
#   "PIapprov_budg_to_finalbudg",
#   "feas_approv_to_IRB_submit",
#   "PAFtoPAN"
# )
# 
# ctsu_data$step <- factor(ctsu_data$step, step_levels)
# 
# ctsu_data %<>% 
#   mutate(nstep = case_when(
#          step == "docs_to_feas_approv" ~ 1,
#          step == "feas_approv_to_plan_mtg" ~ 2,
#          step == "mtg_to_PIapprov_budget" ~ 3,
#          step == "PIapprov_budg_to_finalbudg" ~ 4,
#          step == "feas_approv_to_IRB_submit" ~ 5,
#          step == "PAFtoPAN" ~ 6)
#   )
# 
# 
# ctsu2 <- ctsu_data %>%
#   mutate(days = as.numeric(days)) %>% 
#   mutate(nstep = nstep + runif(nrow(ctsu_data), 
#   min = -0.3, max = 0.3)) #add some jitter manually to get some spread
# 
# ctsu_days <- ctsu2 %>% group_by(step) %>%
#   summarize(
#     N = n(),
#     min = min(days),
#     `25th pctile` = quantile(days, na.rm=TRUE, probs = 0.25),
#     median = median(days, na.rm=TRUE),
#     mean = round(mean(days, na.rm=TRUE), 2),
#     `75th pctile` = quantile(days, na.rm=TRUE, probs = 0.75),
#     max = max(days)
#   )
# 
# # transpose ctsu_days with tidyverse
# ctsu_days[7, ] <- c(0:7)
# days_table <- ctsu_days %>%
#   gather(N:max, value, -step) %>%
#   spread(step, value)
# names(days_table)[8] <- "order"
# names(days_table)[1] <- "parameter"
# 
# days_table <- days_table %>% 
#   arrange(order) %>% 
#   select(-order) %>% 
#   mutate_if(is.numeric, funs(round(.,0)))

#### to do:: make days_table available in app
