install.packages("tidyverse")
install.packages("DRDID")
install.packages("haven")
install.packages("lfe")
install.packages("zoo")
install.packages("dplyr")
install.packages("readstata13")
install.packages("fixest")
install.packages("did")
install.packages("fect")
install.packages("panelView")
install.packages("PanelMatch")
install.packages("ggplot2")
install.packages("openxlsx")
install.packages("bacondecomp")

library(DRDID)
library(tidyverse)
library(haven)
library(lfe)
library(zoo)
library(dplyr)
library(readstata13)
library(fixest)
library(did)
library(fect)
library(panelView)
library(PanelMatch)
library(ggplot2)
library(openxlsx)
library(bacondecomp)

# Set filepath 
filepath <- "C:\\Users\\bmontgomery\\OneDrive - Research Triangle Institute\\Desktop\\NIBRS work\\Cannabis Legalization and Seizures\\Data\\eventmodeldata_rcl_quarters.csv"

# Read CSV into dataframe 
df <- read.csv(file = filepath, header = TRUE)

# Remove observations with no rates
df <- df[complete.cases(df$Q_Rate), ]

# Calculate the inverse hyperbolic sine of Q_Rate
df$Q_Rate_ihs <- asinh(df$Q_Rate)

# Print the list of unique state names
unique_states <- unique(df$state)
print(unique_states)

# Define a function to categorize states into numeric census regions
get_census_region <- function(state) {
  northeast <- c("ct", "me", "ma", "nh", "ri", "vt", "nj", "ny", "pa")
  midwest <- c("il", "in", "mi", "oh", "wi", "ia", "ks", "mn", "mo", "ne", "nd", "sd")
  south <- c("de", "fl", "ga", "md", "nc", "sc", "va", "dc", "wv", "al", "ky", "ms", "tn", "ar", "la", "ok", "tx")
  west <- c("az", "co", "id", "mt", "nv", "nm", "ut", "wy", "ak", "ca", "hi", "or", "wa")
  if (state %in% northeast) {
    return(1)
  } else if (state %in% midwest) {
    return(2)
  } else if (state %in% south) {
    return(3)
  } else if (state %in% west) {
    return(4)

# Add region categorization to the dataframe
df$region <- factor(sapply(df$state, get_census_region), 
                    levels = 1:4, 
                    labels = c("Northeast", "Midwest", "South", "West"))
  } else {
    return(NA)
  }
}

# Add census region categorization to the dataframe
df$region <- sapply(df$state, get_census_region)

# Check if the region categorization worked, cross-check state and region variables

state_region_check <- df %>%
  select(state, region) %>%
  distinct() %>%
  arrange(state)

print(state_region_check)

# Count the number of states per region in the treatment group and not in the treatment group
treatment_counts <- df %>%
  group_by(region, treatment) %>%
  summarise(state_count = n_distinct(state)) %>%
  spread(treatment, state_count, fill = 0) %>%
  rename(Not_Treated = `0`, Treated = `1`)

print(treatment_counts)

# Check each state and their rcl_date
state_rcl_check <- df %>%
  select(state, rcl_date) %>%
  distinct() %>%
  arrange(state)

print(state_rcl_check)

# Create the Tquarter_group variable based on rcl_date
df$Tquarter_group <- with(df, ifelse(state == "ct", 19,
                ifelse(state == "mi", 8,
                ifelse(state == "mt", 17,
                ifelse(state == "ri", 22,
                ifelse(state == "va", 19,
                ifelse(state == "vt", 7, 0)))))))

# Check each state and their quarter_group
state_quarter_group_check <- df %>%
  select(state, Tquarter_group) %>%
  distinct() %>%
  arrange(state)

print(state_quarter_group_check)

#Create time series of plots of the logged and raw outcomes by state, separte treatment and control
# Create time series of plots of the logged and raw outcomes by state, separate treatment and control

# Function to create time series plots
# Sort the data by state and quarter
df <- df %>% arrange(state, quarter)

#Create drug group specific datasets 
all_seize <- df[df$drug_group == "alldrugs_fullpoprate", ]
had_can <-df[df$drug_group == "cannabis_fullpoprate", ]
no_can <- df[df$drug_group == "alldrugs_no_c_fullpoprate", ]
can_only <- df[df$drug_group == "cannabis_only_fullpoprate", ]
coke <- df[df$drug_group == "cocaine_crack_fullpoprate", ]
coke_nocan <- df[df$drug_group == "cocaine_crack_no_c_fullpoprate", ]
meth <- df[df$drug_group == "meth_fullpoprate", ]
meth_nocan<- df[df$drug_group == "meth_no_c_fullpoprate", ]
opioids<- df[df$drug_group == "opioids_fullpoprate", ]
opioids_nocan<- df[df$drug_group == "opioids_no_c_fullpoprate", ]

#Black-White Risk Ratios
all_nocan<- df[df$drug_group == "alldrugs_no_c_RR", ]
can_only_rr <- df[df$drug_group == "cannabis_only_RR", ]
coke_rr <- df[df$drug_group == "alldrugs_no_c_RR", ]
coke_nocan_rr <- df[df$drug_group == "cocaine_crack_RR", ]
meth_rr <- df[df$drug_group == "meth_RR", ]
meth_nocan_rr<- df[df$drug_group == "meth_no_c_RR", ]
opioids_rr<- df[df$drug_group == "opioids_RR", ]
opioids_nocan_rr<- df[df$drug_group == "opioids_no_c_RR", ]

#Create figure of state timelines and quarters, make sure timeline matches assigned quarter_group
pv <- all_seize[all_seize$study == 1, ]
panelview(log_rate~rcl|sid+quarter, data=pv,index= c("state","quarter"))

#calculate twfes among states that legalized cannabis
#all drug seizures
model.twfe.0 <- feols(log_rate~rcl|sid+quarter,
                      data=all_seize, cluster = "sid") #use the clustered standard error
print(model.twfe.0)

model.twfe.0 <- feols(log_rate~rcl|sid+quarter+region:quarter,
            data=all_seize, cluster = "sid") #use the clustered standard error
print(model.twfe.0)

  # Use the did package to handle multiple treatment periods
did_model_all <- att_gt(
    yname = "log_rate",
    tname = "quarter",
    idname = "sid",
    gname = "Tquarter_group",
    xformla = ~1,
    data = all_seize,
    panel = TRUE,
    clustervars = "sid")

  # Summarize the results
summary(did_model_all)
  # Plot the difference-in-differences estimates across time by group (based on legalization quarter)
ggdid(did_model_all)
  # Simplify the summary
agg.simple <- aggte(did_model_all, type = "simple")
summary(agg.simple)

  # Dynamic Effects and Event Study + plot
agg.es <- aggte(did_model_all, type = "dynamic")
summary(agg.es)
ggdid(agg.es)

#how does all drug seizures model change if I add the region-by-quarter fixed effect?
did_model_all_region_control <- att_gt(
    yname = "log_rate",
    tname = "quarter",
    idname = "sid",
    gname = "Tquarter_group",
    xformla = ~region:quarter,
    data = all_seize,
    panel = TRUE,
    clustervars = "sid")
  
    # Summarize the results
summary(did_model_all_region_control)
  # Plot the difference-in-differences estimates across time.
ggdid(did_model_all_region_control)
  # Simplify the summary
agg.simple <- aggte(did_model_all_region_control, type = "simple")
summary(agg.simple)

#how does the model change if using inverse hyperbolic sine instead?
did_model_all_ihs <- att_gt(
    yname = "Q_Rate_ihs",
    tname = "quarter",
    idname = "sid",
    gname = "Tquarter_group",
    xformla = ~1,
    data = all_seize,
    panel = TRUE,
    clustervars = "sid")

  # Summarize the results
summary(did_model_all_ihs)
  # Plot the difference-in-differences estimates across time by group (based on legalization quarter)
ggdid(did_model_all_ihs)
  # Simplify the summary
agg.simple <- aggte(did_model_all_ihs, type = "simple")
summary(agg.simple)

# drug seizures included cannabis
did_model_had_can <- att_gt(
  yname = "log_rate",
  tname = "quarter",
  idname = "sid",
  gname = "Tquarter_group",
  xformla = ~1,
  data = had_can,
  panel = TRUE,
  clustervars = "sid")

# Summarize the results
summary(did_model_had_can)
# Plot the difference-in-differences estimates across time by group (based on legalization quarter)
ggdid(did_model_had_can)
# Simplify the summary
agg.simple <- aggte(did_model_had_can, type = "simple")
summary(agg.simple)

#cannabis only seizures
did_model_can_only <- att_gt(
  yname = "log_rate",
  tname = "quarter",
  idname = "sid",
  gname = "Tquarter_group",
  xformla = ~1,
  data = can_only,
  panel = TRUE,
  clustervars = "sid")

# Summarize the results
summary(did_model_can_only)
# Plot the difference-in-differences estimates across time by group (based on legalization quarter)
ggdid(did_model_can_only)
# Simplify the summary
agg.simple <- aggte(did_model_can_only, type = "simple")
summary(agg.simple)

# Dynamic Effects and Event Study + plot
agg.es <- aggte(did_model_all, type = "dynamic")
summary(agg.es)
ggdid(agg.es)

#all drugs except cannabis seizures
did_model_no_can <- att_gt(
  yname = "log_rate",
  tname = "quarter",
  idname = "sid",
  gname = "Tquarter_group",
  xformla = ~1,
  data = no_can,
  panel = TRUE,
  clustervars = "sid")

# Summarize the results
summary(did_model_no_can)
# Plot the difference-in-differences estimates across time by group (based on legalization quarter)
ggdid(did_model_no_can)
# Simplify the summary
agg.simple <- aggte(did_model_no_can, type = "simple")
summary(agg.simple)

#seizures including coke
did_model_coke <- att_gt(
  yname = "log_rate",
  tname = "quarter",
  idname = "sid",
  gname = "Tquarter_group",
  xformla = ~1,
  data = coke,
  panel = TRUE,
  clustervars = "sid")

# Summarize the results
summary(did_model_coke)
# Plot the difference-in-differences estimates across time by group (based on legalization quarter)
ggdid(did_model_coke)
# Simplify the summary
agg.simple <- aggte(did_model_coke, type = "simple")
summary(agg.simple)

#seizures including coke, no cannabis
did_model_coke_nocan <- att_gt(
  yname = "log_rate",
  tname = "quarter",
  idname = "sid",
  gname = "Tquarter_group",
  xformla = ~1,
  data = coke_nocan,
  panel = TRUE,
  clustervars = "sid")

# Summarize the results
summary(did_model_coke_nocan)
# Plot the difference-in-differences estimates across time by group (based on legalization quarter)
ggdid(did_model_coke_nocan)
# Simplify the summary
agg.simple <- aggte(did_model_coke_nocan, type = "simple")
summary(agg.simple)

#seizures including meth
did_model_meth <- att_gt(
  yname = "log_rate",
  tname = "quarter",
  idname = "sid",
  gname = "Tquarter_group",
  xformla = ~1,
  data = meth,
  panel = TRUE,
  clustervars = "sid")

# Summarize the results
summary(did_model_meth)
# Plot the difference-in-differences estimates across time by group (based on legalization quarter)
ggdid(did_model_meth)
# Simplify the summary
agg.simple <- aggte(did_model_meth, type = "simple")
summary(agg.simple)

#seizures including meth, no cannabis
did_model_meth_nocan <- att_gt(
  yname = "log_rate",
  tname = "quarter",
  idname = "sid",
  gname = "Tquarter_group",
  xformla = ~1,
  data = meth_nocan,
  panel = TRUE,
  clustervars = "sid")

# Summarize the results
summary(did_model_meth_nocan)
# Plot the difference-in-differences estimates across time by group (based on legalization quarter)
ggdid(did_model_meth_nocan)
# Simplify the summary
agg.simple <- aggte(did_model_meth_nocan, type = "simple")
summary(agg.simple)

#seizures including opioids
did_model_opioids <- att_gt(
  yname = "log_rate",
  tname = "quarter",
  idname = "sid",
  gname = "Tquarter_group",
  xformla = ~1,
  data = opioids,
  panel = TRUE,
  clustervars = "sid")

# Summarize the results
summary(did_model_opioids)
# Plot the difference-in-differences estimates across time by group (based on legalization quarter)
ggdid(did_model_opioids)
# Simplify the summary
agg.simple <- aggte(did_model_opioids, type = "simple")
summary(agg.simple)

#seizures including opioids, no cannabis
did_model_opioids_nocan <- att_gt(
  yname = "log_rate",
  tname = "quarter",
  idname = "sid",
  gname = "Tquarter_group",
  xformla = ~1,
  data = opioids_nocan,
  panel = TRUE,
  clustervars = "sid")

# Summarize the results
summary(did_model_opioids_nocan)
# Plot the difference-in-differences estimates across time by group (based on legalization quarter)
ggdid(did_model_opioids_nocan)
# Simplify the summary
agg.simple <- aggte(did_model_opioids_nocan, type = "simple")
summary(agg.simple)

#Black-White risk Ratios
#all drugs except cannabis seizure black-white risk ratio
did_model_all_nocan_rr <- att_gt(
  yname = "log_rate",
  tname = "quarter",
  idname = "sid",
  gname = "Tquarter_group",
  xformla = ~1,
  data = all_nocan,
  panel = TRUE,
  clustervars = "sid")

# Summarize the results
summary(did_model_all_nocan_rr)
# Plot the difference-in-differences estimates across time by group (based on legalization quarter)
ggdid(did_model_all_nocan_rr)
# Simplify the summary
agg.simple <- aggte(did_model_all_nocan_rr, type = "simple")
summary(agg.simple)

#cannabis seizure black-white risk ratio
# Define
did_model_can_only_rr <- att_gt(
  yname = "log_rate",
  tname = "quarter",
  idname = "sid",
  gname = "Tquarter_group",
  xformla = ~1,
  data = can_only_rr,
  panel = TRUE,
  clustervars = "sid")

# Summarize the results
summary(did_model_can_only_rr)
# Plot the difference-in-differences estimates across time by group (based on legalization quarter)
ggdid(did_model_can_only_rr)
# Simplify the summary
agg.simple <- aggte(did_model_can_only_rr, type = "simple")
summary(agg.simple)

#Included cocaine and crack black-white risk ratio
did_model_coke_rr <- att_gt(
  yname = "log_rate",
  tname = "quarter",
  idname = "sid",
  gname = "Tquarter_group",
  xformla = ~1,
  data = coke_rr,
  panel = TRUE,
  clustervars = "sid")

# Summarize the results
summary(did_model_coke_rr)
# Plot the difference-in-differences estimates across time by group (based on legalization quarter)
ggdid(did_model_coke_rr)
# Simplify the summary
agg.simple <- aggte(did_model_coke_rr, type = "simple")
summary(agg.simple)

#Included cocaine and crack, excluded cannabis black-white risk ratio
did_model_coke_nocan_rr <- att_gt(
  yname = "log_rate",
  tname = "quarter",
  idname = "sid",
  gname = "Tquarter_group",
  xformla = ~1,
  data = coke_nocan_rr,
  panel = TRUE,
  clustervars = "sid")

# Summarize the results
summary(did_model_coke_nocan_rr)
# Plot the difference-in-differences estimates across time by group (based on legalization quarter)
ggdid(did_model_coke_nocan_rr)
# Simplify the summary
agg.simple <- aggte(did_model_coke_nocan_rr, type = "simple")
summary(agg.simple)

#Included meth black-white risk ratio
did_model_meth_rr <- att_gt(
  yname = "log_rate",
  tname = "quarter",
  idname = "sid",
  gname = "Tquarter_group",
  xformla = ~1,
  data = meth_rr,
  panel = TRUE,
  clustervars = "sid")

# Summarize the results
summary(did_model_meth_rr)
# Plot the difference-in-differences estimates across time by group (based on legalization quarter)
ggdid(did_model_meth_rr)
# Simplify the summary
agg.simple <- aggte(did_model_meth_rr, type = "simple")
summary(agg.simple)

#Included meth, excluded cannabis black-white risk ratio
did_model_meth_nocan_rr <- att_gt(
  yname = "log_rate",
  tname = "quarter",
  idname = "sid",
  gname = "Tquarter_group",
  xformla = ~1,
  data = meth_nocan_rr,
  panel = TRUE,
  clustervars = "sid")

# Summarize the results
summary(did_model_meth_nocan_rr)
# Plot the difference-in-differences estimates across time by group (based on legalization quarter)
ggdid(did_model_meth_nocan_rr)
# Simplify the summary
agg.simple <- aggte(did_model_meth_nocan_rr, type = "simple")
summary(agg.simple)

#Included opioids black-white risk ratio
did_model_opioids_rr <- att_gt(
  yname = "log_rate",
  tname = "quarter",
  idname = "sid",
  gname = "Tquarter_group",
  xformla = ~1,
  data = opioids_rr,
  panel = TRUE,
  clustervars = "sid")

# Summarize the results
summary(did_model_opioids_rr)
# Plot the difference-in-differences estimates across time by group (based on legalization quarter)
ggdid(did_model_opioids_rr)
# Simplify the summary
agg.simple <- aggte(did_model_opioids_rr, type = "simple")
summary(agg.simple)

#Included opioids, excluded cannabis black-white risk ratio
did_model_opioids_nocan_rr <- att_gt(
  yname = "log_rate",
  tname = "quarter",
  idname = "sid",
  gname = "Tquarter_group",
  xformla = ~1,
  data = opioids_nocan_rr,
  panel = TRUE,
  clustervars = "sid")

# Summarize the results
summary(did_model_opioids_nocan_rr)
# Plot the difference-in-differences estimates across time by group (based on legalization quarter)
ggdid(did_model_opioids_nocan_rr)
# Simplify the summary
agg.simple <- aggte(did_model_opioids_nocan_rr, type = "simple")
summary(agg.simple)
