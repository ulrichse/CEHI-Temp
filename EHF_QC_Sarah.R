
library(dplyr)

ehf1 <- read.csv("NC_SC_env_ehf_daily_cty_2002-2019_V2.csv")
ehf2 <- read.csv("NC_SC_env_ehf_daily_cty_2002-2019_V2_2.csv")

ehf1 <- ehf1 %>%
  filter(state=='NC')%>%
  mutate(Date=as.Date(date))

ehf1a <- ehf1 %>%
  filter(Date >= "2010-01-01" & Date <= "2010-01-31")

ehf2 <- ehf2 %>%
  filter(state=='NC')%>%
  mutate(Date=as.Date(date))

ehf2a <- ehf2 %>%
  filter(Date >= "2010-01-01" & Date <= "2010-01-31")