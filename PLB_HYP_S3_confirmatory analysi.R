
### Load packages

library(psych)
library(car)
library(lmtest)
library(tidyverse)



### Load simulated data
# Thiswill be replaced by loading the actual data.

data_raw = read.csv("https://raw.githubusercontent.com/kekecsz/PLB-HYP-OTKA-S3/refs/heads/main/data_plbhyp3_simulated.csv")

### Data managent
data_conf = data_raw %>% 
  mutate(induction_type = factor(induction_type, levels = c("whitenoise", "relaxation")),
         depth_suggestion = factor(depth_suggestion))

### manipulation check
## pre-hypnosis expectancy not different between relaxation and white noise groups
t.test(expectancy ~ induction_type, data = data_conf, alternative = "less")
# assumption check
data_conf %>% 
  filter(induction_type == "whitenoise") %>% 
  select(expectancy) %>% 
  describe() %>% 
  select(skew, kurtosis)

data_conf %>% 
  filter(induction_type == "relaxation") %>% 
  select(expectancy) %>% 
  describe() %>% 
  select(skew, kurtosis)

## post-hypnosis expectancy not different between relaxation and white noise groups
# This data is not simulated in this dataset, but this assumption check will be conducted

## whether more participants who got white noise and reported suspecting that there was placebo 
## involved in this research, and suspected that they got sham hypnosis compared to participants who got relaxation induction
# This data is not simulated in this dataset, but this assumption check will be conducted

### Building the statistical model
mod = lm(hypnosis_depth ~ induction_type + depth_suggestion + hypnotizability_total + expectancy, data = data_conf)

### assumption check
## outliers
plot(mod, which = 4)
## normality
describe(mod$residuals)$skew
describe(mod$residuals)$kurtosis
## homoscedasticity
bptest(mod)
## linearity
residualPlots(mod)
## multicollinearity
vif(mod)

### Testing H1
CI90lb_induction_typerelaxation = confint(mod, level = 0.90)["induction_typerelaxation",]
CI90lb_induction_typerelaxation
CI90lb_induction_typerelaxation[2] < 1 # if TRUE, H1 is accepted
CI90lb_induction_typerelaxation[1] > 0 # if above line is FALSE and this line is TRUE, H1 is rejected

### Testing H2
CI90lb_depth_suggestionpresent = confint(mod, level = 0.90)["depth_suggestionpresent",]
CI90lb_depth_suggestionpresent
CI90lb_depth_suggestionpresent[2] < 1 # if TRUE, H2 is rejected
CI90lb_depth_suggestionpresent[1] > 0 # if above line is FALSE and this line is TRUE, H2 is supported

