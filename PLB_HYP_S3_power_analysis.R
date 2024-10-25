#####################################
#           Packages                #
#####################################

library(MASS)
library(tidyverse)
library(pbapply)

#####################################
#       Simulation functions        #
#####################################

### Simulation function testing for main effects only

sim_function_main_effects = function(n = NA,
                        induction_type_relaxation_effect = NA,
                        depth_suggestion_present_effect = NA,
                        interaction_effect = NA,
                        mean_hypnotizability_total = NA,
                        sd_hypnotizability_total = NA,
                        mean_expectancy = NA,
                        sd_expectancy = NA,
                        mean_hypnosis_depth = NA,
                        sd_hypnosis_depth = NA){


  data_sim = as.data.frame(
    mvrnorm(n = n,
            mu = rep(0, 3),
            Sigma = matrix(c(  1,0.35,0.4,
                               0.35, 1, 0.4,
                               0.4, 0.4, 1)
                           , nrow = 3)))
  
  names(data_sim) = c("hypnotizability_total", "expectancy", "hypnosis_depth")
  
  ### expand variables to scale
  
  data_sim$hypnotizability_total = data_sim$hypnotizability_total * sd_hypnotizability_total + mean_hypnotizability_total
  data_sim$expectancy = data_sim$expectancy * sd_expectancy + mean_expectancy
  data_sim$hypnosis_depth = data_sim$hypnosis_depth * sd_hypnosis_depth + mean_hypnosis_depth
  
  data_sim$induction_type = rep(c("relaxation", "whitenoise"), each = n/2)
  data_sim$depth_suggestion = rep(c("absent", "present"), n/2)

  data_sim[data_sim[,"induction_type"] == "relaxation", "hypnosis_depth"] = data_sim[data_sim[,"induction_type"] == "relaxation", "hypnosis_depth"] + induction_type_relaxation_effect
  data_sim[data_sim[,"depth_suggestion"] == "present", "hypnosis_depth"] = data_sim[data_sim[,"depth_suggestion"] == "present", "hypnosis_depth"] + depth_suggestion_present_effect
  data_sim[(data_sim[,"induction_type"] == "relaxation") & (data_sim[,"depth_suggestion"] == "present"), "hypnosis_depth"] = data_sim[(data_sim[,"induction_type"] == "relaxation") & (data_sim[,"depth_suggestion"] == "present"), "hypnosis_depth"] + interaction_effect

  data_sim = data_sim %>% 
    mutate(induction_type = factor(induction_type, levels = c("whitenoise", "relaxation")),
           depth_suggestion = factor(depth_suggestion))
  
  mod = lm(hypnosis_depth ~ induction_type + depth_suggestion + hypnotizability_total + expectancy, data = data_sim)
  
  CI90lb = confint(mod, level = 0.90)["depth_suggestionpresent",]
  CI90lb
  return(CI90lb)
}

### Simulation function testing for interaction effect

sim_function_interaction = function(n = NA,
                                    induction_type_relaxation_effect = NA,
                                    depth_suggestion_present_effect = NA,
                                    interaction_effect = NA,
                                    mean_hypnotizability_total = NA,
                                    sd_hypnotizability_total = NA,
                                    mean_expectancy = NA,
                                    sd_expectancy = NA,
                                    mean_hypnosis_depth = NA,
                                    sd_hypnosis_depth = NA){
  
  
  data_sim = as.data.frame(
    mvrnorm(n = n,
            mu = rep(0, 3),
            Sigma = matrix(c(  1,0.35,0.4,
                               0.35, 1, 0.4,
                               0.4, 0.4, 1)
                           , nrow = 3)))
  
  names(data_sim) = c("hypnotizability_total", "expectancy", "hypnosis_depth")
  
  ### expand variables to scale
  
  data_sim$hypnotizability_total = data_sim$hypnotizability_total * sd_hypnotizability_total + mean_hypnotizability_total
  data_sim$expectancy = data_sim$expectancy * sd_expectancy + mean_expectancy
  data_sim$hypnosis_depth = data_sim$hypnosis_depth * sd_hypnosis_depth + mean_hypnosis_depth
  
  data_sim$induction_type = rep(c("relaxation", "whitenoise"), each = n/2)
  data_sim$depth_suggestion = rep(c("absent", "present"), n/2)
  
  data_sim[data_sim[,"induction_type"] == "relaxation", "hypnosis_depth"] = data_sim[data_sim[,"induction_type"] == "relaxation", "hypnosis_depth"] + induction_type_relaxation_effect
  data_sim[data_sim[,"depth_suggestion"] == "present", "hypnosis_depth"] = data_sim[data_sim[,"depth_suggestion"] == "present", "hypnosis_depth"] + depth_suggestion_present_effect
  data_sim[(data_sim[,"induction_type"] == "relaxation") & (data_sim[,"depth_suggestion"] == "present"), "hypnosis_depth"] = data_sim[(data_sim[,"induction_type"] == "relaxation") & (data_sim[,"depth_suggestion"] == "present"), "hypnosis_depth"] + interaction_effect
  
  data_sim = data_sim %>% 
    mutate(induction_type = factor(induction_type, levels = c("whitenoise", "relaxation")),
           depth_suggestion = factor(depth_suggestion))
  
  mod = lm(hypnosis_depth ~ induction_type * depth_suggestion + hypnotizability_total + expectancy, data = data_sim)
  
  CI90lb = confint(mod, level = 0.90)["induction_typerelaxation:depth_suggestionpresent",]
  CI90lb
  return(CI90lb)
}

#####################################
#     Simulation parameters         #
#####################################

# number of times the simulation should be run
iter = 10000

#####################################
#          Power analysis           #
#####################################

### Power to detect main effect of depth suggestion being present
## raw effect size of interest = 1 point on the hypnosis depth scale

sim_out = pbreplicate(iter, sim_function_main_effects(n = 124,
                                         induction_type_relaxation_effect = 0,
                                         depth_suggestion_present_effect = 1,
                                         interaction_effect = 0,
                                         mean_hypnotizability_total = 6,
                                         sd_hypnotizability_total = 2.3,
                                         mean_expectancy = 6,
                                         sd_expectancy = 2,
                                         mean_hypnosis_depth = 5,
                                         sd_hypnosis_depth = 2.5))

sum(t(sim_out)[,1] > 0)/length(t(sim_out)[,1])


### Power to detect equivalence of depth suggestion being present vs. absent
## raw effect size of interest = 1 point on the hypnosis depth scale

sim_out = pbreplicate(iter, sim_function_main_effects(n = 124,
                                         induction_type_relaxation_effect = 0,
                                         depth_suggestion_present_effect = 0,
                                         interaction_effect = 0,
                                         mean_hypnotizability_total = 6,
                                         sd_hypnotizability_total = 2.3,
                                         mean_expectancy = 6,
                                         sd_expectancy = 2,
                                         mean_hypnosis_depth = 5,
                                         sd_hypnosis_depth = 2.5))


sum(t(sim_out)[,2] < 1)/length(t(sim_out)[,2])


### Power to detect interaction effect of relaxation induction * depth suggestion being present
## raw effect size of interest = 1 point on the hypnosis depth scale

sim_out = pbreplicate(iter, sim_function_interaction(n = 124,
                                         induction_type_relaxation_effect = 0,
                                         depth_suggestion_present_effect = 0,
                                         interaction_effect = 1,
                                         mean_hypnotizability_total = 6,
                                         sd_hypnotizability_total = 2.3,
                                         mean_expectancy = 6,
                                         sd_expectancy = 2,
                                         mean_hypnosis_depth = 5,
                                         sd_hypnosis_depth = 2.5))

sum(t(sim_out)[,1] > 0)/length(t(sim_out)[,1])


### Power to detect that there is no substantial interaction effect of relaxation induction * depth suggestion being presen
## raw effect size of interest = 1 point on the hypnosis depth scale

sim_out = pbreplicate(iter, sim_function_interaction(n = 124,
                                         induction_type_relaxation_effect = 0,
                                         depth_suggestion_present_effect = 0,
                                         interaction_effect = 0,
                                         mean_hypnotizability_total = 6,
                                         sd_hypnotizability_total = 2.3,
                                         mean_expectancy = 6,
                                         sd_expectancy = 2,
                                         mean_hypnosis_depth = 5,
                                         sd_hypnosis_depth = 2.5))


sum(t(sim_out)[,2] < 1)/length(t(sim_out)[,2])


### Power to detect interaction effect of relaxation induction * depth suggestion being present
## raw effect size of interest = 2 point on the hypnosis depth scale

sim_out = pbreplicate(iter, sim_function_interaction(n = 124,
                                                     induction_type_relaxation_effect = 0,
                                                     depth_suggestion_present_effect = 0,
                                                     interaction_effect = 2,
                                                     mean_hypnotizability_total = 6,
                                                     sd_hypnotizability_total = 2.3,
                                                     mean_expectancy = 6,
                                                     sd_expectancy = 2,
                                                     mean_hypnosis_depth = 5,
                                                     sd_hypnosis_depth = 2.5))

sum(t(sim_out)[,1] > 0)/length(t(sim_out)[,1])


### Power to detect that there is no substantial interaction effect of relaxation induction * depth suggestion being presen
## raw effect size of interest = 2 point on the hypnosis depth scale

sim_out = pbreplicate(iter, sim_function_interaction(n = 124,
                                                     induction_type_relaxation_effect = 0,
                                                     depth_suggestion_present_effect = 0,
                                                     interaction_effect = 0,
                                                     mean_hypnotizability_total = 6,
                                                     sd_hypnotizability_total = 2.3,
                                                     mean_expectancy = 6,
                                                     sd_expectancy = 2,
                                                     mean_hypnosis_depth = 5,
                                                     sd_hypnosis_depth = 2.5))


sum(t(sim_out)[,2] < 2)/length(t(sim_out)[,2])


