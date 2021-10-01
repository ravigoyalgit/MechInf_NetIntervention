
library('rgexf')
library('tidyverse')
library('igraph')
library('statnet')
library(intergraph)

source('C:/Users/ravij/Dropbox/Academic/Research/Projects/MechInf_JP/MechInf_NetIntervention/MechInf_NetIntervention/MechInf_NetIntervention_gen_school_net.R', echo=TRUE)
source('C:/Users/ravij/Dropbox/Academic/Research/Projects/MechInf_JP/MechInf_NetIntervention/MechInf_NetIntervention/MechInf_NetIntervention_interventions.R', echo=TRUE)

school.net = gen_school_net() 

gest <- ergm(school.net ~ edges + nodemix("classroom"))
summary(gest)

netprop_val_all.df = data.frame(value = NULL,
                                netprop = NULL,
                                time = NULL,
                                intervention = NULL)

#########################################################

for (i in c(1:1000)) {
  
  SIM_school.net = simulate(gest, nsim = 1)
  netprop_vals = summary(SIM_school.net ~ edges + triangles + nodematch("classroom")) 
  netprop_val_ind.df = netprop_vals %>% 
    as_tibble() %>%
    mutate(netprop = names(netprop_vals),
           time = 1,
           intervention = "None")
  
  netprop_val_all.df = bind_rows(netprop_val_ind.df, netprop_val_all.df)
  
  SIM_school_1.net = remove_cross_edges(SIM_school_1.net)
  netprop_vals = summary(SIM_school_1.net ~ edges + triangles + nodematch("classroom")) 
  netprop_val_ind.df = netprop_vals %>% 
    as_tibble() %>%
    mutate(netprop = names(netprop_vals),
           time = 2,
           intervention = "Removal")

  netprop_val_all.df = bind_rows(netprop_val_ind.df, netprop_val_all.df)
  
}

netprop_val_all.df %>%
  ggplot(aes(x=value, fill=netprop))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  facet_wrap(~time, scales = "fixed")+
  theme()+
  labs(x="Network Intervention")


