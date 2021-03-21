library(ggplot2)
library(dplyr)
library(ggridges)
library(ggpubr)
library(gridExtra)
library(tidyr)

mycols <-  c("#AF8DC3", "#D3D3D3","#7FBF7B")
###############3###############3###############3###############3###############3

# ---- ELEMENTARY SCHOOLS ----- # 

###############3###############3###############3###############3###############3



# --- load Paul's simulations 

elem = readr::read_csv("elementary.csv") %>% mutate(index_asymp = ifelse(index_asymp==0, "Index symptomatic", "Index asymptomatic")) %>% 
   mutate(interv =  ifelse(protocol == "I", "Baseline",
                                   ifelse(protocol == "II", "Contact" ,  
                                          ifelse(protocol == "III", "Two groups\nis an outbreak" ,  
                                                 ifelse(protocol == "IV", "Whole class",NA ))))) %>% 
  mutate( panelnew = ifelse(panelname =="env beta low, index same", "Index and class low",
                            ifelse(panelname ==  "env beta low, index higher", "Index medium, class low",
                                   ifelse(panelname =="env beta high, index same", "Index and class medium",
                                          ifelse(panelname =="env beta high, index higher", "Index high, class medium", NA))))) %>% 
  mutate(panelnew = factor(panelnew, levels = c("Index and class low",
                                                "Index medium, class low",
                                                "Index and class medium",
                                                "Index high, class medium"))) %>% 
  mutate(nathist  =  ifelse(world =="a", "1-day mean PIP,  low asymp. transn, low mixing/aerosol",
                                 ifelse(world =="b", "2-day PIP, low asymp. transn, low mixing/aerosol" ,  
                       ifelse(world =="c","2-day PIP, higher asymp. transn, low mixing/aerosol",  
                              ifelse(world =="d","2-day PIP, higher asymp. transn, medium mixing/aerosol",
                 ifelse(world =="e","2-day PIP, higher asymp. transn, higher mixing/aerosol", NA))))))
elem$nathist= factor(elem$nathist, levels = c("1-day mean PIP,  low asymp. transn, low mixing/aerosol",
                                              "2-day PIP, low asymp. transn, low mixing/aerosol" ,
                                              "2-day PIP, higher asymp. transn, low mixing/aerosol",
                                              "2-day PIP, higher asymp. transn, medium mixing/aerosol",
                                              "2-day PIP, higher asymp. transn, higher mixing/aerosol"))


 
# ----   
# ----  

 ggplot(data=filter(elem, world=="d"), aes(x= total_infected, y=interv, fill=index_asymp,
                                          color=index_asymp))+
  stat_binline(breaks=0:26, scale = 1, 
               draw_baseline = FALSE, 
               alpha=0.5, color= "grey44", closed = "left") +
  facet_grid( panelnew ~ index_asymp )   + theme_light() +
  scale_fill_manual(values = mycols[c(3,1)]) + 
          scale_color_manual(values = mycols[c(3,1)]) + 
  theme(strip.text = element_text(face="bold", size=8,color="black"),
        strip.background = element_rect(fill="grey65",colour="white",size=0.7),
        strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")),
         legend.position = "bottom")  +ylab("Count")+
  xlab("Total cluster size") + guides(fill=FALSE,color=FALSE)  # guides(fill=guide_legend(title="Index asymptomatic"), color=FALSE)

 ggsave("figure3-revised.pdf", width = 6, height=8)
 
# boxplot version 
 ggplot(data=filter(elem, world=="d"), aes(x=interv, y=total_infected,fill=index_asymp)) +
   facet_grid( panelname ~ index_asymp )   + theme_light() + 
   geom_boxplot(width = 0.35, alpha=0.5) +
   scale_fill_manual(values = mycols[c(3,1)]) + 
theme(axis.title.x = element_blank()) + 
   theme(strip.text = element_text(face="bold", size=8,color="black"),
         strip.background = element_rect(fill="grey65",colour="white",size=0.7),
         strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")),
         axis.title.y = element_blank(), legend.position = "bottom")+ guides(fill=guide_legend(title="Index asymptomatic"))



###########################################
# ------ SUPPLEMENTARY figure : all the "worlds" no longer split by index symptom status
###########################################
  
         
         
ggplot(data=filter(elem, panel == 4 ), aes(x= total_infected, y=interv))+
  stat_binline(breaks=0:25, scale = 1, 
               draw_baseline = FALSE, 
               alpha=0.5, color= "grey44", fill="blue") +
  facet_wrap( ~ nathist,ncol=1 )  + theme_light() +
  scale_fill_manual(values = mycols[c(3,1)]) + 
  scale_color_manual(values = mycols[c(3,1)]) + 
  theme(strip.text = element_text(face="bold", size=8,color="black"),
        strip.background = element_rect(fill="grey65",colour="white",size=0.7),
        strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")),
        axis.title.y = element_blank(), legend.position = "bottom")  + xlab("Total cluster size") 

# ggsave(file = "supp-fig-compare-worlds.pdf", width = 5, height = 8)




################################################################################
# extract information about the stats, and save a table 
################################################################################


nSims = max(elem$simulation_number)
elem_table =elem %>% 
  dplyr::group_by(world,interv,protocol,index_asymp) %>%
  summarise(percent_over_5 = sum(total_infected >= 5)*100/nSims,
            mean_clustersize = mean(total_infected), 
            q25_clustersize = quantile(total_infected, probs = 0.25), 
            q50_clustersize = quantile(total_infected, probs = 0.5), 
            q75_clustersize = quantile(total_infected, probs = 0.75),
            mean_affected = mean(students_disrupted),
            median_affected = median(students_disrupted), 
            mean_days_lax=mean(days_asymp_lax),
            q25_days_lax = quantile(days_asymp_lax, probs = 0.25),
            q25_days_lax = quantile(days_asymp_lax, probs = 0.5),
            q25_days_lax = quantile(days_asymp_lax, probs = 0.75),
            mean_days_strict = mean(days_asymp_strict), 
            q25_days_strict = quantile(days_asymp_strict, probs = 0.25),
            q50_days_strict = quantile(days_asymp_strict, probs = 0.5),
            q75_days_strict = quantile(days_asymp_strict, probs = 0.75)
             )
            


# write.csv(filter(elem_table , world %in% c("d","e")), file = "elem_table.csv")


################################################################################
# figure 4 -- from plots for the total numbers affected
################################################################################


# students disrupted = total affected (in protocols II, III, IV) or total_infected-total_not_detected
ibline =which(elem$protocol == "I")
elem$students_disrupted[ibline]  =
  elem$total_infected[ibline] - elem$total_not_detected[ibline]
worlds = c("a","b","c","d","e")

mytitles = c("Low pre-symptomatic period, asymp low transmission, low aerosol", 
             "2-day pre-symp period, asymp low transmission, low aerosol", 
             "2-day pre-symp period, asymp 0.8 transmission, low aerosol",              
             "2-day pre-symp period, asymp 0.8 transmission, more aerosol",
             "2-day pre-symp period, asymp 0.8, more aerosol, high mixing", 
             "2-day pre-symp period, asymp 0.8, high aerosol, high mixing")


plotlist = lapply(1:5, function(x) {
  ggplot(data=filter(elem, world==worlds[x]), aes(x= students_disrupted, y=interv, fill=index_asymp,
                                                  color=index_asymp))+
    stat_binline(breaks=0:26, scale = 1, 
                 draw_baseline = TRUE, 
                 alpha=0.5, color= "grey44", closed = "left") +
    facet_grid( panelnew ~ index_asymp )   + theme_light() +
    scale_fill_manual(values = mycols[c(3,1)]) + 
    scale_color_manual(values = mycols[c(3,1)]) + 
    theme(strip.text = element_text(face="bold", size=8,color="black"),
          strip.background = element_rect(fill="grey65",colour="white",size=0.7),
          strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")),
           legend.position = "bottom") + ggtitle(mytitles[x])   +
    xlab("Total disrupted") +ylab("Count")+ guides(fill=FALSE,color=FALSE) })# guides(fill=guide_legend(title="Index asymptomatic"), color=FALSE) })

plotlist[[4]]
 ggsave(plotlist[[4]], file = "elem-tot-disrupted-d-revised.pdf", width = 6, height = 8)


############################################################
# Figure 5 :  information about the "force of infection" from the cluster 
# violin version
############################################################


elem_long  = gather(elem, laxorstrict, days_exp, days_asymp_lax, days_asymp_strict)
elem_long$laxorstrict = factor(elem_long$laxorstrict)

ggplot(data=filter(filter(elem_long, days_exp >=0), world=="d"), aes(y= days_exp, x=interv, fill=laxorstrict,
                                                                     color=laxorstrict))+
  geom_violin(width = 0.9, alpha=0.5,position = position_dodge(width = 0.5)) + #  geom_jitter(alpha=0.2, width = 0.1, size = 0.3)+
  facet_grid( panelnew ~ index_asymp )   + theme_light() +
#  scale_fill_manual(values = mycols[c(3,1)]) + 
#  scale_color_manual(values = mycols[c(3,1)]) + 
  theme(strip.text = element_text(face="bold", size=8,color="black"),
        strip.background = element_rect(fill="grey65",colour="white",size=0.7),
        strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")),
        axis.title.x = element_blank(), legend.position = "bottom")  +
  scale_fill_discrete(name="Lax or strict", labels = c("Lax", "Strict"))+ guides( color=FALSE) 

ggsave("figure5-revised.pdf", width=6, height=8)



###############3###############3###############3###############3###############3

# ---- HIGH SCHOOLS ----- # 

###############3###############3###############3###############3###############3

# ---- read data ---- 
highmorn = readr::read_csv("highschool_morning.csv") %>% mutate(index_asymp = ifelse(index_asymp==0, "Index symptomatic", "Index asymptomatic")) %>% 
  mutate(interv =  ifelse(protocol == "I", "Baseline",
                          ifelse(protocol == "II", "Contact\n" ,  
                                 ifelse(protocol == "III", "Two groups\nis an outbreak" ,  
                                        ifelse(protocol == "IV", "Whole class",NA ))))) %>% 
  mutate( panelnew = ifelse(panelname =="env beta low, index same", "Index and class low",
                            ifelse(panelname ==  "env beta low, index higher", "Index medium, class low",
                                   ifelse(panelname =="env beta high, index same", "Index and class medium",
                                          ifelse(panelname =="env beta high, index higher", "Index high, class medium", NA))))) %>% 
  mutate(panelnew = factor(panelnew, levels = c("Index and class low",
                                                "Index medium, class low",
                                                "Index and class medium",
                                                "Index high, class medium")))

  
  highaft = readr::read_csv("highschool_afternoon.csv") %>% mutate(index_asymp = ifelse(index_asymp==0, "Index symptomatic", "Index asymptomatic")) %>% 
    mutate(interv =  ifelse(protocol == "I", "Baseline",
                            ifelse(protocol == "II", "Contact\n" ,  
                                   ifelse(protocol == "III", "Two groups\nis an outbreak" ,  
                                          ifelse(protocol == "IV", "Whole class",NA ))))) %>% 
    mutate( panelnew = ifelse(panelname =="env beta low, index same", "Index and class low",
                              ifelse(panelname ==  "env beta low, index higher", "Index medium, class low",
                                     ifelse(panelname =="env beta high, index same", "Index and class medium",
                                            ifelse(panelname =="env beta high, index higher", "Index high, class medium", NA))))) %>% 
    mutate(panelnew = factor(panelnew, levels = c("Index and class low",
                                                  "Index medium, class low",
                                                  "Index and class medium",
                                                  "Index high, class medium")))
  
highnorm = readr::read_csv("highschool_normal.csv") %>% mutate(index_asymp = ifelse(index_asymp==0, "Index symptomatic", "Index asymptomatic")) %>% 
  mutate(interv =  ifelse(protocol == "I", "Baseline",
                          ifelse(protocol == "II", "Contact\n" ,  
                                 ifelse(protocol == "III", "Two groups\nis an outbreak" ,  
                                        ifelse(protocol == "IV", "Whole class",NA ))))) %>% 
  mutate( panelnew = ifelse(panelname =="env beta low, index same", "Index and class low",
                            ifelse(panelname ==  "env beta low, index higher", "Index medium, class low",
                                   ifelse(panelname =="env beta high, index same", "Index and class medium",
                                          ifelse(panelname =="env beta high, index higher", "Index high, class medium", NA))))) %>% 
  mutate(panelnew = factor(panelnew, levels = c("Index and class low",
                                                "Index medium, class low",
                                                "Index and class medium",
                                                "Index high, class medium")))
ibline =which(highnorm$protocol == "I")
highnorm$students_disrupted[ibline]  = highnorm$total_infected[ibline] - 
  highnorm$total_not_detected[ibline]


# ----  check some things ----
all(highmorn$protocol == highaft$protocol)
all(highmorn$panelname == highaft$panelname)
all(highmorn$world == highaft$world)
all(highmorn$index_asymp == highaft$index_asymp)
all(highmorn$beta_aerosol_factor == highaft$beta_aerosol_factor)
all(highmorn$mu_pip == highaft$mu_pip)



# ---- make new cols for totals ---- 
highboth = highmorn
highboth$total_infected = highmorn$total_infected+ highaft$total_infected - 1 # minus 1 dont' count index twice 
highboth$total_not_detected = highmorn$total_not_detected + highaft$total_not_detected
highboth$students_affected = highmorn$students_affected + highaft$students_affected
highboth$students_disrupted = highmorn$students_disrupted + highaft$students_disrupted

# students disrupted = total affected (in protocols II, III, IV) or total_infected-total_not_detected
ibline =which(highboth$protocol == "I")
highboth$students_disrupted[ibline]  = highboth$total_infected[ibline] - highboth$total_not_detected[ibline]








################################################################################
# extract information and save a table 
################################################################################
# worlde = filter(scho2, world=="d") %>% mutate(world = "e")

nSims = max(high$simulation_number)
high_table =highboth %>% 
  dplyr::group_by(world,interv,protocol,index_asymp) %>%
  summarise(percent_over_5 = sum(total_infected >= 5)*100/nSims,
            mean_clustersize = mean(total_infected), 
            q25_clustersize = quantile(total_infected, probs = 0.25), 
            q50_clustersize = quantile(total_infected, probs = 0.5), 
            q75_clustersize = quantile(total_infected, probs = 0.75),
            mean_affected = mean(students_disrupted))



# write.csv(filter(high_table , world %in% c("d","e")), file = "high_table.csv")


########### ---- #############
#  HIGH SCHOOL - BC and normal, plots for supplement (analogous to figure 3 but high school) 
########### ---- ###############

mytitles = c("Low pre-symptomatic period, asymp low transmission, low aerosol", 
             "2-day pre-symp period, asymp low transmission, low aerosol", 
             "2-day pre-symp period, asymp 0.8 transmission, low aerosol",              
             "2-day pre-symp period, asymp 0.8 transmission, more aerosol",
             "2-day pre-symp period, asymp 0.8, more aerosol, high mixing", 
             "2-day pre-symp period, asymp 0.8, high aerosol, high mixing")

worlds=c("a","b","c","d", "e")

plotlist_normal = lapply(1:5, function(x) {
  ggplot(data=filter(highnorm, world==worlds[x]), aes(x= total_infected, y=interv, fill=index_asymp,
                                                  color=index_asymp))+
    stat_binline(breaks=0:46, scale = 1, 
                 draw_baseline = FALSE, 
                 alpha=0.5, color= "grey44", closed = "left") +
    facet_grid( panelnew ~ index_asymp )   + theme_light() +
    scale_fill_manual(values = mycols[c(3,1)]) + 
    scale_color_manual(values = mycols[c(3,1)]) + 
    theme(strip.text = element_text(face="bold", size=8,color="black"),
          strip.background = element_rect(fill="grey65",colour="white",size=0.7),
          strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")),
          axis.title.y = element_blank(), legend.position = "bottom") + ggtitle(mytitles[x])   +
    xlab("Total cluster size") +ylab("Count")+ guides(fill=FALSE, color=FALSE) })

plotlist_bc = lapply(1:5, function(x) {
  ggplot(data=filter(highboth, world==worlds[x]), aes(x= total_infected, y=interv, fill=index_asymp,
                                                      color=index_asymp))+
    stat_binline(breaks=0:46, scale = 1, 
                 draw_baseline = FALSE, 
                 alpha=0.5, color= "grey44", closed = "left") +
    facet_grid( panelnew ~ index_asymp )   + theme_light() +
    scale_fill_manual(values = mycols[c(3,1)]) + 
    scale_color_manual(values = mycols[c(3,1)]) + 
    theme(strip.text = element_text(face="bold", size=8,color="black"),
          strip.background = element_rect(fill="grey65",colour="white",size=0.7),
          strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")),
           legend.position = "bottom") + ggtitle(mytitles[x])   +
    xlab("Total cluster size")+ylab("Count") + guides(fill=FALSE, color=FALSE) })
plotlist_normal[[4]]
plotlist_bc[[4]]
# ggsave(plotlist_normal[[4]], file = "highnormal-test.pdf")
# ggsave(plotlist_bc[[4]], file = "highbc-test.pdf")
# it is hard to see the difference in this format. They need to cbe compared side by side. 


################################################
# Directly compare HIGH SCHOOL BC and normal 
################################################

highboth$Setting = "Modified"
highnorm$Setting = "Pre-COVID" 

highall = rbind(highboth[,which(colnames(highboth) %in% colnames(highnorm))], highnorm)
highall$Setting = factor(highall$Setting, levels = c("Pre-COVID", "Modified"))
highall = highall %>% mutate( panelnew = ifelse(panelname =="env beta low, index same", "Index and class low",
                                                ifelse(panelname ==  "env beta low, index higher", "Index medium, class low",
                                                       ifelse(panelname =="env beta high, index same", "Index and class medium",
                                                              ifelse(panelname =="env beta high, index higher", "Index high, class medium", NA))))) %>% 
  mutate(panelnew = factor(panelnew, levels = c("Index and class low",
                                                "Index medium, class low",
                                                "Index and class medium",
                                                "Index high, class medium")))


ggplot(data=filter(highall,world=="d"), aes(x=interv, y=total_infected,fill=Setting) )+
  facet_wrap ( ~ panelnew,nrow=4 )   + theme_light() + 
  geom_boxplot(width = 0.35, alpha=0.5) +
  theme(axis.title.x = element_blank()) + 
  theme(strip.text = element_text(face="bold", size=8,color="black"),
        strip.background = element_rect(fill="grey65",colour="white",size=0.7),
        strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")),
#        axis.title.y = element_text(), 
        axis.text.x = element_text(size = 7), legend.position = "bottom") +ylab("Total cluster size")
# better . 
# ggsave(file = "high-compare-boxplot.pdf", width = 5, height = 8)










# ---- figures one world at a time ---- 
ggplot(data=filter(highboth, world=="d"), aes(x= total_not_detected, y=interv, fill=index_asymp,
                                              color=index_asymp))+
  stat_binline(breaks=0:46, scale = 0.9, 
               draw_baseline = FALSE, 
               alpha=0.5, color= "grey44",closed = "left") +
  facet_grid( panelname ~ index_asymp )   + theme_light() +
  scale_fill_manual(values = mycols[c(3,1)]) + 
  scale_color_manual(values = mycols[c(3,1)]) + 
  theme(strip.text = element_text(face="bold", size=8,color="black"),
        strip.background = element_rect(fill="grey65",colour="white",size=0.7),
        strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")),
        axis.title.y = element_blank(), legend.position = "bottom")  +
  xlab("Cluster size") + guides(fill=FALSE, color=FALSE)

# new - recreate Figure A2 in the paper 
ggplot(data=filter(highall, world=="d" & Setting == "Modified"), aes(x= total_not_detected, y=interv, fill=index_asymp,
                                              color=index_asymp))+
  stat_binline(breaks=0:46, scale = 0.9, 
               draw_baseline = FALSE, 
               alpha=0.5, color= "grey44",closed = "left") +
  facet_grid( panelnew ~ index_asymp )   + theme_light() +
  scale_fill_manual(values = mycols[c(3,1)]) + 
  scale_color_manual(values = mycols[c(3,1)]) + 
  theme(strip.text = element_text(face="bold", size=8,color="black"),
        strip.background = element_rect(fill="grey65",colour="white",size=0.7),
        strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")),
        legend.position = "bottom")  +
  xlab("Cluster size") +ylab("Count")+ guides(fill=FALSE, color=FALSE)
ggsave("high-world-d-revised.pdf",width=6,height = 8)


# and here is Figure A3 in the paper 
ggplot(data=filter(highall, world=="d", Setting =="Modified"), aes(x= students_disrupted, y=interv, fill=index_asymp,
                                                                   color=index_asymp))+
  stat_binline(breaks=0:46, scale = 0.9, 
               draw_baseline = FALSE, 
               alpha=0.5, color= "grey44",closed = "left") +
  facet_grid( panelnew ~ index_asymp )   + theme_light() +
   scale_fill_manual(values = mycols[c(3,1)]) + 
    scale_color_manual(values = mycols[c(3,1)]) + 
  theme(strip.text = element_text(face="bold", size=8,color="black"),
        strip.background = element_rect(fill="grey65",colour="white",size=0.7),
        strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")),
         legend.position = "bottom")  +
  xlab("Students disrupted") +ylab("Count")+ guides(fill=FALSE, color=FALSE)
ggsave("high-tot-disrupted-d-revised.pdf", width = 6, height = 8)

##################################################



high_table = highboth %>% 
  dplyr::group_by(world,panel,index_asymp,interv) %>%
  summarise(num_morethan_5 = sum(total_infected >= 5),
            mean_clustersize = mean(total_infected), 
            q25_clustersize = quantile(total_infected, probs = 0.25), 
            q50_clustersize = quantile(total_infected, probs = 0.5), 
            q75_clustersize = quantile(total_infected, probs = 0.75),
            mean_affected = mean(students_disrupted))
nSims=max(highaft$simulation_number)
high_table$perc_above_5 = high_table$num_morethan_5*(100/nSims)
print(filter(high_table, world=="d"), n=32)



high_long  = gather(highall, laxorstrict, days_exp, days_asymp_lax, days_asymp_strict)
high_long$laxorstrict = factor(high_long$laxorstrict)

ggplot(data=filter(filter(high_long, days_exp >=0), world=="d", Setting=="Modified"), aes(y= days_exp, x=interv, fill=laxorstrict,
                                                                     color=laxorstrict))+
  geom_violin(width = 0.9, alpha=0.5,position = position_dodge(width = 0.5)) + #  geom_jitter(alpha=0.2, width = 0.1, size = 0.3)+
  facet_grid( panelnew ~ index_asymp )   + theme_light() +
  #  scale_fill_manual(values = mycols[c(3,1)]) + 
  #  scale_color_manual(values = mycols[c(3,1)]) + 
  theme(strip.text = element_text(face="bold", size=8,color="black"),
        strip.background = element_rect(fill="grey65",colour="white",size=0.7),
        strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")),
        axis.title.x = element_blank(), legend.position = "bottom")  +
  scale_fill_discrete(name="Lax or strict", labels = c("Lax", "Strict"))+
  ylab("Student-days")+guides( color=FALSE) 

ggsave("high-force-infection-revised.pdf", width=6, height=8)

















###############################################################
# Begin random regular testing 
###############################################################


elempool = readr::read_csv("elementary_pooled.csv") %>% 
  mutate(index_asymp = ifelse(index_asymp==0, "Index symptomatic", "Index asymptomatic")) %>% 
  # mutate(index_asymp = ifelse(index_asymp==0, "No", "Yes"))  %>% 
  mutate(freq= factor(freq)) %>% 
  mutate(freqdesc = case_when(freq ==1 ~ "No random testing",
                              freq == 2 ~ "Weekly",
                              freq==3 ~ "Every three days",
                              TRUE ~ "hi")) %>% 
  mutate(freqdesc = factor(freqdesc, levels = c("No random testing","Weekly","Every three days"))) %>% 
  mutate(`Tests performed on site` = ifelse(onsite==0, "No","Yes"))
 


ggplot(data=elempool, aes(x= total_infected, y=freqdesc, fill=`Tests performed on site`,
                                          color=`Tests performed on site`))+
  stat_binline(breaks=0:26, scale = 1, 
               draw_baseline = FALSE, 
               alpha=0.5, color= "grey44", closed = "left") +
  facet_wrap(~ index_asymp )   + theme_light() +
  scale_fill_manual(values = mycols[c(3,1)]) + 
  scale_color_manual(values = mycols[c(3,1)]) + 
  theme(strip.text = element_text(face="bold", size=8,color="black"),
        strip.background = element_rect(fill="grey65",colour="white",size=0.7),
        strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")),
       legend.position = "bottom")  +
  xlab("Total cluster size")+ylab("Count") + guides( color=FALSE)

ggsave("pooled-testing-revised.pdf", width = 8, height = 6)














##################################################
# BEGIN JUNK 
##################################################











singleone = filter(scho, world=="d" & interv =="Group" &
                    index_asymp== 1 & panel == 4)
hist(singleone$total_infected, breaks = 0:20)
#  scale_y_discrete(expand = c(0, 0)) +
#  scale_x_continuous(expand = c(0, 0)) +
#  scale_fill_grey() +





# --- random other attempts -----# 

ggplot(data=filter(scho, world=="a"), aes(x= total_infected, y=protocol, fill=index_asymp,
                      color=index_asymp, height=stat(density)))+
 geom_density_ridges(center=0.5 ,  scale = 0.5,
                     draw_baseline = FALSE, 
                     position="dodge", alpha=0.2) + 
 #  scale_y_discrete() +
#   geom_point(data = scho, 
#              aes(x= total_infected, y=protocol, color=index_asymp,height=0.1),
#              alpha=0.3,position = position_jitterdodge(jitter.width = 0.2, 
#                                                        jitter.height = 0.1)) +
  facet_wrap( ~ panel )   + theme_light()
      

hist(filter(scho,( world =="a" & panel==1 & protocol == "IV"))$total_infected, 
     breaks =c(0,1,2,3,4))




        

ggplot(data=scho, aes(x= total_infected, y=protocol, fill=index_asymp,
                      color=index_asymp))+ 
  geom_density_ridges(stat="binline") + geom_point( position = position_points_jitter( width = 0.2, height = 0),
                 point_shape = '|', point_size = 3, point_alpha = 0.3, alpha = 0.7)

ggplot(data=scho, aes(x= total_infected, y=protocol, color=index_asymp, group=index_asymp)) + 
  geom_jitter(alpha=0.3, position="dodge")


library(PupillometryR)


 ggplot(data = scho, aes(x=protocol, y=total_infected, fill=index_asymp)) +
#   geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point( aes(x=protocol, y=total_infected), position = position_jitter(width = .07), 
             size = .5, alpha = 0.1) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
 facet_wrap(~ world, scales="free")

 
 
 
 
 ggplot(data=scho, aes(x=protocol, y=total_infected, fill=index_asymp,color=index_asymp)) +
   geom_violin(alpha = 0.5) + facet_wrap( ~world, scales="free") # + 
 #   geom_point(alpha=0.1,position = position_jitterdodge())
 
 
 ggplot(data=scho, aes(x=protocol, y=total_infected, fill=index_asymp,color=index_asymp)) +
   geom_dotplot(alpha = 0.5,binaxis = "y", stackdir = "center") + facet_wrap( ~world, scales="free")  
 #  geom_point(alpha=0.1,position = position_jitterdodge())
 

 
 ### 
 # legend 
 colleg = get_legend( 
   ggplot(data=scho, aes(x=protocol, y=total_infected, fill=index_asymp)) +
     geom_violin(alpha = 0.5) +
     scale_fill_manual(values = c(mycols[3],mycols[1])) + 
     theme(legend.position="bottom"))
 
 # main 
 mainplot = ggplot(data=filter(scho, world=="d" & index_asymp==1),
                   aes(x= total_infected,  y=interv))+
   stat_binline(breaks=0:20, scale = 0.5, 
                alpha=0.6,fill=mycols[1]) +
   stat_binline(data = filter(scho, world=="d" & index_asymp==0),
                breaks=0:20, scale = 0.5, 
                alpha=0.6,fill=mycols[3]) +
   facet_wrap( ~ panelname )   +
   theme_ridges() +
   theme_light()   +
   theme(strip.text = element_text(face="bold", size=8,color="black"),
         strip.background = element_rect(fill="grey65",colour="white",size=0.5),
         strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")),
         axis.title.y = element_blank())  + xlab("Total cluster size") 
 
 grid.arrange(mainplot, colleg, heights=c(8,1))
 
 
 
 
 
 # --- the old one, where it had both histograms together but was harder to see 
 plotlist = lapply(1:4, function(x) {
   ggplot(data=filter(scho, world==worlds[x]), aes(x= total_infected, y=interv, fill=index_asymp,
                                                   color=index_asymp))+
     stat_binline(breaks=0:20, scale = 0.5, 
                  draw_baseline = FALSE, 
                  alpha=0.5) +
     facet_wrap( ~ panelname )   + theme_light() +
     scale_fill_manual(values = mycols[c(3,1)]) + 
     scale_color_manual(values = mycols[c(3,1)]) + 
     theme(strip.text = element_text(face="bold", size=8,color="black"),
           strip.background = element_rect(fill="grey65",colour="white",size=0.7),
           strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")),
           axis.title.y = element_blank(), legend.position = "bottom")  + xlab("Total cluster size") + ggtitle(mytitles[x])})
 
 