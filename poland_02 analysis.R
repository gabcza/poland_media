library(lme4)
library(lmerTest)

#---- Source effects -----
# ArticleTrust = β1Revealed + FEexcerpt + ε | source = TVP
# These analyzes are not what is pre-registered but equivalent (wasn't 
# sure how to run the pre-reg ones)


# H1: Pro-PiS - TVP
m.h1 <- lmer(data = dat_long %>% filter(source == "tvp", pro_pis == 1),
             trust_ind ~ treatment + 
               (1|subj_id) + (1|trial))
summary(m.h1)

# H2: Pro-PiS - TVN
m.h2 <- lmer(data = dat_long %>% filter(source == "tvn", pro_pis == 1),
             trust_ind ~ treatment + 
               (1|subj_id) + (1|trial))
summary(m.h2)

# H3: Anti-PiS - TVP
m.h3 <- lmer(data = dat_long %>% filter(source == "tvp", anti_pis == 1),
             trust_ind ~ treatment + 
               (1|subj_id) + (1|trial))
summary(m.h3)

# H4: Anti-PiS - TVN
m.h4 <- lmer(data = dat_long %>% filter(source == "tvn", anti_pis == 1),
             trust_ind ~ treatment + 
               (1|subj_id) + (1|trial))
summary(m.h4)



