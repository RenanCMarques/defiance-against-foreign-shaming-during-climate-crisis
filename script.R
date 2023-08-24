#------------------------------------------------------------------------------#
############################ Conjoint Analysis ################################
#------------------------------------------------------------------------------#

rm(list = ls())

# Renan's Directory

setwd("C:/Users/renan/Google Drive/FGV RI/Defiance Against Foreign Shaming During Climate Crises")

#------------------------------------------------------------------------------#
### Loading and installing necessary packages ##################################
#------------------------------------------------------------------------------#

# Installing required packages 

packages <- c('tidyverse', 'readxl', 'cregg', 'kableExtra', 'FindIt', 'lme4', 'car', 'fixest',
              'conjointdatachecks', 'clubSandwich')

# Checking if is installed (and install if not)

packages.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

rm(packages, packages.check)

#------------------------------------------------------------------------------#
### Loading data (6432 obs of 41 variables) ####################################
#------------------------------------------------------------------------------#

conjoint <- read_excel("data/FGVXBR_210752_cParte2_20220107_VTC_Peso.xlsx")

#------------------------------------------------------------------------------#
### Cleaning and creating variables ############################################
#------------------------------------------------------------------------------#

# Creating dependent variables (y1 = P8, y2 = P9, y3 = P10)

conjoint <- conjoint %>%
  mutate(y1 = ifelse(P8 == 1, 1, 0),
         y2 = ifelse(P9 == 1, 1, 0), 
         y3 = ifelse(P10 == 1, 1, 0))

# Naming attributes

conjoint <- conjoint %>% 
  mutate(atr1b = as.factor(ifelse(atr1==1, "Legal obligation", "Moral obligation")), #Type of Obligations
         atr2b = as.factor(ifelse(atr2==1, "Hypocrisy Absent", "Hypocrisy Present")), #Hypocrisy
         atr3b = as.factor(ifelse(atr3==1, "Cosmopolitan","Nation-Based")), #Frame of Criticism
         atr4b = as.factor(ifelse(atr4==1, "Diplomatic sanctions", ifelse(atr4==2, "Economic sanctions", "Use of military force"))), #Type of Threats
         atr5b = as.factor(ifelse(atr5==1, "International reputation", ifelse(atr4==2, "Economy", "Safety"))), #Costs
         atr6b = as.factor(ifelse(atr6==1, "Stood Firm","Backed down"))) #Reputation for Resolve

# Defining baseline attributes

conjoint <- conjoint %>%
  mutate(atr1b = relevel(atr1b, ref = "Moral obligation"),
         atr2b = relevel(atr2b, ref = "Hypocrisy Absent"),
         atr3b = relevel(atr3b, ref = "Nation-Based"),
         atr4b = factor(atr4b, levels = c("Use of military force", "Economic sanctions", "Diplomatic sanctions")),
         atr4b = relevel(atr4b, ref = "Diplomatic sanctions"),
         atr5b = relevel(atr5b, ref = "International reputation"),
         atr6b = relevel(atr6b, ref = "Backed down"))

# Creating variables for heterogeneous effects

conjoint <- conjoint %>%
  mutate(nationalism_aux = (P3 + P4 + (6-P5))/3,
         nationalism_quantile = ntile(nationalism_aux, 3),
         nationalism_quartile = ntile(nationalism_aux, 4),
         nationalism = as.factor(ifelse(nationalism_quantile == 3,"High", ifelse(nationalism_quantile == 2,"Medium", "Low"))),
         nationalism2 = as.factor(ifelse(nationalism_quartile == 4,"High", ifelse(nationalism_quartile == 3 | nationalism_quartile == 2 ,"Medium", "Low"))),
         region = as.factor(case_when(
           ZONA_BR == 1 ~ "North",
           ZONA_BR == 2 ~ "Northeast",
           ZONA_BR == 3 ~ "Southeast",
           ZONA_BR == 4 ~ "South",
           ZONA_BR == 5 ~ "Central-West")),
         income_aux = ifelse(P13 == 9 | P13 == 10, NA, P13),
         income_quantile = ntile(income_aux, 3),
         income = as.factor(case_when(income_quantile == 1 ~ "Low", income_quantile == 2 ~ "Medium", income_quantile == 3 ~ "High")),
         itrust = as.factor(ifelse(P1 == 1, "Can trust other nations", "Must be careful when \n dealing with other nations")),
         ideology = as.factor(ifelse(P2 < 5, "Left", "Right")),
         ideology2 = as.factor(ifelse(P16 < 3, "Progressive", "Conservative")),
         education = as.factor(ifelse(ESCOLARIDAD_BR == 1,"Elementary (Primary) \n or less", 
                            ifelse(ESCOLARIDAD_BR == 2,"High school \n  or equivalent",
                                   ifelse(ESCOLARIDAD_BR == 3,"Undergraduate \n  or more","")))),
         approval = as.factor(ifelse(P6 > 3, "Approve", ifelse(P6 == 3, "Neither", "Disapprove"))),
         sex = as.factor(ifelse(P11 == 1, "Male", "Female")))

#------------------------------------------------------------------------------#
### Preparing plot and table settings ##########################################
#------------------------------------------------------------------------------#

# Attribute labels

label <- function(x) { 
  x %>% mutate(feature = case_when(
    feature == "atr1b" ~ "Type of Obligation",
    feature == "atr2b" ~ "Hypocrisy",
    feature == "atr3b" ~ "Frame of Criticism",
    feature == "atr4b" ~ "Type of Threats",
    feature == "atr6b" ~ "Reputation for Resolve"),
    feature = factor(feature, levels = c("Type of Obligation", "Hypocrisy", "Frame of Criticism", 
                                        "Type of Threats", "Reputation for Resolve")))
}

# Set kable to latex format
options(knitr.table.format = "latex")

#------------------------------------------------------------------------------#
### Frequency of Conjoint Features #############################################
#------------------------------------------------------------------------------#

plot(cj_freqs(conjoint, y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b, 
              id =  ~numericalId, 
              weights = ~ponde2) %>% label()) + 
  theme(legend.position = "right",
        axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
        axis.text = element_text(color = "black")) # PDF size 6 x 9

#------------------------------------------------------------------------------#
### Conjoint Analysis for P8 (y1) as dependent variable ########################
#------------------------------------------------------------------------------#

## Weighted

# Average Marginal Component Effects (AMCE)

amce <- cj(conjoint, y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b, 
             id = ~numericalId, 
             weights = ~ponde2) %>% label()

head(amce[c("level", "estimate", "std.error", "p")], 20L) %>% kable(booktabs = T, digits = 3)

# Marginal Means (MM)

mm <- cj(conjoint, y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b,
          id = ~numericalId, 
          weights = ~ponde2, 
          estimate = "mm")  %>% label()

head(mm[c("level", "estimate", "std.error", "p")], 20L) %>% kable(booktabs = T, digits = 3)

# Plotting results

plot(amce) + theme(legend.position = "right",
        axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
        axis.text = element_text(color = "black")) # PDF 4.5 x 9

plot(mm, vline = 0.5) + theme(legend.position = "right",
        axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
        axis.text = element_text(color = "black")) # PDF 4.5 x 9

## Unweighted

# Average Marginal Component Effects (AMCE_uw)

amce_uw <- cj(conjoint, y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b, 
           id = ~numericalId) %>% label()

head(amce_uw[c("level", "estimate", "std.error", "p")], 20L) %>% kable(booktabs = T, digits = 3)

# Marginal Means (MM)

mm_uw <- cj(conjoint, y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b,
         id = ~numericalId,
         estimate = "mm")  %>% label()

head(mm_uw[c("level", "estimate", "std.error", "p")], 20L) %>% kable(booktabs = T, digits = 3)

# Plotting results

plot(amce_uw) + theme(legend.position = "right",
                   axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                   axis.text = element_text(color = "black")) # PDF 4.5 x 9

plot(mm_uw, vline = 0.5) + theme(legend.position = "right",
                              axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                              axis.text = element_text(color = "black")) # PDF 4.5 x 9

#------------------------------------------------------------------------------#
### Interactions for P8 (y1) as dependent variable #############################
#------------------------------------------------------------------------------#

# Hypocrisy + Frame of Criticism
amce_interact1 <- cj(conjoint, y1 ~ atr2b, id = ~numericalId, weights = ~ponde2, estimate = "amce", by = ~atr3b) %>% label() 

plot(amce_interact1) + ggplot2::facet_wrap(~BY, ncol = 3L) # PDF 4.5 x 9

head(amce_interact1[c("level", "estimate", "std.error", "p")], 20L) %>% kable(booktabs = T, digits = 3)

# Type of Threat + Reputation for Resolve
amce_interact2 <- cj(conjoint, y1 ~ atr4b, id = ~numericalId, weights = ~ponde2, estimate = "amce", by = ~atr6b) %>% label()

plot(amce_interact2) + ggplot2::facet_wrap(~BY, ncol = 3L)

head(amce_interact2[c("level", "estimate", "std.error", "p")], 20L) %>% kable(booktabs = T, digits = 3)

# Hypocrisy + Type of Threat
amce_interact3 <- cj(conjoint, y1 ~ atr2b, id = ~numericalId, weights = ~ponde2, estimate = "amce", by = ~atr4b) %>% label()

plot(amce_interact3) + ggplot2::facet_wrap(~BY, ncol = 3L)

head(amce_interact3[c("level", "estimate", "std.error", "p")], 20L) %>% kable(booktabs = T, digits = 3)

# Fame of Criticism + Type of Threat
amce_interact4 <- cj(conjoint, y1 ~ atr3b, id = ~numericalId, weights = ~ponde2, estimate = "amce", by = ~atr4b) %>% label()

plot(amce_interact4) + ggplot2::facet_wrap(~BY, ncol = 3L)

head(amce_interact4[c("level", "estimate", "std.error", "p")], 20L) %>% kable(booktabs = T, digits = 3)

#------------------------------------------------------------------------------#
### Interactions for P8 (y1) as dependent variable w/ FindIT package #########
#------------------------------------------------------------------------------#

# Reordering factor levels because package for AMIE changes the level

conjoint2 <- conjoint %>%
  mutate(atr1b = relevel(atr1b, ref = "Legal obligation"),
         atr2b = relevel(atr2b, ref = "Hypocrisy Present"),
         atr3b = relevel(atr3b, ref = "Cosmopolitan"),
         atr4b = factor(atr4b, levels = c("Use of military force", "Diplomatic sanctions", "Economic sanctions")),
         atr4b = relevel(atr4b, ref = "Economic sanctions"),
         atr6b = relevel(atr6b, ref = "Stood Firm"))

# Hypocrisy + Frame of Criticism

anova_interact1 <- CausalANOVA(formula = y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b,
                               int2.formula = ~ atr2b:atr3b,
                               data = conjoint2,
                               cluster = conjoint2$numericalId, 
                               nway = 2)

summary(anova_interact1)

ConditionalEffect(anova_interact1, treat.fac="atr2b", cond.fac="atr3b")

plot(anova_interact1, type="ConditionalEffect", fac.name=c("atr2b","atr3b"), 
     space=13, xlim = c(-0.2,0.2))

# Type of Threat + Reputation for Resolve

anova_interact2 <- CausalANOVA(formula = y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b,
                               int2.formula = ~ atr4b:atr6b,
                               data = conjoint2,
                               cluster = conjoint2$numericalId, 
                               nway = 2)

summary(anova_interact2)

ConditionalEffect(anova_interact2, treat.fac="atr4b", cond.fac="atr6b", base.ind = 3)

plot(anova_interact2, type="ConditionalEffect", fac.name=c("atr4b","atr6b"), 
     space=13, xlim = c(-0.1,0.1))

# Hypocrisy + Type of Threat

anova_interact3 <- CausalANOVA(formula = y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b,
                               int2.formula = ~ atr2b:atr4b,
                               data = conjoint2,
                               cluster = conjoint2$numericalId, 
                               nway = 2)

summary(anova_interact3)

ConditionalEffect(anova_interact3, treat.fac="atr2b", cond.fac="atr4b")

plot(anova_interact3, type="ConditionalEffect", fac.name=c("atr2b","atr4b"), 
     space=13, xlim = c(-0.1,0.1))

# Fame of Criticism + Type of Threat

anova_interact4 <- CausalANOVA(formula = y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b,
                               int2.formula = ~ atr3b:atr4b,
                               data = conjoint2,
                               cluster = conjoint2$numericalId, 
                               nway = 2)

summary(anova_interact4)

ConditionalEffect(anova_interact4, treat.fac="atr3b", cond.fac="atr4b")

plot(anova_interact4, type="ConditionalEffect", fac.name=c("atr3b","atr4b"), 
     space=13, xlim = c(-0.1,0.1))

#------------------------------------------------------------------------------#
### Heterogeneous effects for P8 (y1) as dependent variable ####################
#------------------------------------------------------------------------------#

#### By Ideology (Left-Right) ####################################################

amce_ideo <- cj(conjoint, y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b, 
                id = ~numericalId,
                weights = ~ponde2,
                by = ~ideology) %>% label()

head(amce_ideo[c("level", "estimate", "std.error", "p")], 22L) %>% kable(booktabs = T, digits = 3)

mm_ideo <- cj(conjoint, y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b, 
              id = ~numericalId,
              weights = ~ponde2,
              by = ~ideology, 
              estimate = "mm") %>% label()

plot(amce_ideo, group = "ideology", legend_title = "Ideology") + theme(legend.position = "right",
                                                                       axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                                                                       axis.text = element_text(color = "black"))

plot(mm_ideo, group = "ideology", legend_title = "Ideology", vline = 0.5) + theme(legend.position = "right",
                                                                                  axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                                                                                  axis.text = element_text(color = "black"))

#### By Ideology (Conservative - Progressive) ####################################

amce_ideo2 <- cj(conjoint, y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b, 
                 id = ~numericalId,
                 weights = ~ponde2,
                 by = ~ideology2) %>% label()

head(amce_ideo2[c("level", "estimate", "std.error", "p")], 22L) %>% kable(booktabs = T, digits = 3)

mm_ideo2 <- cj(conjoint, y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b, 
               id = ~numericalId,
               weights = ~ponde2,
               by = ~ideology2,
               estimate = "mm") %>% label()

plot(amce_ideo2, group = "ideology2", legend_title = "Ideology") + theme(legend.position = "right",
                                                                         axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                                                                         axis.text = element_text(color = "black"))

plot(mm_ideo2, group = "ideology2", legend_title = "Ideology", vline = 0.5) + theme(legend.position = "right",
                                                                                    axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                                                                                    axis.text = element_text(color = "black"))

#### By International Trust ######################################################

amce_tru <- cj(conjoint, y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b, 
                 id = ~numericalId,
                 weights = ~ponde2,
                 by = ~itrust) %>% label()

head(amce_tru[c("level", "estimate", "std.error", "p")], 22L) %>% kable(booktabs = T, digits = 3)

mm_tru <- cj(conjoint, y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b, 
                 id = ~numericalId,
                 weights = ~ponde2,
                 by = ~itrust,
                 estimate = "mm") %>% label()

plot(amce_tru, group = "itrust", legend_title = "International Trust") + theme(legend.position = "right",
                                                                               axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                                                                               axis.text = element_text(color = "black"))

plot(mm_tru, group = "itrust", legend_title = "International Trust", vline = 0.5) + theme(legend.position = "right",
                                                                                          axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                                                                                          axis.text = element_text(color = "black"))

#### By Income ###################################################################

amce_inc <- cj(conjoint, y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b, 
                 id = ~numericalId,
                 weights = ~ponde2,
                 by = ~income) %>% label()

head(amce_inc[c("level", "estimate", "std.error", "p")], 36L) %>% kable(booktabs = T, digits = 3)

mm_inc <- cj(conjoint, y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b, 
                 id = ~numericalId,
                 weights = ~ponde2,
                 by = ~income,
                 estimate = "mm") %>% label()
 
plot(amce_inc, group = "income", legend_title = "Income") + theme(legend.position = "right",
                                                                  axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                                                                  axis.text = element_text(color = "black"))

plot(mm_inc, group = "income", legend_title = "Income", vline = 0.5) + theme(legend.position = "right",
                                                                             axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                                                                             axis.text = element_text(color = "black"))

#### By Approval #################################################################

amce_app <- cj(conjoint, y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b, 
                   id = ~numericalId,
                   weights = ~ponde2,
                   by = ~approval) %>% label()

head(amce_app[c("level", "estimate", "std.error", "p")], 36L) %>% kable(booktabs = T, digits = 3)

mm_app <- cj(conjoint, y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b, 
                 id = ~numericalId,
                 weights = ~ponde2,
                 by = ~approval,
                 estimate = "mm") %>% label()

plot(amce_app, group = "approval", legend_title = "Approval") + theme(legend.position = "right",
                                                                      axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                                                                      axis.text = element_text(color = "black"))

plot(mm_app, group = "approval", legend_title = "Approval", vline = 0.5) + theme(legend.position = "right",
                                                                                 axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                                                                                 axis.text = element_text(color = "black"))

#### By Education ################################################################

amce_educ <- cj(conjoint, y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b, 
                 id = ~numericalId,
                 weights = ~ponde2,
                 by = ~education) %>% label()

head(amce_educ[c("level", "estimate", "std.error", "p")], 36L) %>% kable(booktabs = T, digits = 3)

mm_educ <- cj(conjoint, y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b, 
                  id = ~numericalId,
                  weights = ~ponde2,
                  by = ~education,
                  estimate = "mm") %>% label()

plot(amce_educ, group = "education", legend_title = "Education") + theme(legend.position = "right",
                                                                         axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                                                                         axis.text = element_text(color = "black"))

plot(mm_educ, group = "education", legend_title = "Education", vline = 0.5) + theme(legend.position = "right",
                                                                                    axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                                                                                    axis.text = element_text(color = "black"))

#------------------------------------------------------------------------------#
### Heterogeneous effects for P8 (y1) as dependent variable with interactions ##
#------------------------------------------------------------------------------#

## Hypocrisy + Frame of Criticism by (Left-Right) 
amce_hetero_interact1 <- cj(conjoint, y1 ~ atr2b,
                     id = ~numericalId,
                     weights = ~ponde2,
                     by = ~atr3b + ideology,
                     estimate = "amce") %>% label() %>%
  mutate(BY = case_when(
    BY == "Nation-Based***Left" ~ "Nation-Based",
    BY =="Nation-Based***Right" ~ "Nation-Based",
    BY =="Cosmopolitan***Right" ~ "Cosmopolitan",
    BY =="Cosmopolitan***Left" ~ "Cosmopolitan"
  ))

plot(amce_hetero_interact1, group = "ideology", legend_title = "Ideology") + 
  facet_wrap(BY~.) # PDF 4.5 x 9

head(amce_hetero_interact1[c("level", "estimate", "std.error", "p")], 20L) %>% kable(booktabs = T, digits = 3)

## Hypocrisy + Frame of Criticism by (Conservative-Progressive) 
amce_hetero_interact2 <- cj(conjoint, y1 ~ atr2b,
                            id = ~numericalId,
                            weights = ~ponde2,
                            by = ~atr3b + ideology2,
                            estimate = "amce") %>% label() %>%
  mutate(BY = case_when(
    BY == "Nation-Based***Conservative" ~ "Nation-Based",
    BY =="Nation-Based***Progressive" ~ "Nation-Based",
    BY =="Cosmopolitan***Conservative" ~ "Cosmopolitan",
    BY =="Cosmopolitan***Progressive" ~ "Cosmopolitan"
  ))

plot(amce_hetero_interact2, group = "ideology2", legend_title = "Ideology") + 
  facet_wrap(BY~.) # PDF 4.5 x 9

head(amce_hetero_interact2[c("level", "estimate", "std.error", "p")], 20L) %>% kable(booktabs = T, digits = 3)

# Type of Threat + Reputation for Resolve by (Left-Right) 
amce_hetero_interact3 <- cj(conjoint, y1 ~ atr4b, 
                     id = ~numericalId, 
                     weights = ~ponde2,
                     by = ~atr6b + ideology, 
                     estimate = "amce") %>% label() %>%
  mutate(BY = case_when(
    BY == "Backed down***Left" ~ "Backed down",
    BY == "Stood Firm***Left" ~ "Stood Firm",
    BY == "Backed down***Right" ~ "Backed down",
    BY == "Stood Firm***Right" ~ "Stood Firm"
  ))

plot(amce_hetero_interact3, group = "ideology", legend_title = "Ideology") + 
  facet_wrap(BY~.) # PDF 4.5 x 9

head(amce_hetero_interact3[c("level", "estimate", "std.error", "p")], 20L) %>% kable(booktabs = T, digits = 3)

# Type of Threat + Reputation for Resolve by (Conservative-Progressive)  
amce_hetero_interact4 <- cj(conjoint, y1 ~ atr4b, 
                     id = ~numericalId, 
                     weights = ~ponde2,
                     by = ~atr6b + ideology2, 
                     estimate = "amce") %>% label() %>%
  mutate(BY = case_when(
    BY == "Backed down***Progressive" ~ "Backed down",
    BY == "Stood Firm***Progressive" ~ "Stood Firm",
    BY == "Backed down***Conservative" ~ "Backed down",
    BY == "Stood Firm***Conservative" ~ "Stood Firm"
  ))

plot(amce_hetero_interact4, group = "ideology2", legend_title = "Ideology") + 
  facet_wrap(BY~.) # PDF 4.5 x 9

head(amce_hetero_interact4[c("level", "estimate", "std.error", "p")], 20L) %>% kable(booktabs = T, digits = 3)

# Hypocrisy + Type of Threat by (Left-Right) 
amce_hetero_interact5 <- cj(conjoint, y1 ~ atr2b, 
                     id = ~numericalId,
                     weights = ~ponde2, 
                     by = ~atr4b+ideology,
                     estimate = "amce") %>% label() %>%
  mutate(BY = case_when(
    BY == "Diplomatic sanctions***Left" ~ "Diplomatic sanctions",
    BY == "Use of military force***Left" ~ "Use of military force",
    BY == "Economic sanctions***Left" ~ "Economic sanctions",
    BY == "Diplomatic sanctions***Right" ~ "Diplomatic sanctions",
    BY == "Use of military force***Right" ~ "Use of military force",
    BY == "Economic sanctions***Right" ~ "Economic sanctions"
  ))

plot(amce_hetero_interact5, group = "ideology", legend_title = "Ideology") +
  ggplot2::facet_wrap(BY~.)

head(amce_hetero_interact5[c("level", "estimate", "std.error", "p")], 20L) %>% kable(booktabs = T, digits = 3)

# Hypocrisy + Type of Threat by (Conservative-Progressive)   
amce_hetero_interact6 <- cj(conjoint, y1 ~ atr2b, 
                            id = ~numericalId,
                            weights = ~ponde2, 
                            by = ~atr4b+ideology2,
                            estimate = "amce") %>% label() %>%
  mutate(BY = case_when(
    BY == "Diplomatic sanctions***Conservative" ~ "Diplomatic sanctions",
    BY == "Use of military force***Conservative" ~ "Use of military force",
    BY == "Economic sanctions***Conservative" ~ "Economic sanctions",
    BY == "Diplomatic sanctions***Progressive" ~ "Diplomatic sanctions",
    BY == "Use of military force***Progressive" ~ "Use of military force",
    BY == "Economic sanctions***Progressive" ~ "Economic sanctions"
  ))

plot(amce_hetero_interact6, group = "ideology2", legend_title = "Ideology") +
  ggplot2::facet_wrap(BY~.)

head(amce_hetero_interact6[c("level", "estimate", "std.error", "p")], 20L) %>% kable(booktabs = T, digits = 3)

# Fame of Criticism + Type of Threat by (Left-Right) 
amce_hetero_interact7 <- cj(conjoint, y1 ~ atr3b, 
                     id = ~numericalId,
                     weights = ~ponde2,
                     by = ~atr4b + ideology,
                     estimate = "amce") %>% label() %>%
  mutate(BY = case_when(
    BY == "Diplomatic sanctions***Left" ~ "Diplomatic sanctions",
    BY == "Use of military force***Left" ~ "Use of military force",
    BY == "Economic sanctions***Left" ~ "Economic sanctions",
    BY == "Diplomatic sanctions***Right" ~ "Diplomatic sanctions",
    BY == "Use of military force***Right" ~ "Use of military force",
    BY == "Economic sanctions***Right" ~ "Economic sanctions"
  ))

plot(amce_hetero_interact7, group = "ideology", legend_title = "Ideology") + 
  ggplot2::facet_wrap(BY~.)

head(amce_hetero_interact7[c("level", "estimate", "std.error", "p")], 20L) %>% kable(booktabs = T, digits = 3)

# Fame of Criticism + Type of Threat by (Conservative-Progressive) 
amce_hetero_interact8 <- cj(conjoint, y1 ~ atr3b, 
                            id = ~numericalId,
                            weights = ~ponde2,
                            by = ~atr4b + ideology2,
                            estimate = "amce") %>% label() %>%
  mutate(BY = case_when(
    BY == "Diplomatic sanctions***Progressive" ~ "Diplomatic sanctions",
    BY == "Use of military force***Progressive" ~ "Use of military force",
    BY == "Economic sanctions***Progressive" ~ "Economic sanctions",
    BY == "Diplomatic sanctions***Conservative" ~ "Diplomatic sanctions",
    BY == "Use of military force***Conservative" ~ "Use of military force",
    BY == "Economic sanctions***Conservative" ~ "Economic sanctions"
  ))

plot(amce_hetero_interact8, group = "ideology2", legend_title = "Ideology") + 
  ggplot2::facet_wrap(BY~.)

head(amce_hetero_interact8[c("level", "estimate", "std.error", "p")], 20L) %>% kable(booktabs = T, digits = 3)

#------------------------------------------------------------------------------#
### Testing carryover effects #############################################
#------------------------------------------------------------------------------#

# Task order

# Creating dummy for task order

conjoint <- conjoint %>%
  mutate(task_order = as.factor(rep(1:6, 1072)))

# Estimating AMCE and MM

amce_order <- cj(conjoint, y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b, 
                id = ~numericalId,
                weights = ~ponde2,
                by = ~task_order) %>% label()

head(amce_order[c("level", "estimate", "std.error", "p")], 22L) %>% kable(booktabs = T, digits = 3)

mm_order <- cj(conjoint, y1 ~ atr1b + atr2b + atr3b + atr4b + atr6b, 
              id = ~numericalId,
              weights = ~ponde2,
              by = ~task_order, 
              estimate = "mm") %>% label()

plot(amce_order) + ggplot2::facet_wrap(~BY, ncol = 3L) # PDF 4.5 x 9

plot(mm_order) + ggplot2::facet_wrap(~BY, ncol = 3L) # PDF 4.5 x 9


plot(amce_order, group = "task_order", legend_title = "task_order") + theme(legend.position = "right",
                                                                       axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                                                                       axis.text = element_text(color = "black"))

plot(mm_order, group = "task_order", legend_title = "task_order", vline = 0.5) + theme(legend.position = "right",
                                                                                  axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                                                                                  axis.text = element_text(color = "black"))

# Using carryTest() function
devtools::install_github("https://github.com/cknotz/conjointdatachecks")
library(conjointdatachecks)

attributes <- c("atr1b", "atr2b", "atr3b", "atr4b", "atr6b")

carry <- carryTest(data = conjoint,
          outcome = "y1",
          attributes = attributes,
          task = "task_order",
          resID = "numericalId")

as.data.frame(carry) %>%
  xtable::xtable(caption = "Carry Test Results", label = "tab:carry_test_results")

plot(carry,
     ltype = 5, 
     lcol = "red",
     margins = c(5,8,1,2)) 
