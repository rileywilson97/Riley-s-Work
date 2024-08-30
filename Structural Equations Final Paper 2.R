#Hypotheses:
#H1: Religiosity predicts prejudice against women
#H2: This relationship is mediated by higher levels of authoritarian-traditionalism in highly religious individuals
#H3: This relationship (both direct and indirect) would be stronger in a collectivistic culture (Croatia) than in an individualistic culture (Netherlands)
#H4: Across cultures, this relationship will be different for men than for women

ess <- read.csv("C:/Users/riley/OneDrive/Desktop/ESS11.csv")

library("haven")        
library("dplyr")      
library("psych")
library("stringr")
library("purrr")

### MODELING ###
library("lavaan")       
library("MVN") # Multivariate normality

### VISUALIZATION ###
library("tidySEM")
library("ggplot2")              
library("patchwork")    

ess_nlhr <- filter(ess, cntry == c('NL', 'HR'))
sum(ess_nlhr$cntry =='HR')
mean(ess_nlhr$gndr == '2')
sum(ess_nlhr$cntry[ess_nlhr$gndr == '2'] == 'NL')
sum(ess_nlhr$cntry[ess_nlhr$gndr == '2'] == 'NL')/sum(ess_nlhr$cntry == 'NL')
sum(ess_nlhr$cntry[ess_nlhr$gndr == '2'] == 'HR')
sum(ess_nlhr$cntry[ess_nlhr$gndr == '2'] == 'HR')/sum(ess_nlhr$cntry == 'HR')
mean(ess_nlhr$agea)
mean(ess_nlhr$agea[ess_nlhr$cntry == 'NL'])
mean(ess_nlhr$agea[ess_nlhr$cntry == 'HR'])

ess_nlhr_fp <- ess_nlhr %>% select(
  
  ## Religiosity
  rlgdgr, # Religious
  rlgatnd, # Attend religious services
  pray, # Pray apart from services
  
  ## Anti-Feminist
  wsekpwr, # Women seek power
  weasoff, # Women easily offended
  wexashr, # Women exaggerate SA claims
  wprtbym, # Women should be protected by men
  
  ## Authoritarianism-Traditionalism
  imptrada, # Tradition is important
  ipfrulea, # Important to follow rules
  ipbhprpa, # Important to behave properly
  lrnobed, # Kids need to respect authority
  ipmodsta, # Important to be modest
  
  gndr,
  cntry
)

ess_nlhr = filter(ess_nlhr_fp, # Removing irrelevant data ("I don't know" or "Refused to answer")
                  ## Religiosity
                    rlgdgr != '88' & rlgdgr != '77' &
                    rlgatnd != '88' & rlgatnd != '77' &
                    pray != '88' & pray != '77' &
                  ## Anti-Feminist
                    wsekpwr != '8' & wsekpwr != '7' &
                    weasoff != '8' & weasoff != '7' &
                    wexashr != '8' & wexashr != '7' &
                    wprtbym != '8' &
                  ## Traditional Values
                    imptrada != '99' & imptrada != '88' & imptrada != '77' & imptrada != '66' &
                    ipfrulea != '99' & ipfrulea != '88' & ipfrulea != '77' & ipfrulea != '66' &
                    ipbhprpa != '99' & ipbhprpa != '88' & ipbhprpa != '77' & ipbhprpa != '66' &
                    ipmodsta != '88' & ipmodsta != '77' & ipmodsta != '66' &
                    lrnobed != '7' & lrnobed != '8')

ess_fp <- ess_nlhr %>% mutate(across(matches(c('rlgatnd', 'pray',
                                               'wprtbym',
                                               'imptrada', 'ipfrulea',
                                               'ipbhprpa', 'lrnobed', 'ipmodsta')), ~ 6 - .)) # Reverse code variables as needed

ess_fp_mvn <- ess_fp[,c('rlgdgr', 'rlgatnd', 'pray', # Multivariate normality test
                       'wsekpwr', 'weasoff', 'wexashr', wprtbym',
                        'imptrada', 'ipfrulea', 'ipbhprpa', ipmodstra' 'lrnobed')] 
ess_fp_mvn_na <- na.omit(ess_fp_mvn)
mvn_test_fp <- mvn(data = ess_fp_mvn_na, mvnTest = c('hz'))
mvn_test_fp$multivariateNormality
mvn_test_fp$univariateNormality

measure_model <- '
trad_val =~ imptrada + ipfrulea + ipbhprpa + ipmodsta
anti_fem =~ wsekpwr + weasoff + wexashr
relig =~ rlgdgr + rlgatnd + pray'
fit_measure_model <- cfa(measure_model,
                         data = ess_fp,
                         estimator = 'MLM')
summary(fit_measure_model,
        standardized = TRUE,
        fit.measure = TRUE)
fitmeasure_measure_model <- fitMeasures(measure_model,
            c('chisq',
              'df',
              'pvalue',
              'rmsea',
              'cfi',
              'tli',
              'srmr'), output = 'matrix')

mi <- inspect(fit_measure_model, 'mi')
mi.sorted <- mi[order(-mi$mi),]
mi.sorted[1:5,]

medi_model <- '
trad_val =~ imptrada + ipfrulea + ipbhprpa + ipmodsta
anti_fem =~ wsekpwr + weasoff + wexashr
relig =~ rlgdgr + rlgatnd + pray

## Direct Effect 
anti_fem ~ c*relig

## Mediator (Social health)
trad_val ~ a*relig # Well-being regressed on trust in institutions
anti_fem ~ b*trad_val # Trust in others regressed on well-being

## Indirect effect
ab := a*b

## Total effect
total := c + (a*b)'

fit_medi_model <- cfa(medi_model,
                      data = ess_fp,
                      estimator = 'MLM')
summary(fit_medi_model,
        standardized = TRUE,
        fit.measures = TRUE)
anova(fit_medi_model, fit_measure_model)

lay <- get_layout(
  'ipbhprpa', 'imptrada', 'ipfrulea', 'ipmodsta', '', '',
  '', 'auth_trad', '', '', 'well_being', '',
  '', '', '', '', '', '',
  '', '', '', 'happy', 'health', 'ctrlife',
  '', 'trust_other', '', '', '', '',
  'pplhlp', 'ppltrst', 'pplfair', '', '', '',
  rows = 6
)

plot_medi <- graph_sem(model = fit_medi_model,
                       layout = lay,
                       angle = 170,
                       label = 'est_std')
plot_medi

## Measurement Invariance ##

fit_medi_config <- cfa(medi_model,
                       data = ess_fp,
                       group = 'cntry',
                       estimator = 'MLM')
fit_medi_metric <- cfa(medi_model,
                       data = ess_fp,
                       group = 'cntry',
                       estimator = 'MLM',
                       group.equal = c('loadings'))
fit_medi_scalar <- cfa(medi_model,
                       data = ess_fp,
                       group = 'cntry',
                       estimator = 'MLM',
                       group.equal = c('loadings',
                                       'intercepts'))
fit_medi_strict <- cfa(medi_model,
                       data = ess_fp,
                       group = 'cntry',
                       estimator = 'MLM',
                       group.equal = c('loadings',
                                       'intercepts',
                                       'residuals'))
fit_medi_structure <- cfa(medi_model,
                          data = ess_fp,
                          group = 'cntry',
                          estimator = 'MLM',
                          group.equal = c('loadings',
                                          'intercepts',
                                          'residuals',
                                          'lv.variances',
                                          'lv.covariances'))                  

model_fit <-  function(lavobject) {
  vars <- c("df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "srmr")
  return(fitmeasures(lavobject)[vars] %>% data.frame() %>% round(2) %>% t())
}

table_fit <- 
  list(model_fit(fit_medi_config), 
       model_fit(fit_medi_metric), 
       model_fit(fit_medi_scalar), 
       model_fit(fit_medi_strict),
       model_fit(fit_medi_structure)) %>% 
  reduce(rbind)
rownames(table_fit) <- c('Configural', 'Metric', 'Scalar', "Strict", "Structural")
table_fit           
summary(fit_medi_config, standardized = TRUE,
        fit.measures = TRUE)
table_anova <- list(anova(fit_medi_config, fit_medi_metric),
                    anova(fit_medi_metric, fit_medi_scalar),
                    anova(fit_medi_scalar, fit_medi_strict),
                    anova(fit_medi_strict, fit_medi_structure)) %>%
  reduce(rbind) %>%
  .[-c(3, 5, 7),]
table_anova

anova(fit_medi_config, fit_medi_metric)
lavTestScore(fit_medi_metric)
options(max.print = 9999)
partable(fit_medi_metric)
fit_medi_metric <- cfa(medi_model, #Partial metric invariance
                       data = ess_fp,
                       group = 'cntry',
                       group.equal = c('loadings'),
                       estimator = 'MLM',
                       group.partial = c('trad_val =~ ipmodsta',
                                         'anti_fem =~ wexashr'))
summary(fit_medi_metric, standardized = TRUE, fit.measures = TRUE)

## Partial Scalar Invariance

fit_medi_scalar <- cfa(medi_model,
                       data = ess_fp,
                       group = 'cntry',
                       estimator = 'MLM',
                       group.equal = c('loadings',
                                       'intercepts'),
                       group.partial = c('trad_val =~ ipmodsta',
                                         'anti_fem =~ wexashr',
                                         'ipfrulea ~1',
                                         'rlgdgr ~1'))
lavTestScore(fit_medi_scalar)
partable(fit_medi_scalar)
summary(fit_medi_scalar, standardized = TRUE, fit.measures = TRUE)

## Partial Strict Invariance ##

fit_medi_strict <- cfa(medi_model,
                       data = ess_fp,
                       group = 'cntry',
                       estimator = 'MLM',
                       group.equal = c('loadings',
                                       'intercepts',
                                       'residuals'),
                       group.partial = c('trad_val =~ ipmodsta',
                                         'anti_fem =~ wexashr',
                                         'ipfrulea ~1',
                                         'rlgdgr ~1',
                                         'ipbhprpa ~~ ipbhprpa',
                                         'rlgdgr ~~ rlgdgr',
                                         'ipfrulea ~~ ipfrulea',
                                         'weasoff ~~ weasoff',
                                         'imptrada ~~ imptrada'))

lavTestScore(fit_medi_strict)
partable(fit_medi_strict)
summary(fit_medi_strict,
        standardized = TRUE,
        fit.measures = TRUE)

## Partial Structural Invariance ##

fit_medi_structure <- cfa(medi_model,
                       data = ess_fp,
                       group = 'cntry',
                       estimator = 'MLM',
                       group.equal = c('loadings',
                                       'intercepts',
                                       'residuals',
                                       'lv.variances',
                                       'lv.covariances'),
                       group.partial = c('trad_val =~ ipmodsta',
                                         'anti_fem =~ wexashr',
                                         'ipfrulea ~1',
                                         'rlgdgr ~1',
                                         'ipbhprpa ~~ ipbhprpa',
                                         'rlgdgr ~~ rlgdgr',
                                         'ipfrulea ~~ ipfrulea',
                                         'weasoff ~~ weasoff',
                                         'imptrada ~~ imptrada',
                                         'anti_fem ~~ anti_fem',
                                         'trad_val ~~ trad_val'))
lavTestScore(fit_medi_structure)
partable(fit_medi_structure)
summary(fit_medi_structure, standardized = TRUE,
        fit.measures = TRUE)

## Path Invariance ##

model_medi_mg<-'
trad_val =~ imptrada + ipfrulea + ipbhprpa + ipmodsta
anti_fem =~ wsekpwr + weasoff + wexashr
relig =~ rlgdgr + rlgatnd + pray

## Direct Effect 
anti_fem ~ c("c1", "c2")*relig

## Mediator
trad_val ~ c("a1", "a2")*relig # Well-being regressed on trust in institutions
anti_fem ~ c("b1", "b2")*trad_val # Trust in others regressed on well-being

## Indirect effect
a1b1 := a1*b1
a2b2 := a2*b2

## Total effect
total1 := c1 + (a1*b1)
total2 := c2 + (a2*b2)'

fit_medi_mg <- cfa(model_medi_mg,
                       data = ess_fp,
                       group = 'cntry',
                       estimator = 'MLM',
                       group.equal = c('loadings',
                                       'intercepts',
                                       'residuals',
                                       'lv.variances',
                                       'lv.covariances'),
                       group.partial = c('trad_val =~ ipmodsta',
                                         'anti_fem =~ wexashr',
                                         'ipfrulea ~1',
                                         'rlgdgr ~1',
                                         'ipbhprpa ~~ ipbhprpa',
                                         'rlgdgr ~~ rlgdgr',
                                         'ipfrulea ~~ ipfrulea',
                                         'weasoff ~~ weasoff',
                                         'imptrada ~~ imptrada',
                                         'anti_fem ~~ anti_fem',
                                         'trad_val ~~ trad_val'))
summary(fit_medi_mg, standardized = TRUE, fit.measures = TRUE)

results <- table_results(fit_medi_mg,
                         columns = c('label',
                                     'est_sig'),
                         digits = 2,)
results %>% filter(str_detect(label,
                              'anti_fem.ON|trad_val.ON')) #means welfare support OR egalitarianism

r2 <- round(inspect(fit_medi_mg,'r2'),3)
r2

model_medi_mg_cons <-'
trad_val =~ imptrada + ipfrulea + ipbhprpa + ipmodsta
anti_fem =~ wsekpwr + weasoff + wexashr
relig =~ rlgdgr + rlgatnd + pray

## Direct Effect 
anti_fem ~ c("c1", "c1")*relig

## Mediator (Social health)
trad_val ~ c("a1", "a1")*relig # Well-being regressed on trust in institutions
anti_fem ~ c("b1", "b1")*trad_val # Trust in others regressed on well-being

## Indirect effect
a1b1 := a1*b1

## Total effect
total1 := c1 + (a1*b1)'

fit_medi_mg_cons <- cfa(model_medi_mg_cons,
                        data = ess_fp,
                        group = 'cntry',
                        estimator = 'MLM',
                        group.equal = c('loadings',
                                        'intercepts',
                                        'residuals',
                                        'lv.variances',
                                        'lv.covariances'),
                        group.partial = c('trad_val =~ ipmodsta',
                                          'anti_fem =~ wexashr',
                                          'ipfrulea ~1',
                                          'rlgdgr ~1',
                                          'ipbhprpa ~~ ipbhprpa',
                                          'rlgdgr ~~ rlgdgr',
                                          'ipfrulea ~~ ipfrulea',
                                          'weasoff ~~ weasoff',
                                          'imptrada ~~ imptrada',
                                          'trad_val ~~ trad_val',
                                          'anti_fem ~~ anti_fem'))
summary(fit_medi_mg_cons,
        standardized = TRUE,
        fit.measures = TRUE)
anova(fit_medi_mg, fit_medi_mg_cons) #Regression path invariance

medi_model <- '
trad_val =~ imptrada + ipfrulea + ipbhprpa + ipmodsta
anti_fem =~ wsekpwr + weasoff + wexashr
relig =~ rlgdgr + rlgatnd + pray

## Direct Effect 
anti_fem ~ c*relig

## Mediator (Social health)
trad_val ~ a*relig # Well-being regressed on trust in institutions
anti_fem ~ b*trad_val # Trust in others regressed on well-being

## Indirect effect
ab := a*b

## Total effect
total := c + (a*b)'

fit_medi_model <- cfa(medi_model,
                        data = ess_fp,
                        estimator = 'MLM',
                        group.equal = c('loadings',
                                        'intercepts',
                                        'residuals',
                                        'lv.variances',
                                        'lv.covariances'),
                        group.partial = c('trad_val =~ ipmodsta',
                                          'anti_fem =~ wexashr',
                                          'ipfrulea ~1',
                                          'rlgdgr ~1',
                                          'ipbhprpa ~~ ipbhprpa',
                                          'rlgdgr ~~ rlgdgr',
                                          'ipfrulea ~~ ipfrulea',
                                          'weasoff ~~ weasoff',
                                          'imptrada ~~ imptrada',
                                          'trad_val ~~ trad_val',
                                          'anti_fem ~~ anti_fem'))
summary(fit_medi_model,
        standardized = TRUE,
        fit.measures = TRUE)

results <- table_results(fit_medi_model,
                         columns = c('label',
                                     'est_sig'),
                         digits = 2,)
results %>% filter(str_detect(label,
                              'anti_fem.ON|trad_val.ON')) #means welfare support OR egalitarianism

r2 <- round(inspect(fit_medi_model,'r2'),3)
r2

lay <- get_layout(
  'rlgdgr', 'rlgatnd', 'pray', '', '', '',
  '', 'relig', '', 'trad_val', '', '',
  '', '', '', '', '', '',
  '', 'anti_fem', 'imptrada', 'ipfrulea', 'ipbhprpa', 'ipmodsta',
  '', '', '', '', '', '',
  'wsekpwr', 'weasoff', 'wexashr', '', '', '',
  rows = 6
)

plot_medi <- graph_sem(model = fit_medi_model,
                       layout = lay,
                       angle = 170,
                       label = 'est_std')
plot_medi

results <- table_results(fit_medi,
                         columns = c('label',
                                     'est_sig'),
                         digits = 2,)
results %>% filter(str_detect(label,
                              'welf_supp.ON|egual.ON')) #means welfare support OR egalitarianism

r2 <- round(inspect(fit_medi,'r2'),3)
r2

ess_fp$gndr <- factor(ess_fp$gndr, #Turning gndr into a factor with 2 levels
                      levels = c('1', '2'),
                      labels = c('Male', 'Female'))

fit_medi_config_gndr <- cfa(medi_model,
                           data = ess_fp,
                           group = 'gndr',
                           estimator = 'MLM')
summary(fit_medi_config_gndr, standardized = TRUE, fit.measures = TRUE)
fit_medi_metric_gndr <- cfa(medi_model,
                            data = ess_fp,
                            group = 'gndr',
                            group.equal = c('loadings'),
                            estimator = 'MLM')
fit_medi_scalar_gndr <- cfa(medi_model,
                            data = ess_fp,
                            group = 'gndr',
                            group.equal = c('loadings',
                                            'intercepts'),
                            estimator = 'MLM')
fit_medi_strict_gndr <- cfa(medi_model,
                            data = ess_fp,
                            group = 'gndr',
                            group.equal = c('loadings',
                                            'intercepts',
                                            'residuals'),
                           estimator = 'MLM')
fit_medi_structure_gndr <- cfa(medi_model,
                               data = ess_fp,
                               group = 'gndr',
                               group.equal = c('loadings',
                                               'intercepts',
                                               'residuals',
                                               'lv.variances',
                                               'lv.covariances'),
                               estimator = 'MLM')

table_fit <- 
  list(model_fit(fit_medi_config_gndr), 
       model_fit(fit_medi_metric_gndr), 
       model_fit(fit_medi_scalar_gndr), 
       model_fit(fit_medi_strict_gndr),
       model_fit(fit_medi_structure_gndr)) %>% 
  reduce(rbind)
rownames(table_fit) <- c('Configural', 'Metric', 'Scalar', "Strict", "Structural")
table_fit           
table_anova <- list(anova(fit_medi_config_gndr, fit_medi_metric_gndr),
                    anova(fit_medi_metric_gndr, fit_medi_scalar_gndr),
                    anova(fit_medi_scalar_gndr, fit_medi_strict_gndr),
                    anova(fit_medi_strict_gndr, fit_medi_structure_gndr)) %>%
  reduce(rbind) %>%
  .[-c(3, 5, 7),]
table_anova

lavTestScore(fit_medi_metric_gndr)
partable(fit_medi_metric_gndr)
fit_medi_metric_gndr <- cfa(medi_model,
                            data = ess_fp,
                            group = 'gndr',
                            group.equal = c('loadings'),
                            group.partial = c('relig=~rlgatnd'),
                            estimator = 'MLM')

lavTestScore(fit_medi_scalar_gndr)
partable(fit_medi_scalar_gndr)
fit_medi_scalar_gndr <- cfa(medi_model,
                            data = ess_fp,
                            group = 'gndr',
                            group.equal = c('loadings',
                                            'intercepts'),
                            group.partial = c('relig=~rlgatnd',
                                              'rlgatnd ~1'),
                            estimator = 'MLM')

lavTestScore(fit_medi_strict_gndr)
partable(fit_medi_strict_gndr)
fit_medi_strict_gndr <- cfa(medi_model,
                            data = ess_fp,
                            group = 'gndr',
                            group.equal = c('loadings',
                                            'intercepts',
                                            'residuals'),
                            group.partial = c('relig=~rlgatnd',
                                              'rlgatnd ~1',
                                              'rlgatnd ~~ rlgatnd'),
                            estimator = 'MLM')

lavTestScore(fit_medi_structure_gndr)
partable(fit_medi_structure_gndr)
fit_medi_structure_gndr <- cfa(medi_model,
                            data = ess_fp,
                            group = 'gndr',
                            group.equal = c('loadings',
                                            'intercepts',
                                            'residuals',
                                            'lv.variances',
                                            'lv.covariances'),
                            group.partial = c('relig=~rlgatnd',
                                              'rlgatnd ~1',
                                              'rlgatnd ~~ rlgatnd'),
                            estimator = 'MLM')
summary(fit_medi_structure_gndr,
        standardized = TRUE,
        fit.measures = TRUE)

fit_reg_gndr <- cfa(reg_model,
                    data = ess_fp,
                    group = 'gndr',
                    group.equal = c('loadings',
                                    'intercepts',
                                    'residuals',
                                    'lv.variances',
                                    'lv.covariances'),
                    group.partial = c('relig=~rlgatnd',
                                      'rlgatnd ~1',
                                      'rlgatnd ~~ rlgatnd'),
                    estimator = 'MLM')
summary(fit_reg_gndr,
        standardized = TRUE,
        fit.measures = TRUE) #SLR shows significant relationship in both genders
anova(fit_medi_model_mg_gndr, fit_medi_structure_gndr)

ess_fp$gndr <- factor(ess_fp$gndr, #Turning gndr into a factor with 2 levels
                      levels = c('1', '2'),
                      labels = c('Male', 'Female'))

fit_medi_model_mg_gndr <- cfa(model_medi_mg,
                           data = ess_fp,
                           group = 'gndr',
                           group.equal = c('loadings',
                                           'intercepts',
                                           'residuals',
                                           'lv.variances',
                                           'lv.covariances'),
                           group.partial = c('relig=~rlgatnd',
                                             'rlgatnd ~1',
                                             'rlgatnd ~~ rlgatnd'),
                           estimator = 'MLM')
summary(fit_medi_model_gndr,
        standardized = TRUE,
        fit.measures = TRUE)
fit_medi_model_gndr
fit_medi_mg_cons_gndr <- cfa(model_medi_mg_cons,
                             data = ess_fp,
                             group = 'gndr',
                             group.equal = c('loadings',
                                             'intercepts',
                                             'residuals',
                                             'lv.variances',
                                             'lv.covariances'),
                             group.partial = c('relig=~rlgatnd',
                                               'rlgatnd ~1',
                                               'rlgatnd ~~ rlgatnd'),
                             estimator = 'MLM')
summary(fit_medi_mg_cons_gndr,
        standardized = TRUE,
        fit.measures = TRUE)
anova(fit_medi_model_gndr, fit_medi_mg_cons_gndr)

model_medi_mg_cons <-'
trad_val =~ imptrada + ipfrulea + ipbhprpa + ipmodsta
anti_fem =~ wsekpwr + weasoff + wexashr
relig =~ rlgdgr + rlgatnd + pray

## Direct Effect 
anti_fem ~ c("c1", "c1")*relig

## Mediator (Social health)
trad_val ~ c("a1", "a1")*relig # Well-being regressed on trust in institutions
anti_fem ~ c("b1", "b1")*trad_val # Trust in others regressed on well-being

## Indirect effect
a1b1 := a1*b1

## Total effect
total1 := c1 + (a1*b1)'

fit_medi_mg_cons_gndr <- cfa(model_medi_mg_cons,
                             data = ess_fp,
                             group = 'gndr',
                             group.equal = c('loadings',
                                             'intercepts',
                                             'residuals',
                                             'lv.variances',
                                             'lv.covariances'),
                             group.partial = c('relig=~rlgatnd',
                                               'rlgatnd ~1',
                                               'rlgatnd ~~ rlgatnd'),
                             estimator = 'MLM')
lavTestScore(fit_medi_mg_cons_gndr)
partable(fit_medi_mg_cons_gndr)
