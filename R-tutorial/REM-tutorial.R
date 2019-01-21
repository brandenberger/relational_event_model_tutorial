## Task: Relational event model tutorial using the rem-package
## Author: Laurence Brandenberger, ETH Zurich
## Date: January 2019

################################################################################
################################################################################
################################################################################

## clear workspace
rm(list = ls())

## load libraries
library(rem)
library(survival)
library(ggplot2)
library(Rcpp)
library(texreg)

################################################################################
## load data
################################################################################

## Source: Harvard Dataverse, https://dx.doi.org/doi:10.7910/DVN/Y3QFBP
## Published in: Malang, Brandenberger, Leifeld 2018: Networks and Social 
## Influence in European Legislative Politics. British Journal of Political Science.
## File: R-tutorial/veto_events.RData
## Variables: 
## chamber     = ID of the chamber 
## legnum      = Code of EU legislative proposals
## date.event  = Date of vetoing
## ecadoption  = Date of adoption by the European Commission (start date)
## deadline    = Deadline of legnum
## parliament.name = Name of the chamber
## country     = Country of chamber  
## entryround  = year of accession to the EU
## parfam      = Party family of the majority party (longst ruling period) (Source: Manifesto Project https://manifestoproject.wzb.eu)
## per110      = CMP EU negative mentions (Source: Manifesto Project https://manifestoproject.wzb.eu)
## second.chamber = Dummy variable: Is the chamber a second chamber?
## capacity    = Capacity of the chamber  
## control     = Executive control 
## population  = average population, measured at country level (Source: Worldbank http://data.worldbank.org/)
## gdp.capita  = average gdp per capita, measured at country level (Source: Worldbank http://data.worldbank.org/)
## agriculture = Dummy variable: Does the legnum belong to the agriculture policy sector?
## dg          = policy domain of each legnum (Number of the Directorate General)
## polsys      = Type of political system (according to Armingeon 1 and 2 can be classified as 'presidential')
## Please note that the original analysis published in BJPS contains time-varying
## variables (party family, per110, population, gdp) as well as additional variables.
## For sake of a tutorial some of these variables and additional analyses were cut.

load("R-tutorial/veto_events.RData")
names(dt)

################################################################################
## Create the ordinal event sequence
## Uses createEventSequence-Function in the rem-package
################################################################################

dt <- eventSequence(dt$date.event, dateformat = "%Y-%m-%d", 
                      data = dt, 
                      type = "ordinal", 
                      returnData = TRUE, 
                      sortData = TRUE)

## Task 1: create a new event sequence with continuous time and exclude all
## Saturday and Sundays from the event sequence. Furthermore, exclude 
## Christmas/Winter Holiday from the 24.12. to 2.1.
## What do you notice?
## Task 2: Why should you use "returnData=TRUE"?

################################################################################
## Expand data set: Create Null-Events for each event between start and the veto
## Uses createRemDataset-Function in the rem-Package.
################################################################################

## simplest data structure: 
dtremfull <- createRemDataset(data = dt, 
                        sender = dt$chamber, 
                        target = dt$legnum,
                        eventSequence = dt$event.seq.ord, 
                        atEventTimesOnly = TRUE, 
                        untilEventOccurrs = TRUE,
                        returnInputData = TRUE)
#Note: runs for about 30 seconds

## now taking into account that each legislative proposal has a start and end-date
dtremfull <- createRemDataset(data = dt, 
                        sender = dt$chamber, 
                        target = dt$legnum,
                        eventSequence = dt$event.seq.ord, 
                        time = dt$date.event,
                        startDate = dt$ecadoption, 
                        timeformat = "%Y-%m-%d",
                        atEventTimesOnly = TRUE, 
                        untilEventOccurrs = TRUE,
                        returnInputData = TRUE)
dtrem <- dtremfull[[1]] # this is the counting process data

## change the variable names (optional)
names(dtrem)
names(dtrem)[2] <- 'chamber'
names(dtrem)[3] <- 'legnum'

## sort the data frame according to the event sequence
dtrem <- dtrem[order(dtrem$eventTime), ]

## now let's match in the attributes
dtrem$country <- dt$country[match(dtrem$chamber, dt$chamber)]
dtrem$entryround <- dt$entryround[match(dtrem$chamber, dt$chamber)]
dtrem$parfam <- dt$parfam[match(dtrem$chamber, dt$chamber)]
dtrem$per110 <- dt$per110[match(dtrem$chamber, dt$chamber)]
dtrem$second.chamber <- dt$second.chamber[match(dtrem$chamber, dt$chamber)]
dtrem$capacity <- dt$capacity[match(dtrem$chamber, dt$chamber)]
dtrem$control <- dt$control[match(dtrem$chamber, dt$chamber)]
dtrem$polsys <- dt$polsys[match(dtrem$chamber, dt$chamber)]
dtrem$pop <- dt$pop[match(dtrem$chamber, dt$chamber)]
dtrem$gdp.capita <- dt$gdp.capita[match(dtrem$chamber, dt$chamber)]
dtrem$agriculture <- dt$agriculture[match(dtrem$legnum, dt$legnum)]
dtrem$dg <- dt$dg[match(dtrem$legnum, dt$legnum)]

## 
names(dtrem)

## Task 3: Create the largest possible rem-dataset (works best with the 
## continuous event sequence!)

################################################################################
## Party family aka. ideological homophily
## Calculate the sender node-match variable with the ruling party according to 
## the Manifesto project.
## Uses degreeStat-function in the rem-package
################################################################################

## change factor level of party family variable
dtrem$parfam <- factor(dtrem$parfam, levels = c("30", "20", "40", "50", "60", 
                                              "70", "90"))

## Sender node-match: target popularity with value match 
dtrem$parfam_homophily <- degreeStat(dt, 
                                     time = dtrem$eventTime, 
                                     degreevar = dtrem$legnum, 
                                     halflife = 10, 
                                     eventtypevar = dtrem$parfam, 
                                     eventtypevalue = "valuematch",  
                                     returnData = FALSE, 
                                     eventvar = dtrem$eventdummy, 
                                     showprogressbar = TRUE)

################################################################################
## EU accession round homophily 
## Calculate the sender node-match variable with EU accession round variable.
## Uses degreeStat-function in the rem-package
################################################################################

## activity entry round member on same legnum
dtrem$entryRound_homophily <- degreeStat(dtrem, time = dtrem$eventTime, 
                                       degreevar = dtrem$legnum, halflife = 10, 
                                       eventtypevar = dtrem$entryround, 
                                       eventtypevalue = "valuematch",  
                                       returnData = FALSE, 
                                       eventvar = dtrem$eventdummy, 
                                       showprogressbar = TRUE)

# ################################################################################
# ## EU location homophily
# ## Calculate sender-sender covariate with the edgelist on neighboring countries
# ## Uses sendSendNeighboringCpp-function
# ## Uses neighboring states vector as well
# ################################################################################
# 
# ## specify halflife parameter and specify halflife term 'xlog'
# halflife <- 10
# xlog <- log(2)/halflife
# 
# ## activity of neighboring country in past on same proposal
# data$contiguityInertiaRO <- sendSendNeighboringCpp(
#   as.character(data$country), 
#   as.character(data$legnum),
#   as.character(data$eventdummy), "1",
#   data$eventTime,
#   data$weight,
#   as.character(cont.edgelist$country1), 
#   as.character(cont.edgelist$country2),
#   xlog)


################################################################################
## Political system at event time
## Institutional homophily
## Calculate the sender node-match variable with information the political
## system of a country by Armingeon.
## Uses degreeStat-function in the rem-package
################################################################################

## recode variables
dtrem$polsys.short <- as.character(dtrem$polsys)
dtrem$polsys.short[dtrem$polsys.short ==   '1'] <- '2' # (1 and 2 can be categorized as presidential)

## activity of chamber with same pol sys on same legnum
dtrem$polSys_homophily <- degreeStat(dtrem, time = dtrem$eventTime, 
                                   degreevar = dtrem$legnum, halflife = 10, 
                                   eventtypevar = dtrem$polsys.short, 
                                   eventtypevalue = "valuematch",  
                                   returnData = FALSE, 
                                   eventvar = dtrem$eventdummy, 
                                   showprogressbar = TRUE)

################################################################################
## Issue specificity: repeatedy vetoing proposal from same policy sector
## Calculate target node-match variable with information on the policy area 
## according to the IPEX database.
## Uses degreeStat-function in the rem-package
################################################################################

## same actor vetoing DG-proposal before
dtrem$sameDG_homophily <- degreeStat(dtrem, time = dtrem$eventTime, 
                                   degreevar = dtrem$chamber, halflife = 10, 
                                   eventtypevar = dtrem$dg, 
                                   eventtypevalue = "valuematch",  
                                   returnData = FALSE, 
                                   eventvar = dtrem$eventdummy, 
                                   showprogressbar = TRUE)

################################################################################
## Covariate and Absolute difference sender-sender covariate for GDP and pop.
## Calculate GDP/capita and population variables (both as covariates and 
## absolute difference variables)
## uses the absDiffWTimeCpp-functions
################################################################################

## Function: absDiffWTimeCpp - Calculates weighted absolute differences between
## the present country involved in a veto and all past countries that issued a
## veto in the past. The weight reduces the difference over time.
## Input variable:
# sender              = chamber involved in the event
# target              = proposal involved in the event
# time                = ordinal event sequence
# weightvar           = event weight
# eventattributevar   = event attribute variable
# eventattribute      = event attribute value that is used to filter the event
#                       sequence
# xlog                = log(2)/halflife;part of the exponential decay function
## Output variable:
# result              = vector of values for the sender-sender covariate

## You can add your own cpp-functions to speed up the network statistics calculations!
cppFunction('
  NumericVector absDiffWTimeCpp(
    std::vector<std::string> sender,
    std::vector<std::string> target,
    NumericVector time,
    NumericVector weightvar,
    std::vector<std::string> eventattributevar,
    std::string eventattribute,
    double xlog) {
    
    NumericVector result(time.size());
    int count = 0;
    
    // for each event, do:
      for ( int i = 0; i < time.size(); i++){
        
        double weight = 0;
        double totaldiff = 0;
        count = 0;
        
        // for each past event, do:
          for ( int w = 0; w < i; w++ ) {
            
            // if past sender is not the same as the present sender, and the past
            // target is the same as the present target, and the eventattribute
            // of the past event matches with the eventattribute specified, do:
              if (eventattributevar[w] == eventattribute && sender[i] != sender[w] && target[i] == target[w]) {
                if ( time[i] != time[w] ) {
                  count = count + 1;
                  // calculate the weight
                  weight = std::abs(weightvar[i] - weightvar[w]) * exp( - ( time[i] - time[w] ) * xlog) * xlog ;
                  totaldiff = totaldiff + weight;
                } 
              }
          }// closes w-loop
        if (count == 0) {
          result[i] = 0;
        }else{
          result[i] = totaldiff/count;
        }
      } // closes i-loop
    return Rcpp::wrap(result);
  }')

## specify halflife parameter and specify halflife term 'xlog'
halflife <- 10
xlog <- log(2)/halflife

## absolute difference between population for sender-target chambers 
## (same legnum, with time)
dtrem$populationAbsDiffWTime <- absDiffWTimeCpp(as.character(dtrem$chamber),
                                                 as.character(dtrem$legnum),
                                                 dtrem$eventTime, 
                                                 dtrem$pop,
                                                 as.character(dtrem$eventDummy), 
                                                 "1", xlog)

## absolute difference between gdp per capita for sender-target chambers 
## (same legnum, with time)
dtrem$gdpAbsDiffWTime <- absDiffWTimeCpp(as.character(dtrem$chamber),
                                                 as.character(dtrem$legnum),
                                                 dtrem$eventTime, 
                                                 dtrem$gdp.capita,
                                                 as.character(dtrem$eventDummy), 
                                                 "1", xlog)

################################################################################
## Activity actor
## Calculates how often in the past a current chamber issued vetoes.
## Uses degreeStat-function in the rem-package
################################################################################

dtrem$activityActor <- degreeStat(dtrem, time = dtrem$eventTime, 
                                  degreevar = dtrem$chamber, halflife = 10, 
                                  returnData = FALSE, 
                                  eventvar = dtrem$eventDummy, 
                                  showprogressbar = TRUE)

################################################################################
## Concept popularity
## Calculates how often in general, the current proposal was vetoed. It 
## encompasses the homophily variables, which include an additional filter in 
## the form of a valuematch or absolute difference.
## Uses the degreeStat-function in the rem-package.
################################################################################

dtrem$popularityConcept <- degreeStat(dtrem, time = dtrem$eventTime, 
                                       degreevar = dtrem$legnum, halflife = 10, 
                                       returnData = FALSE, 
                                       eventvar = dtrem$eventDummy, 
                                       showprogressbar = TRUE)

################################################################################
## Endogenous network statistics: 
## Task 4: calculate the inertia statistic, summarize it and explain the output
## Task 5: calculate the four-cycle statistic; what is the interpretation of it?
## Task 6: run the four-cycle statistic in parallel on as many cores as your computer allows
## Task 7: re-run the statistics using the halflife parameter of your choice
## (remember: smaller half-life = more recent events more important!)
## Task 8: check the correlations between the statistics. Why does it sometimes
## show very high correlations among the statistics (and better question: why is it not that worriesome?)
################################################################################

################################################################################
## Bivariate statistics
################################################################################

## run t-tests on your data: 
t.test(dtrem$fourCycle ~ dtrem$eventDummy)

## plot development of statistic (as time-series)
ggplot(dtrem, aes(x = eventTime, y = parfam_homophily, 
                  group = factor(eventDummy), color = factor(eventDummy)))+
  geom_jitter()+
  geom_smooth()

## or look at correlations
cor(cbind(dtrem$parfam_homophily_hl1, dtrem$parfam_homophily, 
          dtrem$parfam_homophily_hl150))
# see comment in the solution

################################################################################
## Rescale network variables by a specific, meaningful constant
## Task 9: rescale variables with different halflife-parameter
################################################################################

## baseline becomes: +1 event, 10 days ago
timediff <- 10
xlog10 <- log(2)/10  # specify a halflife of 10
baseline <- exp(-(timediff)*xlog10) * xlog10

## divide by meaningful constant
dtrem$parfam_homophily_rescaled <- dtrem$parfam_homophily/baseline
dtrem$entryRound_homophily_rescaled <- dtrem$entryRound_homophily/baseline
dtrem$polSys_homophily_rescaled <- dtrem$polSys_homophily/baseline
dtrem$sameDG_homophily_rescaled <- dtrem$sameDG_homophily/baseline
dtrem$activityActor_rescaled <- dtrem$activityActor/baseline
dtrem$popularityConcept_rescaled <- dtrem$popularityConcept/baseline

## rescaling the four-cycle statistic
# 3 events: 1x 5 days ago, 2x 20 days ago, with halflife of 10
tempfour <- (((exp(-(5)*(log(2)/10))*(log(2)/10)) * (exp(-(20)*(log(2)/10))*(log(2)/10)) * (exp(-(20)*(log(2)/10))*(log(2)/10))))^(1/3)
dtrem$fourCycle_rescaled <- dtrem$fourCycle/tempfour
  
## use slightly different baseline for trade and gdp
baseline.population <- 1000 * exp(-(timediff)*xlog10) * xlog10
dtrem$populationAbsDiffWTime_rescaled <- dtrem$populationAbsDiffWTime/baseline.population
baseline.gdp <- 10 * exp(-(timediff)*xlog10) * xlog10
dtrem$gdpAbsDiffWTime_rescaled <- dtrem$gdpAbsDiffWTime/baseline.gdp

################################################################################
## Estimate conditional logit model
## Task 10: test out different halflife-parameters: check interpretations of results
################################################################################ 

## 
fit0 <- clogit(eventDummy ~ 
                 # main effects at chamber level
                 gdp.capita + second.chamber + capacity  + 
                 control +   as.numeric(per110)  +
                 # main effects at poposal level
                 agriculture +  
                 # specify the strata => stratified cox model
                 strata(eventTime), 
               method="exact", data=dtrem)

##
fit1 <- clogit(eventDummy ~ 
                 # endogenous network statistics
                 activityActor_rescaled +
                 parfam_homophily_rescaled +
                 entryRound_homophily_rescaled +
                 polSys_homophily_rescaled +
                 fourCycle_rescaled + 
                 gdpAbsDiffWTime +
                 # main effects at chamber level
                 gdp.capita + second.chamber + capacity  + 
                 control +   as.numeric(per110)  + 
                 # nw effects at proposal level
                 sameDG_homophily_rescaled + 
                 # main effects at poposal level
                 agriculture +  
                 # specify the strata
                 strata(eventTime), 
               method="exact", data=dtrem)

## check out results
summary(fit1)

## or in the standard regression table format
screenreg(list(fit0, fit1))

## calculate mcFadden pseudo-R^2
## not (yet?) supported
#library(pscl)
#pR2(fit1): 
## own calculation
1- (fit1$loglik[2]/fit1$loglik[1])

################################################################################ 
## save output and data
################################################################################ 

save(dtrem, dt, fit0, fit1, file = "R-tutorial/veto_REMs_final.RData")
