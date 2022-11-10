
#Add Time Factor
timeCat <- function(cond) {
  condTime1 <- cond %>% mutate(Time = as.factor(0))
  condTime2 <- cond %>% mutate(Time = as.factor(1))
  condTimes <- bind_rows(condTime1, condTime2)
  preScore <- condTimes %>% filter(Time == 0) %>% select(-LSAS.SR.post) %>% rename("LSAS.SR" = "LSAS.SR.pre") %>%
    select(-PostAdj) %>% rename("LSASAdj" = "PreAdj")
  postScore <- condTimes %>% filter(Time == 1) %>% select(-LSAS.SR.pre) %>% rename("LSAS.SR" = "LSAS.SR.post") %>%
    select(-PreAdj) %>% rename("LSASAdj" = "PostAdj")
  prePost <- bind_rows(preScore, postScore)
  prePost %>% arrange(Ppn)
} 

#Separating Conditions
cond0 <- data %>% filter(Condition == "0")
cond1 <- data %>% filter(Condition == "1")
cond2 <- data %>% filter(Condition == "2")
cond0 <- timeCat(cond0)
cond1 <- timeCat(cond1)
cond2 <- timeCat(cond2)
cond0Pre <- cond0 %>% filter(Time == 0)
cond0Post <- cond0 %>% filter(Time == 1)
cond1Pre <- cond1 %>% filter(Time == 0)
cond1Post <- cond1 %>% filter(Time == 1)
cond2Pre <- cond2 %>% filter(Time == 0)
cond2Post <- cond2 %>% filter(Time == 1)

#Combined Data
combine <- function(time) {
  if(time == "pre"){
    bind_rows(cond0Pre, cond1Pre, cond2Pre)
  }else if(time == "post"){
    bind_rows(cond0Post, cond1Post, cond2Post)
  } else if(time == "both"){
    bind_rows(cond0, cond1, cond2)
  }
}


#Summary of Data
womenRatio <- function(cond) {
  sum(cond[3] == "woman")/nrow(cond[3])
}

summaryDat <- function(cond) {
  summarize(cond, n = nrow(cond), Ratio_Women = womenRatio(cond),
            Mean_Age = mean(Age), SD_Age = sd(Age),
            Mean_LSAS_Pre = sapply((cond %>% filter(Time == "0") %>% 
                                      select(LSAS.SR)), mean), 
            SD_LSAS_Pre = sapply((cond %>% filter(Time == "0") %>% 
                                    select(LSAS.SR)), sd),
            Mean_LSAS_Post = sapply((cond %>% filter(Time == "1") %>% 
                                       select(LSAS.SR)), mean), 
            SD_LSAS_Post = sapply((cond %>% filter(Time == "1") %>% 
                                     select(LSAS.SR)), sd))
}

#Assumption Tests
assumptionTest <- function(data) { 
  normPre <- data %>% group_by(Condition) %>% shapiro_test(LSAS.SR.pre)
  normPost <- data %>% group_by(Condition) %>% shapiro_test(LSAS.SR.post)
  homPre <- levene_test(preCombined, LSAS.SR ~ Condition, center = median)
  homPost <- levene_test(postCombined, LSAS.SR ~ Condition, center = median) 
  print("Normality Test: Pre-Test:")
  print(normPre)
  print("Normality Test: Post-Test:")
  print(normPost)
  print("Homogeneity of Variance: Pre-Test:")
  print(homPre)
  print("Homogeneity of Variance: Post-Test:")
  print(homPost)
}

#Factorial Mixed ANOVA
mixAOV <- function(data) {
  aov_ez(
    id = "Ppn",
    dv = "LSAS.SR",
    data = data,
    between = "Condition",
    within = "Time",
  )
}

#Reference for Plot
plotRef <- function(data) {
  modelPlot <- aov_ez(
    id = "Ppn",
    dv = "LSASAdj",
    data = datCombined,
    between = "Condition",
    within = "Time"
  )
  lsmeans(modelPlot, specs = c("Condition", "Time"))
}

#Interaction Plot
intPlot <- function(reference) {
  ggplot(data.frame(reference), aes(x = Time, y = lsmean, group = Condition, color = Condition)) +
    geom_line(position = position_dodge(width = .5), size = 1) + 
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(width = .5), size = 1) +
    geom_point(position = position_dodge(width = .5)) +
    scale_x_discrete(labels = c("Pre-Test", "Post-Test")) +
    scale_y_continuous(breaks = seq(45, 110, by = 10), limits = c(45, 110), name = "Mean LSAS-SR (95% CI)") +
    scale_color_discrete(labels = c("IAR", "ICBT", "IVRET"), type = c("#56641a", "#c0affb", "#e6a176"), name = "Treatment") +
    theme_classic()
}
