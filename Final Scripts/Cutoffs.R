# Cutoffs for exposure #

#Both Regions
expcut <- rep(1, 7)

names(expcut) <- data %>% select(TXx, TNx, TX90p, TN90p, DTR, Cdd, R99p) %>% names()
expcut

#Wet Homoclimatic Zone
wet_expcut <- expcut

#Dry Homoclimatic Zone
dry_expcut <- expcut

# Cutoffs for susceptibility #

data %<>% group_by(dzone) %>%
  mutate(elderly = case_when(
    old>quantile(old,probs=c(0.75)) ~ 0,
    old<=quantile(old,probs=c(0.75)) ~ 1),
        children = case_when(
    kid>quantile(kid,probs=c(0.75)) ~ 0,
    kid<=quantile(kid,probs=c(0.75)) ~ 1
        ))


table(data$elderly, data$dzone)

table(data$children, data$dzone)

suscut <- c(rep(1,2),
            255,
            quantile(data$poor,probs=c(0.75)),
            quantile(data$literate,probs=c(0.25)))
names(suscut) <- c("Elderly","Children","Income","Poor","Literate")
suscut

wet_suscut <- c(rep(1,2),
                255,
                quantile(data$poor[data$dzone==1],probs=c(0.75)),
                quantile(data$literate[data$dzone==1],probs=c(0.25)))
names(wet_suscut) <- c("Elderly","Children","Income","Poor","Literate")
wet_suscut

dry_suscut <- c(rep(1,2),
                255,
                quantile(data$poor[data$dzone==2],probs=c(0.75)),
                quantile(data$literate[data$dzone==2],probs=c(0.25)))
names(dry_suscut) <- c("Elderly","Children","Income","Poor","Literate")
dry_suscut

# Cuttoffs for adaptive capacity #

adapcut <- c(quantile(data$sewage,probs=c(0.25)),
             quantile(data$water,probs=c(0.25)),
             quantile(data$garbage,probs=c(0.25)),
             quantile(data$urb,probs=c(0.25)),
             quantile(data$fhs,probs=c(0.25)),
             quantile(data$beds,probs=c(0.25)))

names(adapcut) <- c("Sewage","Water","Garbage","Urbanization","Primary Care","Hospital Beds")
round(adapcut,2)

wet_adapcut <- c(quantile(data$sewage[data$dzone==1],probs=c(0.25)),
                   quantile(data$water[data$dzone==1],probs=c(0.25)),
                   quantile(data$garbage[data$dzone==1],probs=c(0.25)),
                   quantile(data$urb[data$dzone==1],probs=c(0.25)),
                   quantile(data$fhs[data$dzone==1],probs=c(0.25)),
                   quantile(data$beds[data$dzone==1],probs=c(0.25)))

names(wet_adapcut) <- c("Sewage","Water","Garbage","Urbanization","Primary Care","Hospital Beds")
round(wet_adapcut,2)

dry_adapcut <- c(quantile(data$sewage[data$dzone==2],probs=c(0.25)),
                       quantile(data$water[data$dzone==2],probs=c(0.25)),
                       quantile(data$garbage[data$dzone==2],probs=c(0.25)),
                       quantile(data$urb[data$dzone==2],probs=c(0.25)),
                       quantile(data$fhs[data$dzone==2],probs=c(0.25)),
                       quantile(data$beds[data$dzone==2],probs=c(0.25)))

names(dry_adapcut) <- c("Sewage","Water","Garbage","Urbanization","Primary Care","Hospital Beds")
round(dry_adapcut,2)

