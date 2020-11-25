# Cutoffs for exposure #

#Both Regions
expcut <- rep(1, 7)

names(expcut) <- data %>% select(TXx, TNx, TX90p, TN90p, DTR, Cdd, R99p) %>% names
expcut

#North
north_expcut <- expcut

#Northeast
northeast_expcut <- expcut

# Cutoffs for susceptibility #

data %>% filter(Regiao==1) %>%
  filter(old>quantile(old,probs=c(0.75))) %>%
  mutate(elderly=0) %>%
  select(Mesorregião,elderly) -> x
data %>% filter(Regiao==1) %>%
  filter(old<=quantile(old,probs=c(0.75))) %>%
  mutate(elderly=1) %>%
  select(Mesorregião,elderly) -> y

data %>% filter(Regiao==2) %>%
  filter(old>quantile(old,probs=c(0.75))) %>%
  mutate(elderly=0) %>%
  select(Mesorregião,elderly) -> w
data %>% filter(Regiao==2) %>%
  filter(old<=quantile(old,probs=c(0.75))) %>%
  mutate(elderly=1) %>%
  select(Mesorregião,elderly) -> z

elderly <- as.data.frame(rbind(x,y,w,z))

data <- inner_join(data,elderly, by="Mesorregião", suffix = c("", ""))
table(data$elderly, data$Regiao)

data %>% filter(Regiao==1) %>%
  filter(kid>quantile(kid,probs=c(0.75))) %>%
  mutate(children=0) %>%
  select(Mesorregião,children) -> x
data %>% filter(Regiao==1) %>%
  filter(kid<=quantile(kid,probs=c(0.75))) %>%
  mutate(children=1) %>%
  select(Mesorregião,children) -> y

data %>% filter(Regiao==2) %>%
  filter(kid>quantile(kid,probs=c(0.75))) %>%
  mutate(children=0) %>%
  select(Mesorregião,children) -> w
data %>% filter(Regiao==2) %>%
  filter(kid<=quantile(kid,probs=c(0.75))) %>%
  mutate(children=1) %>%
  select(Mesorregião,children) -> z

children <- as.data.frame(rbind(x,y,w,z))

data <- inner_join(data,children, by="Mesorregião", suffix = c("", ""))
table(data$children, data$Regiao)

suscut <- c(rep(1,2),255,quantile(data$literate,probs=c(0.25)))
names(suscut) <- c("Elderly","Children","Income","Literate")
suscut

north_suscut <- c(rep(1,2),255,quantile(data$literate[data$Regiao==1],probs=c(0.25)))
names(north_suscut) <- c("Elderly","Children","Income","Literate")
north_suscut

northeast_suscut <- c(rep(1,2),255,quantile(data$literate[data$Regiao==2],probs=c(0.25)))
names(northeast_suscut) <- c("Elderly","Children","Income","Literate")
northeast_suscut

# Cuttoffs for adaptive capacity #

adapcut <- c(quantile(data$sewage,probs=c(0.25)),
             quantile(data$water,probs=c(0.25)),
             quantile(data$garbage,probs=c(0.25)),
             quantile(data$urb,probs=c(0.25)))

names(adapcut) <- c("Sewage","Water","Garbage","Urbanization")
round(adapcut,2)

north_adapcut <- c(quantile(data$sewage[data$Regiao==1],probs=c(0.25)),
                   quantile(data$water[data$Regiao==1],probs=c(0.25)),
                   quantile(data$garbage[data$Regiao==1],probs=c(0.25)),
                   quantile(data$urb[data$Regiao==1],probs=c(0.25)))

names(north_adapcut) <- c("Sewage","Water","Garbage","Urbanization")
round(north_adapcut,2)

northeast_adapcut <- c(quantile(data$sewage[data$Regiao==2],probs=c(0.25)),
                       quantile(data$water[data$Regiao==2],probs=c(0.25)),
                       quantile(data$garbage[data$Regiao==2],probs=c(0.25)),
                       quantile(data$urb[data$Regiao==2],probs=c(0.25)))

names(northeast_adapcut) <- c("Sewage","Water","Garbage","Urbanization")
round(northeast_adapcut,2)

