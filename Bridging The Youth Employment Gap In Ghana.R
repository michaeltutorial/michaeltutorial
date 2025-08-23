## IMPORTING THE DATASETS.
# install.packages('readxl')
# Importing the Economic Activity Dataset.
library(dplyr)
library(plotly)
library(ggplot2)
library(tidyverse)
#install.packages("janitor")
library(janitor) # for tabyl() frequency tables
#install.packages('psych')
library(psych) # for describe() summaries.
library(readxl)
library(vcd)
library(nnet)
library(broom)
econ <- read_excel('D:/Research Items/Bridging the Youth Employment Gap in Ghana Analyzing the Impact of Education and Locality on Unemployment and Occupational Access/Economic Activity .xlsx')
# Shuffling The 'econ' data.
econ_shuff <- econ[sample(nrow(econ)), ]
View(econ_shuff)

# Importing the Occupational Dataset.
occup <- read_xlsx('D:/Research Items/Bridging the Youth Employment Gap in Ghana Analyzing the Impact of Education and Locality on Unemployment and Occupational Access/Occupation.xlsx')
# Shuffling The 'Occup' data.
occup_shuff <- occup[sample(nrow(occup)), ]

# Importing the Unemployment Rate Dataset.
unemp <- read_xlsx('D:/Research Items/Bridging the Youth Employment Gap in Ghana Analyzing the Impact of Education and Locality on Unemployment and Occupational Access/Unemployment Rate .xlsx')
# Shuffling The 'unemp' data.
unemp_shuff <- unemp[sample(nrow(unemp)), ]
View(unemp_shuff)

# HANDLING MISSING VALUES AND DECIMAL PLACES IN THE unemp_shuff DATASET.
# 1. Replace '...' with NA across the dataset.
unemp_shuff[unemp_shuff == '..'] <- NA
View(unemp_shuff)
# 2. Convert age group columns to numeric since '..' may have forced them to character type.
age_cols <- c('15-19', '20-24', '25-29', '30-34', '35-39')
# Step 2: Convert all to numeric safely
unemp_shuff <- unemp_shuff %>%
  mutate(across(all_of(age_cols), ~ as.numeric(as.character(.))))

# Step 3: Handle missing values (set NA = 0 here)
unemp_shuff <- unemp_shuff %>%
  mutate(across(all_of(age_cols), ~ ifelse(is.na(.), 0, .)))

# Step 4: Round to 1 decimal place
unemp_shuff <- unemp_shuff %>%
  mutate(across(all_of(age_cols), ~ round(., 1)))
head(unemp_shuff)
str(unemp_shuff)


## 1. Exploring The econ_shuff data.
## DESCRIPTIVE SUMMARIES FOR econ_shuff DATASET.
# Column names.
colnames(econ_shuff)
# Data Structure.
str(econ_shuff)
head(econ_shuff, 10)
# Summary Of numeric variables(age groups)
describe(econ_shuff %>% select('15-19', '20-24', '25-29', '30-34', '35-39'))

# 2. Frequency Counts For Categorical Variables.
## Economic Activity.
table(econ$Econact)
## Education Levels.
table(econ_shuff$Education)
## Urban Vs Rural Distribution.
table(econ_shuff$Locality)
## Regional Distribution.
table(econ_shuff$Geographic_Area)
## Gender Distribution.
table(econ_shuff$Sex)

# 3. Descriptive Statistics For Age Groups.
#install.packages("plotly")
## Total Across Each Age Group.
colSums(econ_shuff[, c('15-19', '20-24', '25-29', '30-34', '35-39')])
## Mean And Standard Deviation By Age Group.
sapply(econ_shuff[, c('15-19', '20-24', '25-29', '30-34', '35-39')],
       function(x) c(mean = mean(x), sd = sd(x)))

## 1. Education Vs Age groups.
edu_summary <- econ_shuff %>%
  group_by(Education) %>%
  summarise(across('15-19':'35-39', sum, na.rm = TRUE))
print(edu_summary)
# Plot: Education Distribtion Across Age Groups.
edu_summary_long <- edu_summary %>%
  tidyr::pivot_longer(cols = '15-19':'35-39', names_to = 'AgeGroup', values_to = 'Count')

E <- ggplot(edu_summary_long, aes(x = Education, y = Count, fill = AgeGroup)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_minimal() +
  coord_flip() +
  labs(title = 'Distribution Of Age Groups by Education Level', x = 'Education', y = 'Count')

# Convert to Interactive loltly.
ggplotly(E, tooltip = 'text')


## 2. Locality vs Age Groups.
loc_summary <- econ_shuff %>%
  group_by(Locality) %>%
  summarise(across('15-19':'35-39', sum, na.rm = TRUE))

loc_summary_long <- loc_summary %>%
  tidyr::pivot_longer(cols = '15-19':'35-39', names_to = 'AgeGroup', values_to = 'Count')

L <- ggplot(loc_summary_long, aes(x = Locality, y = Count, fill = AgeGroup)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_minimal() +
  coord_flip() +
  labs(title = 'Distribution Of Age Groups by Locality', x = 'Locality', y = 'Count')

ggplotly(L, tooltip = 'numeric')

## 3.Gender vs Age Groups.
sex_summary <- econ_shuff %>%
  group_by(Sex) %>%
  summarise(across('15-19':'35-39', sum, na.rm = TRUE))

sex_summary_long <- sex_summary %>%
  tidyr::pivot_longer(cols = '15-19':'35-39', names_to = 'AgeGroup', values_to = 'Count')

S <- ggplot(sex_summary_long, aes(x = Sex, y = Count, fill = AgeGroup)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_minimal() +
  coord_flip() +
  labs(title = 'Distribution Of Age Groups Gender', x = 'Gender', y = 'Count')

ggplotly(S, tooltip = 'numeric')

## 4. Geographical_Area vs Age Groups.
area_summary <- econ_shuff %>%
  group_by(Geographic_Area) %>%
  summarise(across('15-19':'35-39', sum, na.rm = TRUE))

area_summary_long <- area_summary %>%
  tidyr::pivot_longer(cols = '15-19':'35-39', names_to = 'AgeGroup', values_to = 'Count')

G <- ggplot(area_summary_long, aes(x = Geographic_Area, y = Count, fill = AgeGroup)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_minimal() +
  coord_flip() +
  labs(title = 'Distribution Of Age Groups by Geographic Area', x = 'Geographic Area', y = 'Count')

ggplotly(G, tooltip = 'numeric')


## DESCRIPTIVE SUMMARIES FOR occ_shuff DATASET.
# 1. Structure.
str(occup_shuff)
head(occup_shuff, 10)

# 2. Frequency tables for key categorical variables.
View(table(occup_shuff$Occupation))
View(table(occup_shuff$Education))
View(table(occup_shuff$Sex))
View(table(occup_shuff$Geographic_Area))

# 3. Cross-tab 
table(occup_shuff$Occupation, occup_shuff$Sex)
table(occup_shuff$Occupation, occup_shuff$Education)
table(occup_shuff$Occupation, occup_shuff$Geographic_Area)

# 4.Age group Summaries.
colSums(occup_shuff[, age_cols], na.rm = TRUE)
describe(occup_shuff %>% select('15-19', '20-24', '25-29', '30-34', '35-39'))

# 5. Plot Age group distribution by occupation.
library(reshape2); library(ggplot2)
occup_long <- melt(occup_shuff, id.vars = c('Occupation', 'Education', 'Sex', 'Geographic_Area'),
                   measure.vars = age_cols, variable.name = 'AgeGroup', value.name = 'Count')

ggplot(occup_long, aes(x = AgeGroup, y = Count, fill = Occupation)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_minimal() +
  labs(title = 'Distribution Of Age Groups By Occupation.')


## DESCRIPTIVE SUMMARIES FOR occ_shuff DATASET.
# 1. Structure.
str(unemp_shuff)
summary(unemp_shuff)

# 2. Frequency tables.
#install.packages('summarytools')
library(summarytools)
dfSummary(unemp_shuff)

# 3. Age Group Totals.
colSums(unemp_shuff[, age_cols], na.rm = TRUE)

# 4. Average Unemployment Rate By Education Level.
unemp_shuff %>% 
  group_by(Education) %>%
  summarise(across(all_of(age_cols), mean, na.rm = TRUE))
# Average Unemployment Rate By Sex.
unemp_shuff %>% 
  group_by(Sex) %>%
  summarise(across(all_of(age_cols), mean, na.rm = TRUE))

# 5. Plots
# BOXPLOT: 1.Unemployment Rate By Sex Across Age Groups.
unemp_long <- unemp_shuff %>%
  tidyr::pivot_longer(cols = all_of(age_cols), names_to = 'AgeGroup', values_to = 'UnemploymentRate')

ggplot(unemp_long, aes(x = AgeGroup, y = UnemploymentRate, fill = Sex)) +
  geom_boxplot() +
  labs(title = 'Unemployment Rate by Age Group And Sex.')

# 2. Boxplot by sex within each region.
ggplot(unemp_long, aes(x = Sex, y = UnemploymentRate, fill = Sex)) +
  geom_boxplot() +
  facet_wrap(~ Geographic_Area) +
  labs(title = 'Unemployment Rate Distribution by Sex Across Regions',
       x = 'Sex', y = 'Unemployment Rate (%)')

# BAR CHART: 1. mean unemployment by education.
edu_summary <- unemp_long %>%
  group_by(Education, AgeGroup) %>%
  summarise(MeanRate = mean(UnemploymentRate, na.rm = TRUE), .groups = 'drop')

ggplot(edu_summary, aes(x = AgeGroup, y = MeanRate, fill = Education)) +
  geom_col(position = 'dodge') +
  labs(title = 'Age Unemployment Rate by Education Level and Age Group.')

# 2. Bar Plot by Education Within Each Region.
ggplot(unemp_long, aes(x = AgeGroup, y = UnemploymentRate, fill = Education)) +
  geom_col(position = 'dodge') +
  facet_wrap(~ Geographic_Area) +
  labs(title = 'Unemployment by Education Level Across Regions',
       x = 'Age Group', y = 'Unemployment Rate(%)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# LINE: 1. Unemployment Trend Across Age Groups.
ggplot(edu_summary, aes(x = AgeGroup, y = MeanRate, color = Education, group = Education)) +
  geom_line() +
  labs(title = 'Unemployment Trend by Education Level.')

# 2. Line plot by region.
ggplot(unemp_long, aes(x = AgeGroup, y = UnemploymentRate, color = Sex, group = Sex)) +
  geom_line(size = 1) +
  facet_wrap(~ Geographic_Area) +
  labs(title = 'Unemployment Trends by Age Group Across Regions',
       x = 'Age Group', y = 'Unemployment Rate (%)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

head(econ_shuff)
head(occup_shuff)
head(unemp_shuff)
View(age_cols)



# Pivoting The Age Groups Into A Single Age Column.
## 1. Reshape econ_shuff.
econ_piv <- econ_shuff %>%
  pivot_longer(cols = all_of(age_cols),
               names_to = 'Age',
               values_to = 'Count')

## 2. Reshape occup_shuff.
occup_piv <- occup_shuff %>%
  pivot_longer(cols = all_of(age_cols),
               names_to = 'Age',
               values_to = 'Count')

## 3. Reshape unemp_shuff.
unemp_piv <- unemp_shuff %>%
  pivot_longer(cols = all_of(age_cols),
               names_to = 'Age',
               values_to = 'Rate')
## Checking The Reshaped Data.
head(econ_piv)
head(occup_piv)
head(unemp_piv)
View(econ_piv)
# Keeping Consistent Factor Baselines.
unemp_piv <- unemp_piv %>% 
  mutate(
    Education = factor(Education),
    Sex = factor(Sex),
    Locality = factor(Locality),
    Geographic_Area = factor(Geographic_Area),
    Age = factor(Age, levels = c('15-19', '20-24', '25-29', '30-34', '35-39'))
  )
occup_piv <- occup_piv %>% 
  mutate(
    Education = factor(Education),
    Sex = factor(Sex),
    Geographic_Area = factor(Geographic_Area),
    Age = factor(Age, levels = c('15-19', '20-24', '25-29', '30-34', '35-39'))
  )
econ_piv <- econ_piv %>% 
  mutate(
    Education = factor(Education),
    Sex = factor(Sex),
    Locality = factor(Locality),
    Geographical_Area = factor(Geographic_Area),
    Age = factor(Age, levels = c('15-19', '20-24', '25-29', '30-34', '35-39'))
  )



# ASSOCIATION TESTS (CHI-SQUARE + CRAMER'S V)
## A. Education X Economic Activity (using tool across ages)
econ_tab <- econ_piv %>%
  group_by(Education, Econact) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = 'drop') %>%
  xtabs(Count ~ Education + Econact, data = .)
# Chi-square
chisq.test(econ_tab)
# Cramer's V
assocstats(econ_tab)

## B. Education X Occupation (using totals across ages).
occup_tab <- occup_piv %>%
  group_by(Education, Occupation) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = 'drop') %>%
  xtabs(Count ~ Education + Occupation, data = .)
# Chi-square
chisq.test(occup_tab)
# Cramer's V
assocstats(occup_tab)

## C. Locality X High Unemployment (create binary by age-specific median).
## Rate above median within each Age.
unemp_flag <- unemp_piv %>%
  group_by(Age) %>%
  mutate(HighUnemp = Rate > median(Rate, na.rm = TRUE)) %>%
  ungroup()


# DIFFERENCE TESTS (t-test / ANOVA / nonparametric)
## A. Sex differences in unemployment (eg. Age = 20 -24)
t.test(Rate ~ Sex, data = filter(unemp_piv, Age == '20-24'))

# B. Education Differences In Unemployment (ANOVA + post-hoc)
anova_fit <- aov(Rate ~ Education, data = filter(unemp_piv, Age == '20-24'))
summary(anova_fit)
TukeyHSD(anova_fit)

# C. Nonparametric Alternative(Kruskal-Wallis)
kruskal.test(Rate ~ Education, data = filter(unemp_piv, Age == '20-24'))


# CORRELATION ANALYSIS.
# A. Correlation Among Age Groups Of Unemployment.(wide matrix per region)
unemp_wide_region <- unemp_piv %>%
  group_by(Geographic_Area, Age) %>%
  summarise(MeanRate = mean(Rate, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = Age, values_from = MeanRate)

# Compute Correlation Matrix Across Age Groups.
corr_mat <- unemp_wide_region %>%
  select('15-19', '20-24', '25-29', '30-34', '35-39') %>%
  cor(use = 'pairwise.complete.obs', method = 'spearman')
corr_mat
# Correlation Heatmap.
corr_df <- melt(corr_mat)
ggplot(corr_df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(low = 'steelblue', mid = 'white', high = 'firebrick', midpoint = 0) +
  labs(title = 'Spearman Correlation Across Age Groups (Unemployment)') +
  theme_minimal()

# B. Correlation Between Economic Activity Counts And Unemployment Rate (merged).
econ_age <- econ_piv %>% 
  group_by(Education, Sex, Locality, Geographic_Area, Age) %>%
  summarise(EconCount = sum(Count, na.rm = TRUE), .groups = 'drop')

unemp_age <- unemp_piv %>%
  group_by(Education, Sex, Locality, Geographic_Area, Age) %>%
  summarise(UnempRate = mean(Rate, na.rm = TRUE), .groups = 'drop')

econ_unemp <- inner_join(econ_age, unemp_age,
                         by = c('Education', 'Sex', 'Locality', 'Geographic_Area', 'Age'))
# Spearman Correlation Overall And By Age.
cor.test(econ_unemp$EconCount, econ_unemp$UnempRate, method = 'spearman')

econ_unemp %>%
  group_by(Age) %>%
  summarise(rho = suppressWarnings(cor(EconCount, UnempRate, method = 'spearman', use = 'pairwise.complete.obs')),
            .groups = 'drop')


# REGRESSION MODELS.
# A. Linear Regression: Unemployment Rate ~ Education + Sex + Locality + Region + Age.
lm_fit <- lm(Rate ~ Education + Sex + Locality + Geographic_Area + Age, data = unemp_piv)
summary(lm_fit)

# B. Logistic Regression: High vs Low Unemployment (by age median).
unemp_bin <- unemp_piv %>%
  group_by(Age) %>%
  mutate(HighUnemp = as.integer(Rate > median(Rate, na.rm = TRUE))) %>%
  ungroup()

logit_fit <- glm(HighUnemp ~ Education + Sex + Locality + Geographic_Area + Age,
                 data = unemp_bin, family = binomial)
summary(logit_fit)
broom::tidy(logit_fit, exponentiate = TRUE, conf.int = TRUE) # Odds Ratios.

# C. Multinomial Regression: Occupation Choice ~ demographics.
# (aggregate to one row per profile across ages: use total Count as 'prescene'.)
occ_tot <- occup_piv %>%
  group_by(Occupation, Education, Sex, Geographic_Area) %>%
  summarise(OccTotal = sum(Count, na.rm = TRUE), .groups = 'drop') %>%
  filter(OccTotal > 0)

mult_fit <- nnet::multinom(Occupation ~ Education + Sex + Geographic_Area, data = occ_tot, trace = FALSE)
summary(mult_fit)

# D. Multinomial On Economic Activity.
econ_tot <- econ_piv %>%
  group_by(Econact, Education, Sex, Locality, Geographic_Area) %>%
  summarise(EconTotal = sum(Count, na.rm = TRUE), .groups = 'drop') %>%
  filter(EconTotal > 0)

econ_mult <- nnet::multinom(Econact ~ Education + Sex + Locality + Geographic_Area, data = econ_tot, trace = FALSE)
summary(econ_mult)


# REGIONAL & DEMOGRAPHIC PROFILING.
# A. Regional age-profile (mean unemployment by region x age).
unemp_region_age <- unemp_piv %>% 
  group_by(Geographic_Area, Age) %>%
  summarise(MeanRate = mean(Rate, na.rm = TRUE), .groups = 'drop')

ggplot(unemp_region_age, aes(Age, MeanRate, group = Geographic_Area)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~ Geographic_Area) +
  labs(title = 'Unemployment Profiles by Region (Age Trajectories)', x = 'Age Group', y = 'Mean Unemployment Rate') +
  theme_minimal()

# B. Heatmap: Region x Age (mean unemployment)
ggplot(unemp_region_age, aes(x = Age, y = Geographic_Area, fill = MeanRate)) +
  geom_tile() +
  scale_fill_gradient(low = 'white', high = 'red') +
  labs(title = 'Heatmap: Mean Unemployment Rate by Region and Age', x = 'Age Group', y = 'Region', fill = 'Mean %') +
  theme_minimal()

# C. Education x Locality profiles(nation view).
unemp_edu_loc <- unemp_piv %>%
  group_by(Education, Locality, Age) %>%
  summarise(MeanRate = mean(Rate, na.rm = TRUE), .groups = 'drop')

ggplot(unemp_edu_loc, aes(Age, MeanRate, color = Locality, group = Locality)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Education) +
  labs(title = 'Unemployment by Education & Locality Across Age', x = 'Age Group', y = 'Mean Unemployment Rate') +
  theme_minimal()



# PRINCIPAL COMPONENT ANALYSIS (PCA) (On wide numeric matrices rebuilt from long).
# Optional viz for clustering/PCA
# suppressWarnings({
#    if (!require(factoextra)) install.packages("factoextra", quiet = TRUE)
#    library(factoextra)
# })
library(factoextra)
## Build Wide Numeric Matrices (rows = profiles, cols = Ages)
unemp_wide <- unemp_piv %>%
  group_by(Education, Sex, Locality, Geographic_Area, Age) %>%
  summarise(Rate = mean(Rate, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = Age, values_from = Rate, values_fill = 0)

## Keep Only Age Columns For PCA.
age_cols <- c('15-19', '20-24', '25-29', '30-34', '35-39')
pca_mat <- unemp_wide %>% select(all_of(age_cols)) %>% as.matrix()

## Run & inspect PCA.
pca_fit <- prcomp(scale(pca_mat), center = TRUE, scale. = TRUE)
summary(pca_fit)
pca_fit$rotation

## Visuals.
fviz_eig(pca_fit, addlabels = TRUE) + ggtitle('PCA: Variance Explained (Unemployment)')
fviz_pca_biplot(pca_fit, repel = TRUE, geom.ind = 'point',
                title = 'PCA Biplot: Age-Group Structure Of Unemployment.')


# CLUSTER ANALYSIS (Segment Unemployment Profiles).
#install.packages("FactoMineR")
library(FactoMineR)
# 1. Prepare PCA on unemployment rates
unemp_pca <- prcomp(unemp_wide[ , age_cols], scale. = TRUE)

# 2. Extract PCA scores (same nrow as unemp_wide)
scores <- as.data.frame(unemp_pca$x)

# 3. Perform clustering (choose 3 clusters as example)
set.seed(123)
kmeans_res <- kmeans(scores[, 1:2], centers = 3, nstart = 25)

# 4. Add cluster assignments
scores$Cluster <- as.factor(kmeans_res$cluster)
unemp_wide$Cluster <- as.factor(kmeans_res$cluster)

# 5. Visualize clusters on first two PCs
fviz_cluster(kmeans_res, data = scores[, 1:2],
             geom = "point", ellipse.type = "convex",
             ggtheme = theme_minimal(),
             main = "Clusters on PCA of Unemployment Rates")

## OR
# Use standardized age matrix from PCA step
X <- scale(pca_mat)

# Find k
fviz_nbclust(X, kmeans, method = "wss") + ggtitle("Elbow (WSS)")
fviz_nbclust(X, kmeans, method = "silhouette") + ggtitle("Silhouette")

# Example k = 3
set.seed(123)
km <- kmeans(X, centers = 3, nstart = 25)
table(km$cluster)

# Attach back to profile IDs
unemp_wide$Cluster <- factor(km$cluster)

# Cluster profile means
cluster_profiles <- unemp_wide %>%
  group_by(Cluster) %>%
  summarise(across(all_of(age_cols), mean, na.rm = TRUE), .groups = "drop")
cluster_profiles

# Visualize clusters on first two PCs
scores <- as.data.frame(pca_fit$x[,1:2])
scores$Cluster <- unemp_wide$Cluster
ggplot(scores, aes(PC1, PC2, color = Cluster)) +
  geom_point(size = 2, alpha = 0.8) +
  theme_minimal() + labs(title = "K-means Clusters on PCA Scores (Unemployment)")
