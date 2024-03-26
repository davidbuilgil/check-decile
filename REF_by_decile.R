
####################################
#
# Check REF submissions
#
####################################

rm(list = ls())

#load packages
library(here)
library(readxl)
library(ggplot2)
library(ggpubr)
library(stringr)
library(dplyr)

#load REF data
REF_2021 <- read_excel(here("database/REF 2021 Results - All - 2022-05-06.xlsx"),
                       skip = 6)
REF_outputs <- read_excel(here("database/REF 2021 Outputs - All - 2023-09-08.xlsx"), 
                          skip = 4)

#load first decile and quartile data
D1_journals <- read.csv(here("output/D1_journals.csv"))
Q1_journals <- read.csv(here("output/Q1_journals.csv"))

#select unit of assessment
REF_2021 <- REF_2021 %>%
  filter(`Unit of assessment name` == 'Law') %>%
  filter(`Profile` == "Outputs")
  
#Check stars add to 100
REF_2021 <- REF_2021 %>%
  mutate(total_stars = `4*` + `3*` + `2*` + `1*` + `Unclassified`,
         three_four = `4*` + `3*`)

#Delete submissions that do not add to 100
REF_2021 <- REF_2021 %>%
  filter(!is.na(total_stars))

#Top 10 performing submissions
top_n(REF_2021, 10, `4*`)

#Select REF outputs which are journal articles
REF_outputs <- REF_outputs %>%
  filter(`Output type` == "D")

#All journal names to capital letters for consistency
REF_outputs <- REF_outputs %>%
  mutate(journal = toupper(`Volume title`))
D1_journals <- D1_journals %>%
  mutate(journal = toupper(x))
Q1_journals <- Q1_journals %>%
  mutate(journal = toupper(x))

#Standardise names
REF_outputs <- REF_outputs %>%
  mutate(journal = recode(journal,
                          'BR J SOCIOL' = 'BRITISH JOURNAL OF SOCIOLOGY',
                          '(2017) 13 EUROPEAN CONSTITUTIONAL LAW REVIEW 124-165' = 'EUROPEAN CONSTITUTIONAL LAW REVIEW',
                          'YOUTH JUSTICE: AN INTERNATIONAL JOURNAL' = 'YOUTH JUSTICE',
                          'WORLD COMPETITION: LAW AND ECONOMICS REVIEW' = 'WORLD COMPETITION',
                          'U.C. DAVIS LAW REVIEW' = 'UC DAVIS LAW REVIEW',
                          'TRUSTS AND TRUSTEES' = 'TRUSTS & TRUSTEES',
                          'TIJDSCHRIFT VOOR RECHTSGESCHIEDENIS: LEGAL HISTORY REVIEW' = 'TIJDSCHRIFT VOOR RECHTSGESCHIEDENIS',
                          'THEORY, CULTURE AND SOCIETY' = 'THEORY, CULTURE & SOCIETY',
                          'THEORETICAL CRIMINOLOGY: AN INTERNATIONAL JOURNAL' = 'THEORETICAL CRIMINOLOGY',
                          'THE INTERNATIONAL JOURNAL OF CHILDREN’S RIGHTS' = "THE INTERNATIONAL JOURNAL OF CHILDREN'S RIGHTS",
                          'THE HOWARD JOURNAL OF CRIMINAL JUSTICE' = 'THE HOWARD JOURNAL OF CRIME AND JUSTICE',
                          'THE BRITISH JOURNAL OF CRIMINOLOGY: AN INTERNATIONAL REVIEW OF CRIME AND SOCIETY' = 'THE BRITISH JOURNAL OF CRIMINOLOGY',
                          'STUDIES IN CONFLICT AND TERRORISM' = 'STUDIES IN CONFLICT & TERRORISM',
                          'STANFORD LAW AND POLICY REVIEW' = 'STANFORD LAW & POLICY REVIEW',
                          'SOCIAL & LEGAL STUDIES' = 'SOCIAL AND LEGAL STUDIES',
                          'REVIEW OF LAW AND ECONOMICS' = 'REVIEW OF LAW & ECONOMICS',
                          'REVIEW OF EUROPEAN, COMPARATIVE AND INTERNATIONAL ENVIRONMENTAL LAW (RECIEL)' = 'REVIEW OF EUROPEAN, COMPARATIVE AND INTERNATIONAL ENVIRONMENTAL LAW',
                          'REVIEW OF EUROPEAN, COMPARATIVE & INTERNATIONAL ENVIRONMENTAL LAW' = 'REVIEW OF EUROPEAN, COMPARATIVE AND INTERNATIONAL ENVIRONMENTAL LAW',
                          'REGULATION & GOVERNANCE' = 'REGULATION AND GOVERNANCE',
                          'QUEEN MARY JOURNAL OF INTELLECTUAL PROPERTY' = 'QUEEN MARY JOURNAL OF INTELLECTUAL PROPERTY LAW',
                          'PUNISHMENT AND SOCIETY: THE INTERNATIONAL JOURNAL OF PENOLOGY' = 'PUNISHMENT AND SOCIETY',
                          'PUNISHMENT & SOCIETY' = 'PUNISHMENT AND SOCIETY',
                          'POLICING AND SOCIETY: AN INTERNATIONAL JOURNAL OF RESEARCH AND POLICY' = 'POLICING AND SOCIETY',
                          'POLICING (OXFORD): A JOURNAL OF POLICY AND PRACTICE' = 'POLICING: A JOURNAL OF POLICY AND PRACTICE',
                          'POLICING & SOCIETY' = 'POLICING AND SOCIETY',
                          'PÓLEMOS' = 'POLEMOS',
                          'PHILOSOPHICAL TRANSACTIONS OF THE ROYAL SOCIETY A: MATHEMATICAL, PHYSICAL AND ENGINEERING SCIENCES' = 'PHILOSOPHICAL TRANSACTIONS A: MATHEMATICAL, PHYSICAL AND ENGINEERING SCIENCES',
                          'ONATI SOCIO-LEGAL SERIES' = 'OÑATI SOCIO-LEGAL SERIES',
                          'OCEAN DEVELOPMENT AND INTERNATIONAL LAW' = 'OCEAN DEVELOPMENT & INTERNATIONAL LAW',
                          'NOTTINGHAM BUSINESS AND INSOLVENCY LAW EJOURNAL' = 'NOTTINGHAM BUSINESS AND INSOLVENCY LAW E-JOURNAL',
                          'NOTTINGHAM INSOLVENCY AND BUSINESS LAW E-JOURNAL' = 'NOTTINGHAM BUSINESS AND INSOLVENCY LAW E-JOURNAL',
                          'NORTHERN IRELAND LEGAL QUATERLY' = 'NORTHERN IRELAND LEGAL QUARTERLY',
                          'NORTHEN IRELAND LEGAL QUARTERLY' = 'NORTHERN IRELAND LEGAL QUARTERLY',
                          'NORDIC JOURNAL OF HUMAN RIGHTS / NORDISK TIDSSKRIFT FOR MENNESKERETTIGHETER' = 'NORDIC JOURNAL OF HUMAN RIGHTS',
                          'NEW CRIMINAL LAW REVIEW: AN INTERNATIONAL AND INTERDISCIPLINARY JOURNAL' = 'NEW CRIMINAL LAW REVIEW',
                          'NETHERLANDS QUARTERLY OF HUMAN RIGHTS LAW' = 'NETHERLANDS QUARTERLY OF HUMAN RIGHTS',
                          'NEDERLANDS INTERNATIONAAL PRIVATRECHT (NIPR' = 'NEDERLANDS INTERNATIONAAL PRIVAATRECHT (NIPR)',
                          'NAT BIOTECHNOL' = 'NATURE BIOTECHNOLOGY',
                          'MICHIGAN JOURNAL OF GENDER AND LAW' = 'MICHIGAN JOURNAL OF GENDER & LAW',
                          'MED LAW REV' = 'MEDICAL LAW REVIEW',
                          'LLOYDS MARITIME AND COMMERCIAL LAW QUARTERLY' = "LLOYD'S MARITIME AND COMMERCIAL LAW QUARTERLY",
                          "LLOYD'S MARITIME & COMMERCIAL LAW QUARTERLY" = "LLOYD'S MARITIME AND COMMERCIAL LAW QUARTERLY",
                          'LEGAL STUDIES: THE JOURNAL OF THE SOCIETY OF LEGAL SCHOLARS' = 'LEGAL STUDIES',
                          'LAW, CULTURE, AND THE HUMANITIES' = 'LAW, CULTURE AND THE HUMANITIES',
                          'LAW QUARTERLY REVIEW.' = 'LAW QUARTERLY REVIEW',
                          'LAW AND JUSTICE: THE CHRISTIAN LAW REVIEW' = 'LAW AND JUSTICE',
                          'LAW AND HUMANTIES' = 'LAW AND HUMANITIES',
                          'LAW AND CONTEMPORARY PROBLEMS JOURNAL' = 'LAW AND CONTEMPORARY PROBLEMS',
                          'LAW AND CONTEMPORARY PROBLEMS : A QUARTERLY PUBLISHED BY THE DUKE UNIVERSITY, SCHOOL OF LAW' = 'LAW AND CONTEMPORARY PROBLEMS',
                          'KINGS LAW JOURNAL' = "KING'S LAW JOURNAL",
                          'JUSTICE, POWER AND RESISTANCE: THE JOURNAL OF THE EUROPEAN GROUP FOR THE STUDY OF DEVIANCE AND SOCIAL CONTROL.' = 'JUSTICE, POWER AND RESISTANCE',
                          'JURISPRUDENCE: AN INTERNATIONAL JOURNAL OF LEGAL AND POLITICAL THOUGHT' = 'JURISPRUDENCE',
                          'JOURNAL OF SOCIAL WELFARE & FAMILY LAW' = 'JOURNAL OF SOCIAL WELFARE AND FAMILY LAW',
                          'JOURNAL OF PRIVATE INTENATIONAL LAW' = 'JOURNAL OF PRIVATE INTERNATIONAL LAW',
                          'JOURNAL OF NATIONAL SECURITY LAW & POLICY' = 'JOURNAL OF NATIONAL SECURITY LAW AND POLICY',
                          'JOURNAL OF INTERNATIONAL BANKING LAW & REGULATION' = 'JOURNAL OF INTERNATIONAL BANKING LAW AND REGULATION',
                          'JOURNAL OF IMMIGRATION, ASYLUM & NATIONALITY LAW.' = 'JOURNAL OF IMMIGRATION, ASYLUM AND NATIONALITY LAW',
                          'JOURNAL OF IMMIGRATION ASYLUM AND NATIONALITY LAW' = 'JOURNAL OF IMMIGRATION, ASYLUM AND NATIONALITY LAW',
                          'JOURNAL OF EUROPEAN CONSUMER AND MARKET LAW (EUCML)' = 'JOURNAL OF EUROPEAN CONSUMER AND MARKET LAW',
                          'JOURNAL OF CONFLICT & SECURITY LAW' = 'JOURNAL OF CONFLICT AND SECURITY LAW',
                          'JOURNAL OF COMPETITION LAW & ECONOMICS' = 'JOURNAL OF COMPETITION LAW AND ECONOMICS',
                          'INTERNATIONAL REVIEW OF LAW, COMPUTERS & TECHNOLOGY' = 'INTERNATIONAL REVIEW OF LAW, COMPUTERS AND TECHNOLOGY',
                          'INTERNATIONAL JOURNAL OF REFUGEE LAW SPECIAL ISSUE 2016' = 'INTERNATIONAL JOURNAL OF REFUGEE LAW',
                          'INTERNATIONAL JOURNAL OF LAW POLICY AND THE FAMILY' = 'INTERNATIONAL JOURNAL OF LAW, POLICY AND THE FAMILY',
                          'INTERNATIONAL JOURNAL FOR THE SEMIOTICS OF LAW - REVUE INTERNATIONALE DE S√©MIOTIQUE JURIDIQUE' = 'INTERNATIONAL JOURNAL FOR THE SEMIOTICS OF LAW',
                          'INTERNATIONAL JOURNAL FOR THE SEMIOTICS OF LAW - REVUE INTERNATIONALE DE SÉMIOTIQUE JURIDIQUE' = 'INTERNATIONAL JOURNAL FOR THE SEMIOTICS OF LAW',
                          'INFORMATION AND COMMUNICATIONS TECHNOLOGY LAW' = 'INFORMATION & COMMUNICATIONS TECHNOLOGY LAW',
                          'GLOBAL CONSTITUTIONALISM: HUMAN RIGHTS, DEMOCRACY AND THE RULE OF LAW' = 'GLOBAL CONSTITUTIONALISM',
                          'FEMINISTS AT LAW' = 'FEMINISTS@LAW',
                          'FEMINIST @LAW' = 'FEMINISTS@LAW',
                          'ENVIRONMENTAL LIABILITY – LAW PRACTICE AND POLICY' = 'ENVIRONMENTAL LIABILITY',
                          'ELECTION LAW JOURNAL: RULES, POLITICS, AND POLICY' = 'ELECTION LAW JOURNAL',
                          'DUKE JOURNAL OF COMPARATIVE & INTERNATIONAL LAW' = 'DUKE JOURNAL OF COMPARATIVE AND INTERNATIONAL LAW',
                          'DEVIANT BEHAVIOR: AN INTERDISCIPLINARY JOURNAL' = 'DEVIANT BEHAVIOR',
                          'CRIMINOLOGY & CRIMINAL JUSTICE' = 'CRIMINOLOGY AND CRIMINAL JUSTICE',
                          'CRIMINAL LAW REVIEW -LONDON-' = 'CRIMINAL LAW REVIEW',
                          'CRIMINAL LAW FORUM: OFFICIAL JOURNAL OF THE SOCIETY FOR THE REFORM OF CRIMINAL LAW' = 'CRIMINAL LAW FORUM',
                          'CRIME, MEDIA, CULTURE: AN INTERNATIONAL JOURNAL' = 'CRIME, MEDIA, CULTURE',
                          'COMPUTER LAW & SECURITY REVIEW' = 'COMPUTER LAW AND SECURITY REVIEW',
                          'CANADIAN JOURNAL OF LAW AND JURISPRUDENCE: AN INTERNATIONAL JOURNAL OF LEGAL THOUGHT' = 'CANADIAN JOURNAL OF LAW AND JURISPRUDENCE',
                          'CANADIAN JOURNAL OF LAW & JURISPRUDENCE' = 'CANADIAN JOURNAL OF LAW AND JURISPRUDENCE',
                          'BRITISH YEAR BOOK OF INTERNATIONAL LAW' = 'BRITISH YEARBOOK OF INTERNATIONAL LAW',
                          'THE BRITISH JOURNAL OF CRIMINOLOGY' = 'BRITISH JOURNAL OF CRIMINOLOGY',
                          'THE BRITISH JOURNAL OF CRIMINOLOGY: AN INTERNATIONAL REVIEW OF CRIME AND SOCIETY' = 'BRITISH JOURNAL OF CRIMINOLOGY',
                          'THE ORGANIZATION OF ISLAMIC COOPERATION AND HUMAN RIGHTS: THE GOOD, THE BAD AND THE UGLY' = 'THE ORGANIZATION OF ISLAMIC COOPERATION AND HUMAN RIGHTS',
                          'CHALLENGES AND CRITIQUES OF THE EU INTERNAL SECURITY STRATEGY : RIGHTS, POWER AND SECURITY' = 'CHALLENGES AND CRITIQUES OF THE EU INTERNAL SECURITY STRATEGY: RIGHTS, POWER AND SECURITY',
                          "‘BOAT REFUGEES AND MIGRANTS AT SEA: A COMPREHENSIVE APPROACH INTEGRATING MARITIME SECURITY WITH HUMAN RIGHTS’" = "BOAT REFUGEES AND MIGRANTS AT SEA: A COMPREHENSIVE APPROACH INTEGRATING MARITIME SECURITY WITH HUMAN RIGHTS",
                          'OXFORD HANDBOOK OF CRIMINOLOGY OXFORD' = 'OXFORD HANDBOOK OF CRIMINOLOGY',
                          'JOURNAL OF DEVELOPMENTAL AND LIFE COURSE CRIMINOLOGY' = 'JOURNAL OF DEVELOPMENTAL AND LIFE-COURSE CRIMINOLOGY',
                          'THE BRITISH JOURNAL OF CRIMINOLOGY' = 'BRITISH JOURNAL OF CRIMINOLOGY',
                          'POLICING: A JOURNAL OF POLICY AND PRACTICE' = 'POLICING (OXFORD)',
                          'THE DEVELOPMENT OF TRANSNATIONAL POLICING: PAST, PRESENT AND FUTURE' = 'THE DEVELOPMENT OF TRANSNATIONAL POLICING',
                          'EU CRIMINAL JUSTICE AND THE CHALLENGES OF DIVERSITY: LEGAL CULTURES IN THE AREA OF FREEDOM, SECURITY AND JUSTICE' = 'EU CRIMINAL JUSTICE AND THE CHALLENGES OF DIVERSITY',
                          'JOURNAL POLICE PRACTICE AND RESEARCH: AN INTERNATIONAL JOURNAL.' = 'POLICE PRACTICE AND RESEARCH',
                          'ELECTION LAW JOURNAL' = 'ELECTION LAW JOURNAL: RULES, POLITICS, AND POLICY',
                          'PROBATION JOURNAL: THE JOURNAL OF COMMUNITY AND CRIMINAL JUSTICE' = 'PROBATION JOURNAL',
                          'CAMBRIDGE QUARTERLY OF HEALTHCARE ETHICS : CQ : THE INTERNATIONAL JOURNAL OF HEALTHCARE ETHICS COMMITTEES' = 'CAMBRIDGE QUARTERLY OF HEALTHCARE ETHICS',
                          'THE NORTHERN IRELAND LEGAL QUARTERLY' = 'NORTHERN IRELAND LEGAL QUARTERLY',
                          'THE POLICE JOURNAL: THEORY, PRACTICE AND PRINCIPLES' = 'POLICE JOURNAL',
                          'UNIVERSITY OF PENNSYLVANIA JOURNAL OF INTERNATIONAL LAW,' = 'UNIVERSITY OF PENNSYLVANIA JOURNAL OF INTERNATIONAL LAW',
                          'THE JOURNAL OF ACCOUNTING, ECONOMICS AND LAW: A CONVIVIUM' = 'ACCOUNTING, ECONOMICS AND LAW: A CONVIVIUM'
                          
                          )) %>%
  mutate(journal = str_replace_all(journal, "&", "AND"))

#Create table of journals in REF
REF_journals <- REF_outputs %>%
  group_by(journal) %>%
  summarise(count = n())

#test <- Q1_journals %>% left_join(REF_journals, by = "journal")

#Add D1 and Q1 variables
D1_journals <- D1_journals %>%
  mutate(D1 = 'Y')
Q1_journals <- Q1_journals %>%
  mutate(Q1 = 'Y')

#Merge list of first decile and first quartile journals
REF_outputs <- REF_outputs %>%
  left_join(D1_journals, by = "journal")
REF_outputs <- REF_outputs %>%
  left_join(Q1_journals, by = "journal")

#Summary by submission
REF_submission <- REF_outputs %>%
  group_by(`Institution name`) %>%
  summarise(total = n(),
            D1 = sum(D1 == "Y", na.rm = TRUE),
            Q1 = sum(Q1 == "Y", na.rm = TRUE)) %>%
  mutate(D1_prop = D1 / total * 100,
         Q1_prop = Q1 / total * 100)
  
#Merge overall results of REF submissions
REF_submission <- REF_2021 %>%
  left_join(REF_submission, by = 'Institution name')
  
#Bivariate correlations
cor_test_D1_4 <- cor.test(REF_submission$D1_prop, REF_submission$`4*`,
                          method = c("spearman"))
cor_test_D1_4
cor_test_Q1_4 <- cor.test(REF_submission$Q1_prop, REF_submission$`4*`,
                          method = c("spearman"))
cor_test_Q1_4

cor_test_D1_34 <- cor.test(REF_submission$D1_prop, REF_submission$three_four,
                           method = c("spearman"))
cor_test_D1_34
cor_test_Q1_34 <- cor.test(REF_submission$Q1_prop, REF_submission$three_four,
                           method = c("spearman"))
cor_test_Q1_34

#Calculate the square of Spearman's rho
cor_test_D1_4_squared <- cor_test_D1_4$estimate^2
cor_test_D1_4_squared

#Observations to annotate
obs_to_annotate <- c("University of Glasgow", "School of Oriental and African Studies", 
                     "The University of Sheffield", "University College London",
                     "University of Cambridge", "Abertay University",
                     "University of Oxford", "The University of Manchester",
                     "The University of Leeds")
obs_to_annotate <- subset(REF_submission, `Institution name` %in% obs_to_annotate)

#Visualise in scatter plots
scatter_plot <- ggplot(REF_submission, aes(x = Q1_prop, y = three_four)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) + # Loess regression line
  geom_text(data = obs_to_annotate, aes(label = `Institution name`), 
            nudge_x = 0.02, nudge_y = 2.8, size = 3, check_overlap = TRUE) + # Adjust nudge_x and nudge_y as needed
  annotate("text", x = Inf, y = Inf, label = sprintf("r = %.2f, p = %.2f", cor_test_Q1_34$estimate, cor_test_Q1_34$p.value),
           hjust = 1.1, vjust = 1.1) + # Adjust text position as needed
  labs(title = "Scatter plot with Loess Regression Line",
       x = "% Publications in first quartile journals",
       y = "% Articles rated 3* or 4*") +
  theme_minimal()
scatter_plot

#Quartile on quartile
# Categorize into quartiles
REF_submission$Q1_prop_quartiles <- cut(REF_submission$Q1_prop, breaks = quantile(REF_submission$Q1_prop, probs = 0:4/4), include.lowest = TRUE, labels = FALSE)
REF_submission$three_four_quartiles <- cut(REF_submission$three_four, breaks = quantile(REF_submission$three_four, probs = 0:4/4), include.lowest = TRUE, labels = FALSE)

# Create contingency table
contingency_table <- table(REF_submission$Q1_prop_quartiles, REF_submission$three_four_quartiles)

# Perform Chi-squared test
chi_squared_test <- chisq.test(contingency_table)

# Analyze the results
print(chi_squared_test)

# Convert the contingency table to a data frame for plotting
heatmap_data <- as.data.frame(as.table(contingency_table))

# Rename the columns appropriately
names(heatmap_data) <- c("Q1_prop_Quartile", "Three_Four_Quartile", "Count")

# Visualise in heatmap
quartile <- ggplot(heatmap_data, aes(x = Q1_prop_Quartile, y = Three_Four_Quartile, fill = Count)) +
  geom_tile() + # This creates the heatmap tiles
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Count") +
  geom_text(aes(label = Count), color = "white") + # Add count numbers on the tiles
  labs(title = "Quartile-on-Quartile Analysis Heatmap",
       x = "% Publications in first quartile journals",
       y = "% Articles rated 3* or 4*") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Improve X axis label readability
quartile

#Visualise graphs
ggarrange(scatter_plot, quartile)
ggsave(here('output/graph.jpg'), width = 10, height = 5)

