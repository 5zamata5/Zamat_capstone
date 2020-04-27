---
title: "2020 IBS538 Capstone"
author: "Ali Zamat"
date: "April 28"
output: html_document
---
1) Provide a brief background and significance about a specific research problem that interests you. It could be project you’re involved with now, or a rotation project, or something you’d like to work on. The reader will need to understand enough background to make sense of the experiment you propose below. Keep it brief. In one short paragraph.

**Current approved CAR T cell immunotherapy targetting cancerous cells have only seen success with acute lymphoblastic leukemia targetting CD19 positive cells. Although CD19 is not exclusive to diseased cells, reinfusions of B cells after CAR T treatment allow for aCD19 CAR T cell therapy to be efficacious. However, many solid tumors do not express antigens exclusive to diseased sites - as such, on-target, off-tumor cytotoxicities remain prevelent. Our goal is to activate CAR expression in engineered T cells remotely at tumor sites to prevent off-tumor killing. This can be tested by innoculating tumors in both flanks of a single mouse, conducting adoptive cell transfer of engineered cells and only heating a single flank. As such, the changes in one tumor can be compared to a paired, internal control.**

2) Briefly state something that is unknown about this system that can be discovered through, and leads to, an experiment.  For example, "It is not known whether....."

**Remote activation of CAR T cells could reduce off target toxicities associated with traditional cancer. By preforming an adoptive cell transfer (ACT) of engineered T cells expressing a heat activated CAR, Tumor cells can be targetted directly at the site of disease. However, it is not known whether heat activated CAR T cells are capable of killing cancerous cells and thus reducing overall tumor burden.**

3) Make an “if” “then” prediction that is related to item #2. It should be of the general form, “if X is true, then Y should happen”.

**If heat activatable T cells are delivered to a mouse bearing two tumors, then tumor sizes should differ (decrease overall) when compared to the unheated flank.**


4) What dependent variable will be observed to test this prediction in item #3? What predictor variable will be used to manipulate the system experimentally? Define the inherent properties of these variables (eg, are they sorted, ordered or measured).

**The dependent variable of this system will be tumor size which will be measured as a volume via caliper measurements conducted in 3 orthogonal directions. each subject (mouse) is an experimental unit containing an internal control (unheated flank). The independent variable is heating treatment which is a discrete and sorted variable at two levels, heated at 43C and unheated. Because one flank will be heated via NIR interaction with gold nanorods injected intratumorally, heat treatment is not intrinsically linked to tumor regression and, as such, if variations are seen between the two tumors in a single mouse, then it could be attributed to the engineered cell activity. All mice will have two tumors innoculated and be systemically injected with engineered CAR T cells (via teil vein). The mice will also receive an intratumoral injection of gold nanorods in both tumors to allow for heat generation from NIR laser stimulation.**

5) Write a statistical hypothesis.  There should be a null and alternate. These should be explicitly consistent with the prediction in item #3 and the response variable in #4. In other words, make sure the statistical hypotheses that you write here serves as a test of the prediction made in item #3.

**In our situation,** $\mu$ **represents the mean tumor volume of the tumor populations corresponding to the sampled groups. The null hypothesis is:**

$H_0 : \mu_{unheated tumor} = \mu_{heated tumor}$

**In which the null hypothesis states that the tumor volumes of unheated and heated tumors will remain the same after treatment**

**On the other hand, the alternative hypothesis states:**

$H_1 : \mu_{unheated tumor} \neq \mu_{heated tumor}$

**In which tumor volumes will not be the same between treated and untreated tumors. Although we expect tumor volumes to decrease with heat treatment, to greater protect against type 1 error, we will maintain a two sided alternative as heat treatment may increase tumor volumes**


6) What is the statistical test you would use to test the hypothesis in item #5? Briefly defend what makes this appropriate for the hypothesis and the experimental variables. If there are alternatives, why is this approach chosen instead? Points will not be awarded if the justification involves something like "because everybody does it this way".

**An omnibus two-way ANOVA with repeated measurements will be conducted on the data set for two main reasons. Because the two treatment conditions will occur on the same mouse, it is a repeated measure as the tumors are theoretically the same. Moreover, the tumor volumes on the mouse will be measured over time, thus our time variable will also be repeated measures. Although most measurements will declare the effect of time to be irrelevent, we will conduct a post-hoc analysis on the data set (using a bonferonni correction to remain stringent) to determine the measurement day in which statistical significance can be observed. This is done to inform future experiments. Post hoc analysis will be conducted as a two sided pairwise t test with a type 1 error of 5% meaning that at p-values less than 0.05, the null hypothesis will be rejected. Although we have a zero boundary for tumor volumes, we do not expect to realistically hit zero as we expect a slow down of tumor burden in the treated group as opposed to complete clearance. The outcome (tumor volume) is continuous and we will assume that it is normally-distributed based on historical findings.** 

7) List the procedures and decision rules you have for executing and interpreting the experiment. These procedures range from selection of experimental units, to randomization to primary endpoint to threshold decisions. Define (and defend) what you believe will be the independent replicate.

**To begin, experimental units will be chosen such that mice will be the same age (4-8 weeks old) and will be innoculated with KF62 tumors subcutaneously on both flanks. These tumors will be allowed to grow for 7 days. Then, the same number of engineered T cells with expression vectors prodcuing CD19 CARs under heat shock promoters will be transferred intravenously via tail vein. The same amount of Gold nano rods will be injected into both tumors for all mice. After 1 day, one flank of the mouse will be heated using an NIR laser for 30 minutes while the other flank remains unheated. The side of flank to be heated will be randomized for each mouse as to not generate bias based on side chosen. tumor volumes will be taken ever 3 daays to track tumor sizes over time. The independent unit will be an individual mouse as it will contain its own control placebo. Because mouse ages and litters will vary, we will have introduced sufficent randomness into the data set to conduct statistical analysis on our treatment.** 

8) Produce a graph of a simulation for the expected results. Create a dataMaker-like function in R to create and plot the data. Label and scale any axis. The graph should illustrate the magnitude of the expected response, or the level of response that you expect to see and would be minimally scientifically relevant. Be sure to illustrate any variation that is expected.
```{r message=FALSE, warning=FALSE}
library(pwr)
library(tidyverse)
library(ez)
library(ggplot2)
library(cowplot)
library(viridis)
```

```{r}
b <- 2.5 #expected volume size of untreated tumors (cm^3)
a <- .7 #Expected volume size to positive (untreated) control
sd <- .4 #sd of our outcome volume 
n <- 5 #number of replicates per group
r <- 0.8 #correlation amongs all groups (because in vivo studies fluctiate, we have set this to 0.8)
k <- sqrt(1-r^2)
sims <- 100

```

```{r}
RMdatamaker <- function(n, b, a, sd, r, k) {
  unheated <- rnorm (n,b,sd)
  heated <- unheated*r+k*rnorm(n, (b*a), sd)
  outcome <- c(unheated, heated)
  Predictor <- c(rep(c("unheated", "heated"), each = n))
  ID <- rep(as.factor(c(1:n)))
  df <- data.frame(ID, Predictor, outcome)
}

simdata <- RMdatamaker(n,b,a,sd,r,k)
ggplot(simdata, aes(Predictor, outcome, color=ID, group=ID))+
  geom_line(size = 1.2)+
  geom_point()+
  scale_color_brewer(type = "qual", palette = 2)+
  xlab("Tumor Heating")+
  ylab("Tumor Volume in cm^3")+
  ggtitle("Tumor volumes of Heated vs Unheated tumors on day 30")
```


9) Write and perform a Monte Carlo analysis to calculate a sample size necessary to test the hypothesis. This Monte Carlo must test the primary endpoint.

```{r}
pval <- replicate(
  sims, {
 
    sample.df <- RMdatamaker(n, b, a, sd, r, k)
    
    sim.ezaov <- ezANOVA(
            data = sample.df, 
            wid = ID,
            dv = outcome,
            within = Predictor,
            type = 2
            )
  
  pval <- sim.ezaov$ANOVA[1,5]
    
    }
  )

pwr.pct <- sum(pval<0.05)/sims*100
paste(pwr.pct, sep="", "% power.")

```
**After conducting the simulation of the experiment above, we see that it is possibel to achieve a power of 96% with an n of 5. With an n of 4, the power of this study is 76%. Related measures require much smaller n for a high power. However, they are prone to lost data and do not protect against unbalanced data sets. For this reason, the experiment will move forward with 7 mice to ensure that if something were to happen, we would still be able to properly power our study.**

10) Write up it all in RMarkdown. Code chunks to illustrate specific points are welcome other than for the Monte Carlo code. Knit and submit and upload the html document by the due data. If it is readable to your best friend, it is readable to us.

**The simulation above is based on the tumor volumes at day 30. To better see the kinetics, we will also view the data per day. The code chunks below simulate the data over time**
```{r}
day <- as.numeric(seq (0, 30, by = 3))

unheat_kin_1 <-  abs(rnorm(n = 1,.01, sd = 0.005)) * exp(.2*day)
unheat_kin_2<-  abs(rnorm(n = 1,.01, sd = 0.005)) * exp(.2*day)
unheat_kin_3<-  abs(rnorm(n = 1,.01, sd = 0.005)) * exp(.2*day)
unheat_kin_4<-  abs(rnorm(n = 1,.01, sd = 0.005)) * exp(.2*day)
unheat_kin_5<-  abs(rnorm(n = 1,.01, sd = 0.005)) * exp(.2*day)
unheat_kin_6<-  abs(rnorm(n = 1,.01, sd = 0.005)) * exp(.2*day)
unheat_kin_7<-  abs(rnorm(n = 1,.01, sd = 0.005)) * exp(.2*day)

heat_kin_1  <-  abs(rnorm(n = 1,.01, sd = 0.005)) * exp(0.6*(.2)*day)
heat_kin_2  <-  abs(rnorm(n = 1,.01, sd = 0.005)) * exp(0.6*(.2)*day)
heat_kin_3  <-  abs(rnorm(n = 1,.01, sd = 0.005)) * exp(0.6*(.2)*day)
heat_kin_4  <-  abs(rnorm(n = 1,.01, sd = 0.005)) * exp(0.6*(.2)*day)
heat_kin_5  <-  abs(rnorm(n = 1,.01, sd = 0.005)) * exp(0.6*(.2)*day)
heat_kin_6  <-  abs(rnorm(n = 1,.01, sd = 0.005)) * exp(0.6*(.2)*day)
heat_kin_7  <-  abs(rnorm(n = 1,.01, sd = 0.005)) * exp(0.6*(.2)*day)


df_unheat <- data.frame(unheat_kin_1,unheat_kin_2,unheat_kin_3,unheat_kin_4,unheat_kin_5,unheat_kin_6,unheat_kin_7)
df_heat <- data.frame(heat_kin_1,heat_kin_2,heat_kin_3,heat_kin_4,heat_kin_5,heat_kin_6,heat_kin_7)
result <-  cbind(day,df_unheat, df_heat)

data1 <- result %>% pivot_longer(cols = 2:15,
                              names_to = "Treatment",
                              values_to="Volumes")
ID <- as.factor(rep(rep(1:7, 1),22))
treat <- as.factor(rep(c(rep("unheated",7), rep("heated", 7)),11))
data1 <- add_column(data1, 
                    treat,
                    ID) %>%
  select(-one_of("Treatment"))
data1
```


```{r}
ggplot(data1, 
       aes(
         day, 
         Volumes, 
         group=ID)
       ) +
  geom_line(
    
  ) +
  geom_point(
    aes(
      color=ID), 
    size=4
    ) +
  facet_grid(
    ~treat, 
    switch="both"
    ) +
  scale_shape_manual(
    values=c(16, 16)
    ) +
  scale_color_viridis(
    discrete=T
    ) +
  theme(
    legend.position="top"
    )  +
  labs(
    y="Tumor Volume (cm^3)", 
    x="Days"
    )+
ggtitle("kinetic traces of tumor volumes over time")
```

**Qualitatively, we see drastic differences between tumors that are heated vs unheated. conducting an ANOVA analysis will serve as an omnibus test to let us know if this is statistically significant.**

```{r warning=FALSE}
ezANOVA(data = data1,
                     dv = Volumes,
                     wid = ID,
                     between = treat,
                     within = day,
                     type = 3,
                     detailed = F,
                     return_aov=F
                     )

pair <- pairwise.t.test(data1$Volumes, interaction(data1$day, data1$treat), paired=T, p.adj = "none")

pvals <- c(pair$p.value[11,1],pair$p.value[12,2],pair$p.value[13,3],pair$p.value[14,4],pair$p.value[15,5], pair$p.value[16,6], pair$p.value[17,7], pair$p.value[18,8], pair$p.value[19,9] , pair$p.value[20,10], pair$p.value[21,11])
adj.pvals <- p.adjust(pvals, method="bonferroni")
adj.pvals
```
**After conducting a paired pairwise t test between treated and untreated tumors with the bonferroni correction method, We can see that treated tumors become statistically signficantly different on day 9. This information can inform future experiments where we will not need to take the measurements out to day 30.**
