---
output:
  html_document:
    mathjax: "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"

---

# What is an SIR Model?

A common tool to understand how infectious diseases spread is the Susceptible-Infected-Recovered-Deceased (SIRD) model. We divide the population into three categories corresponding to whether they have been infected and keep track of the proportion of the population in each category over time. 

A *susceptible* individual who has not gotten sick yet and may become infected. For new diseases, the entire population starts out as susceptible. 

An *infected* person has contracted the virus and can spread it. New infections happen when a susceptible person comes into contact with an infected person and successfully transmits the disease. The rate of new infections depends on the *transmission coefficient* ($\beta$), and more infectious diseases will have a great transmission coefficient. People may be able to transmit the disease for several days, corresponding to the *infectious period* ($\frac{1}{\rho}$). A larger infectious period means that people are infectious for a longer time.

At the end of the *infectious period*, people may recover. Our basic model assumes that *recovered* people are immune and can no longer become infected. Some people may die instead of recovering from the disease and we keep track of deaths with the *deceased* category. The *fatality probability* ($\mu$) gives the likelihood of dying of the disease, and more deadly diseases will have a greater fatality probability.

We can translate these assumptions into a system of equations and then use simulations to predict how epidemics may unfold under different disease properties. Below, we graph the results of a single simulation where we set $\beta=0.2$, $\rho=0.1$, $\mu=0.05$. The graphs below show the proportion of the population in each category over time. The proportion of people who are susceptible decreases as people become infected. Infections increase up to a peak, and then decline. Over time, more people recover or die of the disease. The proportion of the population in these categories levels off as infections decline due to herd immunity. 

![plot of chunk outbreak](figure/outbreak-1.png)

## Herd immunity

In order to spread, the virus needs susceptible people to contact infectious people. As we showed above, there are fewer remaining susceptible people in the population over time. The herd immunity threshold is the point where enough people have become immune that the epidemic begins to slow down. In this case, the herd immunity threshold is reached when infections peak, after about 50% of people have been infected. Before then, each infected person infected greater than one person so infections increased. After the peak, fewer people will become infected each day than the day prior. There can still be new infections after the threshold is reached. In fact, there are about 30% infections after that point. The herd immunity threshold and other features of the epidemic (including its size and duration) depend on the disease's transmissibility.


## What is $R_0$

$R_0$, also known as R-naught is the basic reproduction number, which can help us predict how an epidemic unfold. This value represents the average number of cases an infected individual will produce in a population where everyone is susceptible. This number tells us how transmissible a disease is and helps us predict the trajectory of the disease. In our basic model above, $R_0=\frac{\beta}{\rho}$. This means that the disease will spread more quickly if it has a greater transmission coefficient ($\beta$) or if people are infectious for a longer period of time ($\frac{1}{\rho}$). 

$R_0>1$ predicts an epidemic. $R_0<1$ predicts that the disease will die out without being able to sustain an epidemic. Chicken pox has an $R_0$ of about 11, which is why chicken pox outbreaks are fairly common. One child in a classroom with chicken pox could on average infect 11 classmates if nobody is already vaccinated or immune. The flu has an $R_0$ of between 1 and 2, explaining why it is fairly common but not as infectious as diseases with higher $R_0$. MERS has an $R_0$ of 0.3 to 0.8 making it less likely to cause an outbreak. It is harder to calculate the $R_0$ early on in an outbreak of a novel disease due to data limitations, but calculating this quantity can help us prepare for an outbreak. Using our interactive simulation tool, you can examine the relationship between $R_0$ and epidemic metrics like cumulative infection incidence, peak infection prevalence, and the date of peak infections. In our next model, we will examine how protective measures can change help suppress epidemics.