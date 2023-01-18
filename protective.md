---
output:
  html_document:
    mathjax: "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
runtime: shiny

---

# What is protective behavior?

Protective behaviors are things such as vaccination, mask-wearing, hand washing, and social distancing. These are behaviors that people participate in that can help prevent the spread of infectious diseases. Our model looks at both the efficacy and uptake of these behaviors.

Protective behavior efficacy ($\kappa$) scales the transmission term and may range between perfect protection ($\kappa=0$) and no impact on transmission ($\kappa=1$). In our model, protective behaviors prevent susceptible people from becoming sick and prevent infected people from spreading the pathogen. 

In the following figure, we simulate an epidemic of the same disease from the explainer about the basic model. Everyone adopts protective behavior. We compare perfect protection efficacy ($\kappa=0$), intermediate protection efficacy ($\kappa=0.6$), and the baseline epidemic where protective measures have no effect ($\kappa=1$). How do protective measures change the shape and size of the infection trajectory? Where or not there is an epidemic? When the herd immunity threshold is reached?

![plot of chunk kappa](figure/kappa-1.png)


We also consider protective behavior uptake ($P_0$), which is the proportion of the population participating in the protective behaviors. For example, if $P_0=1$ then that means 100% of the population wears masks. But if $P_0=0.25$ then only about a quarter of the population wears mask and the disease will spread more widely. This model assumes that people are either protective or unprotective and that they cannot change their behavior over time. In the next model, we will consider what happens if people can change their behavior. 

In the figure below, we look at what happens if 25%, 50%, or 100% of the population adopts a highly effective protective measure ($\kappa=0.3$). How does protective behavior uptake ($P_0$) affect the epidemic shape and size? Can you find different combinations of values for $P_0$ and $\kappa$ that produce similar epidemic dynamics? 



![plot of chunk P0](figure/P0-1.png)

