---
title: "awareness"
author: "Kimmy"
date: '2022-10-12'
output: html_document
---



# What is awareness-based protective behavior?

During an epidemic, people may decide whether to adopt protective behavior depending on how serious they think the disease is. They may try to estimate their own risk by looking at how many people are dying of the disease. We call this awareness-based protective behavior. Now, people can go from unprotective to protective at a rate that's proportional to recent deaths, reflecting their awareness of these severe disease outcomes. Unlike in the previous model that just included protective behavior, the uptake of protective behavior can change over time.

Awareness is scaled by responsiveness ($\theta$), a measure of how many people will change their behavior based on a single death. The figure below shows different levels of responsiveness when there's awareness-based adoption of a highly effective protective measures ($\kappa$) in a population that start out with no protective behavior ($P_0$). In this case, they consider deaths in the previous day when deciding whether to adopt protective behavior. Greater responsiveness means that more people will adopt protective behavior. How does responsiveness change epidemic shape and size?

![plot of chunk theta](figure/theta-1.png)

![](images/response.png)

Epidemics may span several months or years, so we also consider how awareness-based behavior may change over time with memory and fatigue. Memory ($\ell$) indicates how far into the past people look when evaluating disease risk. For example, if $\ell$ is thirty, people are considering all of the deaths that happened over the past months. People may not maintain protection forever because of waning immunity or difficult in continuing to practice potentially disruptive measures. Fatigue ($\phi$) indicates how quickly people lose protective behavior and become unprotective again. When $\phi=0$, protective behavior uptake never declines. Greater values of $\phi$ mean that people go from protective to unprotective more quickly.

In the figure below, we show infection prevalence [left] and protective behavior prevalence [right] over time. We set responsiveness ($\theta$) to 100. Each color is a different level of fatigue ($\phi$) and each row from top to bottom is longer memory ($\theta$). Fatigue can cause additional peaks depending on memory. How do the number, size, and shape of epidemic waves depend on both memory and fatigue?

![plot of chunk ell-phi](figure/ell-phi-1.png)

Attitudes toward the protective behavior may also be influenced by the way public health officials and the media cover the disease, including: global and local infection and death counts online, press briefing by government officials, personal anecdotes on social media platforms, and misinformation. How might these factors change behavior and epidemic dynamics? In the next section, we will look at the role that social divisions may play in shaping how members of different groups respond to and are affected by infectious diseases.





<!--html_preserve--><div class="vembedr">
<div>
<iframe src="https://www.youtube.com/embed/FHXyU0f8Ywg" width="533" height="300" frameborder="0" allowfullscreen="" data-external="1"></iframe>
</div>
</div><!--/html_preserve-->
