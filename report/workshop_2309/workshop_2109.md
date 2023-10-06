---
marp: true
title: Exploring antisemitic dynamics in online discussions
_class: invert
footer: Stefan Munnes (WZB) - 2023/09/21
size: 16:9
paginate: true
_paginate: false
math: mathjax
headingDivider: 2
---

# Exploring antisemitic dynamics in online discussions

# **Project:** Decoding Antisemitism

$\circ$ joint pilot study (2020 - 2024) in Uk, Germany, and France

$\circ$ monitor online media to improve understanding & detection

$\circ$ three sub-projects:
  1. qualitative content analysis
  2. automatic classification with ML
  3. **quantitative analysis**

![bg right:25% 75%](img/institutes.png)

<!--
Decoding Antisemitism:
An AI-driven Study on Hate Speech and Imagery Online
- Goals: 
  1. insights (present and future of antisemitism)
  2. prevention and counter strategies
  3. improved automated detection
- collect and hand-code social media comments
- 
- discourse events as trigger for data collection
- four different platforms
-->

# **Content:** What to expect?

- preliminary results
- descriptive visualizations

1. Data structure
2. Distribution of ideations
3. Position of antisemitic comments
4. Reactions to antisemitic comments
5. User of antisemitic comments

<!--
- beginning of analysis
- explorative -> get used to data, explore patterns and connections
- semi-structured data, different variables
- not classic from theory, hypothesis, measure (survey) -> test
- explore (and discus) different ideas

- dynamics of discussion/threats
-->

# **Data:** Platforms

![height:550](plots/plot_comments_source_type.png)

<!--
- 
-->

# **Data:** Discourse Events

![height:550](plots/plot_comments_discourse.png)

<!--
- 
-->

# **Code System:** Ideation

![bg left:40% 80%](img/code_system.png)

| Code  | Label            | Freq. | Prop. |
| ----- | ---------------- | ----- | ----- |
| I0    | Not Antisemitism | 62996 | 0.80  |
| I0c   | Counter Speech   | 4949  | 0.06  |
| I1    | Antisemitism     | 3814  | 0.05  |
| I1ASC | Contextual AS    | 5138  | 0.07  |
| I1a   | Confirmation     | 362   | 0.00  |
| I2    | Unclear Ideation | 1423  | 0.02  |

<!--
- 7 main level with 156 sub-codes 
- ideation, conceptual, linguistic, semantic
- focus on 1) Ideation
- Ideation: 6 codes,
  - first two: not antisemitic or counter speech, most comments
  - second: antisemitic; contextual AS, confirmation
  - unclear ideation: not sure of deleted comments
-->

# **Ideation** by country and platform

![height:550](plots/plot_ideation_source_type.png)

<!--
Results:
- difference between countries: UK -> France -> Germany
- variation by platform: social media more than classic news outlets
- variation by ideation: some countries and pages, more explicit, more or less counter speech
-->

# **Ideation** by country and discourse event

![height:550](plots/plot_ideation_discourse.png)

<!--
- strong variation by topic: over all and explicit/implicit antisemitism
- for UK: 
  - most: explicit AS in arab-israeli conflict
  - less: DA ???
- for Germany: 
  - most: contextual AS for court process assistant concentration camp
  - less: ukraine ware, russion invasion
-->

# **Ideation** by same discourse events and country

![height:550](plots/plot_ideation_discourse_same.png)

<!--
- better country comparison: just topics in all three countries the same
- highest over all countries, also explicit, Arab-Israeli conflict
- but also strong differences: Germany Ukraine war least antisemitic comments, Uk and France nearly the same
-->

# **Discussion dynamics:** Position, replies & users

![bg right:45% 95%](img/fb_replies.png)
<!--
- use meta information and structural knowlege to examine patterns where antisemitic comments appear more likely
- without content (text)
-->

## **Relative position:** Where appear antisemitic comments?

![height:550](plots/plot_position.png)

<!--
- idea: pattern of timing?
- antisemitic more in the beginning?
- or after discussion was going for some time?
- relative position: where in the prolonging of the whole comment section?
- not date or time, just relative (b/c different length)
- results: 
  - no clear pattern
  - some variations, some peaks, but hard to interpret
  - seems some variation, but pooled over all documents
-->

## **Relative position:** Differences by discourse events?

![height:550](plots/plot_position_discourse.png)

<!--
- separate by 4 shared discourse events
- results:
  - more clearer differences
  - antisemitic comments for Vaccination in Israel event more often in the beginning and middle, but counter speech more in the end of discussion 

- again: no clear pattern -> needs more investigation
-->

## **Relative position:** Initial comment or reply?

![height:550](plots/plot_reply.png)

<!--
- if no clear differences over length of discussion:
  - how about difference in initiative or reaction?
- average: around 2/3 of all comments are replies
- as aspected: counter speech and confirmation more likely as a reply
- (contex) AS comments more likely to be initial comments
-->

## **Replies:** Lead antisemitic comments to more replies?

![height:550](plots/plot_reply_n.png)

<!--
- we saw: antisemitic comments more often initial
- but do people react to them?
- 
-->

## **Replies:** Lead antisemitic comments to more antisemitic replies?

![height:550](plots/plot_reply_antisem.png)

<!--
- 
-->

## **Users:** Difference in activity?

![height:550](plots/plot_user_coms_n.png)

<!--
- next two slides: who are the users writing antisemitic comments?
- compare users by name: no or at least one antisemitic comment
- result: on average users that write two or more comments 
-->

## **Users:** Following antisemitic agenda?

![height:550](plots/plot_user_coms_prop.png)

<!--
- if at least one comment is antisemitic -> what is with the other comments?
- if two comments: 1/3 both (all) are antisemitic
- proportion declines with amount of comments
- if 6 or more comments: just 1/3 of their comments are antisemitic
-->

# Results

- differences between countries, platforms, and discourse events
- no clear pattern of relative position
- (contextual) antisemitism more often initial comments
- antisemitic comments lead to very long threats and more antisemitic replies
- users that post more, more likely to post antisemitic comments

<!--
- multivariate models: quantify effect sizes by controlling for variables
- text characteristics & analytics (length, words, collocations)
- connection of comments
-->
