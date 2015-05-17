# Talk Outline
1. Iterative development process.
2. Explain. Explain. Explain.

### User-Centered Design
* Plan
* Research
* Design
* Pilot
* Measure
* Design
* Pilot
* Measure
* Design ...

### How to Prototype 
* Plan: What do I want my audience to learn?
* Research: Does my visualization demonstrate this?
* Design: Is my visualization *focused* on this?

### How to Test
* Pilot: Use someone representative of audience knowledge and values.
  - Explain the raw data (what each axes and shape represents)
* Measure: Ask the pattern they see in the data.
  - Explain and show the pattern you are trying to demonstrate if it is different.
  - Ask if the desired pattern is hard to *see* and/or hard to *understand*.

### Iterate
* When data is hard to *see*, consider:
  - Changing colors
  - Changing graph type.
  - Introducing labels/annotations.

### Iterate
* When data is hard to *understand*, consider:
  - Summarzing data
  - Faceting data/small multiples
  - Producing more than one visual to demonstrate concept in smaller steps.

### Color

<div style="display:table-cell;vertical-align:middle;">
  <div style="margin-left:auto;margin-right:auto;">
    <img src = "img/badcolor.png" style="display:block;margin-left:auto;margin-right:auto;"></div>
</div>

### Color Correction

```{r, echo=FALSE, fig.align='center', fig.height=10, fig.width=16, fig.retina=TRUE}
model_data <- data.frame(
  model = rep(c('Model 1', 'Model 2', 'Model 3'), 4),
  race = c(rep('Black/AA', 3),
           rep('Hispanic/Latino', 3),
           rep('Asian', 3),
           rep('White', 3)),
  prob = c(.28, .78, .95, .39, .84, .96, .69, .91, .98, .56, .91, .97))

ggplot(model_data, aes(race, prob, fill = race)) +
  geom_bar(stat='identity') +
  scale_y_continuous('', labels = percent_format()) +
  scale_x_discrete('') +
  geom_text(aes(x = race, y = 0.02, label = race),
            angle = 90, color = 'white', family = 'Open Sans', hjust = 0,
            size = 12) +
  ggtitle('Probablity of College Readiness by Race and Model') +
  scale_fill_manual(values = c('#962A85', '#4BA5AD', '#A7CC46', '#DAD072')) +
  facet_grid(. ~ model) +
  theme_bw() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank(), 
        panel.border = element_blank(),
        legend.position = 'none',
        strip.background = element_rect(fill = '#626366'),
        strip.text = element_text(color = 'white', size = 18, 
                                  family = 'Open Sans'),
        title = element_text(size = 24, family = 'Open Sans'))
```

