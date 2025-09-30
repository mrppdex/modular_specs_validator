data <- data_raw %>%
  mutate(
    cmp = round(cmp_value, 2),
    trt = round(trt_value, 2),
    value = round(ifelse(is.na(effect_estimate), 0, effect_estimate), 2),
    lower = round(ifelse(is.na(effect_lower_ci), -1, effect_lower_ci), 2),
    upper = round(ifelse(is.na(effect_upper_ci), 1, effect_upper_ci), 2)
  ) %>%
  mutate(
    logscale=ifelse(grepl('(^OR)|(^HR)', measure) | (effect_type=='Risk' & difference_measure=='Measure'), TRUE, FALSE),
    logbase=2,
    reversed=improvement_direction!='increase',
  ) %>%
  mutate(
    value = ifelse(logscale & value <= 0, NA, value),
    lower = ifelse(logscale & lower <= 0, NA, lower),
    upper = ifelse(logscale & upper <= 0, NA, upper)
  ) %>%
  rowwise() %>%
  mutate(effect=paste(strwrap(variable_type, 23), collapse = '\n')) %>% # break long labels
  ungroup() %>%
  mutate(axis_name = case_when(
    effect_type=='Risk' & difference_measure=='Measure' ~ 'Relative Rate per Year (95% CI)',
    effect_type=='Risk' ~ 'Risk Difference (95% CI)',
    grepl('^OR', difference_measure) ~ 'Odds Ratio (95% CI)',
    grepl('^HR', difference_measure) ~ 'Hazard Ratio (95% CI)',
    grepl('^Relative Rate', difference_measure) ~ 'Risk Difference (95% CI)',
    grepl('Proportion|Percentage|%', variable_type) | grepl('Percent', measure) ~ 'Difference in Percent Change (95% CI)',
    TRUE ~ 'Change Difference (95% CI)'
  )) %>%
  mutate(
    non_inferiority_margin = NA
  ) %>%
  mutate(
    trt_txt = case_when(
      grepl('Proportion|Percentage|%', variable_type) | grepl('Percent', measure) | grepl('^Relative Rate', difference_measure) ~ sprintf('%.2f%%', trt),
      !grepl('Percent', measure) & !grepl('^Relative Rate', difference_measure) ~ sprintf('%.2f', trt),
      TRUE ~ '-'
    ),
    cmp_txt = case_when(
      grepl('Proportion|Percentage|%', variable_type) | grepl('Percent', measure) | grepl('^Relative Rate', difference_measure) ~ sprintf('%.2f%%', cmp),
      !grepl('Percent', measure) & !grepl('^Relative Rate', difference_measure) ~ sprintf('%.2f', cmp),
      TRUE ~ '-'
    )
  )


data_benefit <- data %>% filter(effect_type=='Benefit')

data_risk <- data %>% filter(effect_type=='Risk')
