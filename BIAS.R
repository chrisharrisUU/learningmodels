# Dependencies ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(papaja)

#     y       A |  B || trial 1, 2, ..., n
# win left    1 |  - ||  1 |  1 | -1 | -1 | -1 | ...
# loss left  -1 |  - || -1 | -1 |  1 | -1 | -1 | ...
# win right   - |  1 ||  1 | -1 | -1 |  1 | -1 | ...
# loss right  - | -1 || -1 | -1 | -1 | -1 |  1 | ...

# Functions ---------------------------------------------------------------
match <- function(colmn, prompt) {
  tibble(prompt, colmn) %>%
    transmute(match = ifelse(prompt == colmn, 1, 0)) %>%
    sum
  
}

bias_main <- function(noise = noise,
                      nTrial = nTrial,
                      outcomeprob = outcomeprob,
                      initevidence = initevidence,
                      subject) {
  # Prompts for left and right option
  prompt_left <- c(1, -1, 0, 0)
  prompt_right <- c(0, 0, 1, -1)
  
  # Matrix with outcomes
  outcome <- data.frame(left = rbinom(nTrial, 1, outcomeprob),
                        right = rbinom(nTrial, 1, outcomeprob))
  
  # Initial evidence
  ninev <- sum(initevidence)
  inev <- matrix(c(rep(c(1, -1, -1, -1), initevidence[1]),
                   rep(c(-1, 1, -1, -1), initevidence[2]),
                   rep(c(-1, -1, 1, -1), initevidence[3]),
                   rep(c(-1, -1, -1, 1), initevidence[4])),
                 ncol = 4, byrow = TRUE) %>%
    data.frame() %>%
    mutate(trial = 1:n()) %>%
    split(.$trial, ) %>%
    map(~select(.x, -trial)) %>%
    map(~unlist(.x)) %>%
    map(~unname(.x))
  
  # Create dataframe
  results <- tibble(
    choice = c(rep(-1, sum(initevidence[1:2])),
               rep(1, sum(initevidence[3:4]))),
    outcome = c(rep(1, initevidence[1]),
                rep(0, initevidence[2]),
                rep(1, initevidence[3]),
                rep(0, initevidence[4]))
  ) %>%
    mutate(evidence = inev)
  results <- results[sample(nrow(results)),] %>%
    mutate(trial = 1:n())
  results <- results %>%
    mutate(left = (results %>%
                     split(.$trial) %>%
                     map_dbl(~match(unlist(.x$evidence), prompt_left))),
           right = (results %>%
                      split(.$trial) %>%
                      map_dbl(~match(unlist(.x$evidence), prompt_right)))) %>%
    mutate(cumleft = cumsum(left),
           cumright = cumsum(right))
  
  # Initialize remaining trials
  init <- tibble(trial = (1:nTrial) + ninev)
  results <- bind_rows(results, init)
  
  # Simulation
  for (t in (1 + ninev):(nTrial + ninev)) {
    # Get recent evidence
    colmn <- results$evidence[t - 1] %>% unlist
    
    # Calculate matches with respective prompts and update cumulative matches
    results$left[t] <- match(colmn, prompt_left)
    results$cumleft[t] <- results$cumleft[t - 1] + results$left[t]
    results$right[t] <- match(colmn, prompt_right)
    results$cumright[t] <- results$cumright[t - 1] + results$right[t]
    
    # Determine choice and outcome
    results$choice[t] <- ifelse(results$cumleft[t] > results$cumright[t], -1, 1)
    results$outcome[t] <- ifelse(results$choice[t] == -1,
                                 outcome[t - ninev, 1],
                                 outcome[t - ninev, 2])
    
    # Determine (idealized) experience
    if (results$choice[t] == -1) {
      if (results$outcome[t] == 1) {
        tmem <- c(1, -1, -1, -1)
      } else {
        tmem <- c(-1, 1, -1, -1)
      }
    } else {
      if (results$outcome[t] == 1) {
        tmem <- c(-1, -1, 1, -1)
      } else {
        tmem <- c(-1, -1, -1, 1)
      }
    }
    # Update counter
    # i <- (subject - 1) * (nTrial + ninev) + t
    # setTxtProgressBar(pb, i)
    
    # Save as (noisy) memory
    results$evidence[t] <- list(tmem * sample(c(rep(-1, noise), rep(1, 4 - noise))))
  }
  
  # Return
  results
}

bias_sim <- function(n = 100,
                     noise = 1,
                     nTrial = 84,
                     outcomeprob = .75,
                     initevidence = c(9, 3, 3, 1)) {
  # Set progres bar
  # m <- n * (nTrial + sum(initevidence))
  # pb <- txtProgressBar(min = 0, max = m, style = 3)
  
  # Run per participant
  pout <- map(1:n, ~bias_main(noise, nTrial, outcomeprob, initevidence, .x))
  # Adjust choiceindex
  pout <- map(pout, ~mutate(.x, choiceindex = ifelse(choice == -1, 1, 0)))
  # Return
  pout
}

sumdata_bi <- function(input, participant = NA) {
  if (is.na(participant)) {
    # All participants
    data.frame(
      map(input, "choice") %>%
        as.data.frame() %>%
        transmute(choice = rowMeans(.)),
      map(input, "choiceindex") %>%
        as.data.frame() %>%
        transmute(choiceindex = rowMeans(.)),
      map(input, "cumleft") %>%
        as.data.frame() %>%
        transmute(cumleft = rowMeans(.)),
      map(input, "cumright") %>%
        as.data.frame() %>%
        transmute(cumright = rowMeans(.))
    ) %>%
      mutate(trial = 1:n())
  } else {
    # Individual participant
    input[[participant]] %>%
      mutate(trial = 1:n())
  }
}

# Simulate ----------------------------------------------------------------

test_0 <- bias_sim(noise = 0, n = 1000)
test_1 <- bias_sim(noise = 1, n = 10)
test_2 <- bias_sim(noise = 2)
test_3 <- bias_sim(noise = 3)

test_0neg <- bias_sim(noise = 0, outcomeprob = .25, initevidence = c(3, 9, 1, 3), n = 1000)
test_1neg <- bias_sim(noise = 1, outcomeprob = .25, initevidence = c(3, 9, 1, 3), n = 10)
test_2neg <- bias_sim(noise = 2, outcomeprob = .25, initevidence = c(3, 9, 1, 3))
test_3neg <- bias_sim(noise = 3, outcomeprob = .25, initevidence = c(3, 9, 1, 3))

# Graphs ------------------------------------------------------------------
p <- seq(0, 1, length = 100)

# Draw as beta distribution
test_1neg %>%
  sumdata_bi() %>%
  split(.$trial) %>%
  map_dfr(~dbeta(p, .$cumleft, .$cumright)) %>%
  gather(key = "trial",
         value = "value") %>%
  group_by(trial) %>%
  mutate(x = p) %>%
  ungroup %>%
  mutate(trial = factor(trial, levels = c(1:100))) %>%
  ggplot() +
  geom_line(aes(x = x,
                y = value,
                color = trial))

# Draw as choiceindex
test_1neg %>%
  sumdata_bi() %>%
  ggplot() +
  geom_line(aes(x = trial, y = choiceindex)) +
  theme_apa()

# combine both conditions
bind_rows(test_1 %>%
            sumdata_bi() %>%
            mutate(condition = "rich"),
          test_1neg %>%
            sumdata_bi() %>%
            mutate(condition = "impoverished")) %>%
  mutate(condition = factor(condition, levels = c("rich", "impoverished"))) %>%
  ggplot() +
  geom_line(aes(x = trial, y = choiceindex, color = condition)) +
  geom_hline(aes(yintercept = .5), linetype = 3) +
  scale_color_brewer(palette = "Dark2") +
  theme_apa()
  
