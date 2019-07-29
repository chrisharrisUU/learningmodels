# Dependencies ------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(papaja)
library(purrr)
# library(tidyr)

# Functions ---------------------------------------------------------------
if (!exists("sim", mode = "environment")) {sim <- new.env()}

sim$mdm_main <- function(noise = noise,
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
    split(.$trial) %>%
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
                     map_dbl(~sim$match_mdm(unlist(.x$evidence), prompt_left))),
           right = (results %>%
                      split(.$trial) %>%
                      map_dbl(~sim$match_mdm(unlist(.x$evidence), prompt_right)))) %>%
    # cumsum is equivalent to I (formula 3)
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
    results$left[t] <- sim$match_mdm(colmn, prompt_left)
    results$cumleft[t] <- results$cumleft[t - 1] + results$left[t]
    results$right[t] <- sim$match_mdm(colmn, prompt_right)
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
    i <- (subject - 1) * (nTrial + ninev) + t
    setTxtProgressBar(pb, i)
    
    # Save as (noisy) memory (Learning parameter L)
    results$evidence[t] <- list(tmem * rbinom(4, 1, (1 - noise)))
  }
  
  # Return
  results
}

# Define matching function
sim$match_mdm <- function(prompt, colmn) {
  tibble(prompt, colmn) %>%
    transmute(match = ifelse(prompt == colmn, 1, 0),
              n = ifelse(prompt != 0 & colmn != 0, 1, 0)) %>%
    summarize(pt = sum(match),
              n = sum(n)) %>%
    summarize(s = ifelse(n != 0, pt/n, 0)) %>%
    summarize(a = s^3) %>%
    unlist()
}

mdm_sim <- function(n = 100,
                    noise = .25,
                    nTrial = 84,
                    outcomeprob = .75,
                    initevidence = c(9, 3, 3, 1)) {
  # Set progres bar
  m <- n * (nTrial + sum(initevidence))
  assign("pb",
         txtProgressBar(min = 0, max = m, style = 3),
         envir = .GlobalEnv)
  
  # Run per participant
  pout <- map(1:n, ~sim$mdm_main(noise, nTrial, outcomeprob, initevidence, .x))
  # Adjust choiceindex
  pout <- map(pout, ~mutate(.x, choiceindex = ifelse(choice == 1, 0, 1)))
  # Return
  pout
}

mdm_sumdata <- function(input, participant = NA) {
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

# Simulate -------------------------------------------------------------
test_pos <- mdm_sim(n = 10000) %>%
  mdm_sumdata()
test_neg <- mdm_sim(n = 10000, outcomeprob = .25, initevidence = c(3, 9, 1, 3)) %>%
  mdm_sumdata()

 

# Graphs ------------------------------------------------------------------

bind_rows(test_pos %>%
            mutate(condition = "rich"),
          test_neg %>%
            mutate(condition = "impoverished")) %>%
  mutate(condition = factor(condition, levels = c("rich", "impoverished"))) %>%
  ggplot() +
  geom_line(aes(x = trial, y = choiceindex, color = condition)) +
  geom_hline(aes(yintercept = .5), linetype = 3) +
  scale_color_brewer(palette = "Dark2") +
  theme_apa()

# What if I rewrite the functions so that they dont run everything for one participant, 
# but each trial for all participants?
# I believe this should speed up the simulations substantially