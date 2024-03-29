# Dependencies ------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(papaja)
library(purrr)
library(tidyr)

# Functions ----------------------------------------------------------------
if (!exists("sim", mode = "environment")) {sim <- new.env()}

sim$bayes_main <- function(subject,
                                nTrial = 84,
                                initevidence,
                                outcomeprob) {
  # Initialize parameters
  outcomes_left <- rbinom(nTrial, 1, outcomeprob)
  outcomes_right <- rbinom(nTrial, 1, outcomeprob)
  ninev <- sum(initevidence)
  dp <- vector(mode = "integer", length = (ninev + nTrial))
  choice <- vector(mode = "integer", length = (ninev + nTrial))
  outcome <- vector(mode = "integer", length = (ninev + nTrial))
  # prior_left <- rep(.5, length(ps))
  # prior_right <- rep(.5, length(ps))
  left <- right <- list(
    total = 0,
    wins = 0
  )
  
  # Initial evidence
  nleft <- sum(initevidence[c(1, 2)])
  nright <- sum(initevidence[c(3, 4)])
  inev <- tibble(choice = c(rep(1, nleft),
                            rep(2, nright)),
                 outcome = c(rep(1, initevidence[1]),
                             rep(0, initevidence[2]),
                             rep(1, initevidence[3]),
                             rep(0, initevidence[4])))
  inev <- inev[sample(nrow(inev)),]
  
  # Trials
  for (t in 1:ninev) {
    a_left <- 1 + left$wins
    b_left <- 1 + left$total - left$wins
    a_right <- 1 + right$wins
    b_right <- 1 + right$total - right$wins
    
    dp[t] <- a_right / (a_right + b_right) - a_left / (a_left + b_left)
    
    trialchoice <- unlist(inev[t, 1])
    trialoutcome <- unlist(inev[t, 2])
    
    if (trialchoice == 1) {
      left$total <- left$total + 1
      if (trialoutcome == 1) {
        left$wins <- left$wins + 1
      }
    } else {
      right$total <- right$total + 1
      if (trialoutcome == 1) {
        right$wins <- right$wins + 1
      }
    }
    # Save
    choice[t] <- trialchoice
    outcome[t] <- trialoutcome
  }
  
  for (t in (ninev + 1):(nTrial + ninev)) {
    a_left <- 1 + left$wins
    b_left <- 1 + left$total - left$wins
    a_right <- 1 + right$wins
    b_right <- 1 + right$total - right$wins
    
    dp[t] <- a_right / (a_right + b_right) - a_left / (a_left + b_left)
    
    if (dp[t] < 0) {
      trialchoice <- 1
      trialoutcome <- outcomes_left[t - ninev]
    } else {
      trialchoice <- 2
      trialoutcome <- outcomes_right[t - ninev]
    }
    
    if (trialchoice == 1) {
      left$total <- left$total + 1
      if (trialoutcome == 1) {
        left$wins <- left$wins + 1
      }
    } else {
      right$total <- right$total + 1
      if (trialoutcome == 1) {
        right$wins <- right$wins + 1
      }
    }
    # Save
    choice[t] <- trialchoice
    outcome[t] <- trialoutcome
    
    # Update counter
    i <- (subject - 1) * (nTrial + ninev) + t
    setTxtProgressBar(pb, i)
  }
  
  # Return
  tibble(choice, outcome, dp, id = rep(subject, ninev + nTrial)) %>%
    mutate(trial = 1:n())
}

bayes_sim <- function(n = 100,
                      nTrial = 84,
                      initevidence = c(9, 3 , 3, 1),
                      outcomeprob = .75) {
  # Set progres bar
  m <- n * (nTrial + sum(initevidence))
  assign("pb",
         txtProgressBar(min = 0, max = m, style = 3),
         envir = .GlobalEnv)
  
  # Get input
  subjects <- 1:n
  
  # Run per participant
  pout <- map(subjects, ~sim$bayes_main(.x, nTrial, initevidence, outcomeprob))
  # Adjust choiceindex
  pout <- map(pout, ~mutate(.x, choiceindex = ifelse(choice == 2, 0, 1)))
  # Return
  pout
}

bayes_sumdata <- function(input, participant = NA) {
  if (is.na(participant)) {
    # All participants
    data.frame(
      map(input, "choice") %>%
        as.data.frame() %>%
        transmute(choice = rowMeans(.)),
      map(input, "choiceindex") %>%
        as.data.frame() %>%
        transmute(choiceindex = rowMeans(.)),
      map(input, "outcome") %>%
        as.data.frame() %>%
        transmute(outcome = rowMeans(.)),
      map(input, "dp") %>%
        as.data.frame() %>%
        transmute(dp = rowMeans(.)),
      map(input, "trial") %>%
        as.data.frame() %>%
        transmute(trial = rowMeans(.))
    )
  } else {
    # Individual participant
    input[[participant]] %>%
      mutate(trial = 1:n())
  }
}


# Simulate ----------------------------------------------------------------
test <- bayes_sim(n = 10000) %>%
  bayes_sumdata()

test_imp <- bayes_sim(n = 10000, outcomeprob = .25, initevidence = c(3, 9, 1, 3)) %>%
  bayes_sumdata()


# Both conditions ---------------------------------------------------------

test %>%
  ggplot() +
  geom_line(aes(x = trial, y = dp))
test_imp %>%
  ggplot() +
  geom_line(aes(x = trial, y = dp))

test %>%
  ggplot() +
  geom_line(aes(x = trial, y = choiceindex)) +
  geom_hline(aes(yintercept = .5), linetype = 3)

bind_rows(test %>%
            mutate(condition = "rich"),
          test_imp %>%
            mutate(condition = "impoverished")) %>%
  mutate(condition = factor(condition, levels = c("rich", "impoverished"))) %>%
  ggplot() +
  geom_line(aes(x = trial, y = choiceindex, color = condition)) +
  geom_hline(aes(yintercept = .5), linetype = 3) +
  scale_color_brewer(palette = "Dark2") +
  theme_apa()
