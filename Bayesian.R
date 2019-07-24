# Dependencies ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(papaja)
library(BayesFactor)
library(lme4)
library(lmerTest)

# Functions ----------------------------------------------------------------
Bayes_simulate_inev <- function(sID,
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
  }
  
  # Return
  tibble(choice, outcome, dp, id = rep(sID, ninev + nTrial)) %>%
    mutate(trial = 1:n())
}

bayes_sim <- function(n = 100,
                      nTrial = 84,
                      initevidence = c(9, 3 , 3, 1),
                      outcomeprob = .75) {
  # Get input
  subjects <- 1:n
  
  # Run per participant
  pout <- map(subjects, ~Bayes_simulate_inev(.x, nTrial, initevidence, outcomeprob))
  # Adjust choiceindex
  pout <- map(pout, ~mutate(.x, choiceindex = ifelse(choice == 2, -1, 1)))
  # Return
  pout
}

sumdata_bl <- function(input, participant = NA) {
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
test <- bayes_sim(n = 1000) %>%
  sumdata_bl()


test %>%
  ggplot() +
  geom_line(aes(x = trial, y = dp))

test %>%
  ggplot() +
  geom_line(aes(x = trial, y = choiceindex)) +
  geom_hline(aes(yintercept = .5), linetype = 3)
