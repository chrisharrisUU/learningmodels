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
RLtutorial_simulate_inev <- function(simpars,
                                     sID,
                                     nTrial = 84,
                                     initevidence,
                                     outcomeprob) {
  # Get alpha and beta parameters
  alpha <- simpars[1]
  beta <- simpars[2]
  # Length of initial evidence
  ninev <- sum(initevidence)
  
  # Init expectations
  v <- rep(.5, 2)
  
  # # initiate output matrices
  # VV <- list(matrix(nrow = n,
  #                   ncol = nTrial),
  #            matrix(nrow = n,
  #                   ncol = nTrial))
  # PP <- matrix(nrow = n,
  #              ncol = nTrial)
  
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
  
  # get the task information
  outcome <- data.frame(left = rbinom(nTrial, 1, outcomeprob),
                        right = rbinom(nTrial, 1, outcomeprob))
  nt <- nTrial + ninev
  nc <- 2
  
  # initialise choice and outcome vectors to store the data.
  loglik <- 0
  choice <- rep(NA, nt)
  VV <- matrix(nrow = nt,
               ncol = nc) # matrix to store value over trials
  PP <- matrix(nrow = nt,
               ncol = nc) # matrix to store choice probability over trials
  
  # Initial evidence
  for (t in 1:nrow(inev)) {
    # Compute likelihood of the each choice option 
    # exponentiate the value
    ev <- exp(beta * v)
    # compute the sum of the values
    sev <- sum(ev)
    # probability each choice i.e. ratio of each of the values and their sum. 
    # This code effectively does: 
    # p(1) = ev(1)/sev;
    # p(2) = ev(2)/sev;
    p <- ev/sev
    
    c <- unlist(inev[t, 1])
    r <- unlist(inev[t, 2])
    
    loglik <- loglik + log(p[c])
    
    # store value of V
    VV[t,] <- v
    PP[t,] <- p
    
    # compute prediction error
    dv <- r - v[c]
    # update value
    v[c] <- v[c] + alpha * dv
    
    # store choice
    choice[t] <- c
    
  }
  
  # Free sampling
  for (t in (ninev + 1):nt) {
    # Compute likelihood of the each choice option 
    # exponentiate the value
    ev <- exp(beta * v)
    # compute the sum of the values
    sev <- sum(ev)
    # probability each choice i.e. ratio of each of the values and their sum. 
    # This code effectively does: 
    # p(1) = ev(1)/sev;
    # p(2) = ev(2)/sev;
    p <- ev/sev
    
    # Do a weighted coinflip to make a choice: choose stim 1 if random
    # number is in the [0 p(1)] interval, and 2 otherwise
    if (runif(1) < p[1]) {
      c <- 1
    } else {
      c <- 2
    }
    # select the outcome for this choice
    r <- outcome[t - ninev, c]
    
    # update the log likelihood with the p(choice|model):
    # note that this line can be done more efficiently after the trial
    # loop, see below
    # alternative, more efficient way of computing the likelihood
    # loglik = sum(log(PP(c1,1)))+sum(log(PP(c2,2)));
    loglik <- loglik + log(p[c])
    
    # store value of V
    VV[t,] <- v
    PP[t,] <- p
    
    # compute prediction error
    dv <- r - v[c]
    # update value
    v[c] <- v[c] + alpha * dv
    
    # store choice
    choice[t] <- c
  }
  
  # Return
  data.frame(choice,
             value_left = VV[,1],
             value_right = VV[,2],
             choiceprob_left = PP[,1],
             choiceprob_right = PP[,2],
             outprob = rep(outcomeprob, nt)) %>%
    mutate(trial = 1:n())
}
reswag_sim <- function(n = 100,
                       alpha = .25,
                       beta = 1,
                       nTrial = 84,
                       initevidence = c(9, 3 , 3, 1),
                       outcomeprob = .75) {
  # Get input
  subjects <- 1:n
  simpars <- c(alpha, beta)
  names(simpars) <- c("alpha", "beta")
  
  # Run per participant
  pout <- map(subjects, ~RLtutorial_simulate_inev(simpars, .x, nTrial, initevidence, outcomeprob))
  # Adjust choiceindex
  pout <- map(pout, ~mutate(.x, choiceindex = ifelse(choice == 2, -1, 1)))
  # Return
  pout
}
sumdata_rw <- function(input, participant = NA) {
  if (is.na(participant)) {
    # All participants
    data.frame(
      map(input, "choice") %>%
        as.data.frame() %>%
        transmute(choice = rowMeans(.)),
      map(input, "choiceindex") %>%
        as.data.frame() %>%
        transmute(choiceindex = rowMeans(.)),
      map(input, "value_left") %>%
        as.data.frame() %>%
        transmute(value_left = rowMeans(.)),
      map(input, "value_right") %>%
        as.data.frame() %>%
        transmute(value_right = rowMeans(.)),
      map(input, "choiceprob_left") %>%
        as.data.frame() %>%
        transmute(choiceprob_left = rowMeans(.)),
      map(input, "choiceprob_right") %>%
        as.data.frame() %>%
        transmute(choiceprob_right = rowMeans(.)),
      map(input, "outprob") %>%
        as.data.frame() %>%
        transmute(outprob = rowMeans(.)),
      map(input, "trial") %>%
        as.data.frame() %>%
        transmute(trial = rowMeans(.))
    ) %>%
      pivot_longer(cols = c(value_left, value_right),
                   names_to = "bandit",
                   values_to = "value")
  } else {
    # Individual participant
    input[[participant]] %>%
      mutate(trial = 1:n()) %>%
      pivot_longer(cols = c(value_left, value_right),
                   names_to = "bandit",
                   values_to = "value")
  }
}
draw <- function(input, choiceindex = FALSE) {
  if (choiceindex) {
    input %>%
      group_by(trial) %>%
      summarize(choiceindex = mean(choiceindex)) %>%
      ggplot() +
      geom_line(aes(x = trial, y = choiceindex)) +
      theme_apa()
  } else {
    input %>%
      ggplot() +
      geom_line(aes(x = trial, y = value, color = bandit)) +
      geom_hline(aes(yintercept = outprob), linetype = 3) +
      geom_line(aes(x = trial, y = choiceprob_left)) +
      theme_apa()
  }
}

# Simulate ----------------------------------------------------------------
# sim_exp1_n10000 <- reswag_sim(n = 10000, initevidence = c(3, 0, 0, 1))
sim_exp1_n1000r <- reswag_sim(n = 1000, initevidence = c(3, 0, 0, 1))
sim_exp1_n1000r %>%
  sumdata_rw() %>%
  draw()

sim_exp1_n1000i <- reswag_sim(n = 1000, initevidence = c(0, 3, 1, 0), outcomeprob = .25)
sim_exp1_n1000i %>%
  sumdata_rw() %>%
  draw()

# sim_exp2_n10000 <- reswag_sim(n = 10000)
sim_exp2_n10000r <- reswag_sim(n = 10000)
sim_exp2_n1000r %>%
  sumdata_rw() %>%
  draw()

sim_exp2_n10000i <- reswag_sim(n = 10000, initevidence = c(3, 9, 1, 3), outcomeprob = .25)
sim_exp2_n1000i %>%
  sumdata_rw() %>%
  draw()

reswag_sim(n = 10000) %>%
  sumdata_rw() %>%
  draw()

reswag_sim(n = 100, initevidence = c(3, 9, 1, 3), outcomeprob = .25) %>%
  sumdata_rw() %>%
  mutate(alpha = .25,
         beta = 1) %>%
  draw(choiceindex = TRUE)


# Combinations -------------------------------------------------------------------
alphas <- c(.1, .25, .5, .75, .9)
betas <- c(1, 5, 9)

combinations <- map(alphas, ~map2(.x, betas,
                                  ~reswag_sim(n = 1000, initevidence = c(3, 9, 1, 3), outcomeprob = .25) %>%
                                    sumdata_rw() %>%
                                    mutate(alpha = .x,
                                           beta = .y))) %>%
  map(., ~bind_rows(.x)) %>%
  bind_rows(.)
combinations %>%
  ggplot() +
  geom_line(aes(x = trial, y = value, color = bandit)) +
  geom_hline(aes(yintercept = outprob), linetype = 3) +
  geom_line(aes(x = trial, y = choiceprob_left)) +
  theme_apa() +
  facet_wrap(vars(alpha, beta))

# Both conditions ---------------------------------------------------------

rw_rich <- sim_exp2_n10000r %>%
  map(., ~mutate(.x, choiceindex = ifelse(choiceindex == -1, 0, 1))) %>%
  sumdata_rw() %>%
  mutate(condition = "rich")
rw_imp <- sim_exp2_n10000i %>%
  map(., ~mutate(.x, choiceindex = ifelse(choiceindex == -1, 0, 1))) %>%
  sumdata_rw() %>%
  mutate(condition = "impoverished")

bind_rows(rw_rich, rw_imp) %>%
  mutate(condition = factor(condition, levels = c("rich", "impoverished"))) %>%
  ggplot() +
  geom_line(aes(x = trial, y = choiceindex, color = condition)) +
  scale_color_brewer(palette = "Dark2") +
  theme_apa()
