# markov_switching_model
Predicting eletricity spot prices using the Markov switching model.

This was a project for the course Matematika z Računalnikom (FMF) tought by Alex Simpson, assisted by Dejan Velušček and Gregor Šega.

A simple two-regime Markov Switching model was chosen. Two Gaussian distributions for the regimes and a Bernoulli distribution
for regime switching. An EM algorithm was implemented in R and used to fit the model to the data (daily electricity prices).

The implementation of the EM algorithm follows [an example](https://personal.eur.nl/kole/rsexample.pdf) by Erik Kole.
