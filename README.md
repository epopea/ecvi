# ecvi
This repository includes a body of scripts and data used to estimate our proposed **Extreme Climate Vulnerability Index** (ECVI). Estimation is based on the Alkire-Foster method (Alkire and Foster, 2011), combined with a newly proposed method on forward variance for the identification of the multidimensional deprivation cutoff.

Our ECVI encompasses three dimensions (exposure, susceptibility, and adaptive capacity). The exposure dimension includes climate extreme indices from the Climdex Project (Xavier et al., 2016). In this version we solve the climate-population scale matching problem by using the Bayesian Unconditional Grade of Membership (GoM) Model with a Dirichlet prior (Evans et al., 2000; Manton et al., 1994). To avoid integration, we use the Gibbs sampler with 5000 replications after a 2500 burnin period (MCMC) (Erosheva, 2007). Posterior distributions of the lambda parameters are provided as Boxplot representations for each extreme profile from the GoM model.

# Repository content
In this respository you will find the following material:
* Script for the estimation of the ECVI index (<tt>Final Script.R</tt>);
* Script for the production of radar charts (<tt>Radar Charts.R</tt>)
* Script for the production of dimensional decompositions (<tt>Barplots and Boxplots.R</tt>)
* Script for unidimensional cutoffs (<tt>Cutoffs.R</tt>)
* Script for the optimal identification of multidimensional cutoffs (sensitivity and dominance analysis) (<tt>ECVI Functions.R</tt>)
* Data on health, socioeconomic, infrastructure, and climate extreme variables (<tt>ECVI data.RData</tt>)

# How to cite

Please, when you use the ECVI material from this GitHub give the appropriate credit to this repository and the journal article that provides the methodology and formulas.

* **Traditional format**:

  - For the <tt>ECVI</tt> <tt>R</tt> material:\
  \
  Epopea Research Group (2021). *The estimation of an Extreme Climate Vulnerability Index (ECVI)*. Available at: <https://github.com/epopea/ecvi.git>.
  
  - For the formulas and methodology that supports the ECVI:\
  \
    Andrade, L., Guedes, G., Noronha, K., Silva, C., Andrade, J. (forthcoming). Health-Related Vulnerability to Climate Extremes in homoclimatic zones of Amazonia and the Northeast Region of Brazil. *PlosOne*.
    
* **Bibtex format**:

  - For the <tt>ECVI</tt> <tt>R</tt> material:\
  \
  <code>@misc{epopea_2021,</code>\
  <code>author = {EPOPEA Research Group},</code>\
  <code>title = {The estimation of an Extreme Climate Vulnerability Index (ECVI)},</code>\
  <code>year = {2021},</code>\
  <code>publisher = {GitHub},</code>\
  <code>journal = {GitHub repository},</code>\
  <code>howpublished = {\url{https://github.com/epopea/ecvi.git}},</code>\
  <code>commit = {38d4d72}</code>\
  <code>}</code>
  
  - For the formulas and methodology that support CEVI:\
  \
  <code>@article{andrade_etal_XXXX,</code>\
  <code>title={Health vulnerability related to climate extremes in Amazonia and the Northeast region of Brazil},</code>\
  <code>author={Andrade, L. and Guedes, G. and Noronha, K. and Silva, C. and Andrade, J. },</code>\
  <code>journal={PlosOne},</code>\
  <code>volume={XX},</code>\
  <code>pages={XX-XX},</code>\
  <code>year={XXXX},</code>\
  <code>publisher={Public Library of Science}</code>\
  <code>}</code>

# References
* Alkire, S. and J. Foster (2011). Counting and multidimensional poverty measurement. *Journal of public economics*, 95(7-8), 476-487.
* Evans, M. E., N. Hastings and B. Peacock. (2000). *Statistical Distributions*. New York: John Wiley & Sons.
* Manton, K. G., M. A. Woodbury and H. D. Tolley. (1994). *Statistical Applications Using Fuzzy Sets*. New York: John Wiley & Sons.
* Erosheva, E. A., S. E. Fienberg and C. Joutard. (2007). Describing disability through individual-level mixture models for multivariate binary data. *The Annals of Applied Statistics*, 1, 502–537.
* Xavier, A. C., C. W. King and B. R. Scanlon. (2016). Daily gridded meteorological variables in Brazil 724 (1980–2013). *International Journal of Climatology*, 36(6), 2644-2659.

