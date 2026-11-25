source('r/example.R')
source('r/advanced.R')
source('r/simulation.R')
sim <- run_simulation()
export_simulation(sim)
print(subset(sim$summary, parameter %in% c('mean_SBP','slope_SBP') & scenario=='MAR',
             select=c(method,parameter,bias,empirical_se,mean_model_se,coverage,mcse_coverage)))
