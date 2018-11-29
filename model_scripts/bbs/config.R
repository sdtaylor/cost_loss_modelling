#bioclim data folder. Used to store yearly rasters of bioclim vars made from prism data
bioclim_data_folder='~/data/yearly_bioclim/'

dataFolder='raw_data/bbs/'

initial_results_file=paste('initial_model_output/bbs/bbs_results_method1.csv',sep='')

training_years=1971:1980
testing_years=2001:2010

#Mininum years in either the training or testing period to keep a site
minimum_years = 5

# Formula to pass to models
modelFormula=as.formula('presence ~ bio1+bio2+bio4+bio5+bio6+bio7+bio8+bio9+bio10+bio11+bio12+bio13+bio14+bio16+bio17+bio18+bio19')
modelFormula=as.formula('presence ~ bio2+bio3+bio5+bio8+bio9+bio15+bio16+bio18')


options(prism.path = "~/data/prism")
