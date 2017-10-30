import rpy2
from rpy2.robjects.packages import STAP
from rpy2.robjects import r, pandas2ri
from rpy2.robjects.packages import importr
import rpy2.robjects.packages as rpackages
import pandas as pd
import numpy as np
import os


class MOB(object):


	def __init__(self,data_path=None, full_data_path=None,id_key=None,depth=8,min_size=400,trn_split=.8,ver_string=None):
		self.data_path = data_path
		self.full_data_path =  full_data_path
		self.id_key = id_key
		self.depth = depth
		self.min_size = min_size
		self.trn_split = trn_split
		self.ver_string = ver_string
		self.reg =['balance', 'age', 'campaign', 'pdays', 'previous','calls', 'emails', 'coupons']
		self.part =['job', 'marital', 'education', 'default', 'housing', 'loan', 'contact', 'month', 'poutcome', 'day','age']
		self.target = ['target']
		self.formula = None
		self.summary = None
		self.impact_Q4 = False
		self.seg_lvl_data = None
		self.beta = None
		self.var_mean = None
		self.id_type = {self.id_key : str}

		
	def make_formula(self):
		reg_var = " + ".join(self.reg)
		reg_var = " ~ ".join([self.target[0],reg_var])
		part_var = " + ".join(self.part)
		self.formula = " | ".join([reg_var, part_var])

	def show_formula(self):
		self.make_formula()
		print(self.formula)
	

	def mob_fit(self):
		self.make_formula()
		with open('launch/02_RScripts/mob_V0.R', 'r') as f:
			string = f.read()
		mob_train = STAP(string, "mob_V0.R")
		print (mob_train.mob_run(self.data_path, self.full_data_path, self.formula,id_key=self.id_key,train_split=self.trn_split, max_depth=self.depth,
					min_size=self.min_size, ver_string = self.ver_string))
		

	# def md_profile_generator (self): 
	# 	self.summary = self.master_data.groupby('nodes').agg({'XTAMPZA_TRx':'mean', 'IMS_ID':'nunique'})
	# 	self.summary['NBRx_coverage_Q4'] = self.master_data.groupby('nodes')['PERC_NBRx_Q4'].sum()/4
	# 	self.summary['NBRx_coverage_ALL'] = self.master_data.groupby('nodes')['PERC_NBRx_ALL'].sum()/4
	# 	#self.summary['Count'] = master_data.groupby('nodes')['IMS_ID'].count()
	# 	self.summary['ER_BR_rat_ALL'] = self.master_data.groupby('nodes')['PERC_ER_BRAND_GEN_TRX_Q4'].mean()
	# 	self.summary['Target'] = self.master_data.groupby(['Target_Q32017','nodes'])['IMS_ID'].nunique()['Y']
	# 	self.summary['Writers_Q4'] =self.master_data[self.master_data['Quater']=='Q4'][self.master_data['XTAMPZA_TRx']>0].groupby('nodes')['IMS_ID'].nunique()
	# 	self.summary['Called_Q4'] = self.master_data[self.master_data['Quater']=='Q4'][self.master_data['Calls']>0].groupby('nodes')['IMS_ID'].nunique()
	# 	self.summary['Tot_Calls_Q4'] = self.master_data.groupby(['Quater','nodes'])['Calls'].sum()['Q4']
	# 	self.summary['Calls_Q4_PER_TRx'] = self.summary['Tot_Calls_Q4']/self.master_data.groupby('nodes')['XTAMPZA_TRx'].sum()/4
	# 	self.summary['Tot_Calls_AllQs'] = self.master_data.groupby(['nodes'])['Calls'].sum()
	# 	self.summary = self.summary.reset_index()
	# 	temp_data = self.master_data.pivot_table(values='IMS_ID', columns='CALLED_FLAG', index='nodes', aggfunc='nunique', fill_value=0).rename(columns={0:'Not Called AllQs', 1:'Called AllQs'}).reset_index()
	# 	self.summary = self.summary.merge(temp_data, on='nodes', how='left')
	# 	temp_data = self.master_data.pivot_table(values='IMS_ID', columns='WRITER_FLAG', index='nodes', aggfunc='nunique', fill_value=0).rename(columns={0:'Non Writer AllQs', 1:'Writer AllQs'}).reset_index()
	# 	self.summary = self.summary.merge(temp_data, on='nodes', how='left')
	# 	self.summary['% Non Writers AllQs'] = (self.summary['Non Writer AllQs']/(self.summary['Non Writer AllQs'] + self.summary['Writer AllQs']))*100
	# 	self.summary['% Writers AllQs'] = (self.summary['Writer AllQs']/(self.summary['Non Writer AllQs'] + self.summary['Writer AllQs']))*100
	# 	temp_data = self.master_data.pivot_table(values='IMS_ID', columns='CGM_Specialty_Group_Bkt_4cat', index='nodes', aggfunc='nunique', fill_value=0).reset_index()
	# 	self.summary = self.summary.merge(temp_data, on='nodes', how='left')
	# 	display(self.summary.T)
	# 	temp_data = self.master_data.pivot_table(values='IMS_ID', columns='AM_No_See_Rating', index='nodes', aggfunc='nunique', fill_value=0).reset_index()
	# 	self.summary = self.summary.merge(temp_data, on='nodes', how='left')
	# 	self.summary.T.reset_index().to_csv('Collegium_Pharma/04_Results/'+self.ver_string+'/summary.csv', index = False)


	


	@staticmethod
	def impact(means, betas):
		B_X = means*betas
		y_pred = np.sum(B_X)
		impact = B_X/y_pred
		return ((list(impact)))


	def impact_table(self):
		self.var_mean = ['(Intercept)'] + self.reg
		self.beta = ['beta_'+i for i in self.var_mean]
		## Todo add more options
		color_col = ['month']
		Impact_Var = self.var_mean+self.beta+self.target+color_col
		if self.impact_Q4:
			print ('*******************\nImpact over latest quaters\n*******************')
			master_data = self.master_data[self.master_data['Quater'] == 'Q4']
		else:
			print ('*******************\nImpact over all quaters\n*******************')
			master_data = self.master_data
		master_data['(Intercept)'] = 1
		self.seg_lvl_data = master_data.groupby('nodes')[Impact_Var].mean().reset_index()


		i_var = ['Impact_'+i for i in self.var_mean]
		temp_data = pd.DataFrame(columns=i_var)
		for m, b in zip(self.seg_lvl_data[self.var_mean].values, self.seg_lvl_data[self.beta].values):
			temp_data.loc[len(temp_data)] = self.impact(m,b)
		temp_data['nodes'] = self.seg_lvl_data['nodes']

		#self.seg_lvl_data[i_var] = pd.DataFrame((seg_lvl_data[Impact_Var].apply(self.impact, axis=1)).values.tolist())
		#print ('******************* Overall Impact *********************')
		o_imp = np.sum(temp_data[i_var],axis=0)/np.sum(np.sum(temp_data[i_var],axis=0))
		#display(o_imp)
		o_imp.to_csv('04_Results/'+self.ver_string+'/overall_impact.csv', index = False)
		#print ('\n******************* Segment Level Impact ***********************')
		s_imp = temp_data[['nodes']+i_var].T
		#display(s_imp)
		s_imp.to_csv('04_Results/'+self.ver_string+'/segment_impact.csv', index = False)




	def create_summary(self):
		self.master_data = pd.read_csv('04_Results/'+self.ver_string+'/master_data_full.csv', dtype=self.id_type)
		print ('*******************\nTREE\n*******************')
		#display(Image(filename='04_Results/'+self.ver_string+'/box_tree.jpg', width=1500, unconfined=True))
		print ('\n-----------------------------------------------------------------------------------------------------------------------------\n')
		print ('*******************\nSummary on all quaters\n*******************')
		#self.md_profile_generator()
		print ('\n-----------------------------------------------------------------------------------------------------------------------------\n')
		self.impact_table()





