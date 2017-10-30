
# coding: utf-8



from IPython.core.display import display, HTML


# ### Importing Data and Packages


import os
import pandas as pd
import numpy as np
from tqdm import tqdm

import ipywidgets as widgets
from ipywidgets import interact, interactive, fixed, interact_manual, SelectMultiple, Layout, RadioButtons, Checkbox


def melt (df, col = ['index','Q1_calls', 'Q2_calls', 'Q3_calls', 'Q4_calls'], var_name = 'Calls'):
	temp_df = df[col]
	temp_df.columns = ['index','Q1', 'Q2', 'Q3', 'Q4']
	return (pd.melt(temp_df, id_vars='index', value_vars=['Q1', 'Q2', 'Q3', 'Q4'], var_name='Quater',value_name=var_name))

def do_er_br_gen_perc(x):
	return (x[0]+x[1])/+(x[0]+x[1]+x[2]) 

def do_er_gen_perc(x):
	return (x[1])/+(x[0]+x[1]+x[2]) 

def fill_medium(x):
	return x.fillna(np.median(x.dropna()))

def to_spcl_bkt(x):
	if x == 'PAIN' or x =='NP/PA' : return 'Specialist'
	elif x == 'PCP': return 'PCP'
	else: return 'All Others'


def add_lag (data_feed, col = 'XTAMPZA_TRx'):
	df = data_feed.groupby(['IMS_ID', 'Quater'])[col].first()
	lag= df.shift(1).values
	df = df.reset_index()
	df[col+'_Lag'] = lag
	df[col+'_Lag'][df['Quater']=='Q1']= 0
	df = df.fillna(0).drop(col, axis =1)
	return pd.merge(data_feed, df, how='left', on=['IMS_ID', 'Quater'])    

def to_spcl_bkt(x):
	if x == 'PAIN' : return 'PAIN'
	elif x == 'NP/PA' : return 'NP/PA'
	elif x == 'PCP': return 'PCP'
	else: return 'All Others'

def to_group(x):
	if x == 'unk': x=0
	else: x = float(x)
	if x == 0: x=1
	elif x < 4: x=1
	elif x>7: x=3
	else: x=2
	return x

def to_int(x):
	if x == 'unk': x=0
	else: x = int(float(x))
	return x

def pre_process():
	os.chdir('D:/Anaconda3/projects/Collegium_Pharma/03_PyScripts')
	print ('preprocessing..')
	id_type = {'IMS_ID' : str}
	hcp_data = pd.read_csv('../01_Data/0920_MasterData/Final HCP Database v0.6.csv', dtype=id_type, thousands=',')
	switch_rx = pd.read_csv('../01_Data/0920_MasterData/switch_addon_10_13.csv', dtype=id_type)
	switch_rx['IMS_ID'] = switch_rx['IMS_ID'].apply(lambda x: x.zfill(7))
	hcp_data = hcp_data.reset_index()

	# #### Adding SWITCH Rx data
	switch_rx['SWITCH_ADD_ON_Rx'] = switch_rx['Switch_Addon_Rx_Q4'] + switch_rx['Switch_Addon_Rx_Q3']
	hcp_data = hcp_data.merge(switch_rx[['IMS_ID','SWITCH_ADD_ON_Rx']], how='left', on='IMS_ID')

	# #### Adding Writer & Called Flag
	hcp_data['WRITER_FLAG'] = ((hcp_data['XTAMPZA_ER_Q4_TRx'].fillna(0)+hcp_data['XTAMPZA_ER_Q3_TRx'].fillna(0)+hcp_data['XTAMPZA_ER_Q2_TRx'].fillna(0)+
		hcp_data['XTAMPZA_ER_Q1_TRx'].fillna(0))>0).astype(int)
	hcp_data['CALLED_FLAG'] = ((hcp_data['Q1_calls'].fillna(0)+hcp_data['Q2_calls'].fillna(0)+hcp_data['Q3_calls'].fillna(0)+
		hcp_data['Q4_calls'].fillna(0))>0).astype(int)

	# #### Adding Total Calls
	hcp_data['CALLS_TOT_Qs'] = (hcp_data['Q1_calls'].fillna(0)+hcp_data['Q2_calls'].fillna(0)+hcp_data['Q3_calls'].fillna(0)+
		hcp_data['Q4_calls'].fillna(0))

	# #### Adding % growth rate features(10/09/2017)
	hcp_data['PERC_ER_BRANDED_GROWTH_Q4'] = (hcp_data['ER_BRANDED_Q4_TRx'] - hcp_data['ER_BRANDED_Q3_TRx'])/hcp_data['ER_BRANDED_Q3_TRx']
	hcp_data['PERC_ER_BRANDED_GROWTH_Q3'] = (hcp_data['ER_BRANDED_Q3_TRx'] - hcp_data['ER_BRANDED_Q2_TRx'])/hcp_data['ER_BRANDED_Q2_TRx']
	hcp_data['PERC_ER_BRANDED_GROWTH_Q2'] = (hcp_data['ER_BRANDED_Q2_TRx'] - hcp_data['ER_BRANDED_Q1_TRx'])/hcp_data['ER_BRANDED_Q1_TRx']

	over_all_opioid_Q4 = hcp_data['ER_BRANDED_Q4_TRx'] + hcp_data['ER_GENERIC_Q4_TRx'] + hcp_data['IR_GENERIC_Q4_TRx']
	over_all_opioid_Q3 = hcp_data['ER_BRANDED_Q3_TRx'] + hcp_data['ER_GENERIC_Q3_TRx'] + hcp_data['IR_GENERIC_Q3_TRx']
	over_all_opioid_Q2 = hcp_data['ER_BRANDED_Q2_TRx'] + hcp_data['ER_GENERIC_Q2_TRx'] + hcp_data['IR_GENERIC_Q2_TRx']
	over_all_opioid_Q1 = hcp_data['ER_BRANDED_Q1_TRx'] + hcp_data['ER_GENERIC_Q1_TRx'] + hcp_data['IR_GENERIC_Q1_TRx']
	hcp_data['PERC_OPIOID_GROWTH_Q4'] = (over_all_opioid_Q4 - over_all_opioid_Q3)/over_all_opioid_Q3
	hcp_data['PERC_OPIOID_GROWTH_Q3'] = (over_all_opioid_Q3 - over_all_opioid_Q2)/over_all_opioid_Q2
	hcp_data['PERC_OPIOID_GROWTH_Q2'] = (over_all_opioid_Q2 - over_all_opioid_Q1)/over_all_opioid_Q1

	# #### Add % NBRx Variable for NBRx Coverage (10/10/17)
	NBRx_col = ['XTAMPZA_ER_Q1_NBRX', 'XTAMPZA_ER_Q2_NBRX' ,'XTAMPZA_ER_Q3_NBRX', 'XTAMPZA_ER_Q4_NBRX']
	hcp_data['PERC_NBRx_Q4'] = hcp_data['XTAMPZA_ER_Q4_NBRX'].fillna(0)/hcp_data['XTAMPZA_ER_Q4_NBRX'].sum()
	hcp_data['PERC_NBRx_ALL'] = hcp_data[NBRx_col].fillna(0).sum(axis=1)/np.sum(hcp_data[NBRx_col].sum(axis=1))

	# #### Melt data on quater level
	calls = (melt(hcp_data, col=['index','Q1_calls', 'Q2_calls', 'Q3_calls', 'Q4_calls'], var_name='Calls'))
	copay = (melt(hcp_data, col=['index','Q1_copay_cards','Q2_copay_cards','Q3_copay_cards','Q4_copay_cards'], var_name='Copay_cards'))
	TRx = (melt(hcp_data, col=['index','XTAMPZA_ER_Q1_TRx','XTAMPZA_ER_Q2_TRx','XTAMPZA_ER_Q3_TRx','XTAMPZA_ER_Q4_TRx'], 
		var_name='XTAMPZA_TRx'))
	NBRx = (melt(hcp_data, col=['index','XTAMPZA_ER_Q1_NBRX','XTAMPZA_ER_Q2_NBRX','XTAMPZA_ER_Q3_NBRX','XTAMPZA_ER_Q4_NBRX'], 
		var_name='XTAMPZA_NBRx'))
	ER_BR_NBRx = (melt(hcp_data, col=['index','ER_BRANDED_Q2_NBRX','ER_BRANDED_Q1_NBRX', 'ER_BRANDED_Q3_NBRX', 'ER_BRANDED_Q4_NBRX'], 
		var_name='ER_BRANDED_NBRx'))
	ptp_prog = melt(hcp_data, col=['index','Number_programs_attended_Q1','Number_programs_attended_Q2',
		'Number_programs_attended_Q3','Number_programs_attended_Q4'], var_name = 'PTP_Program_Numbers')

	melted_data_feat = pd.concat([calls, copay.drop(['index', 'Quater'], axis=1), 
		TRx.drop(['index','Quater'], axis=1),
		ptp_prog.drop(['index','Quater'], axis=1),
		NBRx.drop(['index','Quater'], axis=1),
		ER_BR_NBRx.drop(['index','Quater'], axis=1)]
		,axis=1)
	melted_data = pd.merge(melted_data_feat, hcp_data, on='index', how='left') #.drop(melted_cols, axis=1)

	# ### Derived Features
	ER_branded = ['ER_BRANDED_Q1_TRx','ER_BRANDED_Q2_TRx','ER_BRANDED_Q3_TRx','ER_BRANDED_Q4_TRx']
	ER_generic = ['ER_GENERIC_Q1_TRx', 'ER_GENERIC_Q2_TRx', 'ER_GENERIC_Q3_TRx', 'ER_GENERIC_Q4_TRx']
	IR_generic = ['IR_GENERIC_Q1_TRx','IR_GENERIC_Q2_TRx' , 'IR_GENERIC_Q3_TRx', 'IR_GENERIC_Q4_TRx']
	target = 'XTAMPZA_TRx'#['XTAMPZA_ER_Q4_TRx', 'XTAMPZA_ER_Q3_TRx', 'XTAMPZA_ER_Q2_TRx', 'XTAMPZA_ER_Q1_TRx']
	calls = 'Calls'#['Q4_calls','Q3_calls','Q2_calls','Q1_calls']
	switch = ['Switch_ER_Brand_to_ER_Brand', 'Switch_ER_Generic_to_ER_Brand', 'Switch_IR_Generic_to_ER_Brand']
	add_on = ['Add_ons_ER_Brand_to_ER_Brand', 'Add_ons_ER_Generic_to_ER_Brand', 'Add_ons_IR_Generic_to_ER_Brand']

	# #### ER to IR Ratios(Repeated for the melted data)
	ratios = pd.DataFrame()
	for i in range(4):
		#print (ER_branded[i], ER_generic[i], IR_generic[i])
		ratios['PERC_ER_BRAND_GEN_TRX_Q'+str(i+1)] = melted_data[[ER_branded[i], ER_generic[i], IR_generic[i]]].apply(do_er_br_gen_perc, axis=1)
		ratios['PERC_ER_GEN_TRX_Q'+str(i+1)] = melted_data[[ER_branded[i], ER_generic[i], IR_generic[i]]].apply(do_er_gen_perc, axis=1)

	# #### % Switches and Add-on
	sw_add = pd.DataFrame()
	sw_add[switch]=melted_data[switch]
	sw_add[add_on]=melted_data[add_on]
	sw_add['Rx_SW_ADD'] = melted_data['SWITCH_ADD_ON_Rx']
	sw_add['TOTAL_SWITCHES_TO_ER_BRAND'] = np.sum(sw_add[switch], axis=1)
	sw_add['TOTAL_ADD_ON_ER_BRAND'] = np.sum(sw_add[add_on], axis=1)
	sw_add['TOTAL_SWITCHES_ADD_ON_TO_ER_BRAND'] = np.sum(sw_add, axis=1)
	sw_add['PERC_SWITCH_IR_Generic_to_ER_Brand'] = sw_add['Switch_IR_Generic_to_ER_Brand']/sw_add['TOTAL_SWITCHES_TO_ER_BRAND']
	sw_add['PERC_SWITCH_Add_ons_ER_Brand_to_ER_Brand']=(sw_add['Switch_ER_Brand_to_ER_Brand']+sw_add['Add_ons_ER_Brand_to_ER_Brand'])/sw_add['TOTAL_SWITCHES_ADD_ON_TO_ER_BRAND']
	sw_add['PERC_SWITCH_Add_ons_ER_Generic_to_ER_Brand']=(sw_add['Switch_ER_Generic_to_ER_Brand']+sw_add['Add_ons_IR_Generic_to_ER_Brand'])/sw_add['TOTAL_SWITCHES_ADD_ON_TO_ER_BRAND']
	sw_add['PERC_SWITCH_Add_ons_IR_Generic_to_ER_Brand']=(sw_add['Switch_IR_Generic_to_ER_Brand']+sw_add['Add_ons_IR_Generic_to_ER_Brand'])/sw_add['TOTAL_SWITCHES_ADD_ON_TO_ER_BRAND']
	sw_add['PERC_SWITCH_Add_ons_ER_Brand_to_ER_Brand_updated'] = sw_add['Rx_SW_ADD']/(sw_add['Switch_ER_Generic_to_ER_Brand']+ sw_add['Switch_IR_Generic_to_ER_Brand']+sw_add['Add_ons_ER_Generic_to_ER_Brand']+ sw_add['Add_ons_IR_Generic_to_ER_Brand']+sw_add['Rx_SW_ADD'])
	sw_add= sw_add.drop(switch, axis=1)
	sw_add= sw_add.drop(add_on, axis=1)
	sw_add=sw_add.drop('Rx_SW_ADD', axis=1)

	# #### Create Exhaustive dataset
	melted_data_derv = pd.concat([melted_data,ratios,sw_add], axis=1)
	#melted_data_derv.to_csv('../01_Data/0920_MasterData/HCP_Derived_feat_melted.csv', index=False)

	# #### Normalize Demo Data
	melted_data_derv['Population_Black_Norm'] = melted_data_derv['Population_Black']/melted_data_derv['Population']
	melted_data_derv['Population_White_Norm'] = melted_data_derv['Population_White']/melted_data_derv['Population']
	melted_data_derv['X25_HS_Graduate_Norm'] = melted_data_derv['X25_HS_Graduate']/melted_data_derv['Population']
	melted_data_derv['Total_Crime_Norm'] = melted_data_derv['Total_Crime']/melted_data_derv['Population']

	# #### Adding Ratio of Total Switches Addons to TRx
	melted_data_derv['SWITCH_ADD_ON_RAT_TRx'] = (melted_data_derv['TOTAL_SWITCHES_ADD_ON_TO_ER_BRAND']/
		(melted_data_derv['ER_BRANDED_Q4_TRx'] + melted_data_derv['ER_BRANDED_Q3_TRx']))
	# Distribution tells us that this variable is not making sense as the ratio goes over 1.


	# #### Create Input Data
	col = ['Quater', 'Calls','CALLS_TOT_Qs','Q4_calls', 'Copay_cards', 'XTAMPZA_TRx',
			'PTP_Program_Numbers', 'XTAMPZA_NBRx', 'ER_BRANDED_NBRx',
			'IMS_ID', 'NPI', 'ER_BRANDED_Q2_AO', 'ER_BRANDED_Q1_AO',
			'PERC_SWITCH_Add_ons_ER_Brand_to_ER_Brand_updated',
			'PERC_NBRx_Q4', 'PERC_NBRx_ALL', 'OXYCONTIN_share_ER_Brand_Q4',

			'XTAMPZA_ER_Q1_TRx', 'XTAMPZA_ER_Q2_TRx', 'XTAMPZA_ER_Q3_TRx',
			'XTAMPZA_ER_Q4_TRx',
			'ER_BRANDED_Q4_TRx',
			'ER_Branded_Decile',
			'Specialty', 'CGM_Specialty_Group', 'Region_Name', 'Area_Name',

			'Percent_Commercial_Rx', 'Percent_Medicare_Rx',
			'Percent_State_Medicaid_Rx', 'Percent_Managed_Medicaid_Rx',
			'PTP_Programs_Attended', 'PTP_Speaker_Flag', 

			'PDRP_Flag_Q22017', 'Seg_Q32017', 'Target_Q32017',

			'Decile_for_product_group', 'Branded_ER_NBRx_deciles',
			'Decile_C12_Xtampza', 'Decile_C12_Nucynta',
			'Decile_C12_Branded_ER_Market', 'Population', 'Population_White',
			'Population_Black', 'X25_HS_Graduate', 'Unemployment_Rate',
			'Median_Age', 'Median_Household_Income', 'Quality_of_Life_Score',
			'Economic_Score', 'Health_Score', 'Total_Crime',

			'Deaths_2014', 'Deaths_2015','Terr_Target_Q32016', 'Terr_Target_Q42016',
			'Terr_Target_Q12017', 'Terr_Target_Q22017', 'Terr_Target_Q32017',
			'Q2_Target_Seg_A', 'Q2_Target_Seg_B', 'Q2_Target_Seg_C',
			'AM_No_See_Rating', 'HCPs_w_Rating', 'Terr_Access',
			'Terr_Target_Access_Q22017', 'Years_of_practice', 'WRITER_FLAG',
			'CALLED_FLAG', 'PERC_ER_BRANDED_GROWTH_Q4',
			'PERC_ER_BRANDED_GROWTH_Q3', 'PERC_ER_BRANDED_GROWTH_Q2',
			'PERC_OPIOID_GROWTH_Q4', 'PERC_OPIOID_GROWTH_Q3',
			'PERC_OPIOID_GROWTH_Q2', 'PERC_ER_BRAND_GEN_TRX_Q1',
			'PERC_ER_GEN_TRX_Q1', 'PERC_ER_BRAND_GEN_TRX_Q2',
			'PERC_ER_GEN_TRX_Q2', 'PERC_ER_BRAND_GEN_TRX_Q3',
			'PERC_ER_GEN_TRX_Q3', 'PERC_ER_BRAND_GEN_TRX_Q4',
			'PERC_ER_GEN_TRX_Q4', 'TOTAL_SWITCHES_TO_ER_BRAND',
			'TOTAL_ADD_ON_ER_BRAND', 'TOTAL_SWITCHES_ADD_ON_TO_ER_BRAND',
			'PERC_SWITCH_IR_Generic_to_ER_Brand',
			'PERC_SWITCH_Add_ons_ER_Brand_to_ER_Brand',
			'PERC_SWITCH_Add_ons_ER_Generic_to_ER_Brand',
			'PERC_SWITCH_Add_ons_IR_Generic_to_ER_Brand',
			'Population_Black_Norm', 'Population_White_Norm',
			'X25_HS_Graduate_Norm', 'Total_Crime_Norm', 'SWITCH_ADD_ON_RAT_TRx',
			'Terr_Target_Q32016',
			'Terr_Target_Q42016', 'Terr_Target_Q12017', 'Terr_Target_Q22017',
			'Terr_Target_Q32017', 'Q2_Target_Seg_A', 'Q2_Target_Seg_B',
			'Q2_Target_Seg_C', 'AM_No_See_Rating', 'HCPs_w_Rating',
			'Terr_Access', 'Terr_Target_Access_Q22017', 'Years_of_practice',
			'AETNA_INC_Commercial_perc', 'AETNA.INC.Managed.Medicaid_perc',
			'AETNA.INC.Medicare_perc',
			'AETNA.PHARMACY.MGT.UNSPEC.Commercial_perc',
			'CIGNA.Commercial_perc', 'CIGNA.Managed.Medicaid_perc',
			'CIGNA.Medicare_perc', 'HUMANA.Commercial_perc',
			'HUMANA.Managed.Medicaid_perc', 'HUMANA.Medicare_perc',
			'UHC.PACIFICARE.AARP.MED.D.Medicare_perc',
			'UNITED.HEALTHCARE.Commercial_perc',
			'UNITED.HEALTHCARE.Managed.Medicaid_perc']

	data_feed_melted = melted_data_derv[col]
	data_feed_melted.to_csv('../01_Data/0928_data_feed/data_feed_melted.csv', index=False)
	print ('Melted data shape : ', data_feed_melted.shape)

	print ('Preparing data..')
	id_type = {'IMS_ID' : str}
	data_feed = pd.read_csv('../01_Data/0928_data_feed/data_feed_melted.csv', dtype=id_type)

	# #### Handling Missing Values
	obj_col = ['ER_Branded_Decile', 'Seg_Q32017','Branded_ER_NBRx_deciles']
	data_feed['ER_Branded_Decile'] = (data_feed['ER_Branded_Decile'].astype(object))
	data_feed['Seg_Q32017'] = (data_feed['Seg_Q32017'].astype(object))
	data_feed['Branded_ER_NBRx_deciles'] = (data_feed['Branded_ER_NBRx_deciles'].astype(object))
	demo_col  = ['Unemployment_Rate','Median_Age','Median_Household_Income','Quality_of_Life_Score','Economic_Score','Health_Score', 'Deaths_2015', 'Deaths_2014']
	data_feed[demo_col] = data_feed[demo_col].apply(fill_medium, axis=0)

	for ind in data_feed.columns:
		if data_feed[ind].dtypes == 'O':
			data_feed[ind].fillna(value ="unk", inplace=True)
		else :
			data_feed[ind].fillna(value=0, inplace=True)

	# #### Adding Lags term TRx
	data_feed_lag = data_feed
	data_feed_lag = add_lag(data_feed_lag, col='XTAMPZA_TRx')
	data_feed_lag = add_lag(data_feed_lag, col='Copay_cards')

	# #### Adding Growth for regression
	data_feed_lag['XTAMPZA_TRx_Growth'] = ((data_feed_lag['XTAMPZA_TRx'].fillna(0) - data_feed_lag['XTAMPZA_TRx_Lag'].fillna(0))/data_feed_lag['XTAMPZA_TRx_Lag'].fillna(0)).fillna(0)

	# #### Bucket Speciality features
	data_feed_lag['CGM_Specialty_Group_Bkt'] = data_feed_lag['CGM_Specialty_Group'].apply(to_spcl_bkt)
	data_feed_lag['CGM_Specialty_Group_Bkt_4cat'] = data_feed_lag['CGM_Specialty_Group'].apply(to_spcl_bkt)

	# #### Quantizing Deciles Features
	data_feed_lag['ER_Branded_Decile_Bkt'] = data_feed_lag['ER_Branded_Decile'].apply(to_group)
	data_feed_lag['Branded_ER_NBRx_deciles_Bkt'] = data_feed_lag['Branded_ER_NBRx_deciles'].apply(to_group)
	data_feed_lag['ER_Branded_Decile_Int'] = data_feed_lag['ER_Branded_Decile'].apply(to_int)
	data_feed_lag['Branded_ER_NBRx_deciles_Int'] = data_feed_lag['Branded_ER_NBRx_deciles'].apply(to_int)

	# #### Addin pre-trials calls data
	pr_call = pd.read_csv('../01_Data/0920_MasterData/Copy of calls before initiation.csv', dtype=id_type)
	pr_call['IMSID'] = pr_call['IMSID'].apply(lambda x: '{0:0>7}'.format(x))
	data_feed_lag['PRE_TRIAL_CALLS'] = data_feed_lag.merge(pr_call, how='left', left_on='IMS_ID', right_on='IMSID')['CALLS BEFORE INITIATION']
	data_feed_lag['PRE_TRIAL_CALLS'] = data_feed_lag['PRE_TRIAL_CALLS'].fillna(99).describe()

	# Saving data set
	data_feed_lag.to_csv('../01_Data/0928_data_feed/data_feed_lag_melted.csv', index=False)
	## For Q2 to Q4
	data_feed_lag[data_feed_lag['Quater']!='Q1'].to_csv('../01_Data/0928_data_feed/data_feed_lag_melted_Q1_RM.csv', index=False)


	# #### Taking only with Xtampza TRx Lag (>2)
	data_XT = data_feed_lag[data_feed_lag['XTAMPZA_TRx_Lag']>=2]
	data_XT['WRITER_FLAG'] = (data_XT['XTAMPZA_TRx']>0).astype(int)
	data_XT['WRITER_FLAG'].value_counts()
	data_XT.to_csv('../01_Data/0928_data_feed/data_feed_XT_LAG_2.csv', index=False)

	# #### Undersampling  wrt Writers
	pos_ID = np.unique(data_feed_lag[data_feed['WRITER_FLAG']==1]['IMS_ID']).tolist()
	neg_ID_all = np.unique(data_feed_lag[data_feed['WRITER_FLAG']==0]['IMS_ID']).tolist()
	neg_ID_all = list(set(neg_ID_all) - set(pos_ID))
	np.random.seed(123)
	data_feed_pos = data_feed_lag[data_feed_lag['IMS_ID'].isin(pos_ID)]
	neg_all_data = data_feed_lag[data_feed_lag['IMS_ID'].isin(neg_ID_all)]
	# Sample on the basis of MDs with higher Impact on market
	neg_ID = neg_all_data.sort_values(by='ER_BRANDED_Q4_TRx', ascending=False)['IMS_ID'].unique()[:len(pos_ID)]
	#neg_ID = np.random.choice(neg_ID_all, size=len(pos_ID), replace=False)
	data_feed_neg = data_feed_lag[data_feed_lag['IMS_ID'].isin(neg_ID)]
	data_feed_lag_und = pd.concat([data_feed_pos, data_feed_neg], axis=0)
	# saving undersampled data
	data_feed_lag_und.to_csv('../01_Data/0928_data_feed/data_feed_lag_und_melted.csv', index=False)

	## For Q2 to Q4
	data_feed_lag_und[data_feed_lag['Quater']!='Q1'].to_csv('../01_Data/0928_data_feed/data_feed_lag_und_melted_Q1_RM.csv', index=False)
	os.chdir('D:/Anaconda3/projects/')
