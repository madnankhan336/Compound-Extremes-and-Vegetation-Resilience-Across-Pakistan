from pathlib import Path
import pandas as pd,json,hashlib,argparse
parser=argparse.ArgumentParser()
parser.add_argument('--root',type=Path,default=Path(__file__).resolve().parents[1])
parser.add_argument('--out',type=Path,required=True)
args=parser.parse_args()
B=args.root;W=args.out;W.mkdir(parents=True,exist_ok=True)
P=B/'run_outputs/09_R1C2_Primary_and_Sensitivity'
tr=pd.read_csv(P/'tables/Exposure_trend_comparison.csv');t=tr[tr.elev_band.eq(0)&tr.metric.eq('hd_area_fraction_complete')].copy()
period={'H04':'1985-2004','H14':'1985-2014','FULL':'1985-2025'}
t['reference_years']=t.climate_reference.map(period)
for col in ['ols_per_year','hac_lower95','hac_upper95','theil_sen_per_year']:t[col+'_percentage_points']=100*t[col]
t=t.set_index('climate_reference').loc[['H04','H14','FULL']].reset_index();t.to_csv(W/'Table_S40_Climate_baseline_trends.csv',index=False,na_rep='NA')
rfile=next((P/'frameworks/PRIMARY_H14_ORIGINAL').rglob('Recovery_censoring_by_elevation.csv'));r=pd.read_csv(rfile);r=r[r.response_lag.eq(3)&r.cohort.eq('all_suppressed')].copy()
reason_cols=['n_censored_window_12_months','n_censored_next_HD','n_censored_dataset_end','n_censored_missing_NDVI','n_censored_unknown_climate']
assert (r[reason_cols].sum(axis=1)==r.n_censored).all()
assert (r.n_recovered+r.n_censored==r.n_eligible).all()
for c in ['n_censored']+reason_cols:r[c+'_percent_of_eligible']=100*r[c]/r.n_eligible
r.to_csv(W/'Table_S41_Nonobserved_recovery_by_elevation.csv',index=False,na_rep='NA')
a=pd.read_csv(B/'tables/Historical_contrast_by_elevation.csv');a.to_csv(W/'Historical_contrast_by_elevation.csv',index=False)
h=pd.read_csv(B/'tables/Historical_RF_antecedent_holdout.csv');h.to_csv(W/'Historical_RF_antecedent_holdout.csv',index=False)
sourcepaths=[P/'tables/Exposure_trend_comparison.csv',rfile,B/'tables/Historical_contrast_by_elevation.csv',B/'tables/Historical_RF_antecedent_holdout.csv']
(W/'Evidence_sources.json').write_text(json.dumps({str(p):{'sha256':hashlib.sha256(p.read_bytes()).hexdigest(),'bytes':p.stat().st_size}for p in sourcepaths},indent=2),encoding='utf-8')
print(t[['reference_years','ols_per_year_percentage_points','hac_lower95_percentage_points','hac_upper95_percentage_points','hac_p']].to_string(index=False))
print(r[['elevation','n_eligible','n_censored','n_censored_percent_of_eligible','n_censored_window_12_months']].to_string(index=False))
