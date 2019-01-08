## Historical data set:
Missing data: Not all of the clients have data for all the months
NAs: there are some NAs in the price columns

Findings:
### 
- when price_p*_fix is close to 0, churn seems much less
- more num_years_antig less churn
- date_modif_prod more recent, more churn
- date_active more recent, more churn
- date_renewal more recent, more churn
- one specific channel_sales (4) might have impact on more churn (to find out)
- one specific activity_new (200-300) might have impact on more churn (to find out)
- Correlation: 'cons_last_month', 'cons_12m'
- Correlation: 'forecast_cons_12m', 'cons_12m'

- More 'cons_12m', less churns 

- Correlation: 'date_activ', 'date_first_active'
- Correlation: 'date_activ', 'num_years_antig'

- Correlation: 'forecast_base_bill_ele', 'forecast_base_bill_year'
- Correlation: 'forecast_base_bill_ele', 'forecast_cons'

- Correlation: 'forecast_base_bill_ele', 'forecast_cons_year'
- Correlation: 'forecast_base_bill_ele', 'imp_cons'

- Correlation: 'forecast_cons_12m', 'net_margin'

- More 'pow_max', less churns 
- More 'forecast_discount_energy', less churns 
- Avg 'forecast_price_pow_1', more churns 
- has_gas might have positive posiitve impact on churn (if has_gas, less churn)
- More 'nb_prod_act', less churns 
- origin_up 5 6 has almost no churn
- Negative consumption
