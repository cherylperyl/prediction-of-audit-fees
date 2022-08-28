# Project

library(readr)
library(dplyr)

# FILLING IN OF MISSING VALUES (going down variable by variable)

# 0) Read the dataset and view it if needed
data = read_csv("Team Project.csv")
View(data)

# 1) cid (company identifier)
summary(data$cid) # no NAs
data %>% group_by(cid) %>% count() %>% nrow() # total 5504 companies

# 2) datadate (fiscal year-end date)
summary(data$datadate) # no NAs, no -ve

# 3) fyear (fiscal year)
summary(data$fyear) # no NAs, no -ve

# 4) fyr (fiscal month)
summary(data$fyr) # no NAs, no -ve

# 5) prcc_f (Price Close - Annual - Fiscal Year)
summary(data$prcc_f) # 2421 NAs, no -ve

# Treatment: Replace NA values in prcc_f with company average 
# Assumption: Companies are not delisted and that pprices will fluctuate around its average
data = data %>% 
  group_by(cid) %>% 
  mutate(prcc_f_avg = mean(prcc_f, na.rm=T)) %>% 
  mutate(prcc_f = ifelse(is.na(prcc_f), prcc_f_avg, prcc_f))
summary(data$prcc_f)# distribution of data after replacement, still have 1356 NAs and no -ve
# will not replace these NA values as it does makes sense to replace NA values with industry average

# 6) ffi12_desc
sum(is.na(data$FFI12_desc)) # no NAs
library(caret)
unique(data$FFI12_desc)
FFI12_desc_list = dummyVars("~FFI12_desc",data=data)
FFI12_dummy = data.frame(predict(FFI12_desc_list,data))
data = bind_cols(data,FFI12_dummy)
str(data)
View(data)
summary(data) #no NA for dummy variables

# 7) mv_crsp (market value of shareholders' equity -> market cap)
summary(data$mv_crsp) # 13381 NAs
#could potentially explore market cap as a proxy for size/complexity but too many NAs...

# Treatment: Replace with company avg and avg mv_crsp of similar sized companies in the same industry
# Assumption: Similar assets used within the industry, hence, serving as a proxy for firm size
# We use total assets(at) as the proxy variable to evaluate a company's size
data = data %>% 
  group_by(cid) %>% 
  mutate(avg_mv_crsp_by_company = mean(mv_crsp, na.rm=T))%>% 
  mutate(mv_crsp = ifelse(is.na(mv_crsp), avg_mv_crsp_by_company, mv_crsp))
summary(data$mv_crsp) # 11617 NAs left

plot(density(data$at))                            # initial distribution of at
data = data %>% mutate(log_at = log(at))          # data is very skewed so let us use log
plot(density(data$log_at))                        # new distribution of at
data = data %>% 
  mutate(at_bins=ntile(log_at,10)) %>%            # create 10bins
  group_by(at_bins) %>%                           # assign each observation to a rank based on total assets
  mutate(avg_mv_crsp_by_bin = mean(mv_crsp, na.rm=T)) %>% # get mv_crsp average for each of the 10bins
  mutate(mv_crsp = ifelse(is.na(mv_crsp), avg_mv_crsp_by_bin, mv_crsp))
summary(data$mv_crsp) # all NAs removed, no -ve

# 8) d_fortune(a dummy variable for the Fortune 1000 list)
summary(data$d_fortune) # 35100 NAs

# Treatment: replace NA in d_fortune to 0
# Assumption: NA means not on fortune 1000 list
data = data %>% mutate(d_fortune = ifelse(is.na(d_fortune), 0, d_fortune))
summary(data$d_fortune) # all NAs removed

# 9) busdesc(a brief description about the company's business activities)
sum(is.na(data$busdesc)) # 19 NAs

# Treatment: Just replace with empty string
data = data %>% mutate(busdesc = ifelse(is.na(busdesc),"",busdesc))
sum(is.na(data$busdesc)) # all NAs removed

# 10) e_score (environmental score (minimum = 0, maximum = 100))
summary(data$e_score) # 29995 NAs, no -ve

# 11) act (Current assets)
summary(data$act) # 7578 NAs, min 0

# Treatment: use industry act/at ratio to replace missing values
data = data %>% mutate(act_at = act/at)
data = data %>% 
  group_by(FFI12_desc) %>% 
  mutate(avg_act_at_by_industry = mean(act_at, na.rm=T)) %>% 
  mutate(act = ifelse(is.na(act), avg_act_at_by_industry*at, act))
summary(data$act) # all NAs removed, min 0 

# 12) at (Total assets)
summary(data$at) # 0 NAs , min 0 
# have 0 values but likely to be a numerator so no need to treat -> KIV

# 13) lt (Total liabilities)
summary(data$lt) # 48 NAs
# Treatment: Replace with past company data if available
data = data %>%
  group_by(cid) %>%
  mutate(avg_lt_by_company = mean(lt, na.rm=T)) %>%
  mutate(lt = ifelse(is.na(lt), avg_lt_by_company, lt))
summary(data$lt) # all NAs removed, min = 0 -> take note

# 14) capx (Capital Expenditures)
summary(data$capx) # 334 NAs with negative values

# Treatment: replace with company avg and avg capx of companies in similar industry and of similar size (based on at)
# Assumption: Similar companies sized companies would have similar CapEx and it is specific to industry
data = data %>% 
  group_by(cid) %>% 
  mutate(avg_capx_per_company = mean(capx, na.rm=T)) %>% 
  mutate(capx = ifelse(is.na(capx), avg_capx_per_company, capx))
summary(data$capx) # 140 NAs left

data = data %>% 
  group_by(FFI12_desc, at_bins) %>%
  mutate(avg_capx_by_industry_and_size = mean(capx, na.rm=T)) %>% 
  mutate(capx = ifelse((is.na(capx)) | (capx < 0), avg_capx_by_industry_and_size, capx))
summary(data$act) # all NAs and -ve values removed

# 15) ceq (Common/Ordinary Equity)
summary(data$ceq) # 67 NAs

# Treatment: Use the formula at = lt + ceq to fill in NAs
data = data %>% mutate(ceq = at-lt)
summary(data$ceq) # 0 NAs, -ve values indicate disstressed companies

# 16) che (Cash and Short-Term Investments)
summary(data$che) # 3 NAs
# Treatment: Just remove these rows once all data is cleaned

# 17) cogs (Cost of Goods Sold)
summary(data$cogs) # 28 NAs
sum(data$cogs < 0 & is.na(data$cogs)==FALSE) # 66 negative values
# Treatment: Just remove these rows once all data is cleaned

# 18) dlc (Current Liabilities (Debt), short term debt)
summary(data$dlc) # 16 NAs

# Treatment: Use past company average if available, otherwise industry avg dlc/lt ratio to calculate dlc 
# Assumption: Industry uses a relatively similar capital structure
data = data %>%
  group_by(cid) %>%
  mutate(avg_dlc_by_company = mean(dlc, na.rm=T)) %>%
  mutate(dlc = ifelse(is.na(dlc), avg_dlc_by_company, dlc))
summary(data$dlc) # 7 NAs removed

data = data %>% 
  mutate(dlc_lt = dlc/lt) %>%
  group_by(FFI12_desc) %>%
  mutate(avg_dlc_lt = mean(dlc_lt, na.rm=T)) %>%
  mutate(dlc = ifelse(is.na(dlc), avg_dlc_lt*lt, dlc))
summary(data$dlc) # no NA, but have 0 values -> take note

# 19) dltt (Long-Term Debt)
summary(data$dltt) # 117 NAs

# Treatment: replace with company historical average
# Assumption: Similar capital structures across time
data = data %>% 
  group_by(cid) %>% 
  mutate(avg_dltt_per_company = mean(dltt, na.rm=T)) %>% 
  mutate(dltt = ifelse(is.na(dltt), avg_dltt_per_company, dltt))
summary(data$dltt) # no NAs

# 20) dp (Depreciation and Amortization)
summary(data$dp) # 1740 NAs
sum(data$dp < 0 & is.na(data$dp)==FALSE) # 10 negative values
# Treatment: Nothing, not used in analysis

# 21) dvc (Dividends for Common/Ordinary Equity)
summary(data$dvc) # 348 NAs
sum(data$dvc < 0 & is.na(data$dvc)==FALSE) # 4 negative values

# Treatment: replace NA and negative values with 0
# Assumption: no dividends declared if NA and negative dividends as errors since != <0
data = data %>% mutate(dvc = ifelse(is.na(dvc) | dvc < 0, 0, dvc))
summary(data$dvc) # all NAs removed

# 22) sale (net sales)
summary(data$sale) # 29 NAs
sum(data$sale < 0 & is.na(data$sale)==FALSE) # 61 negative values

# Treatment: replace with company average
data = data %>% 
  group_by(cid) %>% 
  mutate(avg_sale_per_company = mean(sale, na.rm=T)) %>% 
  mutate(sale = ifelse(is.na(sale), avg_sale_per_company, sale))

summary(data$sale) # all NAs removed, some companies are experiencing losses

# 23) emp (The number of employees)
summary(data$emp) # 5376 NAs

# Treatment: replace with company average first, then replace with industry average for similar sized companies (based on sales value)
data = data %>% 
  group_by(cid) %>% 
  mutate(avg_emp_per_company = mean(emp, na.rm=T)) %>% 
  mutate(emp = ifelse(is.na(emp), avg_emp_per_company, emp))
summary(data$emp) # 3779 NAs left

# (using net sales(sale) to estimate the size of the company in terms of manpower
# then replace NA values with emp size of similar sized companies based on net sales
summary(data$sale)
plot(density(data$sale)) # initial distribution of sale
data = data %>% mutate(log_sale = log(sale)) # data is very skewed so let us use log
data = data %>% # since log is undefined for values less then or equals to zero, there will be NAs. So we replace with the company average
  group_by(cid) %>% 
  mutate(avg_log_sale_per_company = mean(log_sale, na.rm=T)) %>% 
  mutate(log_sale = ifelse(is.na(log_sale) | log_sale == -Inf, avg_log_sale_per_company, log_sale))
plot(density(data$log_sale)) # new distribution of transformed sale
data = data %>% 
  mutate(sale_bins=ntile(log_sale,10)) %>% 
  group_by(FFI12_desc, sale_bins) %>% 
  mutate(avg_emp_by_bin_and_industry = mean(emp, na.rm=T)) %>% 
  mutate(emp = ifelse(is.na(emp), avg_emp_by_bin_and_industry, emp)) # replace the NA values in mv_crsp with avg_mv_crsp_by_bin
summary(data$emp) # all NAs removed

# 24) gdwl (Goodwill)
summary(data$gdwl) # 425 NAs

# Treatment: replace NAs with 0
# Assumption: Companies with NA Goodwill have not acquired any
data = data %>% mutate(gdwl = ifelse(is.na(gdwl), 0, gdwl))
summary(data$gdwl) # all NAs removed

# 25) ib (Income Before Extraordinary Items)
summary(data$ib) # 27 NAs
# Treatment: No treatment, not used in analysis

# 26) intan (Intangible Assets)
summary(data$intan) # 493 NAs
# Treatment: Replace NA with 0
# Assumption: Assume if NA intangibles, company has no intangibles (i.e. nvr acquired or early stage)
data = data %>% mutate(intan = ifelse(is.na(intan), 0, intan))
summary(data$intan) # all NAs removed

# 27) intano (Other Intangibles)
summary(data$intano) # 927 NAs
# Treatment: No treatment, not used in analysis

# 28) invt (Inventories)
summary(data$invt) # 433 NAs

# Treatment: replace all NA with company average
data = data %>% 
  group_by(cid) %>% 
  mutate(avg_invt_per_company = mean(invt, na.rm=T)) %>% 
  mutate(invt = ifelse(is.na(invt), avg_invt_per_company, invt))
summary(data$invt) #233NA

#replace with industry average based on size of company (total assets as proxy)
data = data %>% 
  group_by(FFI12_desc, at_bins) %>% 
  mutate(avg_invt_by_bin_and_industry = mean(invt, na.rm=T)) %>% 
  mutate(invt = ifelse(is.na(invt), avg_invt_by_bin_and_industry, invt))
summary(data$invt) #No NA, min 0 -> take note

# Check that inventory figures do not exceed current assets, otherwise,
# assume that invt=act if invt>act to prevent skewed data
data_check = filter(data,invt>act) #205 obs with invt>act
data=data%>% mutate(invt=ifelse(act-invt<0, act, invt))
summary(data$invt)

# 29) lct (Current Liabilities)
summary(data$lct) # 7534 NAs

# Treatment: Use company average if available, otherwise, use industry average lct/lt ratio to calculate lct
data = data %>%
  group_by(cid) %>%
  mutate(avg_lct_by_company = mean(lct, na.rm=T)) %>%
  mutate(lct = ifelse(is.na(lct), avg_lct_by_company, lct))
summary(data$lct) # 7299 NAs

data = data %>% 
  mutate(lct_lt = lct/lt) %>%
  group_by(FFI12_desc) %>%
  mutate(avg_lct_lt = mean(lct_lt, na.rm=T)) %>%
  mutate(lct = ifelse(is.na(lct), avg_lct_lt*lt, lct))
summary(data$lct) # All NAs removed

# 30) ni (Net income)
summary(data$ni) # 27 NAs

# Treatment: replace with company average
data = data %>% 
  group_by(cid) %>% 
  mutate(avg_ni_per_company = mean(ni, na.rm=T)) %>% 
  mutate(ni = ifelse(is.na(ni), avg_ni_per_company, ni))
summary(data$ni) # all NAs removed, note some companies have negative net income

# 31) oancf (Net Cash Flow from Operating Activities)
summary(data$oancf) # 196 NAs
# Treatment: No treatment, not used in analysis

# 32) oiadp (Operating Income After Depreciation)
summary(data$oiadp) # 28 NAs

# Treatment: replace with company average
data = data %>% 
  group_by(cid) %>% 
  mutate(avg_oiadp_per_company = mean(oiadp, na.rm=T)) %>% 
  mutate(oiadp = ifelse(is.na(oiadp), avg_oiadp_per_company, oiadp))
summary(data$oiadp) # all NAs removed, some companies have negative oiadp

# 33) ppegt (Property Plant and Equipment (Gross))
summary(data$ppegt) # 5798 NAs

# Treatment: Replace with past company data if available, otherwise use industry avg based on size (at as proxy)
data = data %>% 
  group_by(cid) %>% 
  mutate(avg_ppegt_per_company = mean(ppegt, na.rm=T)) %>% 
  mutate(ppegt = ifelse(is.na(ppegt), avg_ppegt_per_company, ppegt))
summary(data$ppegt) #4881 NA

#Treatment: replace with industry average
data = data %>% 
  group_by(FFI12_desc, at_bins) %>% 
  mutate(avg_ppegt_by_bin_and_industry = mean(ppegt, na.rm=T)) %>% 
  mutate(ppegt = ifelse(is.na(ppegt), avg_ppegt_by_bin_and_industry, ppegt)) 
summary(data$ppegt) #0 NA, min = 0 -> take note 

# Check that ppegt figures do not exceed non-current assets, otherwise,
# assume that ppegt=nca if ppegt>nca to prevent skewed data
data = data%>% mutate(nca = at-act)
data_check = filter(data,ppegt>nca) #17265 observations with ppegt > nca
data=data%>% mutate(ppegt=ifelse(nca-ppegt<0, nca, ppegt))
summary(data$ppegt)

# 34) ppent (Property Plant and Equipment (Net))
summary(data$ppent) # 1237 NAs
# Treatment: No treatment, not used in analysis

# 35) re (Retained Earnings)
summary(data$re) # 1544 NAs

# Treatment: Replace with past company data if available, otherwise use industry avg based on re/ni ratio as a proxy
data = data %>% 
  group_by(cid) %>% 
  mutate(avg_re_per_company = mean(re, na.rm=T)) %>% 
  mutate(re = ifelse(is.na(re), avg_re_per_company, re))
summary(data$re) # 1005 NAs left

data = data %>% 
  mutate(re_ni = ifelse(re/ni == -Inf, NaN, re/ni)) %>% 
  group_by(FFI12_desc) %>% 
  mutate(avg_re_ni_per_industry = mean(re_ni, na.rm=T)) %>% 
  mutate(re = ifelse(is.na(re), ni*avg_re_ni_per_industry, re))
summary(data$re) # all NAs removed, some companies display negative RE (acc. deficit)

# 36) rect (Account Receivables)
summary(data$rect) # 487 NAs

# Treatment: replace with company average if available and avg rect of companies with similar size (at as a proxy) and industry
data = data %>% 
  group_by(cid) %>% 
  mutate(avg_rect_per_company = mean(rect, na.rm=T)) %>% 
  mutate(rect = ifelse(is.na(rect), avg_rect_per_company, rect))
summary(data$rect) # 210 NAs left

data = data %>% 
  group_by(FFI12_desc, at_bins) %>% 
  mutate(avg_rect_by_bin_and_industry = mean(rect, na.rm=T)) %>% 
  mutate(rect = ifelse(is.na(rect), avg_rect_by_bin_and_industry, rect)) 
summary(data$rect) # all NAs removed

# 37) sstk (Sale of Common and Preferred Stock)
summary(data$sstk) # 827 NAs

# Treatment: replace with 0
# Assumption: NA indicates no sale -> replace with 0
data = data %>% mutate(sstk = ifelse(is.na(sstk), 0, sstk))
summary(data$sstk) # all NAs removed

# 38) txdb (Deferred Taxes (Balance Sheet))
summary(data$txdb) # 1386 NAs

# Treatment: NA indicates no deferred tax -> replace with 0
data = data %>% mutate(txdb = ifelse(is.na(txdb), 0, txdb))
summary(data$txdb) # all NAs removed

# 39) txp (Income Taxes Payable)
summary(data$txp) # 4134 NAs

# Treatment: NA indicates tax exempt or loss making -> replace with 0
data = data %>% mutate(txp = ifelse(is.na(txp), 0, txp))
summary(data$txp) # all NAs removed

# 40) xad (Advertising Expense)
summary(data$xad) # 26134 NAs

# Treatment: replace with company average where available, remainder using industry avg by net sales as proxy
# Assumption: Assume industry avg by revenue assuming equal advertising efficiency
data = data %>% 
  group_by(cid) %>% 
  mutate(avg_xad_per_company = mean(xad, na.rm=T)) %>% 
  mutate(xad = ifelse(is.na(xad), avg_xad_per_company, xad))
summary(data$xad) # 24670 NAs left

data = data %>% 
  group_by(FFI12_desc, sale_bins) %>% 
  mutate(avg_xad_by_sale_bins_and_industry = mean(xad, na.rm=T)) %>% 
  mutate(xad = ifelse(is.na(xad), avg_xad_by_sale_bins_and_industry, xad))
summary(data$xad) #1453 NAs -> KIV

# 41) xlr (Staff Expense)
summary(data$xlr) # 26073 NAs

# Treatment: replace with company average and avg emp of companies with similar emp and industry
data = data %>% 
  group_by(cid) %>% 
  mutate(avg_xlr_per_company = mean(xlr, na.rm=T)) %>% 
  mutate(xlr = ifelse(is.na(xlr), avg_xlr_per_company, xlr))
summary(data$xlr) # 24892 NAs left

plot(density(data$emp)) # initial distribution of emp
data = data %>% mutate(log_emp = log(emp)) # data is very skewed so let us use log
plot(density(data$log_emp)) # new distribution of transformed sale
data = data %>% 
  mutate(emp_bins=ntile(log_emp,10)) %>%
  group_by(emp_bins, FFI12_desc) %>% 
  mutate(avg_xlr_by_bin_and_industry = mean(xlr, na.rm=T)) %>% 
  mutate(xlr = ifelse(is.na(xlr), avg_xlr_by_bin_and_industry, xlr))
summary(data$xlr) # all NAs removed

# 42) xpr (Pension and Retirement Expense)
summary(data$xpr) # 13838 NAs
# Treatment: No treatment, not used in analysis

# 43) xrd (Research and Development Expense)
summary(data$xrd) # 21256 NAs

# Treatment: Replace with company average where available, otherwise based on intangibles
# Assumption: Assumption that intangible value generation correlates with R&D expenditure, with
# acknowledgement that not all R&D expenses creates successful patents/assets -> limitation
data = data %>% 
  group_by(cid) %>% 
  mutate(avg_xrd_per_company = mean(xrd, na.rm=T)) %>% 
  mutate(xrd = ifelse(is.na(xrd), avg_xrd_per_company, xrd))
summary(data$xrd) # 20290 NAs left

plot(density(data$intan)) # initial distribution of intan
data = data %>% mutate(log_intan = log(intan)) # data is very skewed so let us use log
plot(density(data$log_intan)) # new distribution of transformed intangibles
data = data %>% 
  mutate(intan_bins=ntile(log_intan,10)) %>%
  group_by(intan_bins) %>%
  mutate(avg_xrd_by_intan_bin = mean(xrd, na.rm=T)) %>% 
  mutate(xrd = ifelse(is.na(xrd), avg_xrd_by_intan_bin, xrd))
summary(data$xrd) # all NAs removed

# 44) xsga (Selling General and Administrative Expense)
summary(data$xsga) # 7017 NAs

# Treatment: replace with company average and avg xsga of companies with similar size (based on sale) and industry
# Assumption: SG&A expenses correlate with sales 
data = data %>% 
  group_by(cid) %>% 
  mutate(avg_xsga_per_company = mean(xsga, na.rm=T)) %>% 
  mutate(xsga = ifelse(is.na(xsga), avg_xsga_per_company, xsga))
summary(data$xsga) # 5985 NAs left

data = data %>% 
  group_by(FFI12_desc, sale_bins) %>% 
  mutate(avg_xsga_by_bin_and_industry = mean(xsga, na.rm=T)) %>% 
  mutate(xsga = ifelse(is.na(xsga), avg_xsga_by_bin_and_industry, xsga))
summary(data$xsga) # all NAs removed

# 45) big4 (a dummy variable for big 4 auditors)
sum(is.na(data$BIG4)) # 6978 NAs

# Treatment: replace the NA with 0
# Assumption: If NA = not audited by Big 4 / not audited at all 
data = data%>% mutate(BIG4=ifelse(is.na(BIG4), 0, BIG4)) #If NA, means not BIG4 Auditor
summary(data$BIG4) #0 NA

# 46) auop - auditor opinion
summary(data$auop) # 145 NAs

# Treatment: replace NA with 0
# Assumption: If NA indicated that companies are unaudited
data = data %>% mutate(auop = ifelse(is.na(auop), 0, auop))
summary(data$auop) # all NAs removed

# 47) gc (a dummy variable for going concern opinion)
summary(data$gc) # 14529 NAs
# Treatment: No treatment, not used in analysis

# 48) icmw (a dummy variable for internal control weakness)
summary(data$icmw) # 15218 NAs
# Treatment: No treatment, not used in analysis

# 49) audit_fees (the dollar amount of audit fees)
sum(is.na(data$AUDIT_FEES)) # 14321 NAs
# Treatment: No treatment, as we intend to identify determinants of the available audit fees

# 50) non_audit_fees (the dollar amount of non-audit fees)
sum(is.na(data$NON_AUDIT_FEES)) # 14312 NAs
# Treatment: No treatment, not used in analysis

# 51) restate (a dummy variable for accounting restatements)
summary(data$restate) # 6842 NAs
# Treatment: No treatment, not used in analysis

# EXPORT CLEANED DATA
data = data %>% ungroup()
View(data)
data_cleaned = data %>% select(everything()) #38,200 obsv w 110 variables

# New Variables: Accounting Items & Miscellaneous Items
#total debt
data_cleaned = data_cleaned %>% mutate(td = dltt+dlc)
summary(data_cleaned$td) #no NAs

#total equity
data_cleaned = data_cleaned %>% mutate(te = ceq+re)
summary(data_cleaned$te) #no NAs

#Hypothesized factors affecting audit fees -> detailed explanation in slides
#Essentially, factors are chosen based on key determinants of audit fees based on an academic study

#Factor 1: Client Size
#Proxy: Number of Employees (emp) -> min 0, no NA, no INF

#Proxy: Total Assets (at) -> min 0, no NA, no INF

#Factor 2: Client's Profitability
#Proxy: Profit Margin (ni/sale) -> use net sales as revenue is not available
data_cleaned = data_cleaned %>% mutate(profit_margin = ni/sale)
summary(data_cleaned$profit_margin) #5 NAs, Max: Inf, Min: -Inf
#as the input would be used for regression, we remove all infinity (+/-) and NA values
data_cleaned = data_cleaned %>% filter(!is.infinite(profit_margin) & !is.na(profit_margin))
summary(data_cleaned$profit_margin) #no NAs no infinity -> removed 4,103 observations

#Factor 3: Client's Risk
#Proxy: Debt-to-Equity (td/te) -> td and te are new variables added above
data_cleaned = data_cleaned %>% mutate(der = td/te)
summary(data_cleaned$der)
#as the input would be used for regression, we remove all infinity (+/-) and NA values
data_cleaned = data_cleaned %>% filter(!is.infinite(der) & !is.na(der))
summary(data_cleaned$der) #no NAs no infinity -> removed 17 observations

#Proxy: Current Ratio (act/lct) 
data_cleaned = data_cleaned %>% mutate(cr = act/lct)
summary(data_cleaned$cr)
#as the input would be used for regression, we remove all infinity (+/-) and NA values
data_cleaned = data_cleaned %>% filter(!is.infinite(cr) & !is.na(cr))
summary(data_cleaned$cr) #no NAs no infinity -> removed 22 observations

#Factor 4: Client's Complexity
#Proxy: Inventory as a % of Assets
#Assumption: Audit complexity in checking inventory
data_cleaned = data_cleaned %>% mutate(pctg_invt_at=invt/at)
summary(data_cleaned$pctg_invt_at) #485 NA
#as the input would be used for regression, we remove all infinity (+/-) and NA values
data_cleaned = data_cleaned %>% filter(!is.infinite(pctg_invt_at) & !is.na(pctg_invt_at))
summary(data_cleaned$pctg_invt_at) #no NAs no infinity -> removed 2 observations

#Proxy: PPE % of Assets
#Assumption: Audit complexity in revaluation of PPE
data_cleaned = data_cleaned %>% mutate(pctg_ppe_at=ppegt/at)
summary(data_cleaned$pctg_ppe_at) #no NAs no infinity

#Proxy: Whether Fortune 1000
#Assumption: Size of company and presence makes the business more complex

#Factor 5: Industry Type 
#Proxy: Dummy variables of FFI12_desc created above

#Factor 6: Status of Audit Firm
#Proxy: Dummy variables to indicate Big 4 or not -> assumed Big 4 charges higher fees

# EXPORT FINAL DATA
# original dataset cleaned
write_csv(data_cleaned, "Team Project cleaned.csv")
summary(data_cleaned)
# Audit fee version dataset cleaned
data_cleaned_auditFees = data_cleaned %>% filter(!is.na(AUDIT_FEES)) #removed 11,261 observations
#Total observations = 22,795
summary(data_cleaned_auditFees$AUDIT_FEES)
View(data_cleaned_auditFees)
write_csv(data_cleaned_auditFees, "Team Project AuditFees Version cleaned.csv")

#Regression Analysis
#Apart from our identified proxies under the 6 factors which serve as Audit Fee determinants
#We further identified other potentially relevant factors such as:
# 1) Intangible assets (intan) & 2) Auditor opinion (auop)

#Simple Regression
#Proxies of Identified Factors

# 1) Employees (emp)
reg_1 = lm(formula = AUDIT_FEES~emp,data=data_cleaned_auditFees)
summary(reg_1) #R squared = 0.1576

# 2) Total Assets (at)
reg_2 = lm(formula = AUDIT_FEES~at,data=data_cleaned_auditFees)
summary(reg_2) #R squared = 0.3599

# 3) Profit Margin (profit_margin)
reg_3 = lm(formula = AUDIT_FEES~profit_margin,data=data_cleaned_auditFees)
summary(reg_3) #R squared = 0.0004961

# 4) Debt-to-Equity (der)
reg_4 = lm(formula = AUDIT_FEES~der,data=data_cleaned_auditFees)
summary(reg_4) #R squared = 1.553e-05

# 5) Current Ratio (cr)
reg_5 = lm(formula = AUDIT_FEES~cr,data=data_cleaned_auditFees)
summary(reg_5) #R squared = 0.0001297

# 6) Inventory as % of Assets (pctg_invt_at)
reg_6 = lm(formula = AUDIT_FEES~pctg_invt_at,data=data_cleaned_auditFees)
summary(reg_6) #R squared = 0.0005707

# 7) PPE as % of Assets (pctg_ppe_at)
reg_7 = lm(formula = AUDIT_FEES~pctg_ppe_at,data=data_cleaned_auditFees)
summary(reg_7)#R squared = 0.001842

# 8) Fortune 100 (d_fortune)
reg_8 = lm(formula = AUDIT_FEES~d_fortune,data=data_cleaned_auditFees)
summary(reg_8) #R squared = 0.1009

# 9) Industries (FFI12_descBusEq+FFI12_descChems+FFI12_descDurbl+FFI12_descEnrgy+FFI12_descHlth+FFI12_descManuf+FFI12_descMoney+FFI12_descNoDur+FFI12_descOther+FFI12_descShops+FFI12_descTelcm+FFI12_descUtils)
reg_9 = lm(formula = AUDIT_FEES~FFI12_descBusEq+FFI12_descChems+
             FFI12_descDurbl+FFI12_descEnrgy+FFI12_descHlth+
             FFI12_descManuf+FFI12_descMoney+FFI12_descNoDur+
             FFI12_descOther+FFI12_descShops+FFI12_descTelcm+
             FFI12_descUtils,data=data_cleaned_auditFees)
summary(reg_9) #R squared = 0.01093

# 10) Big 4 Status (BIG4)
reg_10 = lm(formula = AUDIT_FEES~BIG4,data=data_cleaned_auditFees)
summary(reg_10) #R squared = 0.08742

# 11) Intangible assets (intan)
reg_11 = lm(formula = AUDIT_FEES~intan,data=data_cleaned_auditFees)
summary(reg_11) #R squared = 0.3214

# 12) Auditor Opinion (auop)
reg_12 = lm(formula = AUDIT_FEES~auop,data=data_cleaned_auditFees)
summary(reg_12) #R squared = 0.0007761

#based on statistical results, we remove the following variables
#der and cr as their p-values prove the variable to be statistically insignificant
#pctg_ppe_at, pctg_ppe_invt, profit_margin and auop as R-squared < 1%


library(forecast)
library(leaps)

#Selecting key variables for the regression model
Audit_Reg = select(data_cleaned_auditFees,c(emp, at, d_fortune,
                                              FFI12_descBusEq, FFI12_descChems,
                                              FFI12_descDurbl, FFI12_descEnrgy, 
                                              FFI12_descHlth, FFI12_descManuf, 
                                              FFI12_descMoney, FFI12_descNoDur,
                                              FFI12_descOther, FFI12_descShops, 
                                              FFI12_descTelcm, FFI12_descUtils,
                                              BIG4, intan, AUDIT_FEES))
summary(Audit_Reg) #no NA, no INF
View(Audit_Reg)

Audit_Regm <- lm(AUDIT_FEES ~ emp + at + d_fortune +
                FFI12_descBusEq + FFI12_descChems +
                FFI12_descDurbl + FFI12_descEnrgy + 
                FFI12_descHlth + FFI12_descManuf + 
                FFI12_descMoney + FFI12_descNoDur + 
                FFI12_descOther + FFI12_descShops + 
                FFI12_descTelcm + FFI12_descUtils +
                BIG4 + intan, data = Audit_Reg)

summary(Audit_Regm) #R-squared = 0.6091

#Plot for Employees (emp) vs Audit Fees
plot(Audit_Reg$AUDIT_FEES, Audit_Reg$emp)

#plot for Total Assets vs Audit Fees
plot(Audit_Reg$AUDIT_FEES, Audit_Reg$at)

#plot for Intangibles (intan) vs Audit Fees
plot(Audit_Reg$AUDIT_FEES, Audit_Reg$intan)

#selected variables for the regression model appear to be skewed

#addition of log functions to satisfy linearity assumption
Audit_Regm_Log = Audit_Reg %>% mutate(emp_log = log(emp + 1),
                                      at_log = log(at+1),
                                      intan_log = log(intan+1))

#plot of Audit Fees against log functions of emp, at and intan
plot(Audit_Regm_Log$AUDIT_FEES, Audit_Regm_Log$emp_log)
plot(Audit_Regm_Log$AUDIT_FEES, Audit_Regm_Log$at_log)
plot(Audit_Regm_Log$AUDIT_FEES, Audit_Regm_Log$intan_log)

Audit_Regm_Log = Audit_Regm_Log %>% mutate(audit_fees_log = log(AUDIT_FEES + 1))

#variables are still largely skewed against audit fees, hence, we utilized the log function
#on audit fees to satisfy the linearity assumption

#plot for log functions: Employees (emp) vs Audit Fees
plot(Audit_Regm_Log$audit_fees_log, Audit_Regm_Log$emp_log)

#plot for log functions: Total Assets vs Audit Fees
plot(Audit_Regm_Log$audit_fees_log, Audit_Regm_Log$at_log)

#plot for log functions: Intangibles (intan) vs Audit Fees
plot(Audit_Regm_Log$audit_fees_log, Audit_Regm_Log$intan_log)


# select the updated variables to use
Audit_Reg_Updt = select(Audit_Regm_Log,c(emp_log, at_log, d_fortune,
                                    FFI12_descBusEq, FFI12_descChems,
                                    FFI12_descDurbl, FFI12_descEnrgy, 
                                    FFI12_descHlth, FFI12_descManuf, 
                                    FFI12_descMoney, FFI12_descNoDur,
                                    FFI12_descOther, FFI12_descShops, 
                                    FFI12_descTelcm, FFI12_descUtils,
                                    BIG4, intan_log, audit_fees_log))
summary(Audit_Reg_Updt)

set.seed(1) # set a random seed so every sample will be the same, hence results will be the same
Audit_train <- sample_frac(Audit_Reg_Updt, 0.7)
Audit_test <- anti_join(Audit_Reg_Updt, Audit_train)

Audit_RegModel <- lm(audit_fees_log ~ emp_log + at_log+ d_fortune+
                 FFI12_descBusEq+ FFI12_descChems+
                 FFI12_descDurbl+ FFI12_descEnrgy+ 
                 FFI12_descHlth+ FFI12_descManuf+ 
                 FFI12_descMoney+ FFI12_descNoDur+
                 FFI12_descOther+ FFI12_descShops+ 
                 FFI12_descTelcm+ FFI12_descUtils+
                 BIG4+ intan_log, data = Audit_train)

summary(Audit_RegModel) #R-squared = 0.8442

#Removed Utilities industry as it gives NA

Audit_RegModel <- lm(audit_fees_log ~ emp_log + at_log+ d_fortune+
                       FFI12_descBusEq + FFI12_descChems+
                       FFI12_descDurbl + FFI12_descEnrgy+ 
                       FFI12_descHlth + FFI12_descManuf+ 
                       FFI12_descMoney + FFI12_descNoDur+
                       FFI12_descOther + FFI12_descShops+ 
                       FFI12_descTelcm + BIG4+ intan_log,
                       data = Audit_train)

summary(Audit_RegModel) #R-squared = 0.8442

library(car)
library(corrplot)

#Check for VIF
vif(Audit_RegModel) #All < 10

corrplot(cor(Audit_train))

#Testing the accuracy
Audit_Reg_pred <- predict(Audit_RegModel, Audit_test)
Audit_Reg_Error <- Audit_test$audit_fees_log - Audit_Reg_pred
Audit_Reg_Final <- data.frame(cbind(Audit_Reg_pred,Audit_test$audit_fees_log,Audit_Reg_Error))
accuracy(Audit_Reg_pred, Audit_test$audit_fees_log)
#                  ME      RMSE       MAE        MPE     MAPE
# Test set 0.003865102 0.5677058 0.4459143 -0.1492596 3.290417

#Forward selection
Audit_Reg_Forward <- step(Audit_RegModel, direction = "forward")
summary(Audit_Reg_Forward)
Audit_Reg_Forward_Pred <- predict(Audit_Reg_Forward, Audit_test)
accuracy(Audit_Reg_Forward_Pred, Audit_test$audit_fees_log)
#                  ME      RMSE       MAE        MPE     MAPE
# Test set 0.003865102 0.5677058 0.4459143 -0.1492596 3.290417 -> same as above, no variables removed

#Backward elimination
Audit_Reg_Back <- step(Audit_RegModel, direction = "backward")
summary(Audit_Reg_Back) 
Audit_Reg_Back_Pred <- predict(Audit_Reg_Back, Audit_test)
accuracy(Audit_Reg_Back_Pred, Audit_test$audit_fees_log)
#                  ME      RMSE       MAE        MPE     MAPE
# Test set 0.003865102 0.5677058 0.4459143 -0.1492596 3.290417 -> same as above, no variables removed


#Stepwise regression
Audit_Reg_Stepwise <- step(Audit_RegModel, direction = "both")
summary(Audit_Reg_Stepwise)
Audit_Reg_Stepwise_Pred <- predict(Audit_Reg_Stepwise, Audit_test)
accuracy(Audit_Reg_Stepwise_Pred, Audit_test$audit_fees_log)
#                  ME      RMSE       MAE        MPE     MAPE
# Test set 0.003865102 0.5677058 0.4459143 -0.1492596 3.290417 -> same as above, no variables removed


########################################
##### CLASSIFICATION TREE - PART 1 #####
########################################

library(rpart)
library(rpart.plot)
library(e1071)
library(pROC)

base<- read_csv ("Team Project AuditFees Version cleaned.csv")

## classification tree 1 - using mean
## step 1: created bins for audit fees by splitting into 2 categories - higher than mean or lower than mean
summary (base$AUDIT_FEES) # mean of audit_fees = 2683117
summary(base$emp)
base <- base %>% mutate(high_AUDIT_FEES = ifelse(AUDIT_FEES>2683117, 1, 0))


## step 2: classification tree 
AFmean_tree <- rpart(high_AUDIT_FEES ~ emp + at + profit_margin + der + cr + 
                       pctg_invt_at + pctg_ppe_at + d_fortune +
                       FFI12_descBusEq + FFI12_descChems +
                       FFI12_descDurbl + FFI12_descEnrgy + 
                       FFI12_descHlth + FFI12_descManuf + 
                       FFI12_descMoney + FFI12_descNoDur + 
                       FFI12_descOther + FFI12_descShops + 
                       FFI12_descTelcm + FFI12_descUtils +
                       BIG4 + intan + auop, data = base,  method = "class")

prp(AFmean_tree, type = 1, extra = 1)
rpart.rules(AFmean_tree)

## no minsplit or CP needed as tree is already trimmed

## step 3: Accuracy statistics (confusion matrix + ROC)
AFmean_tree_pred <- predict(AFmean_tree,
                            base, type = "class")
confusionMatrix(table(AFmean_tree_pred, 
                      base$high_AUDIT_FEES), 
                positive = "1") #Accuracy : 0.8919

AFmean_prob_pred <- predict(AFmean_tree, base, type="prob")
AFmean_tree_combined <- cbind(base,  AFmean_prob_pred)

ROC <- plot.roc(AFmean_tree_combined$high_AUDIT_FEES, AFmean_tree_combined$`1`)
auc(ROC) #Area under the curve: 0.8635

#interpreting results: Classification Tree 1 confirmed that emp and at should be included in the final model

## classification tree 2 - using 25th percentile
summary (base$AUDIT_FEES) 
base <- base %>% mutate(high_AUDIT_FEES = ifelse(AUDIT_FEES>399000, 1, 0))

AF25_tree <- rpart(high_AUDIT_FEES ~ emp + at + profit_margin + der + cr + 
                     pctg_invt_at + pctg_ppe_at + d_fortune +
                     FFI12_descBusEq + FFI12_descChems +
                     FFI12_descDurbl + FFI12_descEnrgy + 
                     FFI12_descHlth + FFI12_descManuf + 
                     FFI12_descMoney + FFI12_descNoDur + 
                     FFI12_descOther + FFI12_descShops + 
                     FFI12_descTelcm + FFI12_descUtils +
                     BIG4 + intan + auop, data = base,  method = "class")

prp(AF25_tree, type = 1, extra = 1)
rpart.rules(AF25_tree)

AF25_tree_pred <- predict(AF25_tree,
                          base, type = "class")
confusionMatrix(table(AF25_tree_pred, 
                      base$high_AUDIT_FEES), 
                positive = "1") #Accuracy : 0.9267  

AF25_prob_pred <- predict(AF25_tree, base, type="prob")
AF25_tree_combined <- cbind(base,  AF25_prob_pred)

ROC <- plot.roc(AF25_tree_combined$high_AUDIT_FEES, AF25_tree_combined$`1`)
auc(ROC) #Area under the curve: 0.9422
#interpreting results: confirmed that BIG4 should be included in the final model. 
#Since CR was rejected earlier due to its high p value, it will not be included in the final model. 

## classfication tree 3 - using 50th percentile (median)
summary (base$AUDIT_FEES)# mean of audit_fees = 2683117
base <- base %>% mutate(high_AUDIT_FEES = ifelse(AUDIT_FEES>1144269, 1, 0))

AFmedian_tree <- rpart(high_AUDIT_FEES ~ emp + at + profit_margin + der + cr + 
                         pctg_invt_at + pctg_ppe_at + d_fortune +
                         FFI12_descBusEq + FFI12_descChems +
                         FFI12_descDurbl + FFI12_descEnrgy + 
                         FFI12_descHlth + FFI12_descManuf + 
                         FFI12_descMoney + FFI12_descNoDur + 
                         FFI12_descOther + FFI12_descShops + 
                         FFI12_descTelcm + FFI12_descUtils +
                         BIG4 + intan + auop, data = base,  method = "class")

prp(AFmedian_tree, type = 1, extra = 1)
rpart.rules(AFmedian_tree)

AFmedian_tree_pred <- predict(AFmedian_tree,
                              base, type = "class")
confusionMatrix(table(AFmedian_tree_pred, 
                      base$high_AUDIT_FEES), 
                positive = "1") #Accuracy : 0.8543 

AFmedian_prob_pred <- predict(AFmedian_tree, base, type="prob")
AFmedian_tree_combined <- cbind(base,  AFmedian_prob_pred)

ROC <- plot.roc(AFmedian_tree_combined$high_AUDIT_FEES, AFmedian_tree_combined$`1`)
auc(ROC) #Area under the curve: 0.906

#interpreting results: Classification Tree 3 confirmed that intan should be included in the final model, 


## classfication tree 4 - using 75th percentile
summary (base$AUDIT_FEES) 
base <- base %>% mutate(high_AUDIT_FEES = ifelse(AUDIT_FEES>2680704, 1, 0))

AF75_tree <- rpart(high_AUDIT_FEES ~ emp + at + profit_margin + der + cr + 
                     pctg_invt_at + pctg_ppe_at + d_fortune +
                     FFI12_descBusEq + FFI12_descChems +
                     FFI12_descDurbl + FFI12_descEnrgy + 
                     FFI12_descHlth + FFI12_descManuf + 
                     FFI12_descMoney + FFI12_descNoDur + 
                     FFI12_descOther + FFI12_descShops + 
                     FFI12_descTelcm + FFI12_descUtils +
                     BIG4 + intan + auop, data = base,  method = "class")

prp(AF75_tree, type = 1, extra = 1)
rpart.rules(AF75_tree)

## same as classification tree 1, hence tests for accuracy (ie confusionmatrix/ROC)  were not carried out
# interpreting results: Classification Tree 4 did not identify any new variables.


########################################
##### CLASSIFICATION TREE - PART 2 #####
########################################

## LOAD DATA

data_cleaned_class2 <- read_csv("Team Project cleaned.csv")

summary(data_cleaned_class2$AUDIT_FEES)


## MAKE A BINARY VARIABLE FOR AUDIT FEES WHERE NA = UNAUDITED FIRM = 0 , NON-NA = AUDITED FIRM = 1
data_cleaned_class2 <- mutate(data_cleaned_class2, audited = ifelse(!is.na(AUDIT_FEES), 1, 0))
summary(data_cleaned_class2$audited)


# OBJECTIVE: DETERMINE VARIABLES THAT AFFECT THE PRESENCE OF AUDIT FEE IN THE FIRST PLACE
## get all variables other than audited

# shift audited to last column; remove variables which are unique for every company, e.g CID, busdesc
data_for_tree <- data_cleaned_class2 %>% select(everything(), -c(cid, datadate, busdesc, AUDIT_FEES, audited), audited)

## if don't put FFI12 descriptions; and new variables group by industry
# data_for_tree <- data_cleaned_class2 %>% 
#   select(everything(), -c(cid, datadate, busdesc, AUDIT_FEES, audited, avg_dlc_lt, avg_lct_lt, avg_re_ni_per_industry, avg_act_at_by_industry, avg_emp_by_bin_and_industry,avg_invt_by_bin_and_industry, avg_ppegt_by_bin_and_industry, avg_rect_by_bin_and_industry, avg_xad_by_sale_bins_and_industry, avg_xlr_by_bin_and_industry, avg_xsga_by_bin_and_industry, avg_capx_by_industry_and_size ), audited) %>% 
#   select(!contains("FFI12", ignore.case = T))
max = ncol(data_for_tree)-1
listvariables = colnames(data_for_tree)[1:max] ## only select independent variables (i.e do not include "audited" dummy variable)

listvariables

fmla <- as.formula(paste("audited", paste(listvariables, collapse=" + "), sep=" ~ ")) ## formula to add all listvariables into the tree formula

tree_all <- rpart(fmla,
                  data = data_for_tree, 
                  method = "class", cp=0.018204) # cp = 0.0182024 will give 7 splits

prp(tree_all, type = 1, extra = 1)

printcp(tree_all)
# CP nsplit rel error  xerror      xstd
# 1 0.195631      0   1.00000 1.00000 0.0077096
# 2 0.069621      1   0.80437 0.79638 0.0072178
# 3 0.027973      2   0.73475 0.73040 0.0070140
# 4 0.018204      7   0.59400 0.60829 0.0065691
# 5 0.018204      9   0.55759 0.59258 0.0065047

rpart.rules(tree_all)
## Simplified results
# 0.13 when FFI12_desc is not Other	& mv_crsp <  5927 	& ceq >= 6035                            
# 0.15 when FFI12_desc is	Other 		& mv_crsp <  5926 	& prcc_f_avg <  14	& avg_capx_per_company >= 35                                          
# 0.16 when FFI12_desc is	Other 		& mv_crsp >= 5926                    
# 0.28 when FFI12_desc is not Other	& mv_crsp >= 5927 	& d_fortune is 0 	& xlr <  682
# 0.40 when FFI12_desc is not Other	& mv_crsp >= 5927 	& d_fortune is 0 	& xlr >= 682			& avg_emp_per_company >= 19
# 0.69 when FFI12_desc is	Other 		& mv_crsp <  5926	& prcc_f_avg <  14	& avg_capx_per_company <  35                                          
# 0.72 when FFI12_desc is not Other	& mv_crsp >= 5927 	& d_fortune is 0	& xlr >= 682			& avg_emp_per_company <  19
# 0.79 when FFI12_desc is	Other 		& mv_crsp <  5926       & prcc_f_avg >= 14
# 0.85 when FFI12_desc is not Other	& mv_crsp <  5927	& ceq <  6035                            
# 0.98 when FFI12_desc is not Other	& mv_crsp >= 5927 	& d_fortune is 1 

#### Interpreting results: We have identified FFI12_DESC, MV_CRSP & D_FORTUNE as being indicators of audit fee presence, 
## which we hypothesise are further related to the magnitude of audit fees. Hence they are potential factors to be included 
## in the final model, subject to regression results


# Generate confusion matrix
tree_pred <- predict(tree_all, data_for_tree, type = "class")
confusionMatrix(table(tree_pred, data_for_tree$audited), positive = "1")
# Confusion Matrix and Statistics
# 
# tree_pred     0     1
# 0  7072  2090
# 1  4189 20705
# 
# Accuracy : 0.8156          
# 95% CI : (0.8115, 0.8197)
# No Information Rate : 0.6693          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.5629          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.9083          
#             Specificity : 0.6280          
#          Pos Pred Value : 0.8317          
#          Neg Pred Value : 0.7719          
#              Prevalence : 0.6693          
#          Detection Rate : 0.6080          
#    Detection Prevalence : 0.7310          
#       Balanced Accuracy : 0.7682          
#                                           
#        'Positive' Class : 1 

## Plot ROC
data_cleaned_class2a <- cbind(data_cleaned_class2, tree_pred)
data_cleaned_class2a <- data_cleaned_class2a %>% select(cid, audited, tree_pred)

data_prob_pred <- predict(tree_all, data_cleaned_class2, type="prob")

tree_combined <- cbind(data_cleaned_class2, data_prob_pred)

ROC <- plot.roc(tree_combined$audited, tree_combined$`1`)

auc(ROC) ## Area under the curve: 0.8148

########################################
#######      TEXT ANALYTICS      #######
########################################

# install required package
#install.packages("naivebayes")
#install.packages("superml", dependencies=TRUE) # to install all dependencies at once

# import the necessary packages
library(readr)
library(dplyr)
library(naivebayes)
library(superml)
library(ggplot2)
#library(stringr)

# import data
data1 = read_csv("Team Project AuditFees Version cleaned.csv")
summary(data1)

# remove entries with is.na(busdesc)
sum(is.na(data1$busdesc)) # 5 NAs
data1 = filter(data1, !is.na(busdesc))
sum(is.na(data1$busdesc)) # 0 NAs

# select the columns needed
data_text = select(data1, c(busdesc, AUDIT_FEES))
summary(data_text)

# group by busdesc to remove duplicate records, take the mean AUDIT_FEES for duplicate records of busdesc
data_text1 = data_text %>% 
  group_by(busdesc) %>% 
  mutate(AUDIT_FEES = mean(AUDIT_FEES)) %>%
  filter(row_number()==1)
summary(data_text1)

# initialize count vectorizer class
cv = CountVectorizer$new(remove_stopwords = TRUE, 
                         lowercase = TRUE,
                         regex = "/^[A-Za-z]+$/",
                         max_df = 0.008)

# takes too long to run, used a python function to complete this step, just import the csv given
# cv_matrix = cv$fit_transform(c(data_text2$busdesc)) 
cv_matrix = read_csv("word counts.csv")

# check summary stats on audit fees
summary(cv_matrix$AUDIT_FEES) # mean = 2613353

# use the mean to classify high and low audit fees
cv_matrix = cv_matrix %>% mutate(AUDIT_FEES = ifelse( AUDIT_FEES > 2583090, "High", "Low"))

# use multinomial Naïve Bayes to find probabilities
X = select(cv_matrix, -AUDIT_FEES)
y = cv_matrix$AUDIT_FEES
mnb = multinomial_naive_bayes(X, y)

# transform result to dataframe to view
result = as.data.frame(mnb$params)
result = mutate(result, word = rownames(result))
rownames(result) = NULL
View(result)

# get the top 10 words with the highest probability for High Audit Fees
High_top_10 = result %>% 
  select(c(word, High, Low)) %>% 
  arrange(desc(High)) %>%  
  head(10)
# write.csv(High_top_10, "visualisations/data/High_top_10.csv")

# get the top 10 words with the highest probability for Low Audit Fees
Low_top_10 = result %>% 
  select(c(word, Low, High)) %>% 
  arrange(desc(Low)) %>%  
  head(10)
# write.csv(Low_top_10, "visualisations/data/Low_top_10.csv")

# FIND OUT WHICH INDUSTRIES THESE WORDS USUALLY APPEAR IN
data_industry = select(data1, c(busdesc, FFI12_desc))

# get top 10 words for each category (high/low audit fees)
High_top_10_words = unique(High_top_10$word)
Low_top_10_words = unique(Low_top_10$word)

# if word is in busdesc, replace busdesc with 1
High_result = data_industry
for (word in High_top_10_words)
{
  High_result = High_result %>% 
    mutate(busdesc = ifelse(is.character(busdesc) & grepl(word, busdesc, ignore.case = T), 1, busdesc))
}
Low_result = data_industry
for (word in Low_top_10_words)
{
  Low_result = Low_result %>% 
    mutate(busdesc = ifelse(is.character(busdesc) & grepl(word, busdesc, ignore.case = T), 1, busdesc))
}

# filter rows with busdesc == 1 to get the FFI12_desc with the top 10 words of each category
High_result_final = filter(High_result, busdesc == 1)
Low_result_final = filter(Low_result, busdesc == 1)

# shows the number of times a FFI12_desc appears in the top 10 words of High Audit Fees
High_FFI12 = as.data.frame(table(High_result_final$FFI12_desc)) %>% arrange(desc(Freq))
View(High_FFI12)
# write.csv(High_FFI12, "visualisations/data/High_FFI12.csv")

# shows the number of times a FFI12_desc appears in the top 10 words of Low Audit Fees
Low_FFI12 = as.data.frame(table(Low_result_final$FFI12_desc))
View(Low_FFI12)
# write.csv(Low_FFI12, "visualisations/data/Low_FFI12.csv")

# VISUALISE RESULTS

# industry frequency in top 10 words for High Audit Fees
ggplot(High_result_final, aes(x = reorder(FFI12_desc,FFI12_desc,function(x)-length(x)))) + 
  geom_bar(fill = "#64D1CC") +
  xlab("FFI12_desc") +
  ylab("Count")

# industry frequency in top 10 words for Low Audit Fees
ggplot(Low_result_final, aes(x = reorder(FFI12_desc,FFI12_desc,function(x)-length(x)))) + 
  geom_bar(fill = "#CCAAEC") +
  xlab("FFI12_desc") +
  ylab("Count")

# Conditional Probabilty for each of the top 10 words for High Audit Fees
ggplot(data=High_top_10, aes(x = word, y = High)) + 
  geom_bar(fill = "#38A29D", stat="identity") +
  xlab("Word") +
  ylab("Probability")

# Conditional Probabilty for each of the top 10 words for Low Audit Fees
ggplot(data=Low_top_10, aes(x = word, y = Low)) + 
  geom_bar(fill = "#D495E9", stat="identity") +
  xlab("Word") +
  ylab("Probability")


# DATA VISUALISATIONS

# mean audit fees by industry
data2 = data1 %>% 
  select(AUDIT_FEES, FFI12_desc) %>%
  group_by(FFI12_desc) %>%
  mutate(AUDIT_FEES_mean = mean(AUDIT_FEES, na.rm=T)) %>%
  filter(row_number()==1) %>% 
  select(-AUDIT_FEES) %>%
  ungroup() %>%
  arrange(AUDIT_FEES_mean)
View(data1)

ggplot(data=data2, aes(x= FFI12_desc, y = AUDIT_FEES_mean)) +
  geom_bar(fill = "#D495E9", stat="identity") +
  xlab("FFI12_desc") +
  ylab("Mean Audit Fee")

########################################
#######     FINAL REGRESSION     #######
########################################

#Based on the support of current variables in Audit_RegModel, we further identified
#several variables to be added into our final regression model, which would be 
#utilized to help firms calculate the appropriate audit fees.

#As a recap on the derivation of our regression model we first selected the variables 
#from our hypothesized model, but this time with mv_crsp

Audit_Reg_Final = select(data_cleaned_auditFees,c(emp, at, d_fortune,mv_crsp,
                                            FFI12_descBusEq, FFI12_descChems,
                                            FFI12_descDurbl, FFI12_descEnrgy, 
                                            FFI12_descHlth, FFI12_descManuf, 
                                            FFI12_descMoney, FFI12_descNoDur,
                                            FFI12_descOther, FFI12_descShops, 
                                            FFI12_descTelcm, FFI12_descUtils,
                                            BIG4, intan, AUDIT_FEES))

#We then utilized the log function to satisfy the linearity assumption
Audit_Reg_Final = Audit_Reg_Final %>% mutate(emp_log = log(emp + 1),
                                      at_log = log(at+1),
                                      intan_log = log(intan+1),
                                      audit_fees_log = log(AUDIT_FEES + 1),
                                      mv_crsp_log = log(mv_crsp + 1))

set.seed(1) # set a random seed so every sample will be the same, hence results will be the same
Audit_train_final <- sample_frac(Audit_Reg_Final, 0.7)
Audit_test_final <- anti_join(Audit_Reg_Final, Audit_train_final)

Audit_RegModel_Final <- lm(audit_fees_log ~ emp_log + at_log + 
                             d_fortune + mv_crsp_log +
                             FFI12_descBusEq + FFI12_descChems +
                             FFI12_descDurbl + FFI12_descEnrgy + 
                             FFI12_descHlth + FFI12_descManuf + 
                             FFI12_descMoney + FFI12_descNoDur +
                             FFI12_descOther + FFI12_descShops + 
                             FFI12_descTelcm + BIG4 + intan_log,
                             data = Audit_train_final)

summary(Audit_RegModel_Final) #R-squared = 0.8464

#Testing the accuracy
Audit_Reg_pred_final <- predict(Audit_RegModel_Final, Audit_test_final)
Audit_Reg_Error_final <- Audit_test_final$audit_fees_log - Audit_Reg_pred_final
Audit_Reg_Final1 <- data.frame(cbind(Audit_Reg_pred_final,Audit_test_final$audit_fees_log,Audit_Reg_Error_final))
accuracy(Audit_Reg_pred_final, Audit_test_final$audit_fees_log)
#                  ME      RMSE       MAE       MPE     MAPE
#Test set 0.004118958 0.5651751 0.4440438 -0.142374 3.272437

#Check for VIF
vif(Audit_RegModel_Final) #All < 10
