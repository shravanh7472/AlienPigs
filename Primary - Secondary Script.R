# ============================================================
# METRICS GAMES ROUND 1: Differential Causal Effect of Emigration Rates on Education Decisions By Sex
# 
# SUMMARY: To estimate the differential casual effect of migration rates on education decisions of males and females in Mexiccan communities,
# this script runs multiple two stage least squares regressions with a shift-share IV design for migration rates (using what's called
# "prevalence ratio") on male and female populations.
# ====================================================

DATA_DIR    <- NULL

LIFE_PATH   <- "life174.sav"
PERS_PATH   <- "pers174.dta"
PRATIO_PATH <- "prevratio174.dta"
SHOCK_PATH  <- "Hisp Unemployment Rate Annual.xlsx"
HOUSE_PATH  <- "house174.dta"

MIG_PATH        <- "mig174.dta"
COMM_STATE_PATH <- "comm_state.csv"
FALLBACK_NATIONAL <- FALSE

SHOCK_RATE_COL <- "rate"
PRATIO_COL     <- NULL

YOUTH_AGE_MIN <- 14L
YOUTH_AGE_MAX <- Inf

PRIMARY_AGE_MIN   <- 5L
PRIMARY_AGE_MAX   <- 12L
MIN_PRIMARY_YEAR  <- 1975L

# schooling margin: 6 vs 7
PRIMARY_COMPLETE_YEARS <- 6
OUTCOME_YEARS          <- 7
STRICT_6V7_ONLY         <- TRUE

PR_BASE_START     <- 1950L
PR_BASE_END       <- 1970L
MIN_PR_BASE_YEARS <- 2L

CY_AGE_MIN <- 15L
CY_AGE_MAX <- 65L

REQUIRE_CHILD_RELHEAD <- TRUE
CLUSTER_VCOV <- ~commun

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(fixest)
})
options(dplyr.summarise.inform = FALSE)

if (!is.null(DATA_DIR)) {
  DATA_DIR <- path.expand(DATA_DIR)
  stopifnot(dir.exists(DATA_DIR))
  setwd(DATA_DIR)
}

stopifnot(
  file.exists(LIFE_PATH),
  file.exists(PERS_PATH),
  file.exists(PRATIO_PATH),
  file.exists(SHOCK_PATH),
  file.exists(HOUSE_PATH),
  file.exists(MIG_PATH),
  file.exists(COMM_STATE_PATH)
)

hh_keys <- c("country","commun","surveyyr","hhnum")
never_codes <- c(8888L, 9999L)

na_8899_int <- function(x){
  xi <- suppressWarnings(as.integer(x))
  xi[xi %in% c(8888L, 9999L)] <- NA_integer_
  xi
}
na_8899_num <- function(x){
  y <- suppressWarnings(as.numeric(x))
  y[y %in% c(8888, 9999)] <- NA_real_
  y
}
first_non_na <- function(x){
  x2 <- x[!is.na(x)]
  if (length(x2) == 0) NA_integer_ else as.integer(x2[1])
}
age_at_svy <- function(age, surveyyr, yrborn){
  a  <- suppressWarnings(as.integer(age))
  sy <- suppressWarnings(as.integer(surveyyr))
  yb <- suppressWarnings(as.integer(yrborn))
  out <- ifelse(is.na(a) | a %in% c(8888L,9999L), sy - yb, a)
  suppressWarnings(as.integer(out))
}
age_at_year <- function(age, year, yrborn){
  a  <- suppressWarnings(as.integer(age))
  y  <- suppressWarnings(as.integer(year))
  yb <- suppressWarnings(as.integer(yrborn))
  out <- ifelse(is.na(a) | a %in% c(8888L,9999L), y - yb, a)
  suppressWarnings(as.integer(out))
}
resolve_cols <- function(meta_names, need_lower){
  map <- setNames(meta_names, tolower(meta_names))
  miss <- setdiff(need_lower, names(map))
  if (length(miss) > 0) stop("missing cols: ", paste(miss, collapse=", "))
  unname(map[need_lower])
}
detect_pratio_col <- function(df){
  cand <- c("pratio","prevratio","prev_ratio","prev_rat","prat","pratio174")
  cand <- cand[cand %in% names(df)]
  if (length(cand) > 0) return(cand[1])
  drop <- c("country","commun","surveyyr","hhnum","year")
  cols <- setdiff(names(df), drop)
  numish <- cols[sapply(df[cols], function(x) is.numeric(x) || is.integer(x))]
  if (length(numish) == 0) stop("couldn't auto-detect pratio column; set PRATIO_COL manually.")
  numish[1]
}
normalize_usyr1 <- function(x){
  x <- suppressWarnings(as.integer(x))
  x[!is.na(x) & !(x %in% never_codes) & (x < 1800L | x > 2100L)] <- NA_integer_
  x
}
ever_migrated <- function(usyr1){
  u <- normalize_usyr1(usyr1)
  !is.na(u) & !(u %in% never_codes)
}
never_migrated_strict <- function(usyr1){
  u <- normalize_usyr1(usyr1)
  !is.na(u) & (u %in% never_codes)
}
clean_asset <- function(x){
  x <- suppressWarnings(as.numeric(x))
  x[x %in% c(8888, 9999)] <- NA_real_
  x[x < 0] <- NA_real_
  x
}

comm_state <- read.csv(COMM_STATE_PATH, stringsAsFactors = FALSE) |>
  as_tibble() |>
  transmute(
    commun = as.integer(commun),
    state  = as.character(state)
  ) |>
  filter(!is.na(commun), !is.na(state)) |>
  distinct(commun, .keep_all = TRUE)

shock <- read_excel(SHOCK_PATH) |> rename_with(tolower)
stopifnot(all(c("year", SHOCK_RATE_COL) %in% names(shock)))

shock2 <- shock |>
  transmute(
    year  = as.integer(.data[["year"]]),
    shock = as.numeric(.data[[SHOCK_RATE_COL]])
  ) |>
  arrange(year) |>
  filter(!is.na(year), !is.na(shock))

pr0 <- read_dta(PRATIO_PATH) |> rename_with(tolower)
if (is.null(PRATIO_COL)) PRATIO_COL <- detect_pratio_col(pr0)
if (!(PRATIO_COL %in% names(pr0))) stop("PRATIO_COL not found: ", PRATIO_COL)

cy_pratio <- pr0 |>
  transmute(
    commun = as.integer(commun),
    year   = as.integer(year),
    pratio = as.numeric(.data[[PRATIO_COL]])
  ) |>
  filter(!is.na(commun), !is.na(year)) |>
  group_by(commun, year) |>
  summarise(pratio = mean(pratio, na.rm = TRUE), .groups = "drop") |>
  arrange(commun, year)

pr_rng <- range(cy_pratio$pratio, na.rm = TRUE)
if (is.finite(pr_rng[2]) && pr_rng[2] > 1.5) cy_pratio <- mutate(cy_pratio, pratio = pratio / 100)

base_pratio_tbl <- cy_pratio |>
  filter(year >= PR_BASE_START, year <= PR_BASE_END) |>
  group_by(commun) |>
  summarise(
    baseline_pratio = mean(pratio, na.rm = TRUE),
    n_pr_base_years = sum(!is.na(pratio)),
    .groups = "drop"
  ) |>
  mutate(baseline_pratio = ifelse(n_pr_base_years < MIN_PR_BASE_YEARS, NA_real_, baseline_pratio))

ASSET_VARS <- c("land","hectars","property","business")
life_need <- c("country","commun","surveyyr","hhnum","year","weight","sex","yrborn","age","inus","usdur","trip", ASSET_VARS)

life_meta <- read_sav(LIFE_PATH, n_max = 0)
life_use  <- resolve_cols(names(life_meta), life_need)

life0 <- read_sav(LIFE_PATH, col_select = all_of(life_use)) |> rename_with(tolower)

life_raw <- life0 |>
  transmute(
    country  = na_8899_int(country),
    commun   = na_8899_int(commun),
    surveyyr = na_8899_int(surveyyr),
    hhnum    = na_8899_int(hhnum),
    year     = na_8899_int(year),
    
    weight   = na_8899_num(weight),
    sex      = na_8899_int(sex),
    yrborn   = na_8899_int(yrborn),
    age      = na_8899_int(age),
    
    inus_raw = suppressWarnings(as.integer(inus)),
    usdur    = na_8899_num(usdur),
    trip_raw = suppressWarnings(as.integer(trip)),
    
    across(all_of(ASSET_VARS), na_8899_num)
  ) |>
  mutate(
    weight = ifelse(is.na(weight) | weight <= 0, NA_real_, weight),
    age_y  = age_at_year(age, year, yrborn),
    
    inus01 = case_when(
      is.na(inus_raw) ~ NA_integer_,
      inus_raw %in% c(0L,1L) ~ as.integer(inus_raw),
      inus_raw == 1L ~ 1L,
      inus_raw == 2L ~ 0L,
      TRUE ~ NA_integer_
    ),
    trip01 = case_when(
      is.na(trip_raw) ~ NA_integer_,
      trip_raw == 1L ~ 1L,
      trip_raw == 2L ~ 0L,
      trip_raw %in% c(0L,1L) ~ as.integer(trip_raw),
      trip_raw > 0L ~ 1L,
      TRUE ~ 0L
    ),
    usdur_m = pmin(pmax(usdur, 0), 12)
  ) |>
  filter(!is.na(commun), !is.na(year), !is.na(weight)) |>
  filter(!is.na(age_y), age_y >= CY_AGE_MIN, age_y <= CY_AGE_MAX)

life_y <- life_raw |>
  group_by(country, commun, surveyyr, hhnum, year, sex, yrborn) |>
  summarise(
    weight  = { w <- suppressWarnings(max(weight, na.rm = TRUE)); if (is.infinite(w)) NA_real_ else as.numeric(w) },
    age_y   = { a <- suppressWarnings(max(age_y,  na.rm = TRUE)); if (is.infinite(a)) NA_integer_ else as.integer(a) },
    inus01  = first_non_na(inus01),
    trip01  = first_non_na(trip01),
    usdur_m = { m <- suppressWarnings(max(usdur_m, na.rm = TRUE)); if (is.infinite(m)) NA_real_ else as.numeric(m) },
    across(all_of(ASSET_VARS), ~ { m <- mean(.x, na.rm = TRUE); if (is.nan(m)) NA_real_ else m }),
    .groups = "drop"
  ) |>
  filter(!is.na(weight), weight > 0) |>
  arrange(country, commun, surveyyr, hhnum, sex, yrborn, year)

life_mig <- life_y |>
  group_by(country, commun, surveyyr, hhnum, sex, yrborn) |>
  mutate(pid = cur_group_id()) |>
  ungroup() |>
  arrange(pid, year) |>
  group_by(pid) |>
  mutate(
    inus_obs  = !is.na(inus01),
    inus1     = (inus01 == 1L),
    year_l1   = lag(year),
    consec    = !is.na(year_l1) & (year == year_l1 + 1L),
    
    new_spell = consec &
      inus_obs & lag(inus_obs, default = FALSE) &
      inus1    & !lag(inus1, default = FALSE),
    
    ever_before = lag(cummax(replace_na(inus1, FALSE)), default = FALSE),
    first_ever_entry = inus_obs & inus1 & !ever_before
  ) |>
  ungroup()

cy_pop <- life_mig |>
  group_by(commun, year) |>
  summarise(pop_w = sum(weight, na.rm = TRUE), .groups = "drop")

cy_trip <- life_mig |>
  filter(!is.na(trip01)) |>
  group_by(commun, year) |>
  summarise(
    trip_pop_w = sum(weight, na.rm = TRUE),
    trip_rate  = ifelse(trip_pop_w > 0, sum(weight * trip01, na.rm = TRUE) / trip_pop_w, NA_real_),
    .groups = "drop"
  )

cy_spell_first <- life_mig |>
  group_by(commun, year) |>
  summarise(
    pop_w2 = sum(weight, na.rm = TRUE),
    spell_flow_rate = ifelse(pop_w2 > 0, sum(weight * new_spell,        na.rm = TRUE) / pop_w2, NA_real_),
    firsttrip_rate  = ifelse(pop_w2 > 0, sum(weight * first_ever_entry, na.rm = TRUE) / pop_w2, NA_real_),
    .groups = "drop"
  ) |>
  transmute(
    commun, year,
    spell_pp     = 100 * spell_flow_rate,
    firsttrip_pp = 100 * firsttrip_rate
  )

cy <- cy_pop |>
  left_join(cy_trip,        by = c("commun","year")) |>
  left_join(cy_spell_first, by = c("commun","year")) |>
  arrange(commun, year)

mig_meta <- read_dta(MIG_PATH, n_max = 0) |> rename_with(tolower)
mig_nm <- names(mig_meta)

pick_mig <- function(cands){
  cands <- tolower(cands)
  hit <- cands[cands %in% mig_nm]
  if (length(hit) == 0) NA_character_ else hit[1]
}

mig_c_country  <- pick_mig(c("country"))
mig_c_commun   <- pick_mig(c("commun"))
mig_c_surveyyr <- pick_mig(c("surveyyr"))
mig_c_hhnum    <- pick_mig(c("hhnum"))
mig_c_weight   <- pick_mig(c("weight","wgt","wt"))
mig_c_usyrl    <- pick_mig(c("usyrl","usyrL","usyr_last","usyr_end","usyr2","usyr"))
mig_c_usdurl   <- pick_mig(c("usdurl","usdurL","usdur_last","usdur2","usdur"))
mig_c_remit    <- pick_mig(c("remit"))

need_mig <- c(mig_c_country, mig_c_commun, mig_c_surveyyr, mig_c_hhnum,
              mig_c_weight, mig_c_usyrl, mig_c_usdurl, mig_c_remit)
if (any(is.na(need_mig))) stop("mig file missing required cols. found: ", paste(mig_nm, collapse=", "))

mig0 <- read_dta(MIG_PATH, col_select = dplyr::all_of(need_mig)) |> rename_with(tolower)

mig <- mig0 |>
  transmute(
    commun   = na_8899_int(.data[[mig_c_commun]]),
    surveyyr = na_8899_int(.data[[mig_c_surveyyr]]),
    weight   = na_8899_num(.data[[mig_c_weight]]),
    usyrl    = na_8899_int(.data[[mig_c_usyrl]]),
    usdurl   = na_8899_num(.data[[mig_c_usdurl]]),
    remit_usd_mo = na_8899_num(.data[[mig_c_remit]])
  ) |>
  mutate(
    weight = ifelse(is.na(weight) | weight <= 0, NA_real_, weight),
    
    span_y  = ifelse(!is.na(usdurl) & usdurl > 0, pmax(1L, as.integer(ceiling(usdurl / 12))), NA_integer_),
    start_y = usyrl,
    end_y   = ifelse(!is.na(start_y) & !is.na(span_y), start_y + span_y - 1L, NA_integer_),
    end_y   = ifelse(!is.na(end_y) & !is.na(surveyyr), pmin(end_y, surveyyr), end_y),
    
    remit_pos = as.integer(!is.na(remit_usd_mo) & remit_usd_mo > 0)
  ) |>
  filter(!is.na(commun), !is.na(weight), !is.na(start_y), !is.na(end_y)) |>
  filter(end_y >= start_y)

mig_py <- mig |>
  mutate(pid = row_number()) |>
  select(pid, commun, weight, start_y, end_y, remit_usd_mo, remit_pos) |>
  tidyr::uncount(weights = (end_y - start_y + 1L), .id = "k") |>
  mutate(year = as.integer(start_y + (k - 1L))) |>
  select(-k, -start_y, -end_y)

cy_mig_num <- mig_py |>
  group_by(commun, year) |>
  summarise(
    send_w = sum(weight * remit_pos, na.rm = TRUE),
    amt_w  = sum(weight * remit_usd_mo * remit_pos, na.rm = TRUE),
    .groups = "drop"
  )

cy_mig_num_state <- mig_py |>
  left_join(comm_state, by = "commun") |>
  filter(!is.na(state)) |>
  group_by(state, year) |>
  summarise(
    send_w_state = sum(weight * remit_pos, na.rm = TRUE),
    amt_w_state  = sum(weight * remit_usd_mo * remit_pos, na.rm = TRUE),
    .groups = "drop"
  )

cy_pop_state <- cy |>
  left_join(comm_state, by = "commun") |>
  filter(!is.na(state)) |>
  group_by(state, year) |>
  summarise(pop_w_state = sum(pop_w, na.rm = TRUE), .groups = "drop")

if (FALLBACK_NATIONAL) {
  cy_mig_num_nat <- mig_py |>
    group_by(year) |>
    summarise(
      send_w_nat = sum(weight * remit_pos, na.rm = TRUE),
      amt_w_nat  = sum(weight * remit_usd_mo * remit_pos, na.rm = TRUE),
      .groups = "drop"
    )
  cy_pop_nat <- cy |>
    group_by(year) |>
    summarise(pop_w_nat = sum(pop_w, na.rm = TRUE), .groups = "drop")
}

cy <- cy |>
  left_join(cy_mig_num, by = c("commun","year")) |>
  left_join(comm_state, by = "commun") |>
  left_join(cy_mig_num_state, by = c("state","year")) |>
  left_join(cy_pop_state, by = c("state","year"))

if (FALLBACK_NATIONAL) {
  cy <- cy |>
    left_join(cy_mig_num_nat, by = "year") |>
    left_join(cy_pop_nat,     by = "year")
}

cy <- cy |>
  mutate(
    remit_send_pp_comm      = ifelse(!is.na(pop_w) & pop_w > 0, 100 * send_w / pop_w, NA_real_),
    remit_mean_usd_mo_comm  = ifelse(!is.na(send_w) & send_w > 0, amt_w / send_w, NA_real_),
    
    remit_send_pp_state     = ifelse(!is.na(pop_w_state) & pop_w_state > 0, 100 * send_w_state / pop_w_state, NA_real_),
    remit_mean_usd_mo_state = ifelse(!is.na(send_w_state) & send_w_state > 0, amt_w_state / send_w_state, NA_real_),
    
    remit_send_pp     = coalesce(remit_send_pp_comm,     remit_send_pp_state),
    remit_mean_usd_mo = coalesce(remit_mean_usd_mo_comm, remit_mean_usd_mo_state)
  )

if (FALLBACK_NATIONAL) {
  cy <- cy |>
    mutate(
      remit_send_pp_nat     = ifelse(!is.na(pop_w_nat) & pop_w_nat > 0, 100 * send_w_nat / pop_w_nat, NA_real_),
      remit_mean_usd_mo_nat = ifelse(!is.na(send_w_nat) & send_w_nat > 0, amt_w_nat / send_w_nat, NA_real_),
      remit_send_pp     = coalesce(remit_send_pp,     remit_send_pp_nat),
      remit_mean_usd_mo = coalesce(remit_mean_usd_mo, remit_mean_usd_mo_nat)
    )
}

pers_need <- c("country","commun","surveyyr","hhnum","relhead","marstat","usyr1","sex","yrborn","age","edyrs")
pers_meta <- read_dta(PERS_PATH, n_max = 0)
pers_use  <- resolve_cols(names(pers_meta), pers_need)

pers <- read_dta(PERS_PATH, col_select = all_of(pers_use)) |>
  rename_with(tolower) |>
  mutate(
    country  = na_8899_int(country),
    commun   = na_8899_int(commun),
    surveyyr = na_8899_int(surveyyr),
    hhnum    = na_8899_int(hhnum),
    
    relhead  = suppressWarnings(as.integer(relhead)),
    marstat  = suppressWarnings(as.integer(marstat)),
    usyr1    = suppressWarnings(as.integer(usyr1)),
    
    sex      = na_8899_int(sex),
    yrborn   = na_8899_int(yrborn),
    age_svy  = age_at_svy(as.integer(age), surveyyr, yrborn),
    
    edyrs    = suppressWarnings(as.numeric(na_8899_int(edyrs)))
  ) |>
  filter(!is.na(commun), !is.na(surveyyr), !is.na(hhnum))

# head must exist and have non-missing usyr1
head <- pers |>
  filter(relhead == 1L) |>
  group_by(across(all_of(hh_keys))) |>
  summarise(
    head_usyr1  = first_non_na(usyr1),
    head_sex    = first_non_na(sex),
    head_yrborn = first_non_na(yrborn),
    head_edyrs  = suppressWarnings(as.numeric(first_non_na(edyrs))),
    .groups = "drop"
  ) |>
  filter(!is.na(head_usyr1))

spouse <- pers |>
  filter(relhead == 2L) |>
  group_by(across(all_of(hh_keys))) |>
  summarise(
    spouse_usyr1  = first_non_na(usyr1),
    spouse_sex    = first_non_na(sex),
    spouse_yrborn = first_non_na(yrborn),
    spouse_edyrs  = suppressWarnings(as.numeric(first_non_na(edyrs))),
    .groups = "drop"
  )

hh_other_emig <- pers |>
  filter(!(relhead %in% c(1L,2L))) |>
  group_by(across(all_of(hh_keys))) |>
  summarise(
    hh_other_ever_emig = as.integer(any(ever_migrated(usyr1), na.rm = TRUE)),
    .groups = "drop"
  )

hh_parents <- head |>
  left_join(spouse, by = hh_keys) |>
  left_join(hh_other_emig, by = hh_keys) |>
  mutate(
    hh_other_ever_emig = replace_na(hh_other_ever_emig, 0L),
    
    head_never = never_migrated_strict(head_usyr1),
    spouse_present = !is.na(spouse_usyr1),
    spouse_never = ifelse(spouse_present, never_migrated_strict(spouse_usyr1), TRUE),
    parents_never = head_never & spouse_never,
    
    mother_edyrs = ifelse(head_sex == 2L, head_edyrs,
                          ifelse(!is.na(spouse_sex) & spouse_sex == 2L, spouse_edyrs, NA_real_)),
    father_edyrs = ifelse(head_sex == 1L, head_edyrs,
                          ifelse(!is.na(spouse_sex) & spouse_sex == 1L, spouse_edyrs, NA_real_)),
    single_parent = as.integer(is.na(spouse_usyr1))
  )

youth <- pers |>
  inner_join(hh_parents, by = hh_keys) |>
  filter(parents_never) |>
  filter(!is.na(age_svy), age_svy >= YOUTH_AGE_MIN, age_svy <= YOUTH_AGE_MAX) |>
  filter(!is.na(yrborn)) |>
  filter(!is.na(edyrs), edyrs >= PRIMARY_COMPLETE_YEARS) |>
  mutate(educ_ge7 = as.integer(edyrs >= OUTCOME_YEARS))

youth <- youth %>% filter(yrborn + PRIMARY_AGE_MAX >= MIN_PRIMARY_YEAR)

if (STRICT_6V7_ONLY) youth <- youth |> filter(edyrs %in% c(6,7))
if (REQUIRE_CHILD_RELHEAD) youth <- youth |> filter(relhead == 3L)

house_need <- c("country","commun","surveyyr","hhnum","remus","remussiz")
house_meta <- read_dta(HOUSE_PATH, n_max = 0)
house_use  <- resolve_cols(names(house_meta), house_need)

house0 <- read_dta(HOUSE_PATH, col_select = all_of(house_use)) |> rename_with(tolower)

house_hh <- house0 |>
  transmute(
    country  = na_8899_int(country),
    commun   = na_8899_int(commun),
    surveyyr = na_8899_int(surveyyr),
    hhnum    = na_8899_int(hhnum),
    
    remus    = na_8899_int(remus),
    remussiz = na_8899_int(remussiz),
    remus01  = case_when(remus == 1L ~ 1L, remus == 2L ~ 0L, TRUE ~ NA_integer_)
  ) |>
  group_by(across(all_of(hh_keys))) |>
  summarise(
    remus_any = as.integer(any(remus01 == 1L, na.rm = TRUE)),
    remus_size_max = { m <- suppressWarnings(max(remussiz, na.rm = TRUE)); if (is.infinite(m)) NA_integer_ else as.integer(m) },
    .groups = "drop"
  )

youth <- youth |> left_join(house_hh, by = hh_keys)

life_head_year <- life_y |>
  inner_join(head |> select(all_of(hh_keys), head_sex, head_yrborn), by = hh_keys) |>
  filter(sex == head_sex, yrborn == head_yrborn) |>
  transmute(
    across(all_of(hh_keys)),
    year = as.integer(year),
    land     = clean_asset(land),
    hectars  = clean_asset(hectars),
    property = clean_asset(property),
    business = clean_asset(business)
  ) |>
  mutate(
    wealth_any_y = as.integer(
      coalesce(hectars,0)  > 0 |
        coalesce(land,0)     > 0 |
        coalesce(property,0) > 0 |
        coalesce(business,0) > 0
    ),
    wealth_raw_y = log1p(pmax(hectars,0)) +
      log1p(pmax(land,0)) +
      log1p(pmax(property,0)) +
      log1p(pmax(business,0))
  )

ages_primary <- PRIMARY_AGE_MIN:PRIMARY_AGE_MAX

y_exp <- youth |>
  select(
    all_of(hh_keys),
    country, commun, surveyyr, hhnum,
    sex, yrborn, age_svy,
    edyrs, educ_ge7,
    hh_other_ever_emig,
    mother_edyrs, father_edyrs,
    single_parent,
    remus_any, remus_size_max
  ) |>
  distinct() |>
  mutate(youth_id = row_number())

prim_years <- y_exp |>
  select(youth_id, all_of(hh_keys), commun, yrborn) |>
  tidyr::crossing(age_primary = ages_primary) |>
  mutate(primary_year = as.integer(yrborn + age_primary)) |>
  filter(primary_year >= MIN_PRIMARY_YEAR) |>
  select(youth_id, all_of(hh_keys), commun, primary_year)

prim_controls <- prim_years |>
  left_join(life_head_year, by = c(hh_keys, "primary_year" = "year")) |>
  group_by(youth_id) |>
  summarise(
    wealth_raw_prim = { m <- mean(wealth_raw_y, na.rm = TRUE); if (is.nan(m)) NA_real_ else m },
    wealth_any_prim = as.integer(any(wealth_any_y == 1L, na.rm = TRUE)),
    n_prim_years_wealth = sum(!is.na(wealth_raw_y)),
    .groups = "drop"
  )

prim_env <- prim_years |>
  left_join(
    cy |>
      select(commun, year, trip_rate, firsttrip_pp, spell_pp, remit_send_pp, remit_mean_usd_mo),
    by = c("commun","primary_year" = "year")
  ) |>
  left_join(shock2, by = c("primary_year" = "year")) |>
  left_join(base_pratio_tbl, by = "commun") |>
  group_by(youth_id) |>
  summarise(
    mig_pp_primary       = mean(100 * trip_rate, na.rm = TRUE),
    firsttrip_pp_primary = mean(firsttrip_pp,    na.rm = TRUE),
    spell_pp_primary     = mean(spell_pp,        na.rm = TRUE),
    
    remit_send_pp_primary     = mean(remit_send_pp,     na.rm = TRUE),
    remit_mean_usd_mo_primary = mean(remit_mean_usd_mo, na.rm = TRUE),
    n_prim_years_remit = sum(!is.na(remit_send_pp)),
    
    shock_primary    = mean(shock, na.rm = TRUE),
    baseline_pratio  = first(baseline_pratio),
    n_primary_years_shock = sum(!is.na(shock)),
    .groups = "drop"
  ) |>
  mutate(z_primary = baseline_pratio * shock_primary) |>
  filter(!is.na(baseline_pratio), !is.na(shock_primary), n_primary_years_shock > 0)

y_primary <- y_exp |>
  left_join(prim_env,      by = "youth_id") |>
  left_join(prim_controls, by = "youth_id")

cat("\n--- final y_primary sanity ---\n")
print(y_primary |>
        summarise(
          n = n(),
          share_outcome = mean(!is.na(educ_ge7)),
          share_z = mean(!is.na(z_primary)),
          share_remit_send = mean(!is.na(remit_send_pp_primary)),
          share_remit_amt  = mean(!is.na(remit_mean_usd_mo_primary))
        )
)

y_primary <- y_primary %>%
  mutate(
    hh_id_str = paste(country, commun, surveyyr, hhnum, sep = "_"),
    hh_id = as.integer(factor(hh_id_str))
  )


y_w <- y_primary %>% dplyr::filter(remus_any == 0)

y_w$remit_per_person_primary <- y_w$remit_mean_usd_mo_primary * y_w$remit_send_pp_primary

y_w <- y_w %>%
  filter(!if_any(c(wealth_raw_prim, mother_edyrs, father_edyrs, remit_per_person_primary), is.na))

summary(y_w$educ_ge7)

summary(y_w$yrborn)

ols <- feols(
  educ_ge7 ~
    spell_pp_primary +
    remit_per_person_primary +
    wealth_raw_prim + 
    mother_edyrs + father_edyrs +
    hh_other_ever_emig + 
    spell_pp_primary +
    mig_pp_primary +
    i(sex)
  | commun + yrborn,
  data = y_w,
  vcov = CLUSTER_VCOV
)
print(summary(ols))


m_iv <- feols(
  educ_ge7 ~
    wealth_raw_prim +
    remit_per_person_primary +
    mother_edyrs + 
    father_edyrs +
    hh_other_ever_emig +
    mig_pp_primary +
    i(sex)
  | commun + yrborn + hh_id_str
  | spell_pp_primary ~ z_primary,
  data = y_w,
  vcov = CLUSTER_VCOV
)
print(summary(m_iv))
print(summary(m_iv, stage = 1))


y_boys <- y_w %>% dplyr::filter(sex == 1)

y_girls <- y_w %>% dplyr::filter(sex == 2)

m_boy_iv <- feols(
  educ_ge7 ~
    wealth_raw_prim +
    remit_per_person_primary +
    mother_edyrs + 
    father_edyrs +
    hh_other_ever_emig +
    mig_pp_primary +
    i(sex)
  | commun + yrborn
  | spell_pp_primary ~ z_primary,
  data = y_boys,
  vcov = CLUSTER_VCOV
)
print(summary(m_boy_iv))
print(summary(m_boy_iv, stage = 1))


m_girl_iv <- feols(
  educ_ge7 ~
    wealth_raw_prim +
    remit_per_person_primary +
    mother_edyrs + 
    father_edyrs +
    hh_other_ever_emig +
    mig_pp_primary +
    i(sex)
  | commun + yrborn
  | spell_pp_primary ~ z_primary,
  data = y_girls,
  vcov = CLUSTER_VCOV
)
print(summary(m_girl_iv))
print(summary(m_girl_iv, stage = 1))
