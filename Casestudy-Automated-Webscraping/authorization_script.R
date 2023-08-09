gs4_auth() # Manual authorization to write to your google drive

# Write first time.

if(gs4_has_token()){
gs4_create("unnested_platform",sheets=plat)
gs4_create("unnested_rating",sheets=rate)
gs4_create("RAWG_2019",sheets=final_data_tidy)}
