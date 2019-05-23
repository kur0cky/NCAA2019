# spectrum envelope
# spectral density from AR fit
library(tidtverse)
library(data.table)

index <- c(0)
spec_ar_mean <- spec_ar_mode <- c()
for(i in 1:17){
  df <- fread(str_c("data/processed/quake_", i, ".csv"))
  index[i+1] <- nrow(df) %/% 150000
  
  for(j in 1:(nrow(df) %/% 150000)){
    df_seq <- df[((j-1)*150000 + 1):(j*150000),,]
    spec <- spec.ar(df_seq$acoustic_data, plot=FALSE)
    spec_ar_mean[sum(index[1:i])+j] <- sum(spec$spec/sum(spec$spec) * spec$freq)
    spec_ar_mode[sum(index[1:i])+j] <- spec$freq[which(spec$spec == max(spec$spec))]
    print(sum(index[1:i])+ j)
  }
}

tr_spec <- data.frame(id = str_c("train_", 1:length(spec_ar_mean)),
                      spec_ar_mean = spec_ar_mean,
                      spec_ar_mode = spec_ar_mode)

write_csv(tr_spec, "data/features/tr_spec_ar.csv")

paths <- list.files("data/test")
spec_ar_mean <- spec_ar_mode <- c()
for(i in 1:length(list.files("data/test"))){
  seq_df <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  spec <- spec.ar(seq_df$acoustic_data, plot=FALSE)
  spec_ar_mean[i] <- sum(spec$spec/sum(spec$spec) * spec$freq)
  spec_ar_mode[i] <- spec$freq[which(spec$spec == max(spec$spec))]
  if(i %% 10 == 0)print(i)
}
te_spec <- data.frame(id = str_remove(paths, ".csv"),
                      spec_ar_mean = spec_ar_mean,
                      spec_ar_mode = spec_ar_mode,
                      stringsAsFactors = FALSE)

write_csv(te_spec, "data/features/te_spec_ar.csv")
