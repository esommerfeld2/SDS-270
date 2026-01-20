#fitler for one speices
df_sp <- data21[data21[["species"]] == "tsugca", ]

#extract the quadrat
df_sp$quad <- substr(df_sp$quad_sub_quad, 1, 4)


df_sp_count <- df_sp |>
  group_by(.data[["quad"]]) |>
  summarise(
    counts=  n()
  ) |> arrange(counts) |>
  slice(1:5) 
ts_e <- data21 |>
  filter(species == "tsugca")