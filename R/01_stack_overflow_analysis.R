


library(data.table)
library(stringr)



fls = "data/stack-overflow/" |> 
    list.files(
        full.names = TRUE, recursive = TRUE, 
        pattern = "survey_results_public"
    )

fls = fls[7:length(fls)]

years = fls |> 
    str_split_i("\\/", 3) |> 
    str_remove_all("stack|overflow|developer|survey|\\-") |> 
    as.numeric()

df = list()

for(i in seq_along(fls)) {
    
    q = fls[i] |> fread(header = TRUE)
    
    q$year = years[i]
    
    df[[i]] = q
    
}

df = df |> rbindlist(use.names = TRUE, fill = TRUE)

df_gr = df[which(Country == "Greece")]

df |> nrow()
df_gr |> nrow()

writexl::write_xlsx(df_gr, "data/stack-overflow-greece.xlsx")

# empty_columns = list()
# 
# for(i in colnames(df_gr)) {
#     
#     if(df_gr[[i]] |> is.na() |> all()) empty_columns[[i]] = i
#     
# }






