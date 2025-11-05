dir.create("input/tmp")

R.utils::gunzip(filename = "./input/mesh/desc2024_TN.csv.gz", destname = "./input/tmp/desc2024_TN.csv", remove = FALSE)
R.utils::gunzip(filename = "./input/mesh/MeSH from PMID result_v2.csv.gz", destname = "./input/tmp/MeSH from PMID result_v2.csv", remove = FALSE)
R.utils::gunzip(filename = "./input/openalex/output_openalex-affil.gz", destname = "./input/tmp/output_openalex-affil", remove = FALSE)
