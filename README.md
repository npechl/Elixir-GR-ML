# Mapping the AI Life Sciences Landscape in Greece: A National Survey and Bibliometric Comparison with Global Trends
The repository contains all methodological resources for mapping AI within the Greek landscape. Two complementary approaches were employed: a data-driven analysis of 916,824 AI-related life-science publications harvested from OpenAlex and PubMed, and a targeted national-level survey directed at researchers, engineers, and support staff to serve as supporting material. Each publication was tagged with Medical Subject Headings (MeSH), and topic frequencies were compared between articles affiliated with at least one Greek institution and those from the rest of the world.

## Methods

### Data
The datasets that were used for the present study can be found at:
1. Chatzopoulos, S., Vichos, K., Adamidi, E., & Vergoulis, T. (2025). Publications related to AI, ML, and NLP within Medicine and Biology_OpenAlex [Data set]. Zenodo. https://doi.org/10.5281/zenodo.17415352
2. Adamidi, E., Dimopoulos, A., Krithara, A., Psomopoulos, F., & Vergoulis, T. (2025). A survey on Machine Learning for Life Sciences in Greece. Zenodo. https://doi.org/10.5281/zenodo.17287162
3. Adamidi, E., Dimopoulos, A., Krithara, A., Psomopoulos, F., & Vergoulis, T. (2025). A survey on Machine Learning for Life Sciences in Greece - Responses. Zenodo. https://doi.org/10.5281/zenodo.17414392
4. Nentidis, A., & Krithara, A. (2025). Suplementary data for the analysis the AI Life Sciences Landscape in Greece. [Data set]. Zenodo. https://doi.org/10.5281/zenodo.17407809

### Repository Structure
- `/Python`: Python scripts for analysis.
- `/R`: R scripts for analysis.
- `/output`: Output files (e.g., figures, tables)s.

## Processing steps
1. OpenAlex PMIDs were matched with relevant MeSH terms.
2. Normalization involved calculating the frequency of each term relative to the total number of PMIDs.
3. Pairwise wilcoxon test was applied to identify significant differences between the number of topics of Greece related publications and the rest of the world.
4. Data was visualized using heatmaps, barplots etc.

## License
This work is licensed under the [MIT license](https://opensource.org/license/mit).
