# Elixir-GR ML

**Description:**  


## Methods

### Data

The input datasets for this project are sourced from [OpenAlex: The open catalog to the global research system](https://openalex.org/).

### Repository Structure
- `/R`: Code scripts for analysis.
- `/output`: Output files (e.g., figures, tables)s.

## Processing steps
- OpenAlex PMIDs were matched with relevant MeSH terms.
- Normalization involved calculating the frequency of each term relative to the total number of PMIDs.
- Filtering retained terms that appeared in more than 0.1% of instances.
- Data was visualized using heatmaps.


## License

MIT License
