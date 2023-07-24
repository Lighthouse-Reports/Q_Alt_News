# Q_Alt_News
Private repository for working on the alt news networks underpinning the European Q Anon ecosystem in collaboration between Lighthouse Reports, Bloomberg, Tow, and the NZZ.

# Main Pipeline
- Query: https://euq.bellingcat.com/queries/123/source#153 Use this query as a template to find posts matching a particular subnarrative
- [Spread analysis](https://github.com/Lighthouse-Reports/Q_Alt_News/blob/main/code/230724_justin_spread_template.R): This template works on results returned from the above query. Analysis includes posts over time and by language, most frequent posters, most succsessful posts, and most shared links. Additional tests can be easily integrated. The template is currently implemented for the [15 minutes cities dataset]

# Repo Overview
Use Rmarkdown files and Jupyter Notebooks and comment or describe what the code is doing. If large files cannot be saved in our common repository, use .gitignore to keep them on your disk and save to the Drive instead.

- Code: this is where our notebooks and markdowns live. Use subfolders for specific investigations. 
- Data: This is where the input data that is ingested by various notebooks/markdowns live. Use subfolders corresponding to code subfolders
- Results: This is where you can save tables, figures, CSVs etc. Again, please use the same subfolders as you did for Code and Data.

# File naming conventions: 
date_name_topic, e.g., 220523_justin_network_uk_climate
When you generate outputs, e.g., intermediary results, please set up the scripts so that file names are generated following this convention.

# Featured Notebooks
[Edit this to point people to the most important notebooks you have pushed to the repo]
