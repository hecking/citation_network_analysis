# Explicit semantic analysis on titles and abstracts of papers retrieved from MAG
Explicit semantic analysis requires a database containing the indexed ESA vectors for a set of terms. 
This script uses the following SQLite file that can simply be accessed by commonly used programming language for 
data science such as R and Python without any additional database software: 
[https://github.com/collide-uni-due/esa_db/]. 

## How to use the script esa_concept_extractor.R

First, download the esa.db datafile from [here](https://uni-duisburg-essen.sciebo.de/s/kYNVLdrE7KycpSJ]) and save 
it best in the same folder as the esa_concept_extractor.R script. See also the information about how to access the data at 
[https://github.com/collide-uni-due/esa_db/].

Make sure that the necessary R libraries are installed. For accessing SQLite databases the DBI and RSQLite packages are required.
```R
install.packages(c("DBI", "RSQLite"))
```
 To run the script additional packages are required (see the require block at the beginning of the script).

The script contains an example() function that also shows the necessary steps to query the ESA vectors for certain papers.

```R
example()
```

Additional information is available in the notebook file.
