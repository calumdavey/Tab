# Tab
STATA Tab-like function in R

Once a data frame is 'set' by making a copy called `tab.data`, then the function can be used like the `tab` function in STATA:

- `tab(variable-name)` will produce a frequency table 
- `tab(variable-name-1, variable-name-2)` will produce a cross-tabulated frequency table 
- the option `, m=T` will include missing values 

