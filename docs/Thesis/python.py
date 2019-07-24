import scipy
import json
import pandas

from tabulate import tabulate

root = '/Users/mayrop/Github/thesis/Thesis'

df = pandas.read_csv(root + '/Data/Appendices/appendix-02.csv')
# print(df)
print(tabulate(df, colalign=("left", "left"), headers=["Old Variable Name", "New Variable Name", "Formula"],
               tablefmt="latex_booktabs", floatfmt=".2f", showindex=False))

# print(tabulate(appendix1, colalign=("left", "left"), headers=headers,
#                tablefmt="latex_booktabs", floatfmt=".2f", showindex=False))
