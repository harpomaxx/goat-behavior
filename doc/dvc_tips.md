## Select initial features

Since `initial.features` is a list of features, a file with a python list is used. `params-no-logs.py` is
a file with a python list containing all the selected features.
The the experiment could be run as follow
```
```
dvc exp run -n "no-log" --queue --set-param=select_initial.features="`cat params-no-logs.py`"
```

## showing results

```
dvc exp show --drop ".*" --keep ".*loo.*mean_Sen.*|Experiment"

```
