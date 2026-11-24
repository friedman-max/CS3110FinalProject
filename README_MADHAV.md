# Recognition experiments and evaluation

This contribution adds recognition experiments alongside the existing `Knn`
baseline. `Knn.predict` remains unchanged and is included as a comparison row.

`Knn_experiments` compares k values 1, 3, and 5; Euclidean and Manhattan
distance; and uniform and inverse-distance voting. `Evaluation.evaluate` reports
overall and per-category held-out accuracy and average prediction time. The demo
uses the same five-category training and test split as the baseline demo.

Prepare the dataset and run the comparison from the project root:

```sh
python3 scripts/prepare_quickdraw.py
dune exec bin/demo_knn_experiments.exe
```

The prepared split contains 100 training and 25 held-out drawings for each of
cat, tree, house, bicycle, and airplane. The script uses a fixed shuffle seed,
so every configuration is tested against the same split. This is still a small
initial subset of the Quick, Draw! category files, so the results are
preliminary.

In one run, the baseline k=3 Euclidean/uniform configuration matched the
existing result of 106/125 (84.8%). The experiments scored 104/125 for k=1
Euclidean, 102/125 for k=1 Manhattan, 106/125 for k=3 with either distance,
and 105/125 for k=5 with either distance. Inverse-distance voting produced the
same correct counts as uniform voting on this split. The demo prints per-class
accuracy and average prediction time for each configuration.

Seven OUnit tests cover Manhattan distance, invalid distances, weighted voting,
exact-match handling, invalid k, baseline compatibility, and evaluation
accuracy. Prediction times are for comparing configurations on the same machine
and are not formal benchmarks.
