./main.mpl @mpl procs 20 set-affinity megablock-threshold 14 cc-threshold-ratio 1.1 collection-threshold-ratio 2.0 max-cc-depth 1 -- -dense-thresh 1.1 --scheduler-disable-fusion -scheduler $1 -input $2
# -scheduler-max-branching-stride 1
#  --scheduler-disable-fusion
# -dense-thresh 0.75
# -pull-thresh 0.01
