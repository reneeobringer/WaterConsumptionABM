# WaterConsumptionABM

This is the repository for the data and code associated with an agent-based model (ABM) that models water consumption in Phoenix, Arizona. The results obtained through this work are currently in preparation for submission to _Environmental Modelling and Software_.

The ABM was developed in NetLogo (v6.2.2) and was last run on 11 August 2022. Running the NetLogo code requires several extensions: 

*  array
*  csv
*  profiler
*  time

Additionally, there are two csv files that are required as inputs, which can be found in the `inputData` folder.

The model runs best in the headless form of NetLogo, as it requires substantial computational resources. The code was developed on a Linux server using the SLURM workload manager. Users working in different systems or with different workload managers may need to adapt the code to meet their system requirements (see https://slurm.schedmd.com/rosetta.html for a guide for translating between SLURM and other workload managers). To run the code on a remote system, we recommend the following code. 

First, copy the data over to the remove machine:

```shell
scp -r WaterConsumptionABM [remote machine address]:[remote machine path]
```

Or, use git commands directly on the remote machine: 

```shell
git clone https://github.com/reneeobringer/WaterConsumptionABM
```

Once the repository is loaded on the remote machine, run `jobscript.sh`:

**will need to update to specify any changes needed in jobscript.sh**

```shell
sbatch --partition=[your partition here] --output=jobscript.out --time=7-00:00:00 jobscript.sh
```

Note that the code is set up to run 100 experiments with six different scenarios, for a total of 600 model runs. We ran the code with one high memory node (40 CPUs) and it took just over 5 days to complete all 600 runs. 

Finally, there is an `ABM_supplementalcalc.R` associated with the NetLogo script that contains the calculations of the paramters for various distributions used in this analysis, as well as the post-processing and plotting of the modeled data. This file does not need to be run in order run the NetLogo script, but can be used as a reference for the inputs, as well as viewing code for plotting figures. To run this script, users will need to update the `path` variable to the downloaded or cloned respository. Additionally, the code can be run sequentially, or individual sections can be reun separately, if users load the r data files prior to running. These files are located in the `rdatafiles` folder. The data used within this R script is in the `hydrodata` folder.
