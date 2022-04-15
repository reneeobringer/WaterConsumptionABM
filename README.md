# WaterConsumptionABM

This is the reposotroty for the data and code associated with an agent-based model (ABM) that models water consumption in Phoenix, Arizona. The resutls obtained through this work are under review: 

**Add the citation here**

The ABM was developed in NetLogo (v6.2.2) and was last run on **Add date**. Running the NetLogo code requires several extensions: 

*  array
*  csv
*  profiler
*  time

Additionally, there are two csv files that are required as inputs, which can be found in the `inputData` folder.

The model runs best in the headless form of NetLogo, as it requires substantial computational resources. To run this code, we reommend the following commands. Note this code was developed for a Linux server using the SLURM workload manager. Users working in different systems or with different workload managers may need to adapt the code to meet their system requirements (see https://slurm.schedmd.com/rosetta.html for a guide for translating between SLURM and other wotkload managers). To run the code on a remote system, we recommend the following code. 

First, copy the data over to the remove machine:

```shell
scp -r WaterConsumptionABM-main [remote machine address]:[remote machine path]
```

Then, run `jobscript.sh`:

```shell
sbatch jobscript.sh
```

Finally, there is an `R script` associated with the NetLogo script that contains the calculations of the paramters for various distributions used in this analysis.
