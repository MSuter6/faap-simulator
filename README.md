# FAAP-Simulator
_Author: Marco Suter_

This simulator software was developed in the context of the Master Thesis "Fog Application Allocation in Automation and Control Systems".  
The thesis addresses application allocation in the area of fog computing and targets fog networks in industrial environments.
With the simulator, it is possible to generate, run and evaluate instances of the Fog Application Allocation Problem denoted as FAAP.
The simulator is completely implemented in the *scala* programming language. 

To get a better understanding of how the simulator works, it is recommended to take a look at the test files in the ```test.scala```-folder which has various test classes which implement test cases.

## Getting Started
To run the simulations, we recommend the use the import function of Intellij IDEA IDE using the _scala_-plugin (_File -> New -> Project from Version Control_).

### Basic Requirements
Install the Java JDK (http://www.oracle.com/technetwork/java/javase/downloads/index.html). This is needed as _scala_ runs on the JVM. 
Running the software without an IDE, install the scala build tool SBT (https://www.scala-sbt.org/download.html) which includes _scala_. 

### Optional Requirements
Following software is not strictly required to be installed but is recommended as certain features can't be used without them:

1. ILP Solvers: To run experiments using the optimal solution, an linear programming solver is needed. 
Uses _OptimalAllocator_-class. The *ZIMPL* tool is needed to generate _.lp_ files for the solvers. 
Two solvers supported: 
    - SCIP: Download from http://scip.zib.de/download.php?fname=SCIPOptSuite-5.0.1-win64-VS15.exeand add them to the system path
    - Gurobi: https://user.gurobi.com/download/licenses/free-academic
2. Tex-Plots: To produce pngs from the latex plots, install (also add them to the system/environment variables): 
    - Latex Implementation (e.g. MiKTeX, https://miktex.org/), 
    - ImageMagick http://www.imagemagick.org/script/download.php
    - Ghostscript: https://ghostscript.com/download/. 
3. Graph Visualizations: To produce pdfs from graphs, the graphviz software (https://graphviz.gitlab.io/) has to be installed and the 'dot'-command added to the system path.

### Build and Test
The Scala Build Tool (SBT) is used to build the simulator.
To run the FAAP-Simulator from the command line, run ```sbt run``` to build the software and follow the instructions the run the desired experiment. 
There exist tests for the core functionality of the simulator found in ```./src/test/scala```. To run them, execute ```sbt test```.

### Software structure

This software consists of 3 basic packages:

1. ```./experiments/```: Contains the experiment definitions and basic classes to run the experiments 
    - Object _ExperimentCommon_ contains default values for the experiments configurations
    - Classes _ExperimentRunner_ and _MultiRepetitionExperimentRunner_ contain the basic framework for the experiments
2. ```./io/```: Input output tools for experiment artifact export and import.
3. ```./simulator/```: Core of the simulation software 
    - ```./allocation/```: Contains the allocators 
    - ```./evaluation/```: Functions to evaluate a _Mapping_ of a _FaapProblemInstance_ i.e. constraint violation checks and cost computations
    - ```./generation/```: Functionality for problem instance generation, makes heavy use of _GraphGenerator_-class.
4. _./GlobalConfig_: Contains global config values such as random seed or timestamp format. 

## Experiments
The experiment pipeline is built in such a way that experiments can be run in a easily configurable and fully automated manner.

### Predefined Experiments
Experiment export which were used throughout the thesis. Rerun to reproduce results.
```bash
./results/
│   ├── scale/                      Each of the following experiments includes various files (see Readme.md for further explanation)
│   │   ├── 08-08-2018.13-02-05_constFogGraphOrder_final/*          
│   │   ├── 08-08-2018.14-29-20_constRatio_final/*				Ratio of 2 (fog order vs app order), large scale            
│   │   ├── 08-08-2018.14-38-52_constAppGraphOrder_final/*        
│   │   ├── 09-08-2018.07-46-52_constRatioSmallScale_final/*      
│   │   ├── 09-08-2018.11-15-24_constAppGraphOrder_final/*
│   │   ├── 09-08-2018.15-49-34_fogDensity_final/*        
│   │   ├── 09-08-2018.16-17-43_appNodeCap_final/*
│   │   ├── 09-08-2018.16-38-12_fogNodeCap_final/*
│   │   ├── 09-08-2018.16-55-17_lbCount_final/*  
│   ├── ABBuseCase/
│   │   ├── 12-08-2018.16-31-17_abbControlAppToCactus_final.zip/*		Small scale (optimally solvable), (large file due to solver artifacts)
│   │   ├── 14-08-2018.09-39-17_abbControlAppToCactusLargeScale_final/*    	Large scale (heuristics)
│   ├── latencyPaths/
│   │   ├── 28-08-2018.11-30-43_lpCountSolverComp/*
└── ├── solverComparison/
    │   ├── 10-08-2018.09-24-59_solverComparison_final/*       
    └── improvedHeuristic/
        └── 20-08-2018.16-04-38_constRatioCombHeuristics_final/*  
```
To use data from previous experiments, it is recommended to us the ```scala.experiment.ExperimentReevaluator```. This allows to modify the experiment configuration in various ways and can be adapted according to your needs..

### Exporting Experiments
All data generated by the experiments is exported with the call ```ExperimentExported.writeExperiment```.
Refer to the ```test.scala.io.ExperimentImporterTest``` to see an example of how an experiment can be exported. 
#### Structure of an exported Experiment folder
A full experiment export produces a folder of following directory structure: 

```bash
./type/                                           Experiments evaluating scale of instance
├── xx-xx-xxxx.xx-xx-xx_name/                     Experiment name with prepending timestamp
│   ├── fom_problem_instances/                    Experiment instances
│   │   ├── xx-yy-zz/                             Experiment instance named by configuration (xx: order, yy: xAxis value, zz: Description)
│   │   │   ├── r0/                               Repetition 0
│   │   │   │   ├── app_graph.dot                 .Dot representation of the app graph
│   │   │   │   ├── app_graph.png                  Visualization of the app graph (optional)                                      
│   │   │   │   ├── app_graph_node_weights.dot    Node weights of app graph with full precision                                    
│   │   │   │   ├── fog_graph.dot                 .Dot representation of the fog graph
│   │   │   │   ├── fog_graph.png                 Visualization of the fog graph (optional)                                      
│   │   │   │   ├── fog_graph_node_weights.dot    Node weights of fog graph with full precision                 
│   │   │   │   ├── location_bound_mapping.txt    Location-bound mapping                                    
│   │   │   │   ├── latency_paths.txt             Latency paths (optional).                                    
│   │   │   ├── rx/*                              Repetition x      
│   ├── out_temp/*                               Plots (on latex/pgfplots basis)
│   ├── solutions/                               Experiment solutions
│   │   ├── xx-yy-zz/                            Experiment instance named by configuration (xx: order, yy: xAxis value, zz: Description)
│   │   │   ├── r0/                              Repetition 0
│   │   │   │   ├── evaluation.txt               Evaluation: costs and whether constraints were violated
│   │   │   │   ├── mapping.dot                 .Dot representation of the mapping
│   │   │   │   ├── mapping.png                  Mapping visualization (fog graph with embedded app graph)
│   │   │   │   ├── timestamps.txt              Timestamps: runtime, eval time
│   │   ├── rx/*                                Repetition x      
│   ├── solver_files/*                          Artefacts from solver and ZIMPL (.lp file, .zpl file etc.)
└── └── exp_config.conf                         Experiment configuration


```
The ```ExperimentExporter``` also allows to export only parts of the experiment (e.g. results). 

### Importing Experiment
The data which was generated by an experiment export can be reimported.
Refer to the ```test.scala.io.ExperimentImporterTest``` to see an example of how an experiment can be exported and the reimported. 

## Customization
The modular structure of the FAAP-Simulator allows to make adaptions to the way problem instances are generated and the way the evaluation is done. 
Furthermore, it is straightforward to implement new allocators. 

### Custom Problem Instances
Problem instances can be hardcoded or be generated automatically using a generator.
#### Hardcoded
Refer to  ```test.scala.simulator.TestGraphs``` to see how an FAAP instance can be created by hand.

#### Generated
Refer to ```scala.simulator.generation.FaapProblemGenerator``` to see how an FAAP instance can be generated automatically according to some configuration. 

### Custom Allocator
To implement a new allocator, implement the ```scala.simulator.allocation.Allocator```-trait and refer to the already implemented allocators in the ```scala.simulator.allocation```-package to see how an allocator can be implemented.   

### Custom Evaluation 
To implement a new evaluation method (e.g. for a model variation with a different cost function and/or other constraints), implement the ```scala.simulator.evaluation.MappingEvaluator```-trait and refer to the ```scala.simulator.evaluation.DefaultMappingEvaluator```-class to see how the Evaluator for the basic model of the FAAP is implemented.   


## Contact/Impressum
- Marco Suter
- marco.suter [AT] gmx.net
- _Thesis date: Mar 2018 - Sep 2018, (hand-in 12.09.2018)_

## License
```Copyright 2018 Marco Suter

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.```