# ClmFSharp
## F# modelling of chiral symmetry breaking in chemical systems. Version 6.0.5.
It is a port of https://github.com/kkkmail/ClmFSharp to NET5 with some extensions to support reactions with activated catalysts.

### Complexity
The main difficulty in modeling organic chemical systems is a very large number of possible reagents and reactions. For example, the number of peptide chains grows as: `(2 * M) * ((2 * M) ^ (N + 1) – 1) / ((2 * M) - 1)` where `M` is the number of considered amino acids and `N` is the maximum considered peptide length. That value for 20 amino acids and maximum peptide length of 3 gives 65,640 possible peptide chains. If some of the reactions can be catalyzed by some of these peptide chains, then, in theory, we must consider that *all* peptide chains can catalyze *all* such reactions, though most of the coefficients are exact zeros. This requires considering huge sparse matrices of coefficients of various reactions. Subsequently, it is not possible to use such matrices directly and all exact zeros must be removed from all equations. 

### Statistical Approach
The models solved by this system are statistical in nature. The model parameters control how sparse are the matrices and what are the distributions of non-zero coefficients. Each model describes a homogeneous chemical system. However, some of the reactions imply a separation of soluble and insoluble substances, which implies inhomogeneous systems.

Due to statistical nature of the models, it is necessary to repeat the same model many times to achieve statistically valid results. The system uses tasks, which are stored in the table `ClmTask` to repeat the generating model for the same set of parameters. A task is a combination of some default parameter set (row from `ClmDefaults`), a set of pairs of initial concentration (called `y0` in the system) and a run time (called `tEnd`), and a number of repetitions. 

### System Core
The system core consists of a model generator, which generates models using preconfigured statistical distributions based on some set of default values. These sets of default values are stored in the table ` ClmDefaultValue`, which is populated from F# code located in the project `ClmDefaults` -> `AllDefaults.fs`. This allows adding more default sets / tasks as long as the system core is not changed. The generator continuously loads parameters from the database, produces models, stores the generated sparse matrices in the database, and then schedules tasks to run for all configured values of initial concentration / run time. The tasks will be run on all available nodes in the computing cluster. 

The parameters of the models / tasks can be adjusted on the fly and that will affect the models, which are generated after that. Once any model run is completed the aggregate result is stored in `ResultData` table. The HTML chats are produced if symmetry breaking occurred in the model or if explicitly requested. The intermediate results and charts can be requested for any running model if it has not completed yet.

### Internal Storage and Communication
The system uses MS SQL as a database to store various parameters and generates HTML charts, which are stored in a local folder. All extensive data sets are stored in the database in JSON format and all binary data sets are stored as zipped JSON. Please, refer to `DbData.DatabaseTypes` for the details. The remote and local components of the system "talk" to each other using barebone `WCF` functionality. Remote nodes (`WorkerNodeService`) store the data using a mix of SQLite database and a file system. Storing everything in one or the other is not justified due to various complexities here or there. 


## Build order
The system uses F# type providers, which means that the database must be created first. The compile time connection strings are loaded from `DbData\app.config` because type providers have not switched yet to support `appsettings.json`. The run time connection string are loaded from `appsettings.json` files. See `DbData.DatabaseTypes` for details. Because the databases are primitive (each of them contains less than 10 tables), usage of automated up/down database migrations (like `Entity Framework` based one) does not yet seem justified. So, the procedure is as follows:
1.	Look up the values of `ClmSys.VersionInfo.ClmBaseName` and `ClmSys.VersionInfo.MsgSvcBaseName` (e.g. `clm605`, `msg605`, `wns605`). They must match the values `from DbData\app.config`.
2.	Create MSSQL databases with the names from step #1.
3.	Run `-build.bat` file from `Sql\ContGen`, `Sql\MsgSvc`, and `Sql\WrkNode` folders. they will produce files `001_all.sql` and `002_data.sql` in the folders `Sql\ContGen\!All`, `Sql\MsgSvc\!All`, and `Sql\WrkNode\!All`. If no changes to tables or data were made, then these files will come out the same as in the repository.
4.	Load file `001_all.sql` and run it in the database created on step #2, then load file `002_data.sql` and run it. Repeat that for all three databases. The scripts are fully reentrable, which means that they can be run many times without any side effects.
5.	Once the databases are built, F# solution `CoreClm.sln` can be loaded and compiled.


## Project and Folder Structure
Folders `CoreClm` and below contain F# code. The solution is `CoreClm.sln`. The current code has only a handful of what would be called acceptance tests in C# world. Most of them are currently done as `FSX` files. There are a few more tests that should be added to the system and this will be hopefully covered in future version(s).

Folder `Math` contains various Excel / Wolfram Mathematica helper files and they are not needed for the operation of the system.

Folder `SQL` contains the database code and various convenient select statements. 

Folder `Commands` contain batch files, which allow creating some groups of tasks in one go.


## Executables and Command Line Parameters
The project `ContGenService` is the primary one and it contains all executables needed for the operation of the system. The system uses `Argu` as a command line parser, so running any of the executables (except `ClmDefaults.exe`, which currently does not have any command line parameters) with `help` command will provide up to date command line structure.

The following main executables are used by the system:
1.	`ClmDefaults.exe` stores / updates current default sets into the database. If any of the default sets are modified or new ones are added in F# code, then this command must be run first. Currently, there are no command line parameters for this command.
2.	`ContGenService.exe`. This is the primary service. It can be run as a Windows service or as a regular executable. There are some different benefits running it as a Windows service or as a simple executable. 
3.	When `ContGenService.exe` is running we need the functionality to control and/or monitor it. This is performed by `ContGenAdm.exe`. It can be used in both running modes of `ContGenService.exe` (Windows service / standard executable). It is also used to add tasks / generate mode code for tests / run specific model "by hands".
4.	`SolverRunner` is a low-lever F# wrapper around ALGLIB vector ODE solver. It solves a given model with given parameters (`y0` and `tEnd`), It is spawned as external process by `WorkerNodeService` when a model needs to be run.

The system runs models on a distributed computing cluster. The following additional executable are used to do that:
1.	MessagingService.exe
3.	WorkerNodeService.exe
4.	WorkerNodeAdm.exe


## Detailed Description of the system
The system was designed to solve some statistical models. A model is produced by a model generator, which is controlled by a set of statistical distributions and a random seed. The generator produces a very large system of ODEs (around 50K – 100K of variables). Then the model is run with some small random values of all variables but one and two fixed parameters: evolution time and the value of a single variable, which effective sets the “scale” of nonlinearity in the system. Once completed, a model produces a value between 0 and 1. Due to statistical nature, the same set of parameters (which control the distributions) must be tried many times in order to achieve reasonable statistical resolution (e.g. if the same set of parameters is run with different random seeds 100 times, then the approximate margin of error is about 10%). It takes from a few hours to over a month for a model to complete, though most of them take about from one to three days. A model cannot be parallelized due to the essence of the vector ODE solver. Subsequently, each model is spawned as a single thread, called `SolverRunner`. The models are generated and spawned by a system core based on the set-up tasks. Typical model size is between approximately 2MB and 15MB of zipped JSON, though most of them are at the lower end of this interval. Open source ALGLIB library (http://www.alglib.net) is used as a vector ODE solver and generated HTML charts are powered by Plotly (https://plot.ly/) and F# wrapper (https://muehlhaus.github.io/FSharp.Plotly/) with some small custom modifications.

Most of the system components are either generic enough or use proxies. This means that relatively small changes are needed to change the payload from the above-mentioned `SolverRunner` to almost anything else. The database requirements are rudimentary: there are only 9 tables in the system and most of the data is stored either in JSON or zipped JSON formats. HTML charts are stored as files.

Effectively, it is a map/reduce implementation where the map portion is very heavy but individual results are not important (a loss of almost any number of individual results does not make the overall result invalid), while reduce is nearly instantaneous (some SQL select from tables with relatively small number of rows). Subsequently, map portion was designed to run at up to continuous 100% load and no redundancy, while reduce is routinely handled by SQL.


## System Components
The system consists of the following main components (executables). Each executable with the suffix `Service` in its name is a self-installing, self-starting Windows service and which can also be run directly from the command line as a regular executable. Each executable with the suffix `Adm` is the administrative console for the relevant service. Such consoles allow reconfiguring running services on the fly even if they are run as regular executables and perform various other tasks specific to the service.
All IO is done through proxies. For example, `ContGenService` (see below) has absolutely no idea what models it generates, where all the data is stored, or even where the models are run. The same idea applies to all other components. 
The system can be run either in standalone (currently unsupported) or cluster modes. If run in a standalone mode, then it runs on a single computer and can be configured to use from one to a maximum available number of logical cores of the operating system. If run in a cluster mode, then it will run on all cores provided by all its active nodes (`WorkerNodeService`). The nodes communicate with the system core using strongly typed messages through `MessagingService`. This allows running nodes and system core behind NAT and only messaging service must have a public IP address in such a case.
All internal communication among components is done via TCP protocol. There are three default ports used: one is to communicate with `ContGenService`, another is to communicate with `WorkerNodeService`, and the third is to communicate with the `MessagingService`. 
All ports / IP addresses / etc. are configurable via command line / config files. Any of the services can save their settings into relevant `appsettings.json` and then use the information from there if it is not provided in the command line / config file.

### ContGenService
This is the system core responsible for generating models, scheduling them for execution, monitoring progress of each running model, storing the results in the database, etc. When the system operates in a standalone mode the system core spawns local `SolverRunner` processes. The actual spawning is done by a proxy. So, when the system operates in a cluster mode, then a different proxy is used and it forwards all work to a `Partitioner` (see below) instead of spawning local processes.

### ContGenAdm
This is the admin console for `ContGenService`. It allows scheduling tasks (the actual work to be done), reconfiguring the service on the fly, or just monitoring its work (e.g. how many models are running, what is their progress and estimated completion time, how many models are in the queue, etc.).

### SolverRunner
This is the work horse that does the actual work. `SolverRunner` processes are spawned by `ContGenService` when the system operates in a standalone mode and by `WorkerNodeService` when the system operates in a cluster mode. They communicate their progress to the parent services, store the results on completion, and may also generate some HTML charts, provided that some threshold parameter has been exceeded.

### WorkerNodeService
This component is only used when the system operates in a cluster mode. It receives work from a `Partitioner` (see below) and spawns `SolverRunner` threads. In such a case, solver runners are using a different proxy and talk to `WorkerNodeService` instead of `ContGenService`. This proxy specifies `FileStorage` (see below) instead of SQL for storing the results and then, on completion, `WorkerNodeService` will transmit results and charts (if applicable) back to `Partitioner`, which, in turn, notifies `ContGenService`, stores the results in the SQL, and chart in the file system. A worker node can be dynamically reconfigured to change the number of cores, register / unregister from the cluster, and change various communication related parameters.

### WorkerNodeAdm
This is a rudimentary admin console for `WorkerNodeService`. It allows changing the number of running cores of the service and some other parameters of the service.

### MessagingService
`MessagingService` provides communication between worker nodes and system core. The nodes and system core may be located anywhere and as such might be unable to talk directly. Messaging service allows them to talk by forwarding messaging with guaranteed and non-guaranteed delivery. For example, all progress notification messages (except model completion message) are sent with non-guaranteed delivery. This structure allows any of the nodes even to be temporarily cut off from the internet but still be able to send / receive the necessary data when the connection is restored.


## Critical sub-components
### Partitioner
Partitioner maintains the list of running worker nodes along with their state, splits (partitions) incoming work, stores results and generated charts, and notifies system core about completion of the models. If a running node is unregistered, it splits the work among existing worker nodes. At this point it may temporarily queue cancelled jobs in its internal queue.

### File Storage
File storage is used instead of SQL by worker nodes because it is assumed that worker nodes may not have SQL server installed. It stores the data in XML files.

### ServiceProxy
This sub-component provides IO proxies for all components that need IO. Some of the components may have more than one proxy. For example, `SolverRunner` has two proxies. One is used when it is running on a stand-alone machine and allows it to access database, while the other is used in a cluster mode and substitutes database by the file storage.

### Tasks and Queueing System
The system maintains three queues when it operates in a cluster mode. The first queue is the list of tasks and some supporting parameters. A task is a set of parameters of statistical distributions and some fixed parameters. Once the task is scheduled it contains the number of initial and remaining repetitions. The model generator takes all incomplete tasks, generates models, and informs system core about all these generated models. This is simpler than generating models one by one. Since the system core does not run more models than the number of available cores it puts all extra models into internal run queue. The third queue is used by a partitioner. If a node is unregistered, then all models run by that node are cancelled from the partitioner point of view, but they are still running from the system core point of view and it makes no sense to send them back. Subsequently, partitioner stores such models in its internal queue and then send them to other nodes once they have free cores. Tasks and run queue are stored in SQL database and they can be turned off and back on by changing their status. This is convenient when some task(s) should be run as fast as possible on larger number of cores.
