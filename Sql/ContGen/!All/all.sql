IF OBJECT_ID('[dbo].[ClmTaskStatus]') IS NULL begin
	print 'Creating table [dbo].[ClmTaskStatus] ...'

	CREATE TABLE [dbo].[ClmTaskStatus](
		[clmTaskStatusId] [int] NOT NULL,
		[clmTaskStatusName] [nvarchar](50) NOT NULL,
	 CONSTRAINT [PK_clmTaskStatus] PRIMARY KEY CLUSTERED 
	(
		[clmTaskStatusId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_clmTaskStatus] ON [dbo].[ClmTaskStatus]
	(
		[clmTaskStatusName] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [dbo].[ClmTaskStatus] already exists ...'
end
go


IF OBJECT_ID('[dbo].[RunQueueStatus]') IS NULL begin
	print 'Creating table [dbo].[RunQueueStatus] ...'

	CREATE TABLE [dbo].[RunQueueStatus](
		[runQueueStatusId] [int] NOT NULL,
		[runQueueStatusName] [nvarchar](50) NOT NULL,
	 CONSTRAINT [PK_RunQueueStatus] PRIMARY KEY CLUSTERED 
	(
		[runQueueStatusId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_RunQueueStatus] ON [dbo].[RunQueueStatus]
	(
		[runQueueStatusName] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [dbo].[RunQueueStatus] already exists ...'
end
go


IF OBJECT_ID('[dbo].[WorkerNode]') IS NULL begin
	print 'Creating table [dbo].[WorkerNode] ...'

	CREATE TABLE [dbo].[WorkerNode](
		[workerNodeId] [uniqueidentifier] NOT NULL,
		[workerNodeOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[workerNodeName] [nvarchar](100) NOT NULL,
		[nodePriority] [int] NOT NULL,
		[numberOfCores] [int] NOT NULL,
		[description] [nvarchar](1000) NULL,
		[isLocal] [bit] NOT NULL,
		[isInactive] [bit] NOT NULL,
		[createdOn] [datetime] NOT NULL,
		[modifiedOn] [datetime] NOT NULL,
		[lastErrorOn] [datetime] NULL,
	 CONSTRAINT [PK_WorkerNode] PRIMARY KEY CLUSTERED 
	(
		[workerNodeId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [dbo].[WorkerNode] ADD  CONSTRAINT [DF_WorkerNode_isLocal]  DEFAULT ((0)) FOR [isLocal]
	ALTER TABLE [dbo].[WorkerNode] ADD  CONSTRAINT [DF__WorkerNod__isInactive]  DEFAULT ((0)) FOR [isInactive]
	ALTER TABLE [dbo].[WorkerNode] ADD  CONSTRAINT [DF__WorkerNod__createdOn]  DEFAULT (getdate()) FOR [createdOn]
	ALTER TABLE [dbo].[WorkerNode] ADD  CONSTRAINT [DF__WorkerNod__modifiedOn]  DEFAULT (getdate()) FOR [modifiedOn]
	ALTER TABLE [dbo].[WorkerNode] ADD  CONSTRAINT [DF__WorkerNod__nodePriority]  DEFAULT ((100)) FOR [nodePriority]
	ALTER TABLE [dbo].[WorkerNode] ADD  CONSTRAINT [DF__WorkerNod__numberOfCores]  DEFAULT ((0)) FOR [numberOfCores]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_WorkerNodeName] ON [dbo].[WorkerNode]
	(
		[workerNodeName] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_WorkerNodeOrder] ON [dbo].[WorkerNode]
	(
		[workerNodeOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [dbo].[WorkerNode] already exists ...'
end
go

IF OBJECT_ID('[dbo].[RunQueue]') IS NULL begin
	print 'Creating table [dbo].[RunQueue] ...'

	CREATE TABLE [dbo].[RunQueue](
		[runQueueId] [uniqueidentifier] NOT NULL,
		[runQueueOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[modelDataId] [uniqueidentifier] NOT NULL,
		[runQueueStatusId] [int] NOT NULL,
		[y0] [money] NOT NULL,
		[tEnd] [money] NOT NULL,
		[useAbundant] [bit] NOT NULL,
		[errorMessage] nvarchar(max) NULL,
		[progress] [float] NOT NULL,
		[callCount] [bigint] NOT NULL,
		[yRelative] [float] NOT NULL,
		[maxEe] [float] NOT NULL,
		[maxAverageEe] [float] NOT NULL,
		[maxWeightedAverageAbsEe] [float] NOT NULL,
		[maxLastEe] [float] NOT NULL,
		[workerNodeId] [uniqueidentifier] NULL,
		[createdOn] [datetime] NOT NULL,
		[startedOn] [datetime] NULL,
		[modifiedOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_RunQueue] PRIMARY KEY CLUSTERED 
	(
		[runQueueId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [dbo].[RunQueue] ADD  DEFAULT ((0)) FOR [useAbundant]
	ALTER TABLE [dbo].[RunQueue] ADD  DEFAULT ((0)) FOR [runQueueStatusId]
	ALTER TABLE [dbo].[RunQueue] ADD  DEFAULT ((0)) FOR [progress]
	ALTER TABLE [dbo].[RunQueue] ADD  DEFAULT ((1)) FOR [yRelative]
	ALTER TABLE [dbo].[RunQueue] ADD  DEFAULT ((0)) FOR [maxEe]
	ALTER TABLE [dbo].[RunQueue] ADD  DEFAULT ((0)) FOR [maxAverageEe]
	ALTER TABLE [dbo].[RunQueue] ADD  DEFAULT ((0)) FOR [maxWeightedAverageAbsEe]
	ALTER TABLE [dbo].[RunQueue] ADD  DEFAULT ((0)) FOR [maxLastEe]
	ALTER TABLE [dbo].[RunQueue] ADD  DEFAULT (getdate()) FOR [createdOn]
	ALTER TABLE [dbo].[RunQueue] ADD  DEFAULT (getdate()) FOR [modifiedOn]

	ALTER TABLE [dbo].[RunQueue]  WITH CHECK ADD  CONSTRAINT [FK_RunQueue_RunQueueStatus] FOREIGN KEY([runQueueStatusId])
	REFERENCES [dbo].[RunQueueStatus] ([runQueueStatusId])

	ALTER TABLE [dbo].[RunQueue]  WITH CHECK ADD  CONSTRAINT [FK_RunQueue_WorkerNode] FOREIGN KEY([workerNodeId])
	REFERENCES [dbo].[WorkerNode] ([workerNodeId])

	ALTER TABLE [dbo].[RunQueue] CHECK CONSTRAINT [FK_RunQueue_RunQueueStatus]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_RunQueue] ON [dbo].[RunQueue]
	(
		[runQueueOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

end else begin
	print 'Table [dbo].[RunQueue] already exists ...'
end
go



IF OBJECT_ID('[dbo].[ClmDefaultValue]') IS NULL begin
	print 'Creating table [dbo].[ClmDefaultValue] ...'

	CREATE TABLE [dbo].[ClmDefaultValue](
		[clmDefaultValueId] [bigint] NOT NULL,
		[defaultRateParams] [nvarchar](max) NOT NULL,
		[description] [nvarchar](max) NULL,
	 CONSTRAINT [PK_ClmDefaultValue] PRIMARY KEY CLUSTERED 
	(
		[clmDefaultValueId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
end else begin
	print 'Table [dbo].[ClmDefaultValue] already exists ...'
end
go


IF OBJECT_ID('[dbo].[ClmTask]') IS NULL begin
	print 'Creating table [dbo].[ClmTask] ...'

	CREATE TABLE [dbo].[ClmTask](
		[clmTaskId] [uniqueidentifier] NOT NULL,
		[clmTaskOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[clmDefaultValueId] [bigint] NOT NULL,
		[clmTaskStatusId] [int] NOT NULL,
		[numberOfAminoAcids] [int] NOT NULL,
		[maxPeptideLength] [int] NOT NULL,
		[numberOfRepetitions] [int] NOT NULL,
		[remainingRepetitions] [int] NOT NULL,
		[createdOn] [datetime] NOT NULL,
		[modifiedOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_ClmTask] PRIMARY KEY CLUSTERED 
	(
		[clmTaskId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [dbo].[ClmTask] ADD  DEFAULT ((1)) FOR [numberOfRepetitions]
	ALTER TABLE [dbo].[ClmTask] ADD  DEFAULT ((1)) FOR [remainingRepetitions]
	ALTER TABLE [dbo].[ClmTask] ADD  DEFAULT ((0)) FOR [clmTaskStatusId]
	ALTER TABLE [dbo].[ClmTask] ADD  DEFAULT (getdate()) FOR [createdOn]
	ALTER TABLE [dbo].[ClmTask] ADD  DEFAULT (getdate()) FOR [modifiedOn]

	ALTER TABLE [dbo].[ClmTask]  WITH CHECK ADD  CONSTRAINT [FK_ClmTask_ClmTaskStatus] FOREIGN KEY([clmTaskStatusId])
	REFERENCES [dbo].[ClmTaskStatus] ([clmTaskStatusId])

	ALTER TABLE [dbo].[ClmTask] CHECK CONSTRAINT [FK_ClmTask_ClmTaskStatus]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_ClmTask] ON [dbo].[ClmTask]
	(
		[clmTaskOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [dbo].[ClmTask] already exists ...'
end
go



IF OBJECT_ID('[dbo].[CommandLineParam]') IS NULL begin
	print 'Creating table [dbo].[CommandLineParam] ...'

	CREATE TABLE [dbo].[CommandLineParam](
		[commandLineParamId] [uniqueidentifier] NOT NULL,
		[commandLineParamOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[clmTaskId] [uniqueidentifier] NOT NULL,
		[y0] [money] NOT NULL,
		[tEnd] [money] NOT NULL,
		[useAbundant] [bit] NOT NULL,
		[createdOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_TCommandLineParam] PRIMARY KEY CLUSTERED 
	(
		[commandLineParamId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [dbo].[CommandLineParam] ADD  CONSTRAINT [DF__CommandLi__useAb__403A8C7D]  DEFAULT ((0)) FOR [useAbundant]
	ALTER TABLE [dbo].[CommandLineParam] ADD  CONSTRAINT [DF__CommandLi__creat__412EB0B6]  DEFAULT (getdate()) FOR [createdOn]

	ALTER TABLE [dbo].[CommandLineParam]  WITH CHECK ADD  CONSTRAINT [FK_CommandLineParam_ClmTask] FOREIGN KEY([clmTaskId])
	REFERENCES [dbo].[ClmTask] ([clmTaskId])

	ALTER TABLE [dbo].[CommandLineParam] CHECK CONSTRAINT [FK_CommandLineParam_ClmTask]

	CREATE NONCLUSTERED INDEX [UX_CommandLineParam] ON [dbo].[CommandLineParam]
	(
		[commandLineParamOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [dbo].[CommandLineParam] already exists ...'
end
go



IF OBJECT_ID('[dbo].[ModelData]') IS NULL begin
	print 'Creating table [dbo].[ModelData] ...'

	CREATE TABLE [dbo].[ModelData](
		[modelDataId] [uniqueidentifier] NOT NULL,
		[modelDataOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[clmTaskId] [uniqueidentifier] NOT NULL,
		[seedValue] [int] NULL,
		[modelDataParams] [nvarchar](max) NOT NULL,
		[modelBinaryData] [varbinary](max) NOT NULL,
		[createdOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_ModelData] PRIMARY KEY CLUSTERED 
	(
		[modelDataId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

	ALTER TABLE [dbo].[ModelData] ADD  DEFAULT (getdate()) FOR [createdOn]

	ALTER TABLE [dbo].[ModelData]  WITH CHECK ADD  CONSTRAINT [FK_ModelData_ClmTask] FOREIGN KEY([clmTaskId])
	REFERENCES [dbo].[ClmTask] ([clmTaskId])

	ALTER TABLE [dbo].[ModelData] CHECK CONSTRAINT [FK_ModelData_ClmTask]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_ModelData] ON [dbo].[ModelData]
	(
		[modelDataOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [dbo].[ModelData] already exists ...'
end
go




--declare @clmDefaultValueId bigint
--set @clmDefaultValueId = 4005000020

drop function if exists dbo.getAcBkwCatLigScarcity
go

create function dbo.getAcBkwCatLigScarcity(@clmDefaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'acBkwCatLigParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'acBkwCatLigRndEeParams'
	)
	,t4_1 as
	(
		select a.* 
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t5 as
	(
		select b.*
		from t4_1
		cross apply openjson(t4_1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E09
	from t10
	cross apply openjson(t10.[value]) as a

	--select @retVal

	return @retval
end
go
--declare @clmDefaultValueId bigint
--set @clmDefaultValueId = 4005000020

drop function if exists dbo.getAcBkwCatLigSimilarity
go

create function dbo.getAcBkwCatLigSimilarity(@clmDefaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'acBkwCatLigSimParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'acCatRatesSimGeneration'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
	)
	,t7 as
	(
		select a.*
		from t6
		cross apply openjson(t6.[value]) as a
		where a.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E09
	from t10
	cross apply openjson(t10.[value]) as a

	--select @retVal

	return @retval
end
go
--declare @clmDefaultValueId bigint
--set @clmDefaultValueId = 4005000020

drop function if exists dbo.getAcCatDestrScarcity
go

create function dbo.getAcCatDestrScarcity(@clmDefaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'acCatDestrParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'acCatDestrRndEeParams'
	)
	,t4_1 as
	(
		select a.* 
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t5 as
	(
		select b.*
		from t4_1
		cross apply openjson(t4_1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E06
	from t10
	cross apply openjson(t10.[value]) as a

	return @retval
end
go
--declare @clmDefaultValueId bigint
--set @clmDefaultValueId = 4005000020

--drop function if exists dbo.getAcCatDestrSimilarity
--go

create function dbo.getAcCatDestrSimilarity(@clmDefaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'acCatDestrSimParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'acCatRatesSimGeneration'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
	)
	,t7 as
	(
		select a.*
		from t6
		cross apply openjson(t6.[value]) as a
		where a.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float)
	from t10
	cross apply openjson(t10.[value]) as a

	--select @retVal

	return @retval
end
go
--declare @clmDefaultValueId bigint
--set @clmDefaultValueId = 4005000020


drop function if exists dbo.getAcCatSynthScarcity
go

create function dbo.getAcCatSynthScarcity(@clmDefaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'acCatSynthParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'acCatSynthRndEeParams'
	)
	,t4_1 as
	(
		select a.* 
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t5 as
	(
		select b.*
		from t4_1
		cross apply openjson(t4_1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E06
	from t10
	cross apply openjson(t10.[value]) as a

	--select @retVal

	return @retval
end
go
--declare @clmDefaultValueId bigint
--set @clmDefaultValueId = 4005000020

drop function if exists dbo.getAcCatSynthSimilarity
go

create function dbo.getAcCatSynthSimilarity(@clmDefaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'acCatSynthSimParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'acCatRatesSimGeneration'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
	)
	,t7 as
	(
		select a.*
		from t6
		cross apply openjson(t6.[value]) as a
		where a.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float)
	from t10
	cross apply openjson(t10.[value]) as a

	--select @retVal

	return @retval
end
go
--declare @clmDefaultValueId bigint
--set @clmDefaultValueId = 4005000020


drop function if exists dbo.getAcFwdCatLigScarcity
go

create function dbo.getAcFwdCatLigScarcity(@clmDefaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'acFwdCatLigParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'acFwdCatLigRndEeParams'
	)
	,t4_1 as
	(
		select a.* 
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t5 as
	(
		select b.*
		from t4_1
		cross apply openjson(t4_1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E09
	from t10
	cross apply openjson(t10.[value]) as a

	--select @retVal

	return @retval
end
go
--declare @clmDefaultValueId bigint
--set @clmDefaultValueId = 4005000020

drop function if exists dbo.getAcFwdCatLigSimilarity
go

create function dbo.getAcFwdCatLigSimilarity(@clmDefaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'acFwdCatLigSimParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'acCatRatesSimGeneration'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
	)
	,t7 as
	(
		select a.*
		from t6
		cross apply openjson(t6.[value]) as a
		where a.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E09
	from t10
	cross apply openjson(t10.[value]) as a

	--select @retVal

	return @retval
end
go
drop function if exists dbo.getCatDestrScarcity
go

create function dbo.getCatDestrScarcity(@clmDefaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catDestrParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'catDestrRndEeParams'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E06
	from t10
	cross apply openjson(t10.[value]) as a

	return @retval
end
go
--declare @clmDefaultValueId bigint
--set @clmDefaultValueId = 9028

drop function if exists dbo.getCatDestrSim
go

create function dbo.getCatDestrSim(@clmDefaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catDestrSimParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'catRatesSimGeneration'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t6 as
	(
		select d.*
		from t5
		cross apply openjson(t5.[value]) as a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		cross apply openjson(c.[value]) as d
		where b.[key] = 'Fields' and d.[key] = 'distributionParams'
	)
	,t7 as
	(
		select a.*
		from t6
		cross apply openjson(t6.[value]) as a
		where a.[key] = 'threshold'
	)
	,t8 as
	(
		select a.* 
		from t7
		cross apply openjson(t7.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float)
	from t8
	cross apply openjson(t8.[value]) as a

	return @retval
end
go
drop function if exists dbo.getCatLigMult
go

create function dbo.getCatLigMult(@clmDefaultValueId bigint)
returns float
as
begin
	declare @retVal float
	set @retVal = isnull(dbo.getCatLigMultSim(@clmDefaultValueId), dbo.getCatLigMultRnd(@clmDefaultValueId))
	return @retval
end
go

drop function if exists dbo.getCatLigMultRnd
go

create function dbo.getCatLigMultRnd(@clmDefaultValueId bigint)
returns float
as
begin
	--declare @clmDefaultValueId bigint
	--set @clmDefaultValueId = 4000000000

	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catLigRndEeParams'
	)
	--,t4 as
	--(
	--	select a.* 
	--	from t3
	--	cross apply openjson(t3.[value]) as a
	--	where a.[key] = 'catDestrRndEeParams'
	--)
	,t5 as
	(
		select a.*
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'scale'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float)
	from t10
	cross apply openjson(t10.[value]) as a

	--print ('@retval = ' + isnull(cast(@retval as nvarchar(20)), '<null>'))
	--select * from t10

	return @retval
end
go

drop function if exists dbo.getCatLigMultSim
go

create function dbo.getCatLigMultSim(@clmDefaultValueId bigint)
returns float
as
begin
	--declare @clmDefaultValueId bigint
	--set @clmDefaultValueId = 4002000022

	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catLigParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'catLigRndEeParams'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'scale'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float)
	from t10
	cross apply openjson(t10.[value]) as a

	--print ('@retval = ' + isnull(cast(@retval as nvarchar(20)), '<null>'))
	--select * from t10

	return @retval
end
go

drop function if exists dbo.getCatLigScarcity
go

create function dbo.getCatLigScarcity(@clmDefaultValueId bigint)
returns float
as
begin
	declare @retVal float
	set @retVal = isnull(dbo.getCatLigScarcitySim(@clmDefaultValueId), dbo.getCatLigScarcityRnd(@clmDefaultValueId))
	return @retval
end
go

drop function if exists dbo.getCatLigScarcityRnd
go

create function dbo.getCatLigScarcityRnd(@clmDefaultValueId bigint)
returns float
as
begin
	--declare @clmDefaultValueId bigint
	--set @clmDefaultValueId = 4000000000

	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catLigRndEeParams'
	)
	--,t4 as
	--(
	--	select a.* 
	--	from t3
	--	cross apply openjson(t3.[value]) as a
	--	where a.[key] = 'catDestrRndEeParams'
	--)
	,t5 as
	(
		select a.*
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E06
	from t10
	cross apply openjson(t10.[value]) as a

	--print ('@retval = ' + isnull(cast(@retval as nvarchar(20)), '<null>'))

	return @retval
end
go

drop function if exists dbo.getCatLigScarcitySim
go

create function dbo.getCatLigScarcitySim(@clmDefaultValueId bigint)
returns float
as
begin
	--declare @clmDefaultValueId bigint
	--set @clmDefaultValueId = 4002000022

	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catLigParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'catLigRndEeParams'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E06
	from t10
	cross apply openjson(t10.[value]) as a

	--print ('@retval = ' + isnull(cast(@retval as nvarchar(20)), '<null>'))
	--select * from t10

	return @retval
end
go

drop function if exists dbo.getCatLigSim
go

create function dbo.getCatLigSim(@clmDefaultValueId bigint)
returns float
as
begin
	--declare @clmDefaultValueId bigint
	--set @clmDefaultValueId = 4002000022

	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catLigSimParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'catRatesSimGeneration'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t6 as
	(
		select d.*
		from t5
		cross apply openjson(t5.[value]) as a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		cross apply openjson(c.[value]) as d
		where b.[key] = 'Fields' and d.[key] = 'distributionParams'
	)
	,t7 as
	(
		select a.*
		from t6
		cross apply openjson(t6.[value]) as a
		where a.[key] = 'threshold'
	)
	,t8 as
	(
		select a.* 
		from t7
		cross apply openjson(t7.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float)
	from t8
	cross apply openjson(t8.[value]) as a

	--select * from t7

	return @retval
end
go
drop function if exists dbo.getCatSynthScarcity
go

create function dbo.getCatSynthScarcity(@clmDefaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catSynthParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'catSynthRndEeParams'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E06
	from t10
	cross apply openjson(t10.[value]) as a

	return @retval
end
go
--declare @clmDefaultValueId bigint
--set @clmDefaultValueId = 9028

drop function if exists dbo.getCatSynthSim
go

create function dbo.getCatSynthSim(@clmDefaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catSynthSimParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'catRatesSimGeneration'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t6 as
	(
		select d.*
		from t5
		cross apply openjson(t5.[value]) as a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		cross apply openjson(c.[value]) as d
		where b.[key] = 'Fields' and d.[key] = 'distributionParams'
	)
	,t7 as
	(
		select a.*
		from t6
		cross apply openjson(t6.[value]) as a
		where a.[key] = 'threshold'
	)
	,t8 as
	(
		select a.* 
		from t7
		cross apply openjson(t7.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float)
	from t8
	cross apply openjson(t8.[value]) as a

	return @retval
end
go
drop function if exists dbo.getGroupId
go

create function dbo.getGroupId(@clmDefaultValueId bigint)
returns bigint
as
begin
	return (@clmDefaultValueId / 1000000000)
end
go
drop function if exists dbo.getLigBkw
go

create function dbo.getLigBkw(@clmDefaultValueId bigint)
returns float
as
begin
	--declare @clmDefaultValueId bigint
	--set @clmDefaultValueId = 4002000022

	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.*
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select a.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'ligationDistribution'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'backwardScale'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)

	select @retval = cast(a.[value] as float)
	from t5
	cross apply openjson(t5.[value]) as a

	--print ('@retval = ' + isnull(cast(@retval as nvarchar(20)), '<null>'))
	--select * from t5

	return @retval
end
go

drop function if exists dbo.getLigFwd
go

create function dbo.getLigFwd(@clmDefaultValueId bigint)
returns float
as
begin
	--declare @clmDefaultValueId bigint
	--set @clmDefaultValueId = 4002000022

	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.*
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select a.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'ligationDistribution'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'forwardScale'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)

	select @retval = cast(a.[value] as float)
	from t5
	cross apply openjson(t5.[value]) as a

	--print ('@retval = ' + isnull(cast(@retval as nvarchar(20)), '<null>'))
	--select * from t5

	return @retval
end
go

drop function if exists dbo.getWasteRecyclingRate
go

create function dbo.getWasteRecyclingRate(@clmDefaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	select @retVal = cast(b.[value] as float)
	from t1
	cross apply openjson(t1.[value]) as a
	cross apply openjson(a.[value]) as b
	where b.[key] = 'wasteRecyclingRate'

	return @retVal
end
go
IF OBJECT_ID('[dbo].[DeliveryType]') IS NULL begin
	print 'Creating table [dbo].[DeliveryType] ...'

	CREATE TABLE [dbo].[DeliveryType](
		[deliveryTypeId] [int] NOT NULL,
		[deliveryTypeName] [nvarchar](50) NOT NULL,
	 CONSTRAINT [PK_DeliveryType] PRIMARY KEY CLUSTERED 
	(
		[deliveryTypeId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]
end else begin
	print 'Table [dbo].[DeliveryType] already exists ...'
end
go

IF OBJECT_ID('[dbo].[Message]') IS NULL begin
	print 'Creating table [dbo].[Message] ...'

	CREATE TABLE [dbo].[Message](
		[messageId] [uniqueidentifier] NOT NULL,
		[senderId] [uniqueidentifier] NOT NULL,
		[recipientId] [uniqueidentifier] NOT NULL,
		[messageOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[dataVersion] [int] NOT NULL,
		[deliveryTypeId] [int] NOT NULL,
		[messageData] [varbinary](max) NOT NULL,
		[createdOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_Message] PRIMARY KEY CLUSTERED 
	(
		[messageId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

	ALTER TABLE [dbo].[Message]  WITH CHECK ADD  CONSTRAINT [FK_Message_DeliveryType] FOREIGN KEY([deliveryTypeId])
	REFERENCES [dbo].[DeliveryType] ([deliveryTypeId])

	ALTER TABLE [dbo].[Message] CHECK CONSTRAINT [FK_Message_DeliveryType]
end else begin
	print 'Table [dbo].[Message] already exists ...'
end
go

;with 
	valTbl as
	(
		select * 
		from 
		( values
			  (0, 'Active')
			, (1, 'Inactive')

		) as a (clmTaskStatusId, clmTaskStatusName)
	)
insert into ClmTaskStatus
select valTbl.*
from valTbl
left outer join ClmTaskStatus on valTbl.clmTaskStatusId = ClmTaskStatus.clmTaskStatusId
where ClmTaskStatus.clmTaskStatusId is null
go


;with 
	valTbl as
	(
		select * 
		from 
		( values
			  (0, 'NotStarted')
			, (1, 'Inactive')
			, (7, 'RunRequested')
			, (2, 'InProgress')
			, (3, 'Completed')
			, (4, 'Failed')
			, (5, 'CancelRequested')
			, (6, 'Cancelled')

		) as a (runQueueStatusId, runQueueStatusName)
	)
insert into RunQueueStatus
select valTbl.*
from valTbl
left outer join RunQueueStatus on valTbl.runQueueStatusId = RunQueueStatus.runQueueStatusId
where RunQueueStatus.runQueueStatusId is null
go


;with 
	valTbl as
	(
		select * 
		from 
		( values
			  (cast('3262C12A-2E1B-4D6D-B787-655FDD779BF9' as uniqueidentifier), 'LOCAL', 1)

		) as a (workerNodeId, workerNodeName, isLocal)
	)
insert into WorkerNode (workerNodeId, workerNodeName, isLocal)
select valTbl.*
from valTbl
left outer join WorkerNode on valTbl.workerNodeId = WorkerNode.workerNodeId
where WorkerNode.workerNodeId is null
go

;with 
	valTbl as
	(
		select * 
		from 
		( values
			  (0, 'GuaranteedDelivery')
			, (1, 'NonGuaranteedDelivery')

		) as a (deliveryTypeId, deliveryTypeName)
	)
insert into DeliveryType
select valTbl.*
from valTbl
left outer join DeliveryType on valTbl.deliveryTypeId = DeliveryType.deliveryTypeId
where DeliveryType.deliveryTypeId is null
go


