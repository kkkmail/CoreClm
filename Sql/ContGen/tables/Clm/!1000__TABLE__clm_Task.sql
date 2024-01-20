IF OBJECT_ID('[clm].[Task]') IS NULL begin
	print 'Creating table [clm].[Task] ...'

	CREATE TABLE [clm].[Task](
		[taskId] [uniqueidentifier] NOT NULL,
		[taskOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[defaultValueId] [bigint] NOT NULL,
		[taskStatusId] [int] NOT NULL,
		[taskPriority] [int] NOT NULL,
		[numberOfAminoAcids] [int] NOT NULL,
		[maxPeptideLength] [int] NOT NULL,
		[numberOfRepetitions] [int] NOT NULL,
		[remainingRepetitions] [int] NOT NULL,
		[createdOn] [datetime] NOT NULL,
		[modifiedOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_ClmTask] PRIMARY KEY CLUSTERED 
	(
		[taskId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [clm].[Task] ADD  DEFAULT ((1)) FOR [numberOfRepetitions]
	ALTER TABLE [clm].[Task] ADD  DEFAULT ((1)) FOR [remainingRepetitions]
	ALTER TABLE [clm].[Task] ADD  DEFAULT ((0)) FOR [taskStatusId]
	ALTER TABLE [clm].[Task] ADD  DEFAULT ((1000)) FOR [taskPriority]
	ALTER TABLE [clm].[Task] ADD  DEFAULT (getdate()) FOR [createdOn]
	ALTER TABLE [clm].[Task] ADD  DEFAULT (getdate()) FOR [modifiedOn]

	ALTER TABLE [clm].[Task]  WITH CHECK ADD CONSTRAINT [FK_clm_Task_TaskStatus] FOREIGN KEY([taskStatusId])
	REFERENCES [clm].[TaskStatus] ([taskStatusId])
	ALTER TABLE [clm].[Task] CHECK CONSTRAINT [FK_clm_Task_TaskStatus]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_clm_Task] ON [clm].[Task]
	(
		[taskOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [clm].[Task] already exists ...'
end
go



