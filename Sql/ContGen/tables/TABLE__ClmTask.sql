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



