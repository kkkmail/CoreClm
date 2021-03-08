IF OBJECT_ID('[dbo].[RunQueue]') IS NULL begin
	print 'Creating table [dbo].[RunQueue] ...'

	CREATE TABLE [dbo].[RunQueue](
		[runQueueId] [uniqueidentifier] NOT NULL,
		[workerNodeRunModelData] [varbinary](max) NOT NULL,
		[runQueueOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[runQueueStatusId] [int] NOT NULL,
		[processId] [int] NULL,
		[errorMessage] [nvarchar](max) NULL,
		[notificationTypeId] [int] NOT NULL,
		[progress] [money] NOT NULL,
		[progressDetailed] [float] NOT NULL,
		[callCount] [bigint] NOT NULL,
		[y] [float] NOT NULL,
		[maxEe] [float] NOT NULL,
		[maxAverageEe] [float] NOT NULL,
		[maxWeightedAverageAbsEe] [float] NOT NULL,
		[maxLastEe] [float] NOT NULL,
		[createdOn] [datetime] NOT NULL,
		[startedOn] [datetime] NULL,
		[modifiedOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_WorkerNodeRunModelData] PRIMARY KEY CLUSTERED 
	(
		[runQueueId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

	ALTER TABLE [dbo].[RunQueue] ADD  CONSTRAINT [DF_RunQueue_runQueueStatusId]  DEFAULT ((0)) FOR [runQueueStatusId]
	ALTER TABLE [dbo].[RunQueue] ADD  CONSTRAINT [DF_RunQueue_notificationTypeId]  DEFAULT ((0)) FOR [notificationTypeId]
	ALTER TABLE [dbo].[RunQueue] ADD  CONSTRAINT [DF_RunQueue_progress]  DEFAULT ((0)) FOR [progress]
	ALTER TABLE [dbo].[RunQueue] ADD  CONSTRAINT [DF_RunQueue_progressDetailed]  DEFAULT ((0)) FOR [progressDetailed]
	ALTER TABLE [dbo].[RunQueue] ADD  CONSTRAINT [DF_RunQueue_callCount]  DEFAULT ((0)) FOR [callCount]
	ALTER TABLE [dbo].[RunQueue] ADD  CONSTRAINT [DF_RunQueue_y]  DEFAULT ((0)) FOR [y]
	ALTER TABLE [dbo].[RunQueue] ADD  CONSTRAINT [DF_RunQueue_maxEe]  DEFAULT ((0)) FOR [maxEe]
	ALTER TABLE [dbo].[RunQueue] ADD  CONSTRAINT [DF_RunQueue_maxAverageEe]  DEFAULT ((0)) FOR [maxAverageEe]
	ALTER TABLE [dbo].[RunQueue] ADD  CONSTRAINT [DF_RunQueue_maxWeightedAverageAbsEe]  DEFAULT ((0)) FOR [maxWeightedAverageAbsEe]
	ALTER TABLE [dbo].[RunQueue] ADD  CONSTRAINT [DF_RunQueue_maxLastEe]  DEFAULT ((0)) FOR [maxLastEe]
	ALTER TABLE [dbo].[RunQueue] ADD  CONSTRAINT [DF_RunQueue_createdOn]  DEFAULT (getdate()) FOR [createdOn]
	ALTER TABLE [dbo].[RunQueue] ADD  CONSTRAINT [DF_RunQueue_modifiedOn]  DEFAULT (getdate()) FOR [modifiedOn]
	ALTER TABLE [dbo].[RunQueue]  WITH CHECK ADD  CONSTRAINT [FK_RunQueue_NotificationType] FOREIGN KEY([notificationTypeId]) REFERENCES [dbo].[NotificationType] ([notificationTypeId])
	ALTER TABLE [dbo].[RunQueue] CHECK CONSTRAINT [FK_RunQueue_NotificationType]
	ALTER TABLE [dbo].[RunQueue]  WITH CHECK ADD  CONSTRAINT [FK_RunQueue_RunQueueStatus] FOREIGN KEY([runQueueStatusId]) REFERENCES [dbo].[RunQueueStatus] ([runQueueStatusId])
	ALTER TABLE [dbo].[RunQueue] CHECK CONSTRAINT [FK_RunQueue_RunQueueStatus]
end else begin
	print 'Table [dbo].[RunQueue] already exists ...'
end
go



