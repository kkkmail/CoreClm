IF OBJECT_ID('[dbo].[NotificationType]') IS NULL begin
	print 'Creating table [dbo].[NotificationType] ...'

	CREATE TABLE [dbo].[NotificationType](
		[notificationTypeId] [int] NOT NULL,
		[notificationTypeName] [nvarchar](50) NOT NULL,
	 CONSTRAINT [PK_NotificationType] PRIMARY KEY CLUSTERED 
	(
		[notificationTypeId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_NotificationType] ON [dbo].[NotificationType]
	(
		[notificationTypeName] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [dbo].[NotificationType] already exists ...'
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
		[tEnd] [money] NOT NULL,
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
	ALTER TABLE [dbo].[RunQueue] ADD  CONSTRAINT [DF_RunQueue_tEnd]  DEFAULT ((0)) FOR [tEnd]
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

