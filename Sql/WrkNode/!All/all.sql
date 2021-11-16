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
		[notificationTypeId] [int] NOT NULL,
		[errorMessage] [nvarchar](max) NULL,
		[progress] [decimal](18, 0) NOT NULL,
		[callCount] [bigint] NOT NULL,
		[yRelative] [float] NOT NULL,
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

	ALTER TABLE [dbo].[RunQueue] ADD DEFAULT ((0)) FOR [runQueueStatusId]
	ALTER TABLE [dbo].[RunQueue] ADD DEFAULT ((0)) FOR [notificationTypeId]
	ALTER TABLE [dbo].[RunQueue] ADD DEFAULT ((0)) FOR [progress]
	ALTER TABLE [dbo].[RunQueue] ADD DEFAULT ((0)) FOR [callCount]
	ALTER TABLE [dbo].[RunQueue] ADD DEFAULT ((1)) FOR [yRelative]
	ALTER TABLE [dbo].[RunQueue] ADD DEFAULT ((0)) FOR [maxEe]
	ALTER TABLE [dbo].[RunQueue] ADD DEFAULT ((0)) FOR [maxAverageEe]
	ALTER TABLE [dbo].[RunQueue] ADD DEFAULT ((0)) FOR [maxWeightedAverageAbsEe]
	ALTER TABLE [dbo].[RunQueue] ADD DEFAULT ((0)) FOR [maxLastEe]
	ALTER TABLE [dbo].[RunQueue] ADD DEFAULT (getdate()) FOR [createdOn]
	ALTER TABLE [dbo].[RunQueue] ADD DEFAULT (getdate()) FOR [modifiedOn]

	ALTER TABLE [dbo].[RunQueue] WITH CHECK ADD CONSTRAINT [FK_RunQueue_NotificationType] FOREIGN KEY([notificationTypeId])
	REFERENCES [dbo].[NotificationType] ([notificationTypeId])

	ALTER TABLE [dbo].[RunQueue] CHECK CONSTRAINT [FK_RunQueue_NotificationType]

	ALTER TABLE [dbo].[RunQueue] WITH CHECK ADD CONSTRAINT [FK_RunQueue_RunQueueStatus] FOREIGN KEY([runQueueStatusId])
	REFERENCES [dbo].[RunQueueStatus] ([runQueueStatusId])

	ALTER TABLE [dbo].[RunQueue] CHECK CONSTRAINT [FK_RunQueue_RunQueueStatus]
end else begin
	print 'Table [dbo].[RunQueue] already exists ...'
end
go



IF OBJECT_ID('[dbo].[Setting]') IS NULL begin
	print 'Creating table [dbo].[Setting] ...'

	CREATE TABLE [dbo].[Setting](
		[settingId] [int] NOT NULL,
		[settingName] [nvarchar](50) NOT NULL,
		[settingBool] [bit] NULL,
		[settingGuid] [uniqueidentifier] NULL,
		[settingLong] [bigint] NULL,
		[settingText] [nvarchar](1000) NULL,
	 CONSTRAINT [PK_Setting] PRIMARY KEY CLUSTERED 
	(
		[settingId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	CREATE UNIQUE NONCLUSTERED INDEX [IX_Setting] ON [dbo].[Setting]
	(
		[settingName] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [dbo].[Setting] already exists ...'
end
go



drop function if exists dbo.RunQueueStatus_NotStarted
go
create function dbo.RunQueueStatus_NotStarted() returns int as begin return 0 end
go
drop function if exists dbo.RunQueueStatus_Inactive
go
create function dbo.RunQueueStatus_Inactive() returns int as begin return 1 end
go
drop function if exists dbo.RunQueueStatus_RunRequested
go
create function dbo.RunQueueStatus_RunRequested() returns int as begin return 7 end
go
drop function if exists dbo.RunQueueStatus_InProgress
go
create function dbo.RunQueueStatus_InProgress() returns int as begin return 2 end
go
drop function if exists dbo.RunQueueStatus_Completed
go
create function dbo.RunQueueStatus_Completed() returns int as begin return 3 end
go
drop function if exists dbo.RunQueueStatus_Failed
go
create function dbo.RunQueueStatus_Failed() returns int as begin return 4 end
go
drop function if exists dbo.RunQueueStatus_CancelRequested
go
create function dbo.RunQueueStatus_CancelRequested() returns int as begin return 5 end
go
drop function if exists dbo.RunQueueStatus_Cancelled
go
create function dbo.RunQueueStatus_Cancelled() returns int as begin return 6 end
go

drop procedure if exists deleteRunQueue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure deleteRunQueue @runQueueId uniqueidentifier
as
begin
	declare @rowCount int
	set nocount on;

	delete from dbo.RunQueue where runQueueId = @runQueueId

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

drop procedure if exists tryCancelRunQueue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure tryCancelRunQueue (@runQueueId uniqueidentifier, @errorMessage nvarchar(max))
as
begin
	declare @rowCount int
	set nocount on;

	update dbo.RunQueue
	set
		runQueueStatusId = dbo.RunQueueStatus_Cancelled(),
		processId = null,
		modifiedOn = (getdate()),
		errorMessage = @errorMessage
	where runQueueId = @runQueueId and runQueueStatusId in (dbo.RunQueueStatus_NotStarted(), dbo.RunQueueStatus_InProgress(), dbo.RunQueueStatus_CancelRequested())

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

drop procedure if exists tryClearNotificationRunQueue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure tryClearNotificationRunQueue @runQueueId uniqueidentifier
as
begin
	declare @rowCount int
	set nocount on;

    update dbo.RunQueue
    set
        notificationTypeId = 0,
        modifiedOn = (getdate())
    where runQueueId = @runQueueId and runQueueStatusId in (dbo.RunQueueStatus_InProgress())

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

drop procedure if exists tryCompleteRunQueue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure tryCompleteRunQueue @runQueueId uniqueidentifier
as
begin
	declare @rowCount int
	set nocount on;

	update dbo.RunQueue
	set
		runQueueStatusId = dbo.RunQueueStatus_Completed(),
		processId = null,
		modifiedOn = (getdate())
	where runQueueId = @runQueueId and processId is not null and runQueueStatusId in (dbo.RunQueueStatus_InProgress(), dbo.RunQueueStatus_CancelRequested())

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

drop procedure if exists tryFailRunQueue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure tryFailRunQueue (@runQueueId uniqueidentifier, @errorMessage nvarchar(max))
as
begin
	declare @rowCount int
	set nocount on;

    update dbo.RunQueue
    set
        runQueueStatusId = dbo.RunQueueStatus_Failed(),
        processId = null,
        modifiedOn = (getdate()),
        errorMessage = @errorMessage
    where runQueueId = @runQueueId and runQueueStatusId in (dbo.RunQueueStatus_InProgress(), dbo.RunQueueStatus_CancelRequested())

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

drop procedure if exists tryNotifyRunQueue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure tryNotifyRunQueue (@runQueueId uniqueidentifier, @notificationTypeId int)
as
begin
	declare @rowCount int
	set nocount on;

    update dbo.RunQueue
    set
        notificationTypeId = @notificationTypeId,
        modifiedOn = (getdate())
    where runQueueId = @runQueueId and runQueueStatusId in (dbo.RunQueueStatus_InProgress(), dbo.RunQueueStatus_CancelRequested())

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

drop procedure if exists tryRequestCancelRunQueue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure tryRequestCancelRunQueue (@runQueueId uniqueidentifier, @notificationTypeId int)
as
begin
	declare @rowCount int
	set nocount on;

    update dbo.RunQueue
    set
        runQueueStatusId = dbo.RunQueueStatus_CancelRequested(),
        notificationTypeId = @notificationTypeId,
        modifiedOn = (getdate())
    where runQueueId = @runQueueId and runQueueStatusId = dbo.RunQueueStatus_InProgress()

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

drop procedure if exists tryStartRunQueue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

create procedure tryStartRunQueue (@runQueueId uniqueidentifier, @processId int)
as
begin
	declare @rowCount int
	set nocount on;

	update dbo.RunQueue
	set
		processId = @processId,
		runQueueStatusId = dbo.RunQueueStatus_InProgress(),
		startedOn = (getdate()),
		modifiedOn = (getdate())
	where runQueueId = @runQueueId and runQueueStatusId in (dbo.RunQueueStatus_NotStarted(), dbo.RunQueueStatus_InProgress())


	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

drop procedure if exists tryUpdateProgressRunQueue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure tryUpdateProgressRunQueue (
						@runQueueId uniqueidentifier,
						@progress decimal,
						@callCount bigint,
						@yRelative float,
						@maxEe float,
						@maxAverageEe float,
						@maxWeightedAverageAbsEe float,
						@maxLastEe float)
as
begin
	declare @rowCount int
	set nocount on;

    update dbo.RunQueue
    set
        progress = @progress,
        callCount = @callCount,
        yRelative = @yRelative,
        maxEe = @maxEe,
        maxAverageEe = @maxAverageEe,
        maxWeightedAverageAbsEe = @maxWeightedAverageAbsEe,
        maxLastEe = @maxLastEe,
        modifiedOn = (getdate())
    where runQueueId = @runQueueId and runQueueStatusId in (dbo.RunQueueStatus_InProgress(), dbo.RunQueueStatus_CancelRequested())

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
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

drop procedure if exists deleteExpiredMessages
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure deleteExpiredMessages (@dataVersion int, @createdOn datetime)
as
begin
	declare @rowCount int
	set nocount on;

    delete from dbo.Message
    where
        deliveryTypeId = 1
        and dataVersion = @dataVersion
        and createdOn < @createdOn

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

drop procedure if exists deleteMessage
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure deleteMessage @messageId uniqueidentifier
as
begin
	declare @rowCount int
	set nocount on;

	delete from dbo.Message where messageId = @messageId

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

drop procedure if exists saveMessage
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure saveMessage (
					@messageId uniqueidentifier,
					@senderId uniqueidentifier,
					@recipientId uniqueidentifier,
					@dataVersion int,
					@deliveryTypeId int,
					@messageData varbinary(max))
as
begin
	declare @rowCount int
	set nocount on;

	insert into Message (messageId, senderId, recipientId, dataVersion, deliveryTypeId, messageData, createdOn)
	select @messageId, @senderId, @recipientId, @dataVersion, @deliveryTypeId, @messageData, getdate()
	where not exists (select 1 from Message where messageId = @messageId)

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

;with 
	valTbl as
	(
		select * 
		from 
		( values
			  (0, 'NoChartGeneration')
			, (1, 'RegularChartGeneration')
			, (2, 'ForceChartGeneration')
		) as a (notificationTypeId, notificationTypeName)
	)
insert into NotificationType
select valTbl.*
from valTbl
left outer join NotificationType on valTbl.notificationTypeId = NotificationType.notificationTypeId
where NotificationType.notificationTypeId is null
go


;with 
	valTbl as
	(
		select * 
		from 
		( values
			  (0, 'NotStarted')
			, (1, 'Inactive')
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
			  (0, 'Suspended', 0, NULL, NULL, NULL)

		) as a (settingId, settingName, settingBool, settingGuid, settingLong, settingText)
	)
insert into Setting
select valTbl.*
from valTbl
left outer join Setting on valTbl.settingId = Setting.settingId
where Setting.settingId is null
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


