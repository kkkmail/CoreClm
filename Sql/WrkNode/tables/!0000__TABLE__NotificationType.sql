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


