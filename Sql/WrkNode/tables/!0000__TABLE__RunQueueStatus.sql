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


