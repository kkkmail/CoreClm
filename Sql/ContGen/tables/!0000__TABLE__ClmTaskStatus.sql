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


