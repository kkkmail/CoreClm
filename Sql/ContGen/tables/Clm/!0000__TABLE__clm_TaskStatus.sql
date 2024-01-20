IF OBJECT_ID('[clm].[TaskStatus]') IS NULL begin
	print 'Creating table [clm].[TaskStatus] ...'

	CREATE TABLE [clm].[TaskStatus](
		[taskStatusId] [int] NOT NULL,
		[taskStatusName] [nvarchar](50) NOT NULL,
	 CONSTRAINT [PK_clm_TaskStatus] PRIMARY KEY CLUSTERED 
	(
		[taskStatusId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_clm_TaskStatus] ON [clm].[TaskStatus]
	(
		[taskStatusName] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [clm].[TaskStatus] already exists ...'
end
go


