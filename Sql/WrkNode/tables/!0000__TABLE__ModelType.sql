IF OBJECT_ID('[dbo].[ModelType]') IS NULL begin
	print 'Creating table [dbo].[ModelType] ...'

	CREATE TABLE [dbo].[ModelType](
		[modelTypeId] [int] NOT NULL,
		[modelTypeName] [nvarchar](50) NOT NULL,
	 CONSTRAINT [PK_ModelType] PRIMARY KEY CLUSTERED 
	(
		[modelTypeId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_ModelType] ON [dbo].[ModelType]
	(
		[modelTypeName] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [dbo].[ModelType] already exists ...'
end
go


