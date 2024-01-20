IF OBJECT_ID('[eeInf].[ModelData]') IS NULL begin
	print 'Creating table [eeInf].[ModelData] ...'

	CREATE TABLE [eeInf].[ModelData](
		[modelDataId] [uniqueidentifier] NOT NULL,
		[modelDataOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[modelDataParams] [nvarchar](max) NOT NULL,
		[modelBinaryData] [varbinary](max) NOT NULL,
		[createdOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_eeInf_ModelData] PRIMARY KEY CLUSTERED 
	(
		[modelDataId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

	ALTER TABLE [eeInf].[ModelData] ADD  DEFAULT (getdate()) FOR [createdOn]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_eeInf_ModelData] ON [eeInf].[ModelData]
	(
		[modelDataOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [eeInf].[ModelData] already exists ...'
end
go




