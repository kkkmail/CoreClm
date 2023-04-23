IF OBJECT_ID('[dbo].[EeInfModelData]') IS NULL begin
	print 'Creating table [dbo].[EeInfModelData] ...'

	CREATE TABLE [dbo].[EeInfModelData](
		[eeInfModelDataId] [uniqueidentifier] NOT NULL,
		[eeInfModelDataOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[modelDataParams] [nvarchar](max) NOT NULL,
		[modelBinaryData] [varbinary](max) NOT NULL,
		[createdOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_EeInfModelData] PRIMARY KEY CLUSTERED 
	(
		[eeInfModelDataId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

	ALTER TABLE [dbo].[EeInfModelData] ADD  DEFAULT (getdate()) FOR [createdOn]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_EeInfModelData] ON [dbo].[EeInfModelData]
	(
		[eeInfModelDataOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [dbo].[EeInfModelData] already exists ...'
end
go




