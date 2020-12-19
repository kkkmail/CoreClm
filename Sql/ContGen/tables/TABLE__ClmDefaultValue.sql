IF OBJECT_ID('[dbo].[ClmDefaultValue]') IS NULL begin
	print 'Creating table [dbo].[ClmDefaultValue] ...'

	CREATE TABLE [dbo].[ClmDefaultValue](
		[clmDefaultValueId] [bigint] NOT NULL,
		[defaultRateParams] [nvarchar](max) NOT NULL,
		[description] [nvarchar](2000) NULL,
		[fileStructureVersion] [money] NOT NULL,
	 CONSTRAINT [PK_ClmDefaultValue] PRIMARY KEY CLUSTERED 
	(
		[clmDefaultValueId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
end else begin
	print 'Table [dbo].[ClmDefaultValue] already exists ...'
end
go


