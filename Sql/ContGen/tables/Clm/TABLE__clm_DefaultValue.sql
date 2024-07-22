IF OBJECT_ID('[clm].[DefaultValue]') IS NULL begin
	print 'Creating table [clm].[DefaultValue] ...'

	CREATE TABLE [clm].[DefaultValue](
		[defaultValueId] [bigint] NOT NULL,
		[defaultRateParams] [nvarchar](max) NOT NULL,
		[description] [nvarchar](max) NULL,
	 CONSTRAINT [PK_clm_DefaultValue] PRIMARY KEY CLUSTERED 
	(
		[defaultValueId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
end else begin
	print 'Table [clm].[DefaultValue] already exists ...'
end
go


