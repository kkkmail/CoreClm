IF OBJECT_ID('[clm].[CommandLineParam]') IS NULL begin
	print 'Creating table [clm].[CommandLineParam] ...'

	CREATE TABLE [clm].[CommandLineParam](
		[commandLineParamId] [uniqueidentifier] NOT NULL,
		[commandLineParamOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[taskId] [uniqueidentifier] NOT NULL,
		[y0] [money] NOT NULL,
		[tEnd] [money] NOT NULL,
		[useAbundant] [bit] NOT NULL,
		[createdOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_clm_CommandLineParam] PRIMARY KEY CLUSTERED 
	(
		[commandLineParamId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [clm].[CommandLineParam] ADD DEFAULT ((0)) FOR [useAbundant]
	ALTER TABLE [clm].[CommandLineParam] ADD DEFAULT (getdate()) FOR [createdOn]

	ALTER TABLE [clm].[CommandLineParam]  WITH CHECK ADD  CONSTRAINT [FK_clm_CommandLineParam_Task] FOREIGN KEY([taskId])
	REFERENCES [clm].[Task] ([taskId])

	ALTER TABLE [clm].[CommandLineParam] CHECK CONSTRAINT [FK_clm_CommandLineParam_Task]

	CREATE NONCLUSTERED INDEX [UX_clm_CommandLineParam] ON [clm].[CommandLineParam]
	(
		[commandLineParamOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [clm].[CommandLineParam] already exists ...'
end
go



