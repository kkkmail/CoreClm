#### 2.1.0-beta1 December 17, 2020

* fix reading from app.config vs App.config on case sensitive file system
* move ISqlCommand out of Internals namespace

#### 2.1.0-beta0 December 17, 2020

* SQL Azure handling of `datetimeoffset`, `datetime2`, `time` causing `SqlMetaData` constructor exception (#393)
* Breaking change: Optional parameters are required at call site (#348)
* Breaking change: move anything defined in `FSharp.Data` namespace to `FSharp.Data.SqlClient` (#359)
* Contributors: Christer van der Meeren (https://github.com/cmeeren), Gauthier Segay (https://github.com/smoothdeveloper)

#### 2.0.7 November 18, 2020

* Issue #345 Decimal values are trimmed when used as part of a TVP
* Contributors: Suou Ryuu (https://github.com/suou-ryuu), Gauthier Segay (https://github.com/smoothdeveloper)

#### 2.0.6 October 10, 2019

* Issue #364 Fix issues in ToTraceString
* Contributors: piaste (https://github.com/piaste)

#### 2.0.5 June 11, 2019

* Issue #339 Allow timeout on data table Update commands

Contributor(s): VTJDailey (https://github.com/VTJDailey)

#### 2.0.4 May 27, 2019

* Issue #340 Some primitive types such as Guid weren't supported as output parameters.

Contributor(s): Jérémie Chassaing (https://github.com/thinkbeforecoding)

#### 2.0.3 April 15, 2019
* Issue #332 Invalidate SqlFile type when referenced SQL file is modified.

Contributor(s): Matti Oinas (https://github.com/oikku)

#### 2.0.2 January 19, 2019
* Issue #325 Fix exception when using TVP parameter with fixed length string

Contributor(s): Ryan Riley (https://github.com/panesofglass)

#### 2.0.1-alpha November 11, 2018
* Issue #314 Add netstandard2.0 target

Contributor(s): Sam Hanes (https://github.com/samhanes)

#### 1.8.6 - June 2, 2018
* Issue #303 Enable typed data table constructor to initialize an empty datatable with the expected columns

Contributor(s): Gauthier Segay (https://github.com/smoothdeveloper)

#### 1.8.5 - May 20, 2018
* Issue #278 Add TempTableDefinitions and TableVarMapping

Contributor(s): David Teasdale (https://github.com/davidoptima)

#### 1.8.4 - May 12, 2018
* Issue #221 - Expose custom getter/setter methods on provided DataColumn properties
* Issue #224 - TVP generated type implicitly from SqlDataRecord. 
* Issue #232 - BREAKING CHANGE! SqlFile type provider is a new way to feed external sql files into SqlCommandProvider/CreateCommand
* Issue #233 - Optional continueUpdateOnError parameter added Update method on statically typed data tables
* Issue #234 - Design-time captured value of named connection string used at runtime if code runs under fsi.exe
* Issue #252 - Add factory method to SqlProgrammabilityProvider-generated types
* Issue #280 - Upgrade TSQL parser version
* Issue #286 - SqlCommand disposes underlying connection in its Dispose method
* Issue #289 - Pass `null` if table value argument has zero records

Contributor(s): 
* Dmitry Morozov (https://github.com/dmitry-a-morozov)
* Overlord-Zurg (https://github.com/Overlord-Zurg)
* antonkocherin (https://github.com/antonkocherin)
* Vasily Kirichenko (https://github.com/vasily-kirichenko)
* Gauthier Segay (https://github.com/smoothdeveloper)


#### 1.8.2 - May 16, 2016
	* Issue #192 - Invert order of release notes so most recent is on top
	* Issue #195 - BREAKING CHANGE! Make connection parameter mandatory when literal connection string used at design time
	* Issue #196 - BREAKING CHANGE! API based run-time configuration
	* Issue #197 
	* Issue #199 - Support for Stored procedure and Functions Synonyms
	* Issue #200 - Support for Table Synonyms
	* Issue #201 - Units of measure kind support in SqlEnumProvider
	* Issue #214 - BREAKING CHANGE! Add SqlCommand.Table type for ResultType.DataTable

#### 1.8.1 - March 3, 2016
	* Issue #185 
	
#### 1.8 - December 25, 2015
	* Issue #172
	* Issue #174
	* Issue #176 - Support for SP output params and return values	
	* Issue #177 - SqlEnumProvider generates const fields when possible.
	* Issue #179 
	* Issue #180

#### 1.7.7 - October 4, 2015
	* Issue #170 

#### 1.7.6 - September 16, 2015
	* Issue #146 - Broken XML doc
	* Issue #148 - Quote columnd in OUTPUT clause generated by Update on typed data tables
	* Issue #158 - XML docs for stored proc/functions

#### 1.7.5 - July 13, 2015
	* Redeploy - because previous nuget package 1.7.4 contains binaries stamped with incorrect version (1.7.3).

#### 1.7.4 - July 13, 2015
	* Issue #145 - Stored Procedure and TVP params from diff schemas

#### 1.7.3 - July 6, 2015
	* Issue #137 - support single column select in SqlEnumProvider - natural keys case
	* Issue #135 - create xml doc from MS_Description ext.prop for routines and params
	* Issue #130 - help section dedicated to database updates

#### 1.7.2 - June 19, 2015
	* Issue #136 - SqlProgrammabilityProvider does not update User-Defined Table Types on rebuild 

#### 1.7.1 - June 16, 2015
	* Issue #136 - calling tvp sproc with nullable fields

#### 1.7.0 - June 4, 2015
	* Issue #126 - AsSqlCommand removed
	* Issue #129 - ExecuteSingle/AsyncExecuteSingle method on SqlProgrammability stored procedure/function types
	* Issue #131 - unify SqlProgrammabilityProvider and SqlCommandProvider ctors
	* Issue #124 - DataTable ReadXmlSchema/WriteXmlSchema are not used anymore
	* Issue #132 - Make value types parameters with default value NULL in SqlProgrammabilityProvider of type option<_>

#### 1.6.2 - March 9, 2015
	* Issue #119 fixed

#### 1.6.1 - March 7, 2015
	* Issue #122 fixed

#### 1.6 - March 3, 2015
	* Issue #121 fixed

#### 1.5.12 - March 1, 2015
	* Non-nullable columns with default value should not have getter/setter of type option<_> on typed DataRow
	* When SqlConnection instance passed to command ctor it's suppose to be in open state

#### 1.5.11 - February 26, 2015
	* static factories on provided command types with intellisense
	* BREAKING CHANGE: ctor with transaction also requires connection
	* Ability to pass connection object to ctor/factory
	* Warning on using infrastructure types 
	* Issue #114 fixed

#### 1.5.10 - February 16, 2015
	* Issue #110 FileSystemWatcher not working properly 
	* Issue #105 Error using library in Mono 
	* All top level provided types tagged with design time connection string info

#### 1.5.9 - February 11, 2015
	* Dynamic sql example. Issue #108

#### 1.5.8 - January 13, 2015
	* Fix issue #96 on Sql Azure

#### 1.5.8-beta1 - January 8, 2015
	* Fix default connection management for Update and BulkCopy

#### 1.5.7 - January 7, 2015
	* Update and BulkCopy convenience methods on typed data table

#### 1.5.6 - January 4, 2015
	* Issue #102

#### 1.5.5 - December 17, 2014
	* Issue #101

#### 1.5.4 - December 16, 2014
	* Issue #96
	* Issue #99
	* Optional in methods AddRow/NewRow pushed to the right
	* CommandTimeoput moved to commands ctor

#### 1.5.3 - December 13, 2014
	* Issue #98

#### 1.5.2 - December 9, 2014
	* Issue #97

#### 1.5.1 - December 9, 2014
	* DataTable.AddRow/NewRow with default values for column fo type Guid 

#### 1.5 - December 8, 2014
	* Issue #85 fixed 

#### 1.4.9-beta - November 28 2014
	* Issue #92 work around

#### 1.4.8-beta2 - November 27 2014
	* Issue #91 fixed

#### 1.4.8-beta - November 26 2014
	* Issue #88 fixed

#### 1.4.7-beta - November 25 2014
	* Issue #86 fixed

#### 1.4.6-beta - November 24 2014
	* Issue #73 SqlBulkCopy support

#### 1.4.5 - November 19 2014
	* Issue #80 fixed 

#### 1.4.4 - November 16 2014
	* Replaced dependency on Microsoft.SqlServer.Types from ver 11.0.1 t0 11.0.0

#### 1.4.3 - November 16 2014
	* Switched to implementation of internal cache with expiration for provided types

#### 1.4.2 - October 24 2014
	* Removed targetFramework=".NETFramework4.0" for system assemblies

#### 1.4.1 - October 18 2014
	* Remove forced camel casing in gen record ctor
	* DataDirectory parameter  

#### 1.4 - October 13 2014
	* Promoted from aplha to officail release 
	
#### 1.3.7-beta - October 12 2014
	* Hide obj[] as implementation for TVP row
	
#### 1.3.7-beta - October 12 2014
	* Hide obj[] as implementation for TVP row

#### 1.3.6-beta - October 2 2014
	* SqlProgrammabilityProvider dev experience improved significantly 
	* All entities: UDTT, SPs and Functions under a namespace they belong to 

#### 1.3.5-beta - September 23 2014
	* Fixed connection string by name problem for SqlProgrammabilityProvider

#### 1.3.4-beta - September 23 2014
	* SqlProgrammability significant refactoring.
	* SqlEnumProvider merged in.

#### 1.3.3 - Add KeyInfo to ResultType.DataTable

#### 1.3.2 - Resolution folder overrides only path to ####.sql files.
#### 1.3.1 - Method "With" removed from record type. It was bad design decision to have it.
#### 1.3.0 - With method on generated records handles optionality properly.
#### 1.2.29 - TVP parameters are never optional even with if �AllParametersOptional� is on
#### 1.2.28 - Unique output column names for ResultType.Records
#### 1.2.27 - ISqlCommand and SqlCommand<_> runtime types moved to FSharp.Data name space
#### 1.2.26 - ResolutionFolder parameter for SqlProgrammabilityProvider
#### 1.2.25 - SqlProgrammability types caching. 
#### 1.2.24 - Fixed TVP handling for SQL Azure. Open only one connection.
#### 1.2.23 - ResolutionFolder parameter
#### 1.2.22 - ToTraceString added, can be used to get executable sql statement.
#### 1.2.21 - Bugfix for Record constructor and with method.
#### 1.2.20 - Fix auto-open connection. Introduce IDisposable on SqlCommand.
#### 1.2.19 - More Mono compatibility. Set TypeName property for TVP via reflection.
#### 1.2.18 - More Mono compatibility. Set TypeName property on SqlParameter only for TVP.
#### 1.2.17 - Mono compatibility is back.
#### 1.2.16 - Fixing Azure compatibility for SqlProgrammability.
#### 1.2.15 - bug fix for string param with AllParametersOptional.
#### 1.2.14 - Fix for bit default value. Changes in RuntimeRecord.
#### 1.2.13 - Fixing GetHashCode. Renaming RuntimeRecord.
#### 1.2.12 - IDictionary<string,obj> is implemented on RuntimeRecord.
#### 1.2.11 - With method on RuntimeRecord added.
#### 1.2.10 - Introduce runtime base class for the Record. ConcurrentDictionary for cache.
#### 1.2.9 - Making input string into string option when allParametersOptional is suplied in SqlCommandProvider
#### 1.2.8 - Downgrading back to 4.0 and FSharp.Core 4.3.0
#### 1.2.7 - Runtime SqlCommand<> introduced in SqlCommandProvider. "True" synchronous Execute implemented.
#### 1.2.6 - Bugfix for nullable string iput parameters.
#### 1.2.5 - Timeout on reading command file.
#### 1.2.4 - CommandType removed from SqlCommandProvider.
#### 1.2.3 - Fixing nuget package.
#### 1.2.2 - Merging assemblies.
#### 1.2.1 - Separating namespaces for the providers.
#### 1.2.0 - Dropping Experimental from the name, adding Programmability to nuget.
#### 1.1.31 - Connection management issue. Upgdate ASAP.
#### 1.1.30 - Adding SqlTypes dll for design-time support
#### 1.1.29 - Fix issue #46 : support for SqlTypes - note that additional Nuget package is now required. Minor improvement in design-time opening of the command file.
#### 1.1.28 - BREKAING CHANGE: default ResultType is now Records. Additional xml docs. Fix Issue #43. SingleRow = true now returns option<_> instead of just value. Bugfix for FileWatcher on VS 2013. Transaction support in SqlCommand constructor. Design-time performace improvements with caching.
#### 1.1.27 - Fix issues #39 and #40. Mandatory ConnectionStringOrName parameter. ConnectionStringOrName static property.
#### 1.1.25 - Fix issue #38. Support for serialization in WebAPI. ResultType.Records erased to ExpandoObject.
#### 1.1.24 - Fix issue #34. Inferred reference types assumed to be non-nullable.
#### 1.1.23 - Fix issue #35. Change lazy ctor generation to eager.
#### 1.1.22 - Change TVP representation from tuples to custom types with ctor
#### 1.1.21 - Preserve original exception thrown by sys.spdescribefirstresult_set.
#### 1.0.020 - Support ResultType.Maps for untyped output data
#### 1.0.019 - Removing FallbackToProbeResultTypeInTransaction
#### 1.0.018 - Parametrized queries that can fallback to SET FMTONLY ON
#### 1.0.017 - Fixing FallbackToProbeResultTypeInTransaction value is not respected
#### 1.0.016 - Issue #32 fixed
#### 1.0.015 - Initial release as part of fsprojects