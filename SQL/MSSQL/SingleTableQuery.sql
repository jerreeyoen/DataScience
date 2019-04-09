select * 
from Purchasing.PurchaseOrderDetail

use AdventureWorks2016;
go

select PurchaseOrderDetailID, ProductID, UnitPrice,
	CASE 
		WHEN UnitPrice > 100				THEN 'HIGH Quality'
		WHEN UnitPrice between 30 and 100   THEN 'Medium Quality'
		WHEN UnitPrice <30					THEN 'Low Quality'
		else 'Unknown'
	END AS quality
from Purchasing.PurchaseOrderDetail

select PurchaseOrderDetailID, ProductID, UnitPrice,
	IIF(UnitPrice >100, 'good', 'poor') as quality
from Purchasing.PurchaseOrderDetail

select name, description
from sys.fn_helpcollations()

select 'abc' + 'bcd' as test, CONCAT('abc','bcd') as test2

select CHARINDEX('bcd', 'abcdefgbcd'), CHARINDEX('bcd', 'abcdefgbcd',3)

SELECT  patindex('%[0-3]%', 'ABC942asdf')
use AdventureWorks2016;
go
SELECT BusinessEntityID, RIGHT(REPLICATE('0',9) + CAST(BusinessEntityID as varchar(10)),10) as formattedid
FROM Person.Person

SELECT STUFF('abcdefg', 3, 2,'testtest')

SET LANGUAGE British;
Select CAST('06/11/2018' AS DATETIME);

SET LANGUAGE us_english;
Select CAST('06/11/2018' AS DATETIME);

SELECT CONVERT(DATETIME, '06/11/2018', 101);
SELECT CONVERT(DATETIME, '06/11/2018', 103);

SELECT PARSE('06/11/2018' AS DATETIME USING 'en-US');
SELECT PARSE('06/11/2018' AS DATETIME USING 'en-GB');

SELECT
GETDATE() AS [GETDATE],
CURRENT_TIMESTAMP AS [CURRENT_TIMESTAMP],
GETUTCDATE() AS [GETUTCDATE],
SYSDATETIME() AS [SYSDATETIME],
SYSUTCDATETIME() AS [SYSUTCDATETIME],
SYSDATETIMEOFFSET() AS [SYSDATETIMEOFFSET];

SELECT DATENAME(MONTH,CURRENT_TIMESTAMP)
SELECT SWITCHOFFSET(CURRENT_TIMESTAMP, '-05:00')