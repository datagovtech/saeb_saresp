CREATE TABLE DB_FOCO..TB_IDEB (
 	CD_ESCOLA VARCHAR(100),
 	[2005] VARCHAR(100),
 	[2007] VARCHAR(100),
 	[2009] VARCHAR(100),
 	[2011] VARCHAR(100),
 	[2013] VARCHAR(100),
 	[2015] VARCHAR(100),
 	[2017] VARCHAR(100)
)

 
SELECT *
FROM DB_FOCO..TB_IDEB

DELETE FROM DB_FOCO..TB_IDEB
WHERE CD_ESCOLA < 35000000 
OR CD_ESCOLA >= 36000000

ALTER TABLE DB_FOCO..TB_IDEB ALTER COLUMN CD_ESCOLA INT

UPDATE DB_FOCO..TB_IDEB SET
CD_ESCOLA = CD_ESCOLA - 35000000 

UPDATE DB_FOCO..TB_IDEB SET
[2005] = NULL
WHERE [2005] = '-'

UPDATE DB_FOCO..TB_IDEB SET
[2007] = NULL
WHERE [2007] = '-'

UPDATE DB_FOCO..TB_IDEB SET
[2009] = NULL
WHERE [2009] = '-'

UPDATE DB_FOCO..TB_IDEB SET
[2011] = NULL
WHERE [2011] = '-'

UPDATE DB_FOCO..TB_IDEB SET
[2013] = NULL
WHERE [2013] = '-'

UPDATE DB_FOCO..TB_IDEB SET
[2015] = NULL
WHERE [2015] = '-'

UPDATE DB_FOCO..TB_IDEB SET
[2017] = NULL
WHERE [2017] = '-'


UPDATE DB_FOCO..TB_IDEB SET
[2005] = REPLACE([2005], ',', '.')

ALTER TABLE DB_FOCO..TB_IDEB ALTER COLUMN [2005] DECIMAL(4,2)

UPDATE DB_FOCO..TB_IDEB SET
[2007] = REPLACE([2007], ',', '.')

ALTER TABLE DB_FOCO..TB_IDEB ALTER COLUMN [2007] DECIMAL(4,2)

UPDATE DB_FOCO..TB_IDEB SET
[2009] = REPLACE([2009], ',', '.')

ALTER TABLE DB_FOCO..TB_IDEB ALTER COLUMN [2009] DECIMAL(4,2)

UPDATE DB_FOCO..TB_IDEB SET
[2011] = REPLACE([2011], ',', '.')

ALTER TABLE DB_FOCO..TB_IDEB ALTER COLUMN [2011] DECIMAL(4,2)

UPDATE DB_FOCO..TB_IDEB SET
[2013] = REPLACE([2013], ',', '.')

ALTER TABLE DB_FOCO..TB_IDEB ALTER COLUMN [2013] DECIMAL(4,2)

UPDATE DB_FOCO..TB_IDEB SET
[2015] = REPLACE([2015], ',', '.')

ALTER TABLE DB_FOCO..TB_IDEB ALTER COLUMN [2015] DECIMAL(4,2)

UPDATE DB_FOCO..TB_IDEB SET
[2017] = REPLACE([2017], ',', '.')

ALTER TABLE DB_FOCO..TB_IDEB ALTER COLUMN [2017] DECIMAL(4,2)

SELECT U.CD_ESCOLA, U.ANO, U.DESEMPENHO
FROM DB_FOCO..TB_IDEB S
UNPIVOT
(
  DESEMPENHO
  FOR ANO IN ([2005],[2007],[2009],[2011],[2013],[2015],[2017])
) U;