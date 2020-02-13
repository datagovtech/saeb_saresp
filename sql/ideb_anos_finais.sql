DROP TABLE IF EXISTS DB_FOCO..TB_SAEB_SARESP
CREATE TABLE DB_FOCO..TB_SAEB_SARESP (
	CD_ESCOLA INT,
	CICLO INT,
	ANO_LETIVO INT,
	DESEMPENHO_SAEB DECIMAL(4,2),
	DESEMPENHO_SARESP DECIMAL(4,2)
)

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

INSERT INTO DB_FOCO..TB_SAEB_SARESP (CD_ESCOLA, CICLO, ANO_LETIVO, DESEMPENHO_SAEB)
SELECT U.CD_ESCOLA,  2 AS CICLO,  U.ANO_LETIVO, U.DESEMPENHO_SAEB
FROM DB_FOCO..TB_IDEB S
UNPIVOT
(
  DESEMPENHO_SAEB
  FOR ANO_LETIVO IN ([2005],[2007],[2009],[2011],[2013],[2015],[2017])
) U;

SELECT *
FROM DB_FOCO..TB_SAEB_SARESP

SELECT TOP 10 *
FROM DB_FOCO..ANOS_INICIAIS_LIMPO

DELETE FROM DB_FOCO..ANOS_INICIAIS_LIMPO
WHERE CD_ESCOLA < 35000000 
OR CD_ESCOLA >= 36000000

ALTER TABLE DB_FOCO..ANOS_INICIAIS_LIMPO ALTER COLUMN CD_ESCOLA INT

UPDATE DB_FOCO..ANOS_INICIAIS_LIMPO SET
CD_ESCOLA = CD_ESCOLA - 35000000 

UPDATE DB_FOCO..ANOS_INICIAIS_LIMPO SET
[2005] = NULL
WHERE [2005] = ' '

UPDATE DB_FOCO..ANOS_INICIAIS_LIMPO SET
[2007] = NULL
WHERE [2007] = ' '

UPDATE DB_FOCO..ANOS_INICIAIS_LIMPO SET
[2009] = NULL
WHERE [2009] = ' '

UPDATE DB_FOCO..ANOS_INICIAIS_LIMPO SET
[2011] = NULL
WHERE [2011] = ' '

UPDATE DB_FOCO..ANOS_INICIAIS_LIMPO SET
[2013] = NULL
WHERE [2013] = ' '

UPDATE DB_FOCO..ANOS_INICIAIS_LIMPO SET
[2015] = NULL
WHERE [2015] = ' '

UPDATE DB_FOCO..ANOS_INICIAIS_LIMPO SET
[2017] = NULL
WHERE [2017] = ' '

ALTER TABLE DB_FOCO..ANOS_INICIAIS_LIMPO ALTER COLUMN [2005] DECIMAL(4,2)
ALTER TABLE DB_FOCO..ANOS_INICIAIS_LIMPO ALTER COLUMN [2007] DECIMAL(4,2)
ALTER TABLE DB_FOCO..ANOS_INICIAIS_LIMPO ALTER COLUMN [2009] DECIMAL(4,2)
ALTER TABLE DB_FOCO..ANOS_INICIAIS_LIMPO ALTER COLUMN [2011] DECIMAL(4,2)
ALTER TABLE DB_FOCO..ANOS_INICIAIS_LIMPO ALTER COLUMN [2013] DECIMAL(4,2)
ALTER TABLE DB_FOCO..ANOS_INICIAIS_LIMPO ALTER COLUMN [2015] DECIMAL(4,2)
ALTER TABLE DB_FOCO..ANOS_INICIAIS_LIMPO ALTER COLUMN [2017] DECIMAL(4,2)

INSERT INTO DB_FOCO..TB_SAEB_SARESP (CD_ESCOLA, CICLO, ANO_LETIVO, DESEMPENHO_SAEB)
SELECT U.CD_ESCOLA,  1 AS CICLO,  U.ANO_LETIVO, U.DESEMPENHO_SAEB
FROM DB_FOCO..ANOS_INICIAIS_LIMPO S
UNPIVOT
(
  DESEMPENHO_SAEB
  FOR ANO_LETIVO IN ([2005],[2007],[2009],[2011],[2013],[2015],[2017])
) U;


SELECT *
FROM DB_FOCO..TB_SAEB_SARESP

DELETE FROM DB_FOCO..ENSINO_MEDIO_LIMPO
WHERE CD_ESCOLA < 35000000 
OR CD_ESCOLA >= 36000000

ALTER TABLE DB_FOCO..ENSINO_MEDIO_LIMPO ALTER COLUMN CD_ESCOLA INT

UPDATE DB_FOCO..ENSINO_MEDIO_LIMPO SET
CD_ESCOLA = CD_ESCOLA - 35000000 

UPDATE DB_FOCO..ENSINO_MEDIO_LIMPO SET
[2017] = REPLACE([2017], ';', '')

UPDATE DB_FOCO..ENSINO_MEDIO_LIMPO SET
[2017] = NULL
WHERE [2017] = ' '


ALTER TABLE DB_FOCO..ENSINO_MEDIO_LIMPO ALTER COLUMN [2017] DECIMAL(4,2)

INSERT INTO DB_FOCO..TB_SAEB_SARESP (CD_ESCOLA, CICLO, ANO_LETIVO, DESEMPENHO_SAEB)
SELECT U.CD_ESCOLA,  3 AS CICLO,  U.ANO_LETIVO, U.DESEMPENHO_SAEB
FROM DB_FOCO..ENSINO_MEDIO_LIMPO S
UNPIVOT
(
  DESEMPENHO_SAEB
  FOR ANO_LETIVO IN ([2017])
) U;



SELECT *
FROM DB_FOCO..IDESP_DESEMPENHO

;WITH SARESP AS (
	SELECT 
		U.CODIGO_ESCOLA,  
		CASE
			WHEN U.CICLO = 'IND_DESEMPENHO_5_ANO_EF' THEN 1
			WHEN U.CICLO = 'IND_DESEMPENHO_9_ANO_EF' THEN 2
			WHEN U.CICLO = 'IND_DESEMPENHO_3_ANO_EM' THEN 3
		ELSE NULL END AS CICLO,  
		U.ANO, 
		U.DESEMPENHO_SARESP
	FROM DB_FOCO..IDESP_DESEMPENHO S
	UNPIVOT
	(
	  DESEMPENHO_SARESP
	  FOR CICLO IN ([IND_DESEMPENHO_5_ANO_EF],[IND_DESEMPENHO_9_ANO_EF],[IND_DESEMPENHO_3_ANO_EM])
	) U
)
UPDATE A SET
A.DESEMPENHO_SARESP = B.DESEMPENHO_SARESP
FROM DB_FOCO..TB_SAEB_SARESP A
	INNER JOIN SARESP B 
		ON A.ANO_LETIVO = B.ANO 
		AND A.CICLO = B.CICLO 
		AND A.CD_ESCOLA = B.CODIGO_ESCOLA



SELECT *
FROM DB_FOCO..TB_SAEB_SARESP
WHERE DESEMPENHO_SARESP IS NOT NULL
ORDER BY DESEMPENHO_SAEB DESC