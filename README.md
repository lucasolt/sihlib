# sihlib

Funções em R para analisar o SIH/SUS (AIH reduzida, SIH-RD): carga com cache, filtros,
reconstrução de internações, contagem por município e taxas com razão observado/esperado.

Nasceu do notebook de internações psiquiátricas (CID F) e foi generalizado para qualquer CID.
Nenhuma função lê variável global: tudo entra por argumento. Nenhum dado é versionado neste repositório.

## Instalação

```r
install.packages("remotes")
remotes::install_github("lucasolt/sihlib")   # instala também o microdatasus (GitHub)
install.packages("googledrive")              # opcional, para o cache no Drive
```

## Fluxo

```r
library(sihlib)

base <- sih_carregar(ufs = c("RS", "SC"), comp_ini = "2024-01-01", comp_fim = Sys.Date(),
                     dir_cache = "sih_databases")
attr(base, "log")   # UF, competência, origem (local, drive, datasus, indisponivel) e linhas

int <- base |>
  sih_filtrar(cid = "F", espec = c("05", "87")) |>
  reconstruir_internacoes(gap = 1)

cont <- contar_municipio(int, por = "residencia", anos = 2024:2025)
taxa <- taxa_municipio(cont, pop, por = 10000)   # pop: data.frame(municipio, pop[, ano])
```

## Funções

| Função | O que faz |
|---|---|
| `sih_carregar()` | Baixa o SIH-RD por UF e competência, com cache local → Drive → DATASUS. Mês ainda não publicado é pulado. |
| `sih_filtrar()` | Filtra por CID (prefixos ou tabela de faixas `grupo, de, ate`), especialidade do leito (`ESPEC`) e data de internação (`DT_INTER`). |
| `reconstruir_internacoes()` | Junta as linhas do mesmo `N_AIH` e emenda AIHs do mesmo paciente que começam até `gap` dia(s) após a anterior. |
| `contar_municipio()` | Internações por município (6 dígitos) e ano, de residência ou de ocorrência. |
| `taxa_municipio()` | Taxa do período, esperado pela taxa do conjunto, razão O/E e IC 95% de Poisson exato. |
| `garantir_pasta()`, `subir_pasta()` | Pasta no Google Drive (cria se não existir) e envio de uma pasta local inteira. |

## Cache

Um arquivo por UF e competência: `sih_rd_<uf>_<aaaa_mm>.rds`, com a competência inteira e todas as
colunas como texto (evita conflito de tipos entre meses). O SIH-RD é publicado pela UF do **hospital**:
para UFs diferentes de `uf_residencia` (padrão `"43"`, RS), o cache guarda só os residentes dessa UF.
Use `rebaixar = c("2026_03")` para baixar de novo competências já guardadas (ex.: meses reapresentados).

## Reconstrução das internações

1. Linhas com o mesmo `N_AIH` (AIH principal, `IDENT == "1"`, e continuações) viram uma AIH.
   AIHs cuja principal não está na base são descartadas.
2. AIHs do mesmo paciente no mesmo hospital que começam até `gap` dia(s) após o fim da anterior viram
   uma internação (`emendado = TRUE`). O paciente é identificado por hospital + data de nascimento +
   sexo + município de residência. AIHs sem data de nascimento não são emendadas.

Filtrar por data **antes** da reconstrução pode cortar uma sequência de AIHs na borda do período.
Para evitar, reconstrua com um período mais largo e filtre `ini` depois.

## Taxas

- `taxa` = observado / pessoas-ano × `por`. Pessoas-ano = soma da população nos anos (se `pop` tiver
  coluna `ano`) ou população × número de anos.
- `esperado` = pessoas-ano × taxa do conjunto dos municípios de `pop` (padronização indireta sem estratos:
  não ajusta por idade ou sexo).
- `razao_oe` = observado / esperado, com IC 95% de Poisson exato para o observado, dividido pelo esperado.

## Colab

`modelo_colab.ipynb` (runtime R) tem três células: autenticação no Drive e instalação, parâmetros e o pipeline.

## Licença

MIT.
