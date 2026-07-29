# MeridIAn Comex

Sistema de Inteligência Operacional e Gestão de Comércio Exterior da Meridian.

## O que esta V4 entrega

- Nova identidade **MeridIAn Comex**, com `IA` destacada na marca.
- Interface SaaS desktop-first com sidebar, topbar, pesquisa e módulos operacionais.
- Dashboard executivo, Movimentações, Processos, Cargas, Pendências, Importações, Inteligência documental, Relatórios, Pesquisa, Auditoria, Cadastros e Configurações.
- Preserva integralmente a integração OpenAI da V3 para leitura multimodal de MIC/DTA, incluindo processamento paralelo e fallback seletivo.
- Mantém o mapeamento determinístico MIC -> CONTROLE, incluindo item 36 -> FATURA e item 23 -> CRT.
- Primeiro adapter operacional para relatórios SEARA `.xls/.xlsx/.xlsm`, somente leitura, seguindo o mapeamento inicial da especificação.
- Fundação PostgreSQL com variáveis de ambiente e migration inicial em `db/001_initial_schema.sql`.
- Exportação gerencial em Excel.

## Importante sobre persistência

A aplicação continua funcional sem PostgreSQL, em **Modo sessão**, para não interromper o produto já hospedado. Nesse modo, dados enviados durante a sessão não constituem banco persistente multiusuário.

Para ativar a arquitetura final, configure no Connect Cloud:

- `DB_HOST`
- `DB_PORT`
- `DB_NAME`
- `DB_USER`
- `DB_PASSWORD`
- `DB_SSLMODE`

A próxima etapa de backend é ligar repositories/services às tabelas PostgreSQL e migrar o estado atual para o banco.

## OpenAI

Configure `OPENAI_API_KEY` como secret no Posit Connect Cloud. A chave também pode ser digitada temporariamente na tela Configurações durante desenvolvimento local.

## Atualização no Connect Cloud

Depois de substituir os arquivos no seu projeto local:

```r
rsconnect::writeManifest()
```

Depois faça Commit e Push para o mesmo repositório GitHub. O Connect Cloud republicará o mesmo site conforme a configuração atual do projeto.

## Estrutura

```text
app.R
R/
  ai_extract.R
  helpers.R
  management.R
  adapter_seara.R
  db.R
db/
  001_initial_schema.sql
www/
  logo_meridian_comex.svg
  logo_meridian_comex.png
  styles.css
data/
  controle_inicial.csv
  mapeamento_controle.csv
```

## Segurança

Nunca versione `.Renviron`, chaves OpenAI, credenciais PostgreSQL ou documentos reais de clientes em repositório público.

## Login de acesso (v4.3)

O aplicativo possui uma tela inicial de autenticação. O usuário padrão é `meridian`; a senha padrão é validada por hash e não fica armazenada em texto puro no repositório. Para produção, é possível substituir as credenciais pelos secrets `MERIDIAN_APP_USER` e `MERIDIAN_APP_PASSWORD` no Posit Connect Cloud.
