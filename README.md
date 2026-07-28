# MERIDIAN AI | Controle de Embarques - V3

Aplicativo R Shiny para leitura de MIC/DTA escaneado com IA multimodal, consolidação por MIC e preenchimento automático do controle de embarques.

## O que mudou na V3

### Extração mais rápida
O perfil **Rápido**, recomendado para lotes, converte as páginas para JPEG a 165 dpi, usa GPT-5 mini como primeira leitura e envia até 3 páginas simultaneamente por padrão. O retorno da IA também é mais compacto: em vez de repetir 47 rótulos e 47 objetos completos para cada página, o modelo devolve somente os 47 valores e a lista de campos incertos.

Quando **Reprocessar automaticamente páginas críticas com GPT-5** estiver marcado, somente páginas em que os itens críticos 4, 5, 23, 33, 34, 36 ou 38 estejam ausentes/duvidosos são reenviadas ao modelo de maior precisão.

Perfis disponíveis:
- **Rápido**: GPT-5 mini, JPEG 165 dpi, processamento simultâneo. Recomendado para as 33 páginas.
- **Equilibrado**: 190 dpi, respeita o modelo selecionado e usa paralelismo moderado.
- **Máxima precisão**: 220 dpi e uma chamada por vez.

## Preenchimento automático do CONTROLE

A base principal segue exatamente as colunas A:M do arquivo fornecido:

`URF`, `IMPORTADOR`, `EXPORTADOR`, `MERCADORIA`, `FATURA`, `DI`, `PROCESSO`, `CRT`, `TRANSPORTADORA`, `DATA REGISTRO`, `DATA LIBERAÇÃO`, `ARQUIVO`, `ATUALIZAÇÃO`.

Com **Preencher o Controle automaticamente após a extração** marcado, uma linha é criada para cada MIC consolidado.

Mapeamento principal:
- URF: item 24, com inferência da Aduana de destino.
- IMPORTADOR: item 34.
- EXPORTADOR: item 33.
- MERCADORIA: item 38.
- FATURA: item 36. O número após `FACTURA DE EXPORTACION` é extraído preservando zeros, por exemplo `001-002-0000255`.
- CRT: item 23, por exemplo `PY290200084`.
- TRANSPORTADORA: item 1.
- ARQUIVO: nome do PDF.
- ATUALIZAÇÃO: data/hora da sincronização.

DI, PROCESSO, DATA REGISTRO e DATA LIBERAÇÃO ficam vazios quando não houver uma fonte inequívoca no MIC. O aplicativo não inventa esses valores.

Ao importar uma planilha `.xlsm`, `.xlsx` ou `.xls`, o sistema procura primeiro uma aba chamada **CONTROLE**. O aplicativo evita duplicar registros já existentes quando encontra o mesmo CRT ou a mesma FATURA e complementa somente células vazias.

## Abas
- **Dashboard**: indicadores do lote e do controle.
- **Extração IA**: PDF, intervalo, execução e resumo de tempo.
- **MICs extraídos**: uma linha por MIC, itens 1 a 47.
- **Revisão**: leituras marcadas como incertas.
- **Auditoria**: log de processamento por página.
- **Controle de embarques**: base A:M e tabela de mapeamento MIC → CONTROLE.
- **Configurações**: API, perfil, modelo, simultaneidade e fallback.

## Instalação

```r
source("install_packages.R")
```

Configure a chave:

```r
Sys.setenv(OPENAI_API_KEY = "sua-chave")
```

ou use a aba Configurações.

Execute:

```r
shiny::runApp()
```

## Primeiro teste recomendado
1. Perfil **Rápido**.
2. Chamadas simultâneas = **3**.
3. Fallback GPT-5 = **ativado**.
4. Preenchimento automático do Controle = **ativado**.
5. Processe 3 páginas e confira FATURA e CRT.
6. Em seguida processe o lote completo.

A velocidade real depende da conexão, do limite da conta OpenAI e do tempo de inferência da API. Processamento simultâneo reduz o tempo total do lote, mas não garante uma redução linear.
