# Projeto Webhook

**Webhooks** são mecanismos de comunicação baseados em eventos (*event-driven*) que permitem que um sistema envie notificações automáticas a outro assim que determinados eventos ocorrem — eliminando a necessidade de **polling** contínuo.

Neste projeto, integramos um **gateway de pagamentos simulado** ao backend de uma loja por meio de um endpoint `POST /webhook`, implementado em **Haskell**. Quando o gateway reporta um evento de pagamento, o serviço executa as seguintes etapas:

1. **Valida** o payload JSON recebido, verificando:

   - Se o JSON está completo e bem formado;
   - Se todos os campos obrigatórios estão presentes e não são nulos;
   - Se o campo `amount` representa um número válido (inteiro ou decimal);
   - Se o valor de `amount` é positivo;
   - Se o código de moeda está de acordo com o padrão;
   - Se o campo `timestamp` segue o formato ISO 8601 UTC (e.g., `"2023-10-01T12:34:56Z"`);
   - Se o campo `event` é igual a `"payment_success"`.

2. **Verifica a autenticidade** da requisição por meio do cabeçalho `X-Webhook-Token`;

3. **Confirma** a transação válida com um `POST /confirmar`;

4. **Cancela** a transação se o payload for inválido, enviando um `POST /cancelar`;

5. **Ignora** a requisição completamente se o token for inválido (sem confirmação nem cancelamento).


---
## Principais Bibliotecas 

| Biblioteca                     | Função no Projeto                                             |
|-------------------------------|---------------------------------------------------------------|
| `servant`, `servant-server`   | Definição da API REST e roteamento das requisições HTTP      |
| `warp`                         | Servidor HTTP responsável por escutar na porta 5000 |
| `aeson`                        | Serialização e desserialização de JSON  |
| `req`                          | Usado para enviar requisições HTTP de confirmação e cancelamento |
| `time`                         | Validação e parsing de timestamps      |
| `containers` (`Set`)           | Armazenamento eficiente e consulta de tipos de moedas |
| `text`, `bytestring`, `scientific` | Manipulação eficiente de strings (`Text`, `ByteString`) e números decimais (`Scientific`) |


---

## Organização do Repositório

```
.
├── app/
│   └── Main.hs            -- Ponto de entrada do servidor HTTP
├── src/
│   ├── Config.hs          -- Configurações (URLs, porta, lista de moedas)
│   ├── Logic.hs           -- Regras de negócio: validação, confirmação, cancelamento
│   ├── Types.hs           -- Modelos de dados e definição da API
│   └── Webhook.hs         -- Handlers para a rota /webhook
├── python_tests/
│   ├── requirements.txt   -- Dependências: requests, fastapi, uvicorn
│   └── test_webhook.py    -- Testes automáticos fornecidos
├── Webhook-Project.cabal  -- Descrição do pacote Cabal
└── README.md              -- (este arquivo)
```

---

## Descrição dos Módulos

| Módulo | Responsabilidade Principal |
|--------|----------------------------|
| **`Main.hs`** | Sobe o servidor Warp/Servant na porta 5000 e expõe a API. |
| **`Webhook.hs`** | Handler de alto nível: decodifica JSON, roteia para sucesso/falha e retorna respostas HTTP (200/400/403) ao servidor. |
| **`Types.hs`** | Define tipos e estruturas de dados: `Transaction`, `WebhookConfig` e o tipo de rota `WebhookAPI`. Implementa `FromJSON` com validação customizada de campos. |
| **`Config.hs`** | URLs de confirmação/cancelamento e **set** de moedas válidas. |
| **`Logic.hs`** | Lógica de Negócio: `validateTransaction`, `validateToken`, `confirmTransaction`, `cancelTransaction`, etc. |
| **`python_tests/test_webhook.py`** | Teste Simples em Python. |

---

## Como Executar

1. **Clone** o projeto  
   ```bash
   git clone https://github.com/PedroPauloMorenoCamargo/Webhook_Project.git
   cd Webhook-Project
   ```

2. **Compile** e **execute** o servidor:  
   ```bash
   cabal update
   cabal build
   cabal run
   ```
   O console exibirá:  
   ```
   [INFO] 🔥 Starting server on http://localhost:5000/webhook …
   ```

3. **(Opcional) Rodar testes Python**  
   ```bash
   python -m venv venv && source venv/bin/activate
   cd python_tests
   pip install -r requirements.txt
   python test_webhook.py
   ```

---

## Objetivos Atingidos

| Obejtivos | Status |
|---------------------|:------:|
| Servidor HTTP com rota `POST /webhook` | ✔️ |
| Validação de payload (estrutura + tipos) | ✔️ |
| Verificação de veracidade (token) | ✔️ |
| **Confirma** transação em caso de sucesso | ✔️ |
| **Cancela** transação em caso de divergência | ✔️ |
| **Ignora** transação se token inválido | ✔️ |
| Persistência em banco de dados | ❌ |
| Servidor HTTPS | ❌  |

---

## Uso de Inteligência Artificial

A IA foi empregada para:

- **Geração do esqueleto inicial** do projeto, com estrutura Cabal;
- Sugestão de **exemplos de uso das bibliotecas** `req` (cliente HTTP) e `aeson` (serialização JSON);
- **Refatoração do código** e em alguns casos construção de funçõees;
- Fornecimento de **exemplos de uso de funções de bibliotecas externas**;
- **Debugging**;
- **Sugerir tipos de funções** e esclarecer assinaturas de bibliotecas externas;
- **Melhoria da documentação** e aprimoramento das mensagens de log para torná-las mais descritivas;
- **Comentar o código**.


