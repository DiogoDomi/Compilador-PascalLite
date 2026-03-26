# ⚙️ PascalLite Compiler & MEPA Virtual Machine

Um compilador de passagem única (*single-pass*) construído totalmente do zero em **Python** para a linguagem hipotética PascalLite. O projeto engloba todo o pipeline de compilação, desde a análise léxica até a geração de código e execução em uma Máquina de Pilha (MEPA) customizada.

Este projeto foi desenvolvido com foco em aprofundar conhecimentos em arquitetura de compiladores, teoria da computação, estruturas de dados e Clean Code.

## 🏗️ Arquitetura do Pipeline

O processo de compilação não utiliza geradores de parsers automáticos (como YACC/Bison). Todo o fluxo foi implementado manualmente, garantindo controle total sobre a árvore de execução e a gestão de memória:

1. **Analisador Léxico (Lexer):** Simulação de Autômatos Finitos Determinísticos (AFD) para tokenização do código-fonte e tratamento de *lookahead*.
2. **Analisador Sintático (Parser):** Implementado via Recursão Descendente (*Top-Down*), garantindo a validação estrita da gramática da linguagem.
3. **Analisador Semântico:** Gerenciamento de Tabela de Símbolos com complexidade **O(1)** (utilizando *Hash Maps*/Dicionários), checagem de tipos, controle de escopo e gerador de rótulos para desvios.
4. **Geração de Código:** Tradução simultânea das instruções validadas para o conjunto de instruções da MEPA (Máquina de Execução de Pascal) utilizando a regra de Pós-Ordem para expressões matemáticas.

## 📂 Estrutura do Projeto

A arquitetura do código foi refatorada para manter a separação de responsabilidades (*Separation of Concerns*):

```text
pascallite-compiler/
│
├── src/                        # Núcleo do Compilador
│   ├── AnalisadorLexico.py     # Tokenização
│   ├── AnalisadorSintatico.py  # Regras Gramaticais
│   ├── AnalisadorSemantico.py  # Tabela de Símbolos
│   ├── Atomo.py                # Estrutura de Dados (NamedTuple)
│   └── Atomos.py               # Enums de Tokens
│
├── examples/                   # Casos de teste válidos e inválidos
│   └── teste1.pas
│
└── main.py                     # Orquestrador do pipeline
```

## 💻 Como Executar

Clone o repositório e execute o compilador passando o arquivo fonte como argumento. O compilador irá gerar as instruções da Máquina Virtual no terminal.

```bash
# Clonar o repositório
git clone https://github.com/DiogoDomi/pascallite-compiler.git
cd pascallite-compiler

# Executar o compilador com um arquivo de teste
python main.py examples/teste3.pas
```
*(Nota: Se executado sem argumentos, o `main.py` buscará um arquivo de teste padrão).*

## 📝 Exemplo de Código (PascalLite)

**Entrada (`teste3.pas`):**
```pascal
program calculaFatorial;
var
  num, fatorial, contador: integer;
begin
  read(num);
  fatorial := 1;
  contador := 1;
  while contador <= num do
  begin
    fatorial := fatorial * contador;
    contador := contador + 1;
  end;
  write(fatorial);
end.
```

## 📄 Licença
Distribuído sob a licença MIT. Veja `LICENSE` para mais informações.
