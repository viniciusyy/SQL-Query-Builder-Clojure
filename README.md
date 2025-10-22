# Projeto Clojure - SQL Query Builder

## Visão Geral
Este projeto é um construtor de queries SQL escrito em Clojure. Ele permite criar consultas SQL de forma programática usando uma interface funcional.

## Estrutura do Projeto
- `main.clj` - Arquivo principal contendo todo o código do SQL builder

## Configuração do Ambiente
- **Linguagem**: Clojure 1.11.3
- **Dependências do sistema**: clojure (instalado via Nix)
- **Comando de execução**: `clojure -M main.clj`

## Como Executar
O projeto está configurado com um workflow que executa automaticamente. A função `-main` é chamada ao final do arquivo para demonstrar o uso do builder.

## Funcionalidades
O SQL builder oferece:
- Seleção de campos
- Filtros com operadores (=, <>, >, <, >=, <=, IN, LIKE, IS NULL, IS NOT NULL, BETWEEN)
- Operadores lógicos (AND/OR)
- Escape automático de strings
- Suporte para múltiplos tipos de dados (string, boolean, números, coleções)

