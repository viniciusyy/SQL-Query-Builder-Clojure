# Clojure SQL Query Builder

Query Builder 100% funcional em **Clojure**: você compõe a consulta como “legos” de funções
(`busca_tabela` → `campos` → `filtros` → `gerar`), com suporte a **AND/OR** aninhados,
comparadores variados e estilo imutável/puro.

## ✨ Principais recursos
- **Funções puras** e **alta ordem**: cada etapa é `state -> state`, encadeada com `reduce`.
- **e_s / ou_s**: árvores lógicas com AND/OR aninháveis.
- **Comparadores**: `=`, `<>`, `>`, `<`, `>=`, `<=`, `IN`, `LIKE`, `IS NULL`, `IS NOT NULL`, `BETWEEN`.
- **Currying/Parciais** para construir comparadores de forma elegante.
- Saída SQL em string; opção para `quote-identifiers?`.

