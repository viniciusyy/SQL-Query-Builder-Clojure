(ns main
  (:require [clojure.string :as str]))

;; -----------------------------
;; Utilidades de formatação SQL
;; -----------------------------
;; nada de efeitos colaterais aqui, só transformar dado em string segura

(defn- sql-escape [s]
  ;; escapa apóstrofo: O'Brien -> O''Brien
  (str/replace s "'" "''"))

(defn- sql-lit [v]
  ;; converte qualquer valor em “literal SQL” (com aspas quando precisa)
  (cond
    (string? v)  (str "'" (sql-escape v) "'")
    (keyword? v) (str "'" (name v) "'")
    (symbol? v)  (str "'" (name v) "'")
    (boolean? v) (if v "true" "false")
    (nil? v)     "NULL"
    (coll? v)    (str "(" (->> v (map sql-lit) (str/join ", ")) ")")
    :else        (str v)))

(defn- wrap-parens [s]
  ;; só para colocar parênteses quando for grupo lógico
  (str "(" s ")"))

(defn- strip-parens [s]
  ;; remove parênteses externos do WHERE top-level (só para estética)
  (let [s (str/trim s)]
    (if (and (<= 2 (count s))
             (= \( (first s))
             (= \) (last s)))
      (subs s 1 (dec (count s)))
      s)))

;; -----------------------------
;; Comparadores (folhas do WHERE)
;; -----------------------------
;; aqui é o “dicionário” de operadores suportados

(defn- render-comparador
  "Gera SQL de um mapa comparador."
  [{:keys [campo igual_a diferente maior_que menor_que
           maior_ou_igual menor_ou_igual em como nulo nao_nulo entre] :as m}]
  (let [col (name campo)]
    (cond
      (contains? m :igual_a)        (str col " = "  (sql-lit igual_a))
      (contains? m :diferente)      (str col " <> " (sql-lit diferente))
      (contains? m :maior_que)      (str col " > "  (sql-lit maior_que))
      (contains? m :menor_que)      (str col " < "  (sql-lit menor_que))
      (contains? m :maior_ou_igual) (str col " >= " (sql-lit maior_ou_igual))
      (contains? m :menor_ou_igual) (str col " <= " (sql-lit menor_ou_igual))
      (contains? m :em)             (str col " IN " (sql-lit em))
      (contains? m :como)           (str col " LIKE " (sql-lit como))
      (true? nulo)                  (str col " IS NULL")
      (true? nao_nulo)              (str col " IS NOT NULL")
      (and (vector? entre) (= 2 (count entre)))
      (let [[a b] entre]
        (str col " BETWEEN " (sql-lit a) " AND " (sql-lit b)))
      :else (throw (ex-info "Comparador inválido/ausente" {:comparador m})))))

;; -----------------------------------
;; Árvore lógica (AND/OR) recursiva
;; -----------------------------------
;; e_s / ou_s montam a structure do WHERE; render-expr caminha recursivamente

(defn e_s  [nos] {:and (vec nos)})
(defn ou_s [nos] {:or  (vec nos)})

(defn- render-expr [expr]
  (cond
    (nil? expr) ""
    ;; AND: junta condições com "AND" e põe parênteses no grupo
    (and (map? expr) (:and expr))
    (->> (:and expr) (map render-expr) (filter seq) (str/join " AND ") wrap-parens)

    ;; OR: mesma ideia, com "OR"
    (and (map? expr) (:or expr))
    (->> (:or expr) (map render-expr) (filter seq) (str/join " OR ")  wrap-parens)

    ;; folha (comparador)
    (map? expr) (render-comparador expr)

    :else (throw (ex-info "Expressão WHERE desconhecida" {:expr expr}))))

;; ---------------------------------------------------
;; Etapas do builder (funções de alta ordem)
;; ---------------------------------------------------
;; cada etapa recebe e devolve "state" (padrão state -> state)

(defn campos [xs]
  ;; define as colunas do SELECT; se não chamar, depois vira "*"
  (fn [state] (assoc state :campos (vec xs))))

(defn filtros [expr]
  ;; define a árvore WHERE (comparadores + e_s/ou_s)
  (fn [state] (assoc state :where expr)))

(defn gerar
  "Etapa final — retorna função que, dado o estado, devolve a string SQL."
  ([] (gerar {}))
  ([{:keys [quote-identifiers?] :or {quote-identifiers? false}}]
   (fn [{:keys [tabela campos where]}]
     ;; ident serve para colocar aspas em identificadores se quiser (Postgres-like)
     (let [ident (fn [s] (if quote-identifiers? (str "\"" s "\"") s))
           sel   (if (seq campos)
                   (->> campos (map name) (map ident) (str/join ", "))
                   "*")
           from  (ident tabela)
           wsql  (some-> where render-expr strip-parens str/trim)
           where-clause (when (seq wsql) (str " WHERE " wsql))]
       (str "SELECT " sel " FROM " from (or where-clause ""))))))

;; ---------------------------------------------------
;; Construtor base
;; ---------------------------------------------------
;; busca_tabela devolve uma função que aceita N etapas e aplica via reduce

(defn busca_tabela [tabela]
  (fn [& steps]
    (let [initial {:tabela tabela}]
      (reduce (fn [acc step] (step acc)) initial steps))))

;; ---------------------------------------------------
;; Helpers com currying (qual operador? qual campo? valor?)
;; ---------------------------------------------------
;; fica bem gostoso de compor: ((maior-que :idade) 20)

(defn cmp [op]
  (fn [campo]
    (fn [valor]
      {:campo campo op valor})))

(def igual-a         (cmp :igual_a))
(def diferente-de    (cmp :diferente))
(def maior-que       (cmp :maior_que))
(def menor-que       (cmp :menor_que))
(def maior-ou-igual  (cmp :maior_ou_igual))
(def menor-ou-igual  (cmp :menor_ou_igual))
(def como-like       (cmp :como))

(defn em*       [campo valores] {:campo campo :em valores})
(defn nulo*     [campo]         {:campo campo :nulo true})
(defn nao-nulo* [campo]         {:campo campo :nao_nulo true})
(defn entre*    [campo a b]     {:campo campo :entre [a b]})

;; ----------------
;; Demo (-main)
;; ----------------
;; rodando imprime a SQL do exemplo; bom para testar rápido

(defn -main [& _]
  (let [sql ((busca_tabela "usuario")
             (campos ["abc" "xyz"])
             (filtros
               (e_s
                 [{:campo :nome   :igual_a "José"}
                  {:campo :idade  :maior_que 20}
                  {:campo :id     :em [10 20 30]}
                  {:campo :status :igual_a true}
                  (ou_s [{:campo :camiseta :igual_a :verde}
                         {:campo :camiseta :igual_a :azul}])]))
             (gerar))]
    (println sql)))
