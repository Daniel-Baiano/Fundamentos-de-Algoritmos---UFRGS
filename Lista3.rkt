;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Lista3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;#####################################################################################################
;;
;; LISTA AVALIADA 3
;;
;#####################################################################################################
;; IDENTIFICAÇÃO DO GRUPO
;; Escreva abaixo, em ordem alfabética, o nome e número de matrícula de todos os membros do grupo:
;;    Nome Sobrenome Matrícula
;; 1. André Vitor Gabriel - 00297326
;; 2. Daniel Rocha Silva - 00335625
;; 3. Gabrielly Christine dos Santos Moraes 00343872
;; 4.
;#####################################################################################################

;; ============================================
;; DEFINIÇÕES DE TIPOS DE DADOS (não modificar)
;; ============================================

;; ------------------
;; TIPO GENERO:
;; ------------------
;; Um Genero é
;; 1. a string "feminino", ou
;; 2. a string "masculino", ou
;; 3. a srting "outros"

;; ------------------
;; TIPO FUNCIONARIO:
;; ------------------
(define-struct funcionario (nome genero anos-empresa destaque?))
;; Um elemento do conjunto Funcionario tem o formato
;;     (make-funcionario n g a d) onde
;;      n: String, é o nome do funcionário,
;;      g: Genero, é o genero do funcionário,
;;      a: Número, representa os anos de serviço na empresa do funcionário,
;;      d: Boolean, representa se funcionário foi o destaque o mês.

;; ------------------
;; TIPO SETOR:
;; ------------------
(define-struct setor (nome membros))
;; Um elemento do conjunto Setor tem o formato
;;     (make-setor nome lmemb) onde
;;     nome: String, é o nome do setor e
;;     lmemb: Lista-membros, é uma lista de membros deste setor.

;; ------------------
;; TIPO LISTA-MEMBROS:
;; ------------------
;; Uma Lista-membros é
;; 1. empty (vazia), ou
;; 2. (cons f lm), onde f : Funcionario, e lm : Lista-membros, ou
;; 3. (cons s lm), onde s : Setor, e  lm : Lista-membros.

;; =========================================================================
;;                                 QUESTÃO 1
;; =========================================================================

;;Funcionarios:
(define PEDRO (make-funcionario "Pedro"
                                "Masculino"
                                20
                               #true))
(define ANTONI (make-funcionario "Antoni"
                                "Outros"
                                10
                               #true))
(define MATEO (make-funcionario "Mateo"
                                "Masculino"
                                5
                               #false))
(define MARIA (make-funcionario "Maria"
                                "Feminino"
                                15
                               #true))
(define JULIA (make-funcionario "Julia"
                                "Feminino"
                                30
                               #false))
(define MARCOS (make-funcionario "Marcos"
                                "Masculino"
                                4
                               #false))
(define PAMELA (make-funcionario "Pamela"
                                "Outros"
                                1
                               #false))
(define HARLEY (make-funcionario "Harley"
                                "Feminino"
                                5
                               #false))
(define BRUCE (make-funcionario "Bruce"
                                "Masculino"
                                1
                               #true))
(define MARK (make-funcionario "Mark"
                                "Masculino"
                                1
                               #false))
(define LUARA (make-funcionario "Luara"
                                "Outros"
                                9
                               #true))
(define JAQUI (make-funcionario "Jaqui"
                                "Masculino"
                                3
                               #false))
(define YUNY (make-funcionario "Yuny"
                                "Outros"
                                5
                               #false))
(define MIGUEL (make-funcionario "Miguel"
                                "Outros"
                                1
                               #true))
(define MURI (make-funcionario "Muri"
                                "Masculino"
                                3
                               #false))
(define RENATA (make-funcionario "Renata"
                                "Feminino"
                                3
                               #false))
;;sub-Setores:
(define DEV (make-setor "Desenvolvimento"(list PEDRO ANTONI)))
(define QA(make-setor "Game tester" (list HARLEY MARCOS)))
(define ARTE_CONCEITUAL(make-setor "Artista Conceitual" (list LUARA JAQUI)))
(define ANIMACAO(make-setor "Animação 2D" (list MARK BRUCE)))
(define MIDIA(make-setor "Midia social" (list PAMELA MATEO)))
(define CONTEUDO(make-setor "Marketing de Conteúdo"(list MARIA JULIA)))
(define TRILHA_SONORA(make-setor "Trilha Sonora do jogo"(list YUNY MURI)))
(define DUB(make-setor "Dubladores"(list MIGUEL RENATA)))

;;SETORES:
(define GRAPH (make-setor "Graficos do game"(list ARTE_CONCEITUAL ANIMACAO)))
(define PROG (make-setor "Programação do game"(list DEV QA)))
(define MARKETING (make-setor "Marketing do game"(list MIDIA CONTEUDO))) 
(define SONS(make-setor "Trilha sonora do game"(list TRILHA_SONORA DUB)))

;;Setor maior
(define UFRGS_GAMERS (make-setor "UFRGS_GAMERS" (list GRAPH PROG MARKETING SONS)))
;; =========================================================================
;;                                 QUESTÃO 2
;; =========================================================================
;; quantidade-por-genero: Setor Genero -> Numero
;; Dado um setor e um gênero, devolve o número de funcionários deste gênero no setor

(define (aux lm genero)
  (cond
    [(empty? lm) 0]
    [(funcionario? (first lm))
     (cond
       [(string=? (funcionario-genero (first lm)) genero)
        (+ 1 (aux (rest lm) genero))]
       [else
        (aux (rest lm) genero)])]
    [(setor? (first lm))
     (+ (aux (setor-membros (first lm)) genero)
        (aux (rest lm) genero))]))

(define (quantidade-por-genero setor genero)
  (aux (setor-membros setor) genero))

;;check-expect:

;; =========================================================================
;;                                 QUESTÃO 3
;; =========================================================================
;; maximo: ListaDeNumerosNaturais -> Numero
;;objetivo: Dada uma Lista de numeros naturais, devolve o numero maximo, caso a lista esteja vazia, devolve zero
(define (maximo ldnn)
  (cond
    [(empty? ldnn)0]
    [(empty? (rest ldnn)) (first ldnn)]
    [else
     (cond
       [(> (first ldnn) (maximo (rest ldnn))) (first ldnn)]
       [else (maximo (rest ldnn))]
      )]))

;;check-expect:
(check-expect (maximo empty) 0)
(check-expect (maximo (list 1)) 1)
(check-expect (maximo (list 8 9 14 32 1 0 19 3)) 32)
(check-expect (maximo (list 90 6 100 87 13)) 100)

;; =========================================================================
;;                                 QUESTÃO 4
;; =========================================================================
;---------------------
; TIPO SetorOuFuncionario
;---------------------
;Um SetorOuFuncionario é
;; 1. um Setor, ou
;; 2. um Funcionario




;; Função Auxiliar ------------------  

;; grau-lista: Lista-membros -> Numero
;; Objetivo: Dada uma lista de membros, devolve o número de elementos dessa lista.
;; Exemplos:
;; 1° Exemplo: (grau-lista (list PEDRO ANTONI)) -> 2
;; 2° Exemplo: (grau-lista (list YUNY MURI)) - > 2                                     
                   
                   
(define (grau-lista lista)
  ;Dada uma lista, verfica
  (cond
    ;Se a lista está vazia, devolve 0
    [(empty? lista) 0]
    ;Senão
    [else
     ;Soma 1 ao grau do resto da lista
     (+ 1 (grau-lista (rest lista)))])) 



;; testes:
(check-expect (grau-lista (list PEDRO)) 1)
(check-expect (grau-lista (list PEDRO MURI)) 2)



;; grau-setor: SetorOuFuncionario -> Numero
;; Objetivo: Dado um setor, devolve o grau do setor.
;; Exemplos:
;; 1° exemplo: (grau-setor GRAPH) -> 2
;; 2° exemplo: (grau-setor UFRGS_GAMERS) -> 4

(define (grau-setor setor)
  ;Dado um setor, verifica
  (cond
    ;Se for um funcionario, retorna 0 (porque um funcionario é uma folha e tem grau 0)
    [(funcionario? setor) 0]
    ;Senão
    [else
     ;Verifica o grau da lista de membros do setor
     (grau-lista (setor-membros setor))]))


;;TESTES:
(check-expect (grau-setor GRAPH) 2)
(check-expect (grau-setor UFRGS_GAMERS) 4)



;; grau-recursivo-lista: Lista-membros -> Lista
;; Objetivo: Dado uma Lista-membros, devolve uma lista de graus de cada elemento da Lista-membros
;; Exemplos:
;; 1° EXEMPLO: (grau-recursivo-lista (list GRAPH PROG)) -> (list 2 2)
;; 2° EXEMPLO: (grau-recursivo-lista (list MARKETING PROG)) -> (list 2 2)


;;Corpo
(define (grau-recursivo-lista lista)
  ;Dada uma lista de membros, verifica
  (cond
    ;Se a lista está vazia, devolve empty
    [(empty? lista) empty]
    ;Senão
    [else
     ;Cria uma lista com o grau do primeiro elemento da lista
     (cons (grau (first lista))
           ;e o grau do resto da lista
           (grau-recursivo-lista (rest lista)))]))

;;TESTES
 (check-expect (grau-recursivo-lista (list GRAPH PROG)) (list 2 2))
 (check-expect (grau-recursivo-lista (list MARKETING PROG)) (list 2 2))

 
;função principal ----------------------


;; ;; grau: SetorOuFuncionario -> Número
;; Objetivo: Dado um setor ou um funcionario, devolve o maior grau da árvore a qual esse setor ou esse
;; funcionario pertence
;; Exemplos:
;; 1° Exemplo:  (grau SONS) -> 2
;; 2° Exemplo:  (grau UFRGS_GAMERS) -> 4

(define (grau nó)
  ;Dado um nó (setor ou funcionário), verifica
  (cond
    ;Se o nó for um funcionário, devolve 0
    [(funcionario? nó) 0]
    ;Senão, devolve
    [else
     ;o maior grau da lista formada pelo o grau do setor (já que o nó não é funcionário)
     (maximo
      (cons (grau-setor nó)
            ;e a lista de graus dos elementos da lista de mebros do setor
            (grau-recursivo-lista (setor-membros nó))))]))


;;TESTES
 (check-expect (grau SONS) 2) 
 (check-expect (grau UFRGS_GAMERS) 4)
 
 
 ;;Função Auxiliar --------------
 
;; altura-lista: Lista-membros -> Lista
;; Objetivo: Dada uma lista de membros, devolve uma lista com as alturas
;; de cada elemento da lista
;; Exemplos
;; 1° Exemplo: (altura-lista (list PAMELA MATEO)) -> (list 0 0)
;; 2° Exemplo: (altura-lista (list GRAPH PROG))   -> (list 2 2)               

(define (altura-lista lista)
  ;Dada uma lista de membros, verifica
  (cond
    ;Se a lista está vazia, devolve empty
    [(empty? lista) empty]
    ;Senão
    [else
     ;cria uma lista com a altura do primeiro elemento da lista
     (cons (altura (first lista))
           ;e a altura do resto da lista
                (altura-lista (rest lista)))]))


;Testes:
(check-expect (altura-lista (list GRAPH PROG)) (list 2 2))
(check-expect (altura-lista (list PAMELA MATEO)) (list 0 0))

;; Função Principal -----

;; altura: SetorOuFuncionario -> Numero
;; Objetivo: Dado um setor ou funcionario, devolve a altura da árvore a
;; qual este setor ou essa árvore fazem parte
;; Exemplos e testes:
;; 1° Exemplo: (altura MATEO) -> 0
;; 2° Exemplo: (altura PROG)   -> 2  
                   

(define (altura setor)
  ;Dado um setor ou funcionário, verifica
  (cond
    ;Se for um funcionário, devolve 0(porque um funcionario não possui lista de membros) 
    [(funcionario? setor) 0]
    ;Se a lista de membros do setor estiver vazia, devolve 0 (porque significa que
    ;não hhá setores ou funcionarios nesse setor)
    [(empty? (setor-membros setor)) 0]
    ;Senão;
    [else
     ;soma 1 ao maior número da lista de alturas de setores
     (+ 1 (maximo (altura-lista (setor-membros setor))))]))


;;TESTES
 (check-expect (altura GRAPH) 2)
 (check-expect (altura PAMELA) 0)
 

;; =========================================================================
;;                                 QUESTÃO 5
;; =========================================================================
;; gera-imagem-setor: setor -> imagem
;;obj: dado um setor desenhar a arvore desse setor, ou seja, o setor, seus subsetores e os funcionarios
;;ex:
;;(gera-imagem-setor DUB)


;;; 
(define (gera-imagem-setor setor)
  (above
   (cria-setor setor)
   (line 0 30 "black")
   (line 400 0 "black")
   (cria-imagem-lista (setor-membros setor))))

;;funções auxiliares

;; cria-funcionario: Funcionario -> Imagem
;; Objetivo: Dado um funcionario, gera uma imagem com o nome e os anos que está na
;; empresa. A fonte muda de cor conforme o gênero


(define (cria-funcionario funcionario)
  ;Dado um funcionário,
  (cond
    ;Se for do genero feminino, cria imagem com o texto vermelho
    [(string=? (funcionario-genero funcionario) "Feminino")
     (overlay
      (beside
       (text (funcionario-nome funcionario) 10 "red")
       (rectangle 10 10 "outline" "transparent")
       (text (number->string (funcionario-anos-empresa funcionario)) 10 "red"))
      (rectangle 70 40 "outline" "black"))]
    ;Se for do genero masculino, cria imagem com o texto verde escuro
    [(string=? (funcionario-genero funcionario) "Masculino")
     (overlay
      (beside
       (text (funcionario-nome funcionario) 10 "darkgreen")
       (rectangle 10 10 "outline" "transparent")
       (text (number->string (funcionario-anos-empresa funcionario)) 10 "darkgreen"))
      (rectangle 70 40 "outline" "black"))]
    ;Se for de outro genero, cria imagem com texto laranja escuro
    [(string=? (funcionario-genero funcionario) "Outros")
     (overlay
      (beside
       (text (funcionario-nome funcionario) 10 "darkorange")
       (rectangle 10 10 "outline" "transparent")
       (text (number->string (funcionario-anos-empresa funcionario)) 10 "darkorange"))
      (rectangle 70 40 "outline" "black"))]))

;; cria-destaque: Funcionario -> Imagem
;; Objetivo: Dado um funcionario, gera uma imagem com o nome e os anos que está na
;; empresa. A fonte muda de cor conforme o gênero e para funcionários que possuam destaque,
;; acrescenta uma estrela amarela a imagem
;;

(define (cria-destaque funcionario)
  ;Dado um funcionário
  (cond
    ;Se esse funcionário possuir destaque, coloca uma estrela amarela
    [(funcionario-destaque? funcionario)
     (overlay/align "right" "top"
      (star-polygon 10 5 2 "solid" "yellow")
      (cria-funcionario funcionario))]
    ;Senão, apenas cria a imagem sem a estrela
    [else
     (cria-funcionario funcionario)]))

;; cria-setor: Setor -> Imagem
;; Objetivo: Dado um Setor, gera uma imagem com o nome do setor


(define (cria-setor setor)
  (overlay
    (text (setor-nome setor) 15 "white")
   (rectangle 200 50 "solid" "blue")))

;; cria-imagem-lista: Lista-membros -> Imagem
;; Objetivo: Dado uma lista de membros, gera uma imagem com os elementos da lista, lado a lado


(define (cria-imagem-lista lista)
  ;Dada uma lista de membros
  (cond
    ;Se a lista estiver vazia, devolve uma imagem vazia
    [(empty? lista) empty-image]
    ;Se o primeiro elemento da lista for um funcionário, cria imagem para funcionario
    ;e para o resto da lista
    [(funcionario? (first lista))
        (beside
         (above
          (line 0 35 "black")
          (cria-destaque (first lista)))
         (rectangle 10 10 "outline" "transparent")
         (cria-imagem-lista (rest lista)))]
    ;Se o primeiro elemento da lista for um setor, cria imagem para setor
    ;e para os funcionários desse setor
    [(setor? (first lista))
        (beside
         (rectangle 20 20 "outline" "transparent")
         (above
          (line 0 30 "black")
          (cria-setor (first lista))
          (cria-imagem-lista (setor-membros (first lista))))
         (rectangle 20 20 "outline" "transparent")
         (cria-imagem-lista (rest lista)))]))
;; =========================================================================
;;                                 QUESTÃO 6
;; =========================================================================
;; insere-funcionario: ..... -> ......
