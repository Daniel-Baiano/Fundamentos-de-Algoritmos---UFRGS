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
;; 2.
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
;; grau: ..... -> ......

;; altura: ..... -> ......

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
