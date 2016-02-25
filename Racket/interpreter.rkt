#lang racket

(define calc
  (lambda (exp)
    (match exp                                ; 匹配表达式的两种情况
      [(? number? x) x]                       ; 是数字,直接返回
      [`(,op ,e1 ,e2)                         ; 匹配并且提取出操作符 op 和两个操作数 e1, e2
       (let ([v1 (calc e1)]                   ; 递归调用 calc 自己，得到 e1 的值
             [v2 (calc e2)])                  ; 递归调用 calc 自己，得到 e2 的值
         (match op                            ; 分支：处理操作符 op 的 4 种情况
           ['+ (+ v1 v2)]                     ; 如果是加号，输出结果为 (+ v1 v2)
           ['- (- v1 v2)]                     ; 如果是减号，乘号，除号，相似的处理
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))])))



(define cal
  (lambda (exp)
    (match exp
      [(? number? x) x]
      [(list op e1 e2)
       (let ([v1 (cal e1)]
             [v2 (cal e2)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 以下三个定义 env0, ext-env, lookup 是对环境(environment)的基本操作:

;; 空环境
(define env0 '())

;; 扩展。对环境 env 进行扩展，把 x 映射到 v，得到一个新的环境
(define ext-env
  (lambda (x v env)
    ;(cons `(,x . ,v) env)))
    ;(cons (cons x v) env)))
   ;`((,x . ,v) . ,env)))
    `((,x . ,v) . ,env)))

;; 查找。在环境中 env 中查找 x 的值
(define lookup
  (lambda (x env)
    (let ([p (assq x env)])
      (cond
       [(not p) x]
       [else (cdr p)]))))

;; 闭包的数据结构定义，包含一个函数定义 f 和它定义时所在的环境
(struct Closure (f env))

;; 解释器的递归定义（接受两个参数，表达式 exp 和环境 env）
;; 共 5 种情况（变量，函数，调用，数字，算术表达式）
(define interp1
  (lambda (exp env)
    (match exp                                          ; 模式匹配 exp 的以下情况（分支）
      [(? symbol? x) (lookup x env)]                    ; 变量
      [(? number? x) x]                                 ; 数字
      [`(lambda (,x) ,e)                                ; 函数
       (Closure exp env)]
      [`(,e1 ,e2)                                       ; 调用
       (let ([v1 (interp1 e1 env)]
             [v2 (interp1 e2 env)])
         (match v1
           [(Closure `(lambda (,x) ,e) env1)
            (interp1 e (ext-env x v2 env1))]
           [_ "no matching"]))]
      [`(,op ,e1 ,e2)                                   ; 算术表达式
       (let ([v1 (interp1 e1 env)]
             [v2 (interp1 e2 env)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]
           [_ "no matching"]))]
      [_ "no matching"])))

;; 解释器的“用户界面”函数。它把 interp1 包装起来，掩盖第二个参数，初始值为 env0
(define interp
  (lambda (exp)
    (interp1 exp env0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(cons '(1 2) '((2 3) (3 4)))
;(assq 1 (list (list 1 2) (list 1 3)))

;(assq 2 '((1 2) (2 3) (3 4)))

;(interp '(((lambda (x) (lambda (y) (- x y))) 1) 2))

(define a 1)
(define b 2)
(define c 'ccc)

(list a b)
(list 'a 'b)

`(a ,b)

(match '(+ 1 2)
  [`(,a ,b ,c) a]
  ;[(list a b c) a]
  [_ 0])

(cons `(,c . 3) (cons `(,a . 1) (cons `(b . 2) '())))

(ext-env c 3 (ext-env a 1 (ext-env `b 2 env0)))

(lookup 'a (ext-env c 3 (ext-env 'b 2 (ext-env `a 1 env0))))


(struct test (a b c))

(match (test 'sdf 'asdf 'ads)
  [(test a b c) a]
  [_ "n"])
