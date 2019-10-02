
;;; amb.scm


;;;         Version 1.0 coded by Shoichi Hayashi (9/23/1991)
;;;              E-MAIL :  s-haya@rst.fujixerox.co.jp
;;;              MIX    :  s.hayasi


;;; Copyright (C) 1991, 1992, 1993  Shoichi Hayashi.
;;; This software is completely free to copy and distribute modifed version.
;;; But I would appreciate it if you left my name on the code as the author.

; McCarthyは次のような性質を持つ２項の非決定性の演算子を定義た。[McCarthy1963]

;   amb(e1, bottom)=e1
;   amb(bottom, e2)=e2
;   amb(e1, e2)=非決定的にe1かe2を選ぶ。

; 式amb(e1, e2)を操作的に読むと、e1とe2を並列に評価して、早く返された方の値を
; この式の値とするということである。

; 以下に示すものは、このambの一つの変形である。

; -----------------------------------------------------------------------------
; １−４　非決定的基本演算と後戻りプログラム

; 1.4.1 Schemeへの非決定性の導入
; 　次の関数は、長さがnであるリストaのn+1箇所ある任意の位置に要素を差し込む
; 関数である。

;     (define (insert x a)
;         (if (null? a)
;             (cons x ())
;             (amb (cons x a) (cons (car a) (insert x (cdr a))))))

;   ここで、関数ambは(amb exp1 exp2)を評価すると、exp1かexp2の何れか一方を
; 返す。exp1とexp2のどちらかであるかではあるが、２つの値の何れであるかは定
; まらないものとする。
; （exp1とexp2をそれぞれ計算し、次に硬貨を投げてその何れかを選択すると考え
; ればよい。そうすると (- (amb 1 2) (amb 1 2)) は硬貨を２度投げたことにな
; るので、0となるとは限らないことに注意を要する。）

;   次に示す関数permは任意に並べ変えたリスト（任意の順列）を、またchoiceは
; 1からnまでの整数の中から任意の値を返す。

;     (define (perm b)
;         (if (null? b)
;             ()
;             (insert (car b) (perm (cdr b)))))

;     (define (choice n)
;         (if (= n 1)
;             1
;             (amb n (choice (-1+ n)))))

;   次に(fail)という式を導入する。プログラム（式）の評価の途中で、(fail)と
; いう部分式を評価せずに済むような選択の系列があるならば、その選択の系列が
; 実行されるものとする。
;   ambとfailの意味は、次のように考えるとわかりやすい。まずambとfailを含む
; プログラムを実行する抽象機械があるとする。この機械が(amb exp1 exp2)という
; 式の値の計算に出会うと、その機械は自分と全く同じ状態の機械のコピーを一つ
; 作りだし、片方の機械にはexp1に出会ったのと同じように、他方の機械にはexp2
; に出会ったのと同じように計算を進めさせる。そしてどの機械もfailに出会うと
; そこでその機械は捨てられてなくなると考える。
; 　ambとfailがどのように使われるかを8-QUEENのパズルを例にして説明する。

;     (define (attacks i j place)
;         (if (null? place)
;             #f
;             (let ((ii (caar place))
;                   (jj (cdar place)))
;                 (cond ((= i ii) #t)
;                       ((= j jj) #t)
;                       ((= (+ i j) (+ ii jj)) #t)
;                       ((= (- i j) (- ii jj)) #t)
;                       (else (attacks i j (cdr place)))))))

;     (define (addqueen i n place)
;         (let ((j (choice n)))
;             (if (attacks i j place)
;                 (fail)
;                 (let ((newplace (cons (cons i j) place)))
;                     (if (= i n)
;                         newplace
;                         (addqueen (1+ i) n newplace))))))

;     (define (queen n)
;         (addqueen 1 n ()))

; 　ここで(attacks i j place)は、女王の配置されているます目のリストplaceか
; ら、チェス盤のi行j列のます目に対し、この配置の中のどれかの女王が利いてい
; るときに限って真となる述語である。

; 　非決定性が何箇所かで導入されるような複雑な問題に対しては、非決定的な基
; 本演算を用いて直接定式化する方が後戻りを陽にプログラミングするよりは確実
; に有利である。その有利さは、最も具合の良い選択がなされるものとして、勝手
; な選択をすると考えて良い点に由来する。

; 1.4.2 ambとfailの実現法

; 　これまでambについては、その意味を抽象機械が(amb exp1 exp2)という式の値
; の計算に出会うと、自分と全く同じ状態の機械のコピーを一つ作り出すとしてき
; た。その場合、この二つの抽象的な機械は独立に並列に動作しても良いことにな
; る。また、実際に並列に動作する機械が存在しなければ、ある特定の順序（スケ
; ジューリング）に従って逐次的にシミュレートすれば良い。ここではそのスケジ
; ューリングをexp1,exp2の順に行い、また複数の非決定性が導入されている場合
; は、最も近く（最近）の選択をやり直すという規則とする。これは探索問題の解
; 法として、横型探索ではなく縦型探索をする事に相当する。このようなスケジ
; ューリングをする事による欠点は、exp2を選択すれば計算が停止するが、exp1を
; 選んだ場合に計算が終了しないといった場合に、全体の計算が停止しないといっ
; た問題が起こり得ることである。

; 　さて、後に示すリストによるambの実現方法では、choiceは以下のように展開
; される。

;     (define (expand-choice n)
;         (if (= n 1)
;             1
;             (call-with-current-continuation
;                 (lambda (-c-)
;                     ((%amb-catchers-stack% 'push)
;                      (cons -c- (delay (expand-choice (-1+ n)))))
;                     n))))

;   もしも、choiceを以下のように定義したとすると、全ての可能な継続点を計算
; した後に実行に入ることになる。

;     (define (choice n)
;         (if (= n 1)
;             1
;             (amb (choice (-1+ n)) n)))

;   この場合に気をつけなければならないことは、可能な場合が有限でなければな
; らないということである。例えば任意の0以上の整数を返す関数を考えてみるこ
; とにする。このような無限に可能な継続点が存在する場合には、以下に示すany-
; number1の定義であれば他の部分の計算が進むが、any-number2の定義を用いると
; 全ての可能な環境を計算し続けるだけで、他の部分の計算は永久に遅らされる。
; これはPrologにおける左再帰の問題とよく似ている。

;     (define (any-number1)
;         (amb 0 (1+ (any-number1))))
;     (define (any-number2)
;         (amb (1+ (any-number2)) 0))

; 　以上述べてきたような制約はあるものの、ambとfailはcall-with-current-
; continuationを用いて、以下のように定義することができる。

    (define %amb-catchers-stack% (make-stack))
    (macro amb (lambda (l)
        (let ((exp1 (cadr l))(exp2 (caddr l)))
            `(call-with-current-continuation
                (lambda (-c-)
                    ((%amb-catchers-stack% 'push)
                        (cons -c- (delay ,exp2)))
                    ,exp1)))))

    (define (fail)
        (if (%amb-catchers-stack% 'empty?)
            (error "FAIL evaluated.")
            (let ((first-continuation (%amb-catchers-stack% 'pop)))
                ((car first-continuation) (force (cdr first-continuation))))))

; 　以上で、ambとfailをシミュレートすることはできたわけであるが、次のような
; 問題が残る。ambによってコピーされた機械は、いったい何時までとっておけば良
; いのかということである。一つの機械がfailによって捨てられてなくなってしまっ
; た場合、もう一つの機械が動作を始めなければならない。しかし、failにいつど
; こで出会うのかは明確ではない。それはトップレベルにおいてでさえ起こっては
; いけない理由はない。そこで全ての抽象的な機械のコピーを捨てて初期化するた
; めの明確な手続きが必要である。これをinit-ambとして次のように定義する。

    (macro init-amb (lambda (l)
        `(call-with-current-continuation
            (lambda (-c-)
                (%amb-catchers-stack% 'initialize)
                ((%amb-catchers-stack% 'push)(cons -c- #f))
                #t))))

; 　さらに、ambによってコピーされた複数の機械が、元の機械も含めてどれも解に
; 至ることができずにfailに至ってしまった場合に、例外的な処理を行うことがで
; きると便利である。手続き的にみれば、これはプログラムの実行経過のある地点
; を越えて後戻りを行おうとした場合に例外処理を起動するということになる。こ
; のような機構をCommon Lispのunwind-protectを真似て次のように実現する。

    (macro amb-unwind-protect (lambda (l)
        (let ((exp1 (cadr l))(exp2 (caddr l)))
            `(call-with-current-continuation
                (lambda (-c-)
                    ((%amb-catchers-stack% 'push)
                        (cons -c- (delay ,exp2)))
                    ,exp1)))))

; 　実はambとamb-unwind-protectは同じ定義である。

; 　簡単な実行例を示そう。

;     (define (test-body)
;         (let ((value (choice 10)))
;             (write value)
;             (newline)
;             (fail)))

;     (define (test)
;         (amb-unwind-protect
;             (begin
;                 (display "protected-form")
;                 (newline)
;                 (test-body))
;             (begin
;                 (display "cleanup-form")
;                 (newline))))


;   (if (init-amb) (test) 'end)を実行すると、まずprotected-formが表示され、
; 続いて10,9,...,1が表示される。最後にcleanup-formが表示され終了する。

; 　ここでトップレベルから(fail)を評価しようとするならば、その評価をせずに
; 済むように(init-amb)が#fであったように振る舞う。すなわちendが返ってくる
; であろう。さらにもう一度(fail)を評価しようとするならば、その評価を避ける
; 方法は存在しないので、評価が起こる。これはerrorとなりfailを評価してしまっ
; たというメッセージが出力されることになる。

; 1.4.3 全解収集
; 　先に述べた8-QUEENの例で、(queen 8)を実行すると、92個ある解の中の一つが
; 得られる。それではその他の解を求めるにはどうしたらよいであろうか。コピー
; した機械はまだ存在しているから、トップレベルから強制的に(fail)を評価しよ
; うとすれば、その評価をしないで済むような別の機械（系列が）起動されること
; になる。これはちょうどPrologのトップレベルにおいて`;'を入力し、強制的に
; バックトラックさせることに似ている。このようにして次々に別の解を得ること
; ができる。つまり全解探索の機能があるということになる。しかし、このままで
; はその解の収集は評価結果を監視している人間が行っているのであって、新たな
; 解を求めるために先の解を求めた機械は捨てられてしまっている。また、あらか
; じめいくつの解が存在するのかを知ることができなければ、(fail)を強制的に何
; 度行ってよいのかがわからないという問題もある。そこで、汎用の全ての解の集
; 合（リスト）を返すようなsetofを次のように定義することにする。

    (macro setof (lambda (l)
        (let ((proc (cadr l)))
            `(let ((stack (make-stack)))
                (amb-unwind-protect
                    (let ((ans ,proc))
                        ((stack 'push) ans)
                        (fail))
                    (reverse (stack 'stack->list)))))))

; 　ここで(setof (queen 8))を評価すれば、全ての解のリストが得られる。

; 　一般に一つの解を見つけるプログラムを定義するよりも、全ての解を得るプロ
; グラムを定義することの方がはるかに困難であることが多い。例えばPrologには
; 全解探索機能があるが、強制的にバックトラックを行う方法を用いると、Prolog
; の変数は代入ではなくユニフィケーションであるから、一度得られた情報は失わ
; れてしまい全解収集を行うことができない。しかしながらPrologでもこの２階の
; 述語であるsetofを実現することは可能である。それは副作用のあるassert述語
; を用いて、大域変数への代入と同様なことが行えるからである。上記したsetof
; の定義も副作用を用いて実現されている。

; 　ところで、これまで述べてきたスケジューリングによる問題はあるものの、そ
; の制約の中ではambは並列プロセスへのforkであり、setofは全てのプロセスの終
; 了を待ってその結果を受け取り、一つのプロセスへ渡すjoinであるとみなすこと
; ができる。
