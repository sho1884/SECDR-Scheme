;;
;; keiro.scm    by Shoichi Hayashi.
;;

; レイヤード・ストリームによる経路探索問題

; レイヤード・ストリームの利点は、真にCPUが複数あれば、始点からのみならず、
; 各点から同時に計算を開始できることである。

; 下の図のようなグラフ上で、AからIへ至るためのループを含まない経路を
; 全て求めよ。

;   A --- B --- E --- H --- I
;   |     |     |     |
;   |     D --- G     |
;   |                 |
;   C ------ F ------ J

(extend-syntax (node)
  ((node X List)(fcons-stream X (filter X List))))

(define (good-path)
  (letrec ((A 'a)
	   (B (node 'b (list A E D)))
	   (C (node 'c (list A F)))
	   (D (node 'd (list B G)))
	   (E (node 'e (list B G H)))
	   (F (node 'f (list C J)))
	   (G (node 'g (list D E)))
	   (H (node 'h (list E I J)))
	   (I (node 'i (list H)))
	   (J (node 'j (list F H))))
    I))

(define (filter X List)
  (cond ((null? List) '())
	((null? (head List))(filter X (tail List)))
	((eq? (head List) 'a)(fcons-stream 'a (filter X (tail List))))
	((eq? (caar List) X)(filter X (tail List)))
	(else (fcons-stream
	       (fcons-stream (caar List)
			     (filter X (tail (head List))))
	       (filter X (tail List))))))

(define (instantiate stream)
  (cond ((null? stream) '())
	((pair? stream)
	 (cons (instantiate (head stream))(instantiate (tail stream))))
	(else (touch stream))))

(define ans (good-path))
(pretty-print (instantiate ans))

; 答の読みとり方について

; 以下のような解が得られるが、一般にリストの先頭要素の後ろには '*' が、
; 二つ目以降の要素は括弧で括られて '+' で結合されていると考える。
; すなわち(x y z)は(x*[y+z])である。また(x)は(x*0)である。

; (i
;   (h
;     (e
;       (b a (d (g)))
;       (g (d (b a))))
;     (j (f (c a)))))

; 上の例を書き直すと次のようになる。

; (i*
;   (h*
;    [(e*
;      [(b* [a+(d*(g*0))])+
;       (g* (d* (b* a)))])+
;     (j* (f* (c* a)))]))

; そしてこれを通常の四則演算と同様に分配法則を用いて展開する。

; i*h*[e*[b*[a+d*g*0]+g*d*b*a]+j*f*c*a]
; ih[e[b[a+0]+gdba]+jfca]
; ih[e[b[a]+gdba]+jfca]
; ih[e[ba+gdba]+jfca]
; ih[eba+egdba+jfca]
; iheba+ihegdba+ihjfca

; 最後の行の '+' で結合された各々の項が答である。
; すなわちiheba, ihegdba, ihjfcaの３つの解が得られる。
