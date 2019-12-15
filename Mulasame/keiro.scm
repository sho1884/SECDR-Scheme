;;
;; keiro.scm    by Shoichi Hayashi.
;;

; �쥤�䡼�ɡ����ȥ꡼��ˤ���ϩõ������

; �쥤�䡼�ɡ����ȥ꡼��������ϡ�����CPU��ʣ������С���������Τߤʤ餺��
; ��������Ʊ���˷׻��򳫻ϤǤ��뤳�ȤǤ��롣

; ���οޤΤ褦�ʥ���վ�ǡ�A����I�ػ�뤿��Υ롼�פ�ޤޤʤ���ϩ��
; ���Ƶ��衣

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

; �����ɤߤȤ����ˤĤ���

; �ʲ��Τ褦�ʲ������뤬�����̤˥ꥹ�Ȥ���Ƭ���Ǥθ��ˤ� '*' ����
; ����ܰʹߤ����Ǥϳ�̤ǳ���� '+' �Ƿ�礵��Ƥ���ȹͤ��롣
; ���ʤ��(x y z)��(x*[y+z])�Ǥ��롣�ޤ�(x)��(x*0)�Ǥ��롣

; (i
;   (h
;     (e
;       (b a (d (g)))
;       (g (d (b a))))
;     (j (f (c a)))))

; �������ľ���ȼ��Τ褦�ˤʤ롣

; (i*
;   (h*
;    [(e*
;      [(b* [a+(d*(g*0))])+
;       (g* (d* (b* a)))])+
;     (j* (f* (c* a)))]))

; �����Ƥ�����̾�λ�§�黻��Ʊ�ͤ�ʬ��ˡ§���Ѥ���Ÿ�����롣

; i*h*[e*[b*[a+d*g*0]+g*d*b*a]+j*f*c*a]
; ih[e[b[a+0]+gdba]+jfca]
; ih[e[b[a]+gdba]+jfca]
; ih[e[ba+gdba]+jfca]
; ih[eba+egdba+jfca]
; iheba+ihegdba+ihjfca

; �Ǹ�ιԤ� '+' �Ƿ�礵�줿�ơ��ιब���Ǥ��롣
; ���ʤ��iheba, ihegdba, ihjfca�Σ��Ĥβ������롣
