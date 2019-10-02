
;;; amb.scm


;;;         Version 1.0 coded by Shoichi Hayashi (9/23/1991)
;;;              E-MAIL :  s-haya@rst.fujixerox.co.jp
;;;              MIX    :  s.hayasi


;;; Copyright (C) 1991, 1992, 1993  Shoichi Hayashi.
;;; This software is completely free to copy and distribute modifed version.
;;; But I would appreciate it if you left my name on the code as the author.

; McCarthy�ϼ��Τ褦����������ģ������������α黻�Ҥ��������[McCarthy1963]

;   amb(e1, bottom)=e1
;   amb(bottom, e2)=e2
;   amb(e1, e2)=�����Ū��e1��e2�����֡�

; ��amb(e1, e2)�����Ū���ɤ�ȡ�e1��e2�������ɾ�����ơ��᤯�֤��줿�����ͤ�
; ���μ����ͤȤ���Ȥ������ȤǤ��롣

; �ʲ��˼�����Τϡ�����amb�ΰ�Ĥ��ѷ��Ǥ��롣

; -----------------------------------------------------------------------------
; ���ݣ��������Ū���ܱ黻�ȸ����ץ������

; 1.4.1 Scheme�ؤ����������Ƴ��
; �����δؿ��ϡ�Ĺ����n�Ǥ���ꥹ��a��n+1�սꤢ��Ǥ�դΰ��֤����Ǥ򺹤�����
; �ؿ��Ǥ��롣

;     (define (insert x a)
;         (if (null? a)
;             (cons x ())
;             (amb (cons x a) (cons (car a) (insert x (cdr a))))))

;   �����ǡ��ؿ�amb��(amb exp1 exp2)��ɾ������ȡ�exp1��exp2�β��줫������
; �֤���exp1��exp2�Τɤ��餫�Ǥ��뤫�ǤϤ��뤬�����Ĥ��ͤβ���Ǥ��뤫����
; �ޤ�ʤ���ΤȤ��롣
; ��exp1��exp2�򤽤줾��׻��������˹Ųߤ��ꤲ�Ƥ��β��줫�����򤹤�ȹͤ�
; ��Ф褤����������� (- (amb 1 2) (amb 1 2)) �ϹŲߤ����ꤲ�����Ȥˤ�
; ��Τǡ�0�Ȥʤ�Ȥϸ¤�ʤ����Ȥ����դ��פ��롣��

;   ���˼����ؿ�perm��Ǥ�դ��¤��Ѥ����ꥹ�ȡ�Ǥ�դν���ˤ򡢤ޤ�choice��
; 1����n�ޤǤ��������椫��Ǥ�դ��ͤ��֤���

;     (define (perm b)
;         (if (null? b)
;             ()
;             (insert (car b) (perm (cdr b)))))

;     (define (choice n)
;         (if (= n 1)
;             1
;             (amb n (choice (-1+ n)))))

;   ����(fail)�Ȥ�������Ƴ�����롣�ץ������ʼ��ˤ�ɾ��������ǡ�(fail)��
; ������ʬ����ɾ�������˺Ѥ�褦������η��󤬤���ʤ�С���������η���
; �¹Ԥ�����ΤȤ��롣
;   amb��fail�ΰ�̣�ϡ����Τ褦�˹ͤ���Ȥ狼��䤹�����ޤ�amb��fail��ޤ�
; �ץ�������¹Ԥ�����ݵ���������Ȥ��롣���ε�����(amb exp1 exp2)�Ȥ���
; �����ͤη׻��˽в񤦤ȡ����ε����ϼ�ʬ������Ʊ�����֤ε����Υ��ԡ�����
; �������������ε����ˤ�exp1�˽в�ä��Τ�Ʊ���褦�ˡ�¾���ε����ˤ�exp2
; �˽в�ä��Τ�Ʊ���褦�˷׻���ʤᤵ���롣�����Ƥɤε�����fail�˽в񤦤�
; �����Ǥ��ε����ϼΤƤ��Ƥʤ��ʤ�ȹͤ��롣
; ��amb��fail���ɤΤ褦�˻Ȥ��뤫��8-QUEEN�Υѥ������ˤ����������롣

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

; ��������(attacks i j place)�ϡ����������֤���Ƥ���ޤ��ܤΥꥹ��place��
; �顢�������פ�i��j��Τޤ��ܤ��Ф����������֤���Τɤ줫�ν����������Ƥ�
; ��Ȥ��˸¤äƿ��Ȥʤ�Ҹ�Ǥ��롣

; ��������������ս꤫��Ƴ�������褦��ʣ����������Ф��Ƥϡ������Ū�ʴ�
; �ܱ黻���Ѥ���ľ���꼰�����������������ۤ˥ץ�����ߥ󥰤�����ϳμ�
; ��ͭ���Ǥ��롣����ͭ�����ϡ��Ǥ�����ɤ����򤬤ʤ�����ΤȤ��ơ�����
; ������򤹤�ȹͤ����ɤ�����ͳ�褹�롣

; 1.4.2 amb��fail�μ¸�ˡ

; ������ޤ�amb�ˤĤ��Ƥϡ����ΰ�̣����ݵ�����(amb exp1 exp2)�Ȥ���������
; �η׻��˽в񤦤ȡ���ʬ������Ʊ�����֤ε����Υ��ԡ����ĺ��Ф��Ȥ��Ƥ�
; �������ξ�硢������Ĥ����Ū�ʵ�������Ω�������ư��Ƥ��ɤ����Ȥˤ�
; �롣�ޤ����ºݤ������ư��뵡����¸�ߤ��ʤ���С���������ν���ʥ���
; ���塼��󥰡ˤ˽��ä��༡Ū�˥��ߥ�졼�Ȥ�����ɤ��������ǤϤ��Υ�����
; �塼��󥰤�exp1,exp2�ν�˹Ԥ����ޤ�ʣ�������������Ƴ������Ƥ�����
; �ϡ��Ǥ�᤯�ʺǶ�ˤ��������ľ���Ȥ�����§�Ȥ��롣�����õ������β�
; ˡ�Ȥ��ơ�����õ���ǤϤʤ��ķ�õ���򤹤�����������롣���Τ褦�ʥ�����
; �塼��󥰤򤹤���ˤ������ϡ�exp2�����򤹤�з׻�����ߤ��뤬��exp1��
; ��������˷׻�����λ���ʤ��Ȥ��ä����ˡ����Τη׻�����ߤ��ʤ��Ȥ���
; �����꤬���������뤳�ȤǤ��롣

; �����ơ���˼����ꥹ�Ȥˤ��amb�μ¸���ˡ�Ǥϡ�choice�ϰʲ��Τ褦��Ÿ��
; ����롣

;     (define (expand-choice n)
;         (if (= n 1)
;             1
;             (call-with-current-continuation
;                 (lambda (-c-)
;                     ((%amb-catchers-stack% 'push)
;                      (cons -c- (delay (expand-choice (-1+ n)))))
;                     n))))

;   �⤷�⡢choice��ʲ��Τ褦����������Ȥ���ȡ����Ƥβ�ǽ�ʷ�³����׻�
; ������˼¹Ԥ����뤳�Ȥˤʤ롣

;     (define (choice n)
;         (if (= n 1)
;             1
;             (amb (choice (-1+ n)) n)))

;   ���ξ��˵���Ĥ��ʤ���Фʤ�ʤ����Ȥϡ���ǽ�ʾ�礬ͭ�¤Ǥʤ���Ф�
; ��ʤ��Ȥ������ȤǤ��롣�㤨��Ǥ�դ�0�ʾ���������֤��ؿ���ͤ��Ƥߤ뤳
; �Ȥˤ��롣���Τ褦��̵�¤˲�ǽ�ʷ�³����¸�ߤ�����ˤϡ��ʲ��˼���any-
; number1������Ǥ����¾����ʬ�η׻����ʤब��any-number2��������Ѥ����
; ���Ƥβ�ǽ�ʴĶ���׻���³��������ǡ�¾����ʬ�η׻��ϱʵפ��٤餵��롣
; �����Prolog�ˤ����뺸�Ƶ�������Ȥ褯���Ƥ��롣

;     (define (any-number1)
;         (amb 0 (1+ (any-number1))))
;     (define (any-number2)
;         (amb (1+ (any-number2)) 0))

; ���ʾ�Ҥ٤Ƥ����褦������Ϥ����ΤΡ�amb��fail��call-with-current-
; continuation���Ѥ��ơ��ʲ��Τ褦��������뤳�Ȥ��Ǥ��롣

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

; ���ʾ�ǡ�amb��fail�򥷥ߥ�졼�Ȥ��뤳�ȤϤǤ����櫓�Ǥ��뤬�����Τ褦��
; ���꤬�Ĥ롣amb�ˤ�äƥ��ԡ����줿�����ϡ����ä��������ޤǤȤäƤ�������
; ���Τ��Ȥ������ȤǤ��롣��Ĥε�����fail�ˤ�äƼΤƤ��Ƥʤ��ʤäƤ��ޤ�
; ����硢�⤦��Ĥε�����ư���Ϥ�ʤ���Фʤ�ʤ�����������fail�ˤ��Ĥ�
; ���ǽв񤦤Τ������ΤǤϤʤ�������ϥȥåץ�٥�ˤ����ƤǤ��������äƤ�
; �����ʤ���ͳ�Ϥʤ������������Ƥ����Ū�ʵ����Υ��ԡ���ΤƤƽ�������뤿
; ������Τʼ�³����ɬ�פǤ��롣�����init-amb�Ȥ��Ƽ��Τ褦��������롣

    (macro init-amb (lambda (l)
        `(call-with-current-continuation
            (lambda (-c-)
                (%amb-catchers-stack% 'initialize)
                ((%amb-catchers-stack% 'push)(cons -c- #f))
                #t))))

; ������ˡ�amb�ˤ�äƥ��ԡ����줿ʣ���ε����������ε�����ޤ�Ƥɤ����
; ��뤳�Ȥ��Ǥ�����fail�˻�äƤ��ޤä����ˡ��㳰Ū�ʽ�����Ԥ����Ȥ���
; ����������Ǥ��롣��³��Ū�ˤߤ�С�����ϥץ������μ¹Էв�Τ�������
; ��ۤ��Ƹ�����Ԥ����Ȥ��������㳰������ư����Ȥ������Ȥˤʤ롣��
; �Τ褦�ʵ�����Common Lisp��unwind-protect�򿿻��Ƽ��Τ褦�˼¸����롣

    (macro amb-unwind-protect (lambda (l)
        (let ((exp1 (cadr l))(exp2 (caddr l)))
            `(call-with-current-continuation
                (lambda (-c-)
                    ((%amb-catchers-stack% 'push)
                        (cons -c- (delay ,exp2)))
                    ,exp1)))))

; ���¤�amb��amb-unwind-protect��Ʊ������Ǥ��롣

; ����ñ�ʼ¹���򼨤�����

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


;   (if (init-amb) (test) 'end)��¹Ԥ���ȡ��ޤ�protected-form��ɽ�����졢
; ³����10,9,...,1��ɽ������롣�Ǹ��cleanup-form��ɽ�����콪λ���롣

; �������ǥȥåץ�٥뤫��(fail)��ɾ�����褦�Ȥ���ʤ�С�����ɾ���򤻤���
; �Ѥ�褦��(init-amb)��#f�Ǥ��ä��褦�˿����񤦡����ʤ��end���֤äƤ���
; �Ǥ�����������ˤ⤦����(fail)��ɾ�����褦�Ȥ���ʤ�С�����ɾ�����򤱤�
; ��ˡ��¸�ߤ��ʤ��Τǡ�ɾ���������롣�����error�Ȥʤ�fail��ɾ�����Ƥ��ޤ�
; ���Ȥ�����å����������Ϥ���뤳�Ȥˤʤ롣

; 1.4.3 �������
; ����˽Ҥ٤�8-QUEEN����ǡ�(queen 8)��¹Ԥ���ȡ�92�Ĥ�������ΰ�Ĥ�
; �����롣����ǤϤ���¾�β�����ˤϤɤ�������褤�Ǥ������������ԡ�
; ���������Ϥޤ�¸�ߤ��Ƥ��뤫�顢�ȥåץ�٥뤫�鶯��Ū��(fail)��ɾ������
; ���Ȥ���С�����ɾ���򤷤ʤ��ǺѤ�褦���̤ε����ʷ��󤬡˵�ư����뤳��
; �ˤʤ롣����Ϥ��礦��Prolog�Υȥåץ�٥�ˤ�����`;'�����Ϥ�������Ū��
; �Хå��ȥ�å������뤳�Ȥ˻��Ƥ��롣���Τ褦�ˤ��Ƽ������̤β�����뤳��
; ���Ǥ��롣�Ĥޤ�����õ���ε�ǽ������Ȥ������Ȥˤʤ롣�����������Τޤޤ�
; �Ϥ��β�μ�����ɾ����̤�ƻ뤷�Ƥ���ʹ֤��ԤäƤ���ΤǤ��äơ�������
; �����뤿�����β���᤿�����ϼΤƤ��Ƥ��ޤäƤ��롣�ޤ������餫
; ���ᤤ���Ĥβ�¸�ߤ���Τ����Τ뤳�Ȥ��Ǥ��ʤ���С�(fail)����Ū�˲�
; �ٹԤäƤ褤�Τ����狼��ʤ��Ȥ�������⤢�롣�����ǡ����Ѥ����Ƥβ�ν�
; ��ʥꥹ�ȡˤ��֤��褦��setof�򼡤Τ褦��������뤳�Ȥˤ��롣

    (macro setof (lambda (l)
        (let ((proc (cadr l)))
            `(let ((stack (make-stack)))
                (amb-unwind-protect
                    (let ((ans ,proc))
                        ((stack 'push) ans)
                        (fail))
                    (reverse (stack 'stack->list)))))))

; ��������(setof (queen 8))��ɾ������С����Ƥβ�Υꥹ�Ȥ������롣

; �����̤˰�Ĥβ�򸫤Ĥ���ץ����������������⡢���Ƥβ������ץ�
; ������������뤳�Ȥ������Ϥ뤫�˺���Ǥ��뤳�Ȥ�¿�����㤨��Prolog�ˤ�
; ����õ����ǽ�����뤬������Ū�˥Хå��ȥ�å���Ԥ���ˡ���Ѥ���ȡ�Prolog
; ���ѿ��������ǤϤʤ���˥ե����������Ǥ��뤫�顢��������줿����ϼ���
; ��Ƥ��ޤ����������Ԥ����Ȥ��Ǥ��ʤ����������ʤ���Prolog�Ǥ⤳�Σ�����
; �Ҹ�Ǥ���setof��¸����뤳�Ȥϲ�ǽ�Ǥ��롣����������ѤΤ���assert�Ҹ�
; ���Ѥ��ơ�����ѿ��ؤ�������Ʊ�ͤʤ��Ȥ��Ԥ��뤫��Ǥ��롣�嵭����setof
; ������������Ѥ��Ѥ��Ƽ¸�����Ƥ��롣

; ���Ȥ����ǡ�����ޤǽҤ٤Ƥ����������塼��󥰤ˤ������Ϥ����ΤΡ���
; ���������Ǥ�amb������ץ������ؤ�fork�Ǥ��ꡢsetof�����ƤΥץ������ν�
; λ���ԤäƤ��η�̤������ꡢ��ĤΥץ��������Ϥ�join�Ǥ���Ȥߤʤ�����
; ���Ǥ��롣