;https://rextester.com/l/common_lisp_online_compiler
;(load "reshM-CLISP.lsp")
;(declaim (sb-ext:muffle-conditions cl:warning))
;(declaim (sb-ext:muffle-conditions cl:style-warning))


; ******** НАЧАЛО АЛГОРИТМА ******** ;

;#||#
(defun f1 (x y z) (+ x (* y z)))
(defun f2 (x y z) (SQRT (+ (* 2 x y) (* z z))))
(defun f3 (x y z) (- x (* y z)))
(defun f4 (x y z) (- (/ x y) (* 0.5 y z)))
(defun f5 (x y z) (SQRT (- (* x x) (* 2 y z))))
(defun f6 (x y z) (+ (* x y) (* 0.5 y y z)))
(defun f7 (x y z) (/ (- (* x x) (* y y)) (* 2 z)))
(defun f8 (x y z) (/ (- x y) z))
(defun f9 (x y z) (/ (- (SQRT (+ (* x x) (* 2 y z))) x) y))
(defun f10 (x y z) (/ (* 2 (- x (* y z))) (* z z)))
(defun f11 (x y) (SQRT (/ (* 2 x) y)))
(defun f13 (x y z) (/ (/ x y) z))
(defun f14 (x y) (/ (* 2 x) (* y y)))
(defun f15 (x y z) (* x y z))
(defun f16 (x y) (* 0.5 x y y))
(defun f12 (x y) (/ x y))
(defun f17 (x y z) (+ (- x y) z))
(defun f18 (x y) (x y) (* x y))
;#||#

(defvar Механика '(((f1 3) (f2 3) (f3 3) (f4 3) (f5 3) (f6 3) (f7 3) (f8 3) (f9 3) (f10 3) (f11 2) (f12 2) (f13 3) (f14 2) (f15 3) (f16 2) (f17 3) (f18 2)) (
(<Spd2> (f1 <Spd1> <Accl> <Time>) (f2 <Accl> <Dist> <Spd1>) (f11 <KinEn2> <Mass>)) 
(<Spd1> (f3 <Spd2> <Accl> <Time>) (f4 <Dist> <Time> <Accl>) (f5 <Spd2> <Accl> <Dist>) (f11 <KinEn1> <Mass>)) 
(<Dist> (f6 <Spd1> <Time> <Accl>) (f7 <Spd2> <Spd1> <Accl>) (f12 <OpPull> <FrPull>) (f12 <OpFric> <FrFric>)) 
(<Time> (f8 <Spd2> <Spd1> <Accl>) (f9 <Spd1> <Accl> <Dist>)) 
(<Accl> (f8 <Spd2> <Spd1> <Time>) (f10 <Dist> <Spd1> <Time>) (f7 <Spd2> <Spd1> <Dist>) (f8 <FrPull> <FrFric> <Mass>)) 
(<Mass> (f8 <FrPull> <FrFric> <Accl>) (f13 <FrFric> <CfFric> <Cfg>) (f14 <KinEn2> <Spd2>) (f14 <KinEn1> <Spd1>)) 
(<FrPull> (f1 <FrFric> <Mass> <Accl>) (f12 <OpPull> <Dist>)) 
(<FrFric> (f3 <FrPull> <Mass> <Accl>) (f15 <Cfg> <CfFric> <Mass>) (f12 <OpFric> <Dist>)) 
(<CfFric> (f13 <FrFric> <Mass> <Cfg>)) 
(<KinEn2> (f16 <Mass> <Spd2>) (f17 <OpPull> <OpFric> <KinEn1>)) 
(<KinEn1> (f16 <Mass> <Spd1>) (f17 <KinEn2> <OpPull> <OpFric>)) 
(<OpPull> (f18 <FrPull> <Dist>) (f17 <KinEn2> <KinEn1> <OpFric>)) 
(<OpFric> (f18 <FrFric> <Dist>) (f17 <OpPull> <KinEn2> <KinEn1>))
)))

;#|
;(defun fact (x) (COND ((MINUSP x) (RAISEERROR "Аргумент FACT отрицателен")) ((EQ x 0) 1) (T (TIMES x (fact (DIFFERENCE x 1))))))
(defun copy (x) (COND ((ATOM x) x) (T (CONS (copy (CAR x)) (copy (CDR x))))))
(defun appendd (x y) (COND ((NULL x) y) (T (CONS (CAR x) (appendd (CDR x) y)))))
(defun equall (x y) (COND ((ATOM x) (EQ x y)) ((ATOM y) NIL) ((equall (CAR x) (CAR y)) (equall (CDR x) (CDR y))) (T NIL)))
(defun memberr (x y) (COND ((NULL y) NIL) ((equall x (CAR y)) T) (T (memberr x (CDR y)))))
(defun memb (x y) (PROG NIL L (COND ((NULL y) (RETURN NIL)) ((EQ x (CAR y)) (RETURN T))) (SETQ y (CDR y)) (GO L)))
(defun rev1 (u v) (COND ((NULL u) v) (T (rev1 (CDR u) (CONS (rev (CAR u)) v)))))
(defun rev (x) (COND ((ATOM x) x) (T (rev1 x NIL))))
(defun removee (x L) (COND ((NULL L) NIL) ((equall x (CAR L)) (removee x (CDR L))) (T (CONS (CAR L) (removee x (CDR L))))))
(defun removef (x y) (COND ((NULL y) NIL) ((equall x (CAR y)) (CDR y)) (T (CONS (CAR y) (removef x (CDR y))))))
(defun lastt (x) (COND ((NULL x) NIL) ((NULL (CDR x)) (CAR x)) (T (lastt (CDR x)))))
(defun lengthh (x) (COND ((ATOM x) 0) (T (PLUS 1 (lengthh (CDR x))))))
(defun addifnone (x L) (COND ((memberr x L) L) (T (CONS x L))))
(defun collect (L) (COND ((NULL L) NIL) (T (CONS (CAR L) (collect (COND ((memberr (CAR L) (CDR L)) (CONS (CAR L) (removef (CAR L) (CDR L)))) (T (CDR L))))))))
(defun reversee (x) (PROG (u) a (COND ((NULL x) (RETURN u))) (SETQ u (CONS (CAR x) u)) (SETQ x (CDR x)) (GO a)))
(defun flatten (x)(COND ((NULL x) NIL) ((ATOM x) (LIST x)) (T (appendd (flatten (CAR x)) (flatten (CDR x))))))
(defun attach (x y)(RPLACA (RPLACD y (CONS (CAR y) (CDR y))) x))
(defun dreverse (x)(PROG (u v) a (COND ((NULL x) (RETURN u))) (SETQ v x) (SETQ x (CDR x)) (SETQ u (RPLACD v u)) (GO a)))
(defun nconcc (x y)(COND ((NULL x) y) (T (RPLACD x (nconcc (CDR x) y)))))
(defun tconc (x q)(COND ((NULL q) (CONS (SETQ q (CONS x NIL)) q)) (T (RPLACD q (CDR (RPLACD (CDR q) (CONS x NIL)))))))
(defun efface (x y)(COND ((NULL y) NIL) ((equall x (CAR y)) (CDR y)) (T (RPLACD y (efface x (CDR y))))))
(defun dremove (x y)(COND ((NULL y) NIL) ((equall x (CAR y)) (dremove x (CDR y))) (T (RPLACD y (dremove x (CDR y))))))
(defun lcyclep (x)(AND (NOT (ATOM x)) (NOT (ATOM (CDR x))) (lcycle1 (CDR x) (CDDR x))))
(defun lcycle1 (x y)(OR (EQ x y) (AND (NOT (ATOM y)) (NOT (ATOM (CDR y))) (lcycle1 (CDR x) (CDDR y)))))
(defun cyclep (x)(NOT (cycle1 x NIL (QUOTE (T)))))
(defun cycle1 (x u v)(COND ((ATOM x) v) ((memb x u) NIL) ((memb x v) v) ((NULL (SETQ v (cycle1 (CAR x) (SETQ u (CONS x u)) v))) NIL) ((NULL (SETQ v (cycle1 (CDR x) u v))) NIL) (T (CONS x v))))
(defun forall (L p) (COND ((NULL L) T) ((FUNCALL p (CAR L)) (forall (CDR L) p)) (T NIL)))
(defun forsome (L p) (COND ((NULL L) NIL) ((FUNCALL p (CAR L)) T) (T (forsome (CDR L) p))))
(defun forodd (L p)(COND ((NULL L) NIL) ((FUNCALL p (CAR L)) (NOT (forodd (CDR L) p))) (T (forodd (CDR L) p))))
(defun atomlist (x)(COND ((NULL x) T) ((ATOM x) NIL) ((ATOM (CAR x)) (atomlist (CDR x))) (T NIL)))
(defun listpp (x)(COND ((NULL x) T) ((ATOM x) NIL) ((OR (ATOM (CAR x)) (listpp (CAR x))) (listpp (CDR x))) (T NIL)))
(defun order (x y L)(COND ((NULL L) NIL) ((equall x (CAR L)) T) ((equall y (CAR L)) NIL) (T (order x y (CDR L)))))
(defun order1 (x y L)(COND ((NULL L) (QUOTE orderundef)) ((equall x (CAR L)) T) ((equall y (CAR L)) NIL) (T (order1 x y (CDR L)))))
(defun lexorder (x y L)(COND ((NULL x) T) ((NULL y) NIL) ((equall (CAR x) (CAR y)) (lexorder (CDR x) (CDR y) L)) (T (order1 (CAR x) (CAR y) L))))
(defun lexorder1 (x y L)(COND ((NULL x) T) ((NULL y) NIL) ((NULL L) (QUOTE lexorderundef)) ((equall (CAR x) (CAR y)) (lexorder1 (CDR x) (CDR y) (CDR L))) (T (order1 (CAR x) (CAR y) (CAR L)))))

;(defun first (x y)(COND ((NULL y) (CAR x)) ((memberr (CAR y) x) (CAR y)) (T (first x (CDR y) ))))
;(defun rank (x y)(COND ((NULL x) NIL) (T (CONS (first x y) (rank (removef (first x y) x) y)))))

(defun possessing (p L)(PROG (u) a (COND ((NULL L) (RETURN (reversee u))) ((FUNCALL p (CAR L)) (SETQ u (CONS (CAR L) u)))) (SETQ L (CDR L)) (GO a)))
(defun suchthat (p L)(COND ((NULL L) NIL) ((FUNCALL p (CAR L)) (CAR L)) (T (suchthat p (CDR L)))))
(defun suchthat1 (p L x y)(COND ((NULL L) x) ((FUNCALL p (CAR L)) y) (T (suchthat1 p (CDR L) x y))))
(defun suchthat2 (p L f)(COND ((NULL L) NIL) ((FUNCALL p (CAR L)) (FUNCALL f L)) (T (suchthat2 p (CDR L) f))))
(defun setof (x)(COND ((NULL x) NIL) ((memberr (CAR x) (CDR x)) (setof (CDR x))) (T (CONS (CAR x) (setof (CDR x))))))
(defun makeset (x)(PROG (y) a (COND ((NULL x) (RETURN y)) ((NULL (memberr (CAR x) y)) (SETQ y (CONS (CAR x) y)))) (SETQ x (CDR x)) (GO a)))
(defun diflist (x y)(COND ((NULL y) x) (T (diflist (removee (CAR y) x) (CDR y)))))
(defun subset (x y)(NULL (diflist x y)))
(defun union0 (x y)(COND ((NULL x) y) ((memberr (CAR x) y) (union (CDR x) y)) (T (CONS (CAR x) (union (CDR x) y)))))

;(defun union (x y)(PROG NIL (SETQ y (makeset y)) a (COND ((NULL x) (RETURN y)) ((NOT (memberr (CAR x) y)) (SETQ y (CONS (CAR x) y)))) (SETQ x (CDR x)) (GO a)))
(defun lunion (x)(COND ((NULL x) NIL) (T (union (CAR x) (lunion (CDR x))))))

;(defun intersection (x y)(COND ((NULL x) NIL) ((memberr (CAR x) y) (CONS (CAR x) (intersection (CDR x) y))) (T (intersection (CDR x) y))))

(defun equalset (x y)(AND (subset x y) (subset y x)))
(defun cart (x y)(PROG (u v w) a (COND ((NULL x) (RETURN (reversee w)))) (SETQ u (CAR x)) (SETQ x (CDR x)) (SETQ v y) b (COND ((NULL v) (GO a))) (SETQ w (CONS (LIST u (CAR v)) w)) (SETQ v (CDR v)) (GO b)))
(defun pair (x y)(COND ((NULL x) NIL) (T (CONS (CONS (CAR x) (CAR y)) (pair (CDR x) (CDR y))))))
(defun dpair (x y)(PROG (u) (SETQ u x) a (COND ((NULL u) (RETURN x))) (RPLACA u (CONS (CAR u) (CAR y))) (SETQ u (CDR u)) (SETQ y (CDR y)) (GO a)))
(defun assocc (x a)(COND ((NULL a) NIL) ((equall x (CAAR a)) (CAR a)) (T (assocc x (CDR a)))))

;(defun pairlis (x y a)(COND ((NULL x) a) (T (CONS (CONS (CAR x) (CAR y)) (pairlis (CDR x) (CDR y) a)))))

(defun substt (x y z)(COND ((equall y z) x) ((ATOM z) z) (T (CONS (substt x y (CAR z)) (substt x y (CDR z))))))
(defun subliss (a y)(COND ((NULL y) NIL) ((ATOM y) (sub2 a y)) (T (CONS (subliss a (CAR y)) (subliss a (CDR y))))))
(defun sub2 (a y)(COND ((NULL a) y) ((EQ y (CAAR a)) (CDAR a)) (T (sub2 (CDR a) y))))
(defun sassoc (x a f)(COND ((NULL a) (FUNCALL f)) ((equall x (CAAR a)) (CAR a)) (T (sassoc x (CDR a) f))))
(defun maplistt (x f)(COND ((NULL x) NIL) (T (CONS (FUNCALL f x) (maplistt (CDR x) f)))))
(defun mapcarr (x f)(maplistt x (FUNCTION (LAMBDA (x) (FUNCALL f (CAR x))))))

;(defun map (x f)(PROG NIL a (COND ((ATOM x) (RETURN x))) (FUNCALL f x) (SETQ x (CDR x)) (GO a)))

(defmacro pushh (a p)(list (quote setq) p (list (quote cons) a p)))
(defmacro popp (p)(LIST (QUOTE SETQ) p (LIST (QUOTE CDR) p)))
(defmacro popup (a p)(LIST (QUOTE PROG) NIL (LIST (QUOTE SETQ) a (LIST (QUOTE CAR) p)) (LIST (QUOTE SETQ) p (LIST (QUOTE CDR) p))))
(defmacro define (x)(COND ((NULL x) NIL) (T (LIST (QUOTE CONS) (LIST (QUOTE defun) (CAAR x) (CADAR x)) (LIST (QUOTE define) (CDR x))))))
(defmacro deflist (x i)(COND ((NULL x) NIL) (T (LIST (QUOTE CONS) (LIST (QUOTE QUOTE) (putpropp (CAAR x) i (CADAR x))) (LIST (QUOTE deflist) (CDR x) i)))))
(defun putpropp (a i p)(PROG (u v) (SETQ u (CONS NIL (copy (SPROPL a)))) (SETQ v u) a (COND ((NULL (CDR v)) (RPLACD v (LIST i p))) ((EQ (CAR (SETQ v (CDR v))) i) (COND ((NULL (CDR v)) (RPLACD v (LIST p))) (T (RPLACA (CDR v) p)))) (T (GO a))) (SPROPL a (CDR u)) (RETURN a)))
(defun select (e L e0)(COND ((NULL L) e0) ((EQ e (CAR L)) (CADR L)) (T (select e (CDDR L) e0))))

;(defun getprop (a i)(PROG (u) (SETQ u (SPROPL a)) a (COND ((NULL u) (RETURN NIL)) ((EQ (CAR u) i) (GO b))) (SETQ u (CDR u)) (GO a) b (RETURN (COND ((EQ i EXPR) EXPR) ((EQ i FEXPR) FEXPR) ((EQ i APVAL) APVAL) ((EQ i FIXED) FIXED) ((EQ i BITS) BITS) ((EQ i STRING) STRING) ((EQ i FLOAT) FLOAT) ((EQ i SUBR) SUBR) ((EQ i FSUBR) FSUBR) ((NULL (CDR u)) NIL) (T (CADR u))))))

(defun prop (a i f)(PROG (u) (SETQ u (SPROPL a)) a (COND ((NULL u) (RETURN (f))) ((EQ (CAR u) i) (RETURN (CDR u)))) (SETQ u (CDR u)) (GO a)))

;(defun rempropp (a i)(PROG (u v) (COND ((EQ i EXPR) (GO c)) ((EQ i FEXPR) (GO c)) ((EQ i APVAL) (GO c)) ((EQ i FIXED) (GO c)) ((EQ i BITS) (GO c)) ((EQ i STRING) (GO c)) ((EQ i FLOAT) (GO c))) ((EQ i WINDOW) (GO c))) ((EQ i DIALOG) (GO c)))

(defun putflag (a i)(SPROPL a (CONS i (SPROPL a))))
(defun flagp (a i)(memberr i (SPROPL a)))
(defun flag (L i)(PROG (u) a (COND ((NULL L) (RETURN NIL))) (SETQ u (SPROPL (CAR L))) (COND ((NOT (memberr i u)) (SPROPL (CAR L) (CONS i u)))) (SETQ L (CDR L)) (GO a)))
(defun remflag (L i)(PROG (u v) a (COND ((NULL L) (RETURN NIL))) (SETQ u (CONS NIL (SPROPL (CAR L)))) (SETQ v u) b (COND ((NULL (CDR v)) (GO d)) ((EQ (CADR v) i) (GO c))) (SETQ v (CDR v)) (GO b) c (RPLACD v (CDDR v)) (GO b) d (SPROPL (CAR L) (CDR u)) (SETQ L (CDR L)) (GO a)))

(defun alert ()(PROG NIL (TERPRI) (PRINT "Индикатор не найден!") (TERPRI)))

;(defmacro for ( LAMBDA (i iBeg iEnd body) ( `(PROG (, i) (SETQ , i , iBeg) $loop , @body (SETQ , i (ADD1 , i)) (COND ((<= , i , iEnd) (GO $loop))) (RETURN , iEnd)))) )

(defun prlists (x)(PROG (tail) (SETQ tail x) @l (COND ((NULL tail) (RETURN T))) (prin1 (CAR tail)) (TERPRI) (SETQ tail (CDR tail)) (GO @l)))
(defun FLOORR (x)(floor x))
(defun CEIL (x)(PLUS 1 (floor x)))

;(defun fdir (aDir)(PROG (tail ff fn) (SETQ tail (SYSDIR (STRCAT aDir "\*.*") &H1F)) @l (COND ((NULL tail) (RETURN T))) (SETQ ff (CAR tail)) (SETQ tail (CDR tail)) (SETQ fn (STRCAT (STRCAT aDir "\\") ff)) (prin1 (CONS fn (SYSGETATTR fn))) (TERPRI) (GO @l)))

(defun setmarg (m)(PROG NIL (COND ((GREATERP m 1) (prin1 " ")) (T (RETURN NIL))) (setmarg (DIFFERENCE m 1))))
(defun print-tail (L marg)(COND ((NULL L) (prin1 ")")) (T (PROG NIL (TERPRI) (setmarg marg) (pprint (CAR L) (PLUS marg 3)) (print-tail (CDR L) marg)))))

;(defun pprintt (L marg)(COND ((ATOM L) (PROG NIL (setmarg marg) (PRINT L))) ((STRING-LESSP (lengthh L) 3) (PROG NIL (setmarg marg) (PRINT L))) (T (PROG NIL (TERPRI) (setmarg marg) (prin1 "(") (prin1 (CAR L)) (print-tail (CDR L) marg)))))

(defun mklist1 (x)(COND ((EQ x 1) (QUOTE (1))) (T (CONS x (mklist1 (DIFFERENCE x 1))))))
(defun mklist2 (n1 n2)(COND ((EQ n1 n2) (LIST n1)) (T (appendd (LIST n1) (mklist2 (PLUS n1 1) n2)))))
(defun lastItem (x)(COND ((NULL (CDR x)) (CAR x)) (T (lastItem (CDR x)))))
(defun get_even (x)(COND ((NULL x) NIL) (T (appendd (LIST (CADR x)) (get_even (CDDR x))))))
(defun get_odd (x)(COND ((NULL x) NIL) (T (appendd (LIST (CAR x)) (get_odd (CDDR x))))))
(defun add_uniq (item lst)(COND ((NULL lst) (LIST item)) ((EQ item (CAR lst)) lst) (T (appendd (LIST (CAR lst)) (add_uniq item (CDR lst))))))
(defun amax (x)(COND ((NULL x) NIL) ((NULL (CDR x)) (CAR x)) (T (COND ((GREQP (CAR x) (amax (CDR x))) (CAR x)) (T (amax (CDR x)))))))
(defun amin (x)(COND ((NULL x) NIL) ((NULL (CDR x)) (CAR x)) (T (COND ((LEEQP (CAR x) (amin (CDR x))) (CAR x)) (T (amin (CDR x)))))))
(defun filter (lst n fun)(COND ((NULL lst) NIL) (T (COND ((FUNCALL fun (CAR lst) n) (CONS (CAR lst) (filter (CDR lst) n fun))) (T (filter (CDR lst) n fun))))))
(defun prlist (x)(COND ((NULL x) NIL) (T (PROG NIL (PRINTSLINE (CAR x)) (prlist (CDR x))))))
(defun chk1p (x)(PROG (hd tl c) (SETQ c x) LOOP (SETQ hd (CAR c)) (SETQ tl (CDR c)) (COND ((NOT (ATOM hd)) (RETURN NIL))) (COND ((NULL tl) (RETURN T))) (SETQ c tl) (GO LOOP)))
(defun weight (x)(COND ((EQ x (QUOTE +)) 1) ((EQ x (QUOTE -)) 1) ((EQ x (QUOTE *)) 2) ((EQ x (QUOTE \\)) 2) ((EQ x (QUOTE /)) 2) ((EQ x (QUOTE ^)) 3) (T 5)))
(defun opcode (op)(COND ((EQ op (QUOTE +)) (QUOTE PLUS)) ((EQ op (QUOTE -)) (QUOTE DIFFERENCE)) ((EQ op (QUOTE *)) (QUOTE TIMES)) ((EQ op (QUOTE \\)) (QUOTE QUOTIENT)) ((EQ op (QUOTE /)) (QUOTE DIVIDE)) ((EQ op (QUOTE ^)) (QUOTE EXPT)) (T (RAISEERROR (STRCAT "Неверен код операции " (OUTPUT op))))))
(defun inf-aux (ae operators operands)(inf-iter (CDR ae) operators (CONS (CAR ae) operands)))
(defun inf-iter (ae operators operands)(PROG NIL (COND ((AND (NULL ae) (NULL operators)) (RETURN (CAR operands)))) (COND ((AND (NOT (NULL ae)) (OR (NULL operators) (GREATERP (weight (CAR ae)) (weight (CAR operators))))) (RETURN (inf-aux (CDR ae) (CONS (CAR ae) operators) operands)))) (RETURN (inf-iter ae (CDR operators) (CONS (LIST (opcode (CAR operators)) (CADR operands) (CAR operands)) (CDDR operands))))))
(defun inf2pref (x)(PROG (hd tl cc xx rr) (COND ((chk1p x) (RETURN (inf-aux x NIL NIL)))) (SETQ rr NIL) (SETQ xx x) @loop (SETQ hd (CAR xx)) (SETQ tl (CDR xx)) (COND ((memb hd (QUOTE (SIN COS LOG EXP ATN ASN ACS SH CH SQRT SIGN))) (PROGN (SETQ rr (appendd rr (LIST (LIST hd (inf2pref (CAR tl)))))) (SETQ tl (CDR tl)))) ((ATOM hd) (SETQ rr (appendd rr (LIST hd)))) (T (SETQ rr (appendd rr (LIST (inf2pref hd)))))) (COND ((NULL tl) (RETURN (inf-aux rr NIL NIL)))) (SETQ xx tl) (GO @loop)))
(defun выполнить (x)(COND ((eql x NIL) (prin1 "Функции не добавлены.")) (T (выполнить+ x NIL))))
(defun выполнить+ (x w) (COND ((eql x NIL) (prin1 "Все функции добавлены.")) (T (выполнить+ (CDR x) (EVAL (CAR x))))))
(defun ввести (x)(ввести+ x NIL))
(defun ввести+ (x w)(COND ((eql (CDR x) NIL) (SET (CAR (CAR x)) (элемент (CAR x) 2))) (T (ввести+ (CDR x) (SET (CAR (CAR x)) (элемент (CAR x) 2))))))
;|#

;выбирает из списка x элемент, находящийся на позиции n
(defun элемент (x n)(COND ((eql n 1) (CAR x)) (T (элемент (CDR x) (- n 1)))))

;соединяет два списка  x и  y  в один:
(defun соед (x y)(COND ((eql x NIL) y) (T (CONS (CAR x) (соед (CDR x) y)))))

;добавляет к списку  x элемент y:
(defun добав (x y)(соед x (LIST y)))

;выделяет последний элемент списка  x:
(defun послед (x)(COND ((eql (CDR x) NIL) (CAR x)) (T (послед (CDR x)))))

;добавляет элемент y к последнему элементу (обязательно подсписку) списка  x:
(defun добав1 (x y)(добав (обрез x) (добав (послед x) y)))

;удаляет из списка  x последний элемент
(defun обрез+ (x y)(COND ((eql (CDR x) NIL) y) (T (обрез+ (CDR x) (добав y (CAR x))))))

;удаляет из списка  x последний элемент
(defun обрез (x)(COND ((eql (CDR x) NIL) NIL) (T (обрез+ (CDR x) (LIST (CAR x))))))

;добавляет к списку  x  копию его последнего атома
(defun дубл (x)(добав x (послед x)))

;вырезает из списка x n-1 первых элементов
(defun начало (x n)(COND ((eql n 2) (LIST (CAR x))) ((eql n 1) NIL) (T (соед (LIST (CAR x)) (начало (CDR x) (- n 1))))))

;вырезает из списка x последние элементы, начиная с элемента n
(defun конец (x n)(COND ((eql n 1) x) (T (конец (CDR x) (- n 1)))))

;вставляет атомы списка y  вместо n-го элемента списка x
(defun замена (x y n)(соед (начало x n) (соед y (конец x (+ n 1)))))

;#|
(defun вставка (x y n)(соед (начало x n) (CONS y (конец x n))))
(defun вставка1 (x y n)(соед (начало x n) (CONS (добав (элемент x n) y) (конец x (+ n 1)))))

;выдает значение True если в списке x содержится элемент y
(defun содерж (x y)(COND ((eql x NIL) NIL) ((eql y (CAR x)) T) (T (содерж (CDR x) y))))

;выдает значение T если в списке x содержится хотя бы один из элементов списка y:
(defun пересеч (x y)(COND ((eql y NIL) NIL) ((содерж x (CAR y)) T) (T (пересеч x (CDR y)))))

;выдает значение T если все элементы списка x содержатся в списке y
(defun включ (x y)(COND ((eql x NIL) T) ((содерж y (CAR x)) (включ (CDR x) y)) (T NIL)))
;|#

;выдает номер позиции внутреннего списка в списке x, первым элементом которого является y, 0  – если элемент отсутствует
(defun располож1 (x y)(располож1+ x y 1))
(defun располож1+ (x y n)(COND ((eql x NIL) 0) ((eql y (CAR (CAR x))) n) (T (располож1+ (CDR x) y (+ n 1)))))

;#|
(defun колич (x)(COND ((eql x NIL) 0) (T (+ 1 (колич (CDR x))))))
(defun печать+2 (x w)(prin1 x))
(defun печать+ (x w)(COND ((eql x NIL) NIL) (T (печать+ (CDR x) (печать+2 (CAR x) (prin1 " "))))))
(defun перв (x)(COND ((eql x NIL) NIL) (T (CONS (CAR (CAR x)) (перв (CDR x))))))
(defun функции (x)(функции+ x NIL 1 0 NIL))
(defun функции+ (x v m n w)(COND ((eql x NIL) (LIST v w)) (T (COND ((eql n 0) (функции+ (CDR x) (добав v (CONS (CAR (CAR x)) (LIST (колич (элемент (CAR x) 2))))) (+ m 1) (располож1 v (CAR (CAR (CDR x)))) (CONS (CONS (QUOTE DEFUN) (CAR x)) w))) (T (LIST (LIST 0 (CAR (CAR x)) n m)))))))
(defun правило (x y n)(COND ((eql n 0) (добав y (LIST (CAR x) (CDR x)))) (T (вставка1 y (CDR x) n))))
(defun продукции (x)(продукции+ x NIL))
(defun продукции+ (x y)(COND ((eql x NIL) y) (T (продукции+ (CDR x) (правило (CAR x) y (располож1 y (CAR (CAR x))))))))
(defun проверка1 (x y)(проверка1+ (CDR x) y (располож1 y (CAR (CAR (CDR x))))))
(defun проверка1+ (x y n)(COND ((eql x NIL) NIL) ((eql n 0) (LIST 1 (CAR (CAR x)))) (T (COND ((eql (колич (CDR (CAR x))) (CAR (CDR (элемент y n)))) (проверка1+ (CDR x) y (располож1 y (CAR (CAR (CDR x)))))) (T (LIST 2 (CAR (CAR x))))))))
(defun проверка (x y)(проверка+ x y x (проверка1 (CAR x) y) 1))
(defun проверка+ (x y u v m)(COND ((eql x NIL) u) ((eql v NIL) (проверка+ (CDR x) y u (проверка1 (CAR (CDR x)) y) (+ m 1))) (T (добав v m))))
(defun ошибка (x)(ошибка+ (CAR x) (элемент x 2) (элемент x 3) (элемент x 4) (prin1 "База знаний не принята!")))
(defun ошибка+ (k f m n w)(COND ((eql k 0) (печать (LIST (QUOTE Функция) f (QUOTE повторятся) (QUOTE на) m (QUOTE и) n (QUOTE позициях.)))) ((eql k 1) (печать (LIST (QUOTE Функция) f (QUOTE продукции) m (QUOTE не) (QUOTE задана.)))) ((eql k 2) (печать (LIST (QUOTE Функция) f (QUOTE в) (QUOTE продукции) m (QUOTE использует) (QUOTE неправильное) (QUOTE количество) (QUOTE аргументов.))))))
(defun база (x y z)(база+ x (CAR (функции y)) (проверка (продукции z) (CAR (функции y))) (элемент (функции y) 2)))
(defun база+ (x y z v) (COND ((eql v "Все функции добавлены.") (SET x (LIST y z))) ((eql (CAR y) 0) (ошибка y)) ((eql (CAR z) 1) (ошибка z)) ((eql (CAR z) 2) (ошибка z)) (T (база+ x y z (выполнить v)))))

;удаляет из списка  x подсписки, первые элементы которых встречаются в списке y
(defun сокращ (x y) (COND ((eql x NIL) NIL) ((содерж y (CAR (CAR x))) (сокращ (CDR x) y)) (T (CONS (CAR x) (сокращ (CDR x) y)))))
(defun добстр (x y k)(COND ((eql k 1) (добав x y)) (T (добав (обрез x) (добстр (послед x) y (- k 1))))))
(defun уменьш (x)(COND ((eql x NIL) NIL) ((eql (CAR x) 0) (уменьш (CDR x))) (T (CONS (- (CAR x) 1) (CDR x)))))
(defun формир (x y z)(формир+ x (CDR y) (CDR z) (LIST (CAR z)) (LIST (CAR (CAR y))) (CAR (CDR (CAR y))) 1 1))
(defun формир+ (x y z v f n k i)(COND ((eql x NIL) f) ((eql i n) (формир+ x (CDR y) (CDR z) (CONS (CAR z) v) (добстр f (LIST (CAR (CAR y))) k) (CAR (CDR (CAR y))) (+ k 1) i)) ((eql (CAR v) 1) (формир+ (CDR x) y z (уменьш (уменьш v)) (добстр f (CAR x) k) n (- k 1) (+ i 1))) (T (формир+ (CDR x) y z (уменьш v) (добстр f (CAR x) k) n k (+ i 1)))))
;|#

;находит самый левый нетерминал в списке x, подлежащий замене, и выдает два числа – номер найденного нетерминала в списке x и номер его замены в списке y
(defun коорд (x y)(коорд+ x y 1 (располож1 y (CAR x))))
(defun коорд+ (x y n m)(COND ((eql x NIL) (QUOTE (0 0))) ((eql m 0) (коорд+ (CDR x) y (+ n 1) (располож1 y (CAR (CDR x))))) (T (добав (LIST n) m))))

;формирует из трех списков список дерева; x – сложный список выводов решения; y – сложный список использованных нетерминалов; z – сложный список использованных функций
(defun состав (x y z)(COND ((eql (CDR x) NIL) (LIST (CONS (CAR x) (CONS (CAR y) (LIST (CAR z)))))) (T (CONS (CONS (CAR x) (CONS (CAR y) (LIST (CAR z)))) (состав (CDR x) (CDR y) (CDR z))))))

;производит замену левых нетерминалов; x – список одного узла дерева, состоящий из трех подсписков: подсписка вывода решения, подсписка использованных нетерминалов; подсписка использованных функций; y – список продукций
(defun стрелка (x y)(стрелка+ x y (коорд (CAR x) y)))
(defun стрелка+ (x y z)(стрелка+2 x y (CAR z) (CAR (CDR z))))
(defun стрелка+2 (x y n m)(COND ((eql n 0) x) (T (стрелка+3 (CAR x) (элемент x 2) (элемент x 3) (элемент y m) n))))
(defun стрелка+3 (x y z u n)(стрелка+4 (LIST (замена x (CDR (CAR (CDR u))) n)) (LIST (добав y (CAR u))) (COND ((eql z NIL) (LIST (LIST (LIST (CAR (CAR (CDR u))))))) (T (LIST (добав (добав1 z n) (LIST (CAR (CAR (CDR u)))))))) (CDR (CDR u)) x n))
(defun стрелка+4 (x y z u v n)(COND ((eql u NIL) (состав x y z)) (T (стрелка+4 (добав x (замена v (CDR (CAR u)) n)) (дубл y) (добав z (добав (обрез (CAR z)) (LIST (CAR (CAR u))))) (CDR u) v n))))

;производит замену левых нетерминалов; x – список дерева, состоящий из списков узлов; y – список продукций:
(defun стрелки (x y)(COND ((eql (CDR x) NIL) (стрелка (CAR x) y)) (T (соед (стрелка (CAR x) y) (стрелки (CDR x) y)))))

;удаляет из списка дерева x тупиковые узлы:
(defun тупики (x)(COND ((eql x NIL) NIL) ((пересеч (CAR (CAR x)) (элемент (CAR x) 2)) (тупики (CDR x))) (T (CONS (CAR x) (тупики (CDR x))))))

;создает первоначальный список дерева x – аксиома; y – список продукций:
(defun ствол (x y)(стрелка (CONS (LIST x) (CONS NIL (LIST NIL))) y))

;#|
;осуществляет поиск решения, в виде узла, состоящего только из терминальных символов; x – список дерева; y – список терминальных символов
(defun поиск (x y)(COND ((eql x NIL) NIL) ((включ (CAR (CAR x)) y) (CAR x)) (T (поиск (CDR x) y))))

;строит список количества аргументов всех функций, входящих в суперпозицию; x – список задействованных функций; y – список всех функций
(defun аргум (x y)(COND ((eql x NIL) NIL) (T (CONS (элемент (элемент y (располож1 y (CAR (CAR x)))) 2) (аргум (CDR x) y)))))

;осуществляет построение следующего уровня дерева перебора; x – список ствола дерева; y – список продукций; z – список использованных нетерминалов; u – список функций
(defun уровень (x y z u)(уровень+ (тупики (стрелки x y)) y z u))
(defun уровень+ (x y z u)(COND ((eql x NIL) (prin1 "Задача не имеет решения!")) (T (уровень+2 x y z (поиск x z) u))))
(defun уровень+2 (x y z v u)(COND ((eql v NIL) (уровень x y z u)) (T (формир (CAR v) (элемент v 3) (аргум (элемент v 3) u)))))

;выдает невыполненную суперпозицию; x – список символов; аксиомы и терминалов; y – название базы знаний
(defun решение (x y)(решение+ (CAR x) (сокращ (элемент y 2) (CDR x)) (CDR x) (CAR y)))
(defun решение+ (x y z u)(уровень+ (тупики (ствол x y)) y z u))

;выдает числовой результат; x – список символов; аксиомы и терминалы в виде подсписков с числовыми значениями; y – ;название базы знаний
(defun решение1 (x y)(EVAL (решение1+ (CONS (CAR x) (перв (CDR x))) y (ввести (CDR x)))))
(defun решение1+ (x y z)(решение x y))
;|#


#|
Решим следующую задачу из области механики: 
"При аварийном торможении автомобиль, движущийся со скоростью 72 км/ч, остановился через 5 с. Найти тормозной путь." 
|#
;(Решение '(<Dist> <Spd1> <Time> <Spd2>) Механика) ;суперпозиция: (f6 <Spd1> <Time> (f8 <Spd2> <Spd1> <Time>))
;(Решение1 '(<Dist> (<Spd1> 20) (<Time> 5) (<Spd2> 0)) Механика) ;численный ответ: 50.0

(print (Решение1 '(<Dist> (<Spd1> 20) (<Time> 5) (<Spd2> 0)) Механика)) 


; ******** КОНЕЦ АЛГОРИТМА ******** ;


;Оценка временной сложности:
;Алгоритм является NP-трудным и имеет высокую недетерминированную оценку временной сложности равную 
;в лучшем случае: O(n^k), где k >= 4 - высокополиномиальная сложность (при решении более простых задач) а 
;в худшем случае: O(2^(n^k)) - экспоненциальная сложность (при решении более сложных задач встроенными численными методами).


;#| это код для проверки производительности
;(time(dotimes (i 1300)(print (Решение '(<FrFric> <Dist> <KinEn1> <KinEn2> <FrPull>) Механика))))
;|#
