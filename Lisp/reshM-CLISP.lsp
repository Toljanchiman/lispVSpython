(declaim (sb-ext:muffle-conditions cl:warning))
(declaim (sb-ext:muffle-conditions cl:style-warning))
;https://www.tutorialspoint.com/execute_lisp_online.php
;https://rextester.com/l/common_lisp_online_compiler
;(load "reshM.lsp")
;(�������1 '(<Spd2> (<Accl> 0.3) (<Time> 20) (<Spd1> 4)) ��������) ����� 10.0
;(������� '(<Spd2> <Accl> <Time> <Spd1>) ��������) ����� (f1 <Spd1> <Accl> <Time>)

(defun f1 (x y z) (+ x (* y z)))
#|
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
|#
(defun f12 (x y) (/ x y))
(defun f17 (x y z) (+ (- x y) z))
(defun f18 (x y) (x y) (* x y))

;(������� '(<Spd2> <Accl> <Time> <Spd1>) ��������) ����� (f1 <Spd1> <Accl> <Time>)
(defvar �������� '(((f1 3) (f2 3) (f3 3) (f4 3) (f5 3) (f6 3) (f7 3) (f8 3) (f9 3) (f10 3) (f11 2) (f12 2) (f13 3) (f14 2) (f15 3) (f16 2) (f17 3) (f18 2)) (
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
;(defun fact (x) (COND ((MINUSP x) (RAISEERROR "�������� FACT �����������")) ((EQ x 0) 1) (T (TIMES x (fact (DIFFERENCE x 1))))))
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

(defun alert ()(PROG NIL (TERPRI) (PRINT "��������� �� ������!") (TERPRI)))

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
(defun opcode (op)(COND ((EQ op (QUOTE +)) (QUOTE PLUS)) ((EQ op (QUOTE -)) (QUOTE DIFFERENCE)) ((EQ op (QUOTE *)) (QUOTE TIMES)) ((EQ op (QUOTE \\)) (QUOTE QUOTIENT)) ((EQ op (QUOTE /)) (QUOTE DIVIDE)) ((EQ op (QUOTE ^)) (QUOTE EXPT)) (T (RAISEERROR (STRCAT "������� ��� �������� " (OUTPUT op))))))
(defun inf-aux (ae operators operands)(inf-iter (CDR ae) operators (CONS (CAR ae) operands)))
(defun inf-iter (ae operators operands)(PROG NIL (COND ((AND (NULL ae) (NULL operators)) (RETURN (CAR operands)))) (COND ((AND (NOT (NULL ae)) (OR (NULL operators) (GREATERP (weight (CAR ae)) (weight (CAR operators))))) (RETURN (inf-aux (CDR ae) (CONS (CAR ae) operators) operands)))) (RETURN (inf-iter ae (CDR operators) (CONS (LIST (opcode (CAR operators)) (CADR operands) (CAR operands)) (CDDR operands))))))
(defun inf2pref (x)(PROG (hd tl cc xx rr) (COND ((chk1p x) (RETURN (inf-aux x NIL NIL)))) (SETQ rr NIL) (SETQ xx x) @loop (SETQ hd (CAR xx)) (SETQ tl (CDR xx)) (COND ((memb hd (QUOTE (SIN COS LOG EXP ATN ASN ACS SH CH SQRT SIGN))) (PROGN (SETQ rr (appendd rr (LIST (LIST hd (inf2pref (CAR tl)))))) (SETQ tl (CDR tl)))) ((ATOM hd) (SETQ rr (appendd rr (LIST hd)))) (T (SETQ rr (appendd rr (LIST (inf2pref hd)))))) (COND ((NULL tl) (RETURN (inf-aux rr NIL NIL)))) (SETQ xx tl) (GO @loop)))
(defun ��������� (x)(COND ((eql x NIL) (prin1 "������� �� ���������.")) (T (���������+ x NIL))))
(defun ���������+ (x w) (COND ((eql x NIL) (prin1 "��� ������� ���������.")) (T (���������+ (CDR x) (EVAL (CAR x))))))
(defun ������ (x)(������+ x NIL))
(defun ������+ (x w)(COND ((eql (CDR x) NIL) (SET (CAR (CAR x)) (������� (CAR x) 2))) (T (������+ (CDR x) (SET (CAR (CAR x)) (������� (CAR x) 2))))))
;|#

;�������� �� ������ x �������, ����������� �� ������� n
(defun ������� (x n)(COND ((eql n 1) (CAR x)) (T (������� (CDR x) (- n 1)))))

;��������� ��� ������  x �  y  � ����:
(defun ���� (x y)(COND ((eql x NIL) y) (T (CONS (CAR x) (���� (CDR x) y)))))

;��������� � ������  x ������� y:
(defun ����� (x y)(���� x (LIST y)))

;�������� ��������� ������� ������  x:
(defun ������ (x)(COND ((eql (CDR x) NIL) (CAR x)) (T (������ (CDR x)))))

;��������� ������� y � ���������� �������� (����������� ���������) ������  x:
(defun �����1 (x y)(����� (����� x) (����� (������ x) y)))

;������� �� ������  x ��������� �������
(defun �����+ (x y)(COND ((eql (CDR x) NIL) y) (T (�����+ (CDR x) (����� y (CAR x))))))

;������� �� ������  x ��������� �������
(defun ����� (x)(COND ((eql (CDR x) NIL) NIL) (T (�����+ (CDR x) (LIST (CAR x))))))

;��������� � ������  x  ����� ��� ���������� �����
(defun ���� (x)(����� x (������ x)))

;�������� �� ������ x n-1 ������ ���������
(defun ������ (x n)(COND ((eql n 2) (LIST (CAR x))) ((eql n 1) NIL) (T (���� (LIST (CAR x)) (������ (CDR x) (- n 1))))))

;�������� �� ������ x ��������� ��������, ������� � �������� n
(defun ����� (x n)(COND ((eql n 1) x) (T (����� (CDR x) (- n 1)))))

;��������� ����� ������ y  ������ n-�� �������� ������ x
(defun ������ (x y n)(���� (������ x n) (���� y (����� x (+ n 1)))))

;#|
(defun ������� (x y n)(���� (������ x n) (CONS y (����� x n))))
(defun �������1 (x y n)(���� (������ x n) (CONS (����� (������� x n) y) (����� x (+ n 1)))))

;������ �������� True ���� � ������ x ���������� ������� y
(defun ������ (x y)(COND ((eql x NIL) NIL) ((eql y (CAR x)) T) (T (������ (CDR x) y))))

;������ �������� T ���� � ������ x ���������� ���� �� ���� �� ��������� ������ y:
(defun ������� (x y)(COND ((eql y NIL) NIL) ((������ x (CAR y)) T) (T (������� x (CDR y)))))

;������ �������� T ���� ��� �������� ������ x ���������� � ������ y
(defun ����� (x y)(COND ((eql x NIL) T) ((������ y (CAR x)) (����� (CDR x) y)) (T NIL)))
;|#

;������ ����� ������� ����������� ������ � ������ x, ������ ��������� �������� �������� y, 0  � ���� ������� �����������
(defun ��������1 (x y)(��������1+ x y 1))
(defun ��������1+ (x y n)(COND ((eql x NIL) 0) ((eql y (CAR (CAR x))) n) (T (��������1+ (CDR x) y (+ n 1)))))

;#|
(defun ����� (x)(COND ((eql x NIL) 0) (T (+ 1 (����� (CDR x))))))
(defun ������+2 (x w)(prin1 x))
(defun ������+ (x w)(COND ((eql x NIL) NIL) (T (������+ (CDR x) (������+2 (CAR x) (prin1 " "))))))
(defun ���� (x)(COND ((eql x NIL) NIL) (T (CONS (CAR (CAR x)) (���� (CDR x))))))
(defun ������� (x)(�������+ x NIL 1 0 NIL))
(defun �������+ (x v m n w)(COND ((eql x NIL) (LIST v w)) (T (COND ((eql n 0) (�������+ (CDR x) (����� v (CONS (CAR (CAR x)) (LIST (����� (������� (CAR x) 2))))) (+ m 1) (��������1 v (CAR (CAR (CDR x)))) (CONS (CONS (QUOTE DEFUN) (CAR x)) w))) (T (LIST (LIST 0 (CAR (CAR x)) n m)))))))
(defun ������� (x y n)(COND ((eql n 0) (����� y (LIST (CAR x) (CDR x)))) (T (�������1 y (CDR x) n))))
(defun ��������� (x)(���������+ x NIL))
(defun ���������+ (x y)(COND ((eql x NIL) y) (T (���������+ (CDR x) (������� (CAR x) y (��������1 y (CAR (CAR x))))))))
(defun ��������1 (x y)(��������1+ (CDR x) y (��������1 y (CAR (CAR (CDR x))))))
(defun ��������1+ (x y n)(COND ((eql x NIL) NIL) ((eql n 0) (LIST 1 (CAR (CAR x)))) (T (COND ((eql (����� (CDR (CAR x))) (CAR (CDR (������� y n)))) (��������1+ (CDR x) y (��������1 y (CAR (CAR (CDR x)))))) (T (LIST 2 (CAR (CAR x))))))))
(defun �������� (x y)(��������+ x y x (��������1 (CAR x) y) 1))
(defun ��������+ (x y u v m)(COND ((eql x NIL) u) ((eql v NIL) (��������+ (CDR x) y u (��������1 (CAR (CDR x)) y) (+ m 1))) (T (����� v m))))
(defun ������ (x)(������+ (CAR x) (������� x 2) (������� x 3) (������� x 4) (prin1 "���� ������ �� �������!")))
(defun ������+ (k f m n w)(COND ((eql k 0) (������ (LIST (QUOTE �������) f (QUOTE ����������) (QUOTE ��) m (QUOTE �) n (QUOTE ��������.)))) ((eql k 1) (������ (LIST (QUOTE �������) f (QUOTE ���������) m (QUOTE ��) (QUOTE ������.)))) ((eql k 2) (������ (LIST (QUOTE �������) f (QUOTE �) (QUOTE ���������) m (QUOTE ����������) (QUOTE ������������) (QUOTE ����������) (QUOTE ����������.))))))
(defun ���� (x y z)(����+ x (CAR (������� y)) (�������� (��������� z) (CAR (������� y))) (������� (������� y) 2)))
(defun ����+ (x y z v) (COND ((eql v "��� ������� ���������.") (SET x (LIST y z))) ((eql (CAR y) 0) (������ y)) ((eql (CAR z) 1) (������ z)) ((eql (CAR z) 2) (������ z)) (T (����+ x y z (��������� v)))))

;������� �� ������  x ���������, ������ �������� ������� ����������� � ������ y
(defun ������ (x y) (COND ((eql x NIL) NIL) ((������ y (CAR (CAR x))) (������ (CDR x) y)) (T (CONS (CAR x) (������ (CDR x) y)))))
(defun ������ (x y k)(COND ((eql k 1) (����� x y)) (T (����� (����� x) (������ (������ x) y (- k 1))))))
(defun ������ (x)(COND ((eql x NIL) NIL) ((eql (CAR x) 0) (������ (CDR x))) (T (CONS (- (CAR x) 1) (CDR x)))))
(defun ������ (x y z)(������+ x (CDR y) (CDR z) (LIST (CAR z)) (LIST (CAR (CAR y))) (CAR (CDR (CAR y))) 1 1))
(defun ������+ (x y z v f n k i)(COND ((eql x NIL) f) ((eql i n) (������+ x (CDR y) (CDR z) (CONS (CAR z) v) (������ f (LIST (CAR (CAR y))) k) (CAR (CDR (CAR y))) (+ k 1) i)) ((eql (CAR v) 1) (������+ (CDR x) y z (������ (������ v)) (������ f (CAR x) k) n (- k 1) (+ i 1))) (T (������+ (CDR x) y z (������ v) (������ f (CAR x) k) n k (+ i 1)))))
;|#

;������� ����� ����� ���������� � ������ x, ���������� ������, � ������ ��� ����� � ����� ���������� ����������� � ������ x � ����� ��� ������ � ������ y
(defun ����� (x y)(�����+ x y 1 (��������1 y (CAR x))))
(defun �����+ (x y n m)(COND ((eql x NIL) (QUOTE (0 0))) ((eql m 0) (�����+ (CDR x) y (+ n 1) (��������1 y (CAR (CDR x))))) (T (����� (LIST n) m))))

;��������� �� ���� ������� ������ ������; x � ������� ������ ������� �������; y � ������� ������ �������������� ������������; z � ������� ������ �������������� �������
(defun ������ (x y z)(COND ((eql (CDR x) NIL) (LIST (CONS (CAR x) (CONS (CAR y) (LIST (CAR z)))))) (T (CONS (CONS (CAR x) (CONS (CAR y) (LIST (CAR z)))) (������ (CDR x) (CDR y) (CDR z))))))

;���������� ������ ����� ������������; x � ������ ������ ���� ������, ��������� �� ���� ����������: ��������� ������ �������, ��������� �������������� ������������; ��������� �������������� �������; y � ������ ���������
(defun ������� (x y)(�������+ x y (����� (CAR x) y)))
(defun �������+ (x y z)(�������+2 x y (CAR z) (CAR (CDR z))))
(defun �������+2 (x y n m)(COND ((eql n 0) x) (T (�������+3 (CAR x) (������� x 2) (������� x 3) (������� y m) n))))
(defun �������+3 (x y z u n)(�������+4 (LIST (������ x (CDR (CAR (CDR u))) n)) (LIST (����� y (CAR u))) (COND ((eql z NIL) (LIST (LIST (LIST (CAR (CAR (CDR u))))))) (T (LIST (����� (�����1 z n) (LIST (CAR (CAR (CDR u)))))))) (CDR (CDR u)) x n))
(defun �������+4 (x y z u v n)(COND ((eql u NIL) (������ x y z)) (T (�������+4 (����� x (������ v (CDR (CAR u)) n)) (���� y) (����� z (����� (����� (CAR z)) (LIST (CAR (CAR u))))) (CDR u) v n))))

;���������� ������ ����� ������������; x � ������ ������, ��������� �� ������� �����; y � ������ ���������:
(defun ������� (x y)(COND ((eql (CDR x) NIL) (������� (CAR x) y)) (T (���� (������� (CAR x) y) (������� (CDR x) y)))))

;������� �� ������ ������ x ��������� ����:
(defun ������ (x)(COND ((eql x NIL) NIL) ((������� (CAR (CAR x)) (������� (CAR x) 2)) (������ (CDR x))) (T (CONS (CAR x) (������ (CDR x))))))

;������� �������������� ������ ������ x � �������; y � ������ ���������:
(defun ����� (x y)(������� (CONS (LIST x) (CONS NIL (LIST NIL))) y))

;#|
;������������ ����� �������, � ���� ����, ���������� ������ �� ������������ ��������; x � ������ ������; y � ������ ������������ ��������
(defun ����� (x y)(COND ((eql x NIL) NIL) ((����� (CAR (CAR x)) y) (CAR x)) (T (����� (CDR x) y))))

;������ ������ ���������� ���������� ���� �������, �������� � ������������; x � ������ ��������������� �������; y � ������ ���� �������
(defun ����� (x y)(COND ((eql x NIL) NIL) (T (CONS (������� (������� y (��������1 y (CAR (CAR x)))) 2) (����� (CDR x) y)))))

;������������ ���������� ���������� ������ ������ ��������; x � ������ ������ ������; y � ������ ���������; z � ������ �������������� ������������; u � ������ �������
(defun ������� (x y z u)(�������+ (������ (������� x y)) y z u))
(defun �������+ (x y z u)(COND ((eql x NIL) (prin1 "������ �� ����� �������!")) (T (�������+2 x y z (����� x z) u))))
(defun �������+2 (x y z v u)(COND ((eql v NIL) (������� x y z u)) (T (������ (CAR v) (������� v 3) (����� (������� v 3) u)))))

;������ ������������� ������������; x � ������ ��������; ������� � ����������; y � �������� ���� ������
;(������� '(<Spd2> <Accl> <Time> <Spd1>) ��������) ����� (f1 <Spd1> <Accl> <Time>)
(defun ������� (x y)(�������+ (CAR x) (������ (������� y 2) (CDR x)) (CDR x) (CAR y)))
(defun �������+ (x y z u)(�������+ (������ (����� x y)) y z u))

;������ �������� ���������; x � ������ ��������; ������� � ��������� � ���� ���������� � ��������� ����������; y � ;�������� ���� ������
(defun �������1 (x y)(EVAL (�������1+ (CONS (CAR x) (���� (CDR x))) y (������ (CDR x)))))
(defun �������1+ (x y z)(������� x y))
;|#

;#|
(time(dotimes (i 1300)(print (������� '(<FrFric> <Dist> <KinEn1> <KinEn2> <FrPull>) ��������))))
;|#

#|(load "reshM.lsp")
;(������� '(<FrFric> <Dist> <KinEn1> <KinEn2> <FrPull>) ��������)
==> (f12 (f17 (f18 <FrPull> <Dist>) <KinEn2> <KinEn1>) <Dist>)
������� ������� (x y)
�������::
x � ������ �������� - ������� � ���������, y � ����
���������� x - ������ ������ x �������, � - ������ ������ �� ����, ����������� �� ������� 2 � ������� �� ���� ��������� ������ �������� ������� ���� � ������ ����������, z - ������ ����������, u - ������ ������� ��������������� � �� ���-��� ����������
(������ '((
(<Spd2> (f1 <Spd1> <Accl> <Time>) (f2 <Accl> <Dist> <Spd1>) (f11 <KinEn2> <Mass>)) 
(<Spd1> (f3 <Spd2> <Accl> <Time>) (f4 <Dist> <Time> <Accl>) (f5 <Spd2> <Accl> <Dist>) (f11 <KinEn1> <Mass>)) 
(<Dist> (f6 <Spd1> <Time> <Accl>) (f7 <Spd2> <Spd1> <Accl>) (f12 <OpPull> <FrPull>) (f12 <OpFric> <FrFric>)) 
(<Time> (f8 <Spd2> <Spd1> <Accl>) (f9 <Spd1> <Accl> <Dist>)) 
(<Accl> (f8 <Spd2> <Spd1> <Time>) (f10 <Dist> <Spd1> <Time>) (f7 <Spd2> <Spd1> <Dist>) (f8 <FrPull> <FrFric> <Mass>)) (<Mass> (f8 <FrPull> <FrFric> <Accl>) (f13 <FrFric> <CfFric> <Cfg>) (f14 <KinEn2> <Spd2>) (f14 <KinEn1> <Spd1>)) 
(<FrPull> (f1 <FrFric> <Mass> <Accl>) (f12 <OpPull> <Dist>)) 
(<FrFric> (f3 <FrPull> <Mass> <Accl>) (f15 <Cfg> <CfFric> <Mass>) (f12 <OpFric> <Dist>)) 
(<CfFric> (f13 <FrFric> <Mass> <Cfg>)) 
(<KinEn2> (f16 <Mass> <Spd2>) (f17 <OpPull> <OpFric> <KinEn1>)) 
(<KinEn1> (f16 <Mass> <Spd1>) (f17 <KinEn2> <OpPull> <OpFric>)) 
(<OpPull> (f18 <FrPull> <Dist>) (f17 <KinEn2> <KinEn1> <OpFric>)) 
(<OpFric> (f18 <FrFric> <Dist>) (f17 <OpPull> <KinEn2> <KinEn1>))
)) '(<Dist> <KinEn1> <KinEn2> <FrPull>))
�������::
((<Spd2> (f1 <Spd1> <Accl> <Time>) (f2 <Accl> <Dist> <Spd1>) (f11 <KinEn2> <Mass>)) 
(<Spd1> (f3 <Spd2> <Accl> <Time>) (f4 <Dist> <Time> <Accl>) (f5 <Spd2> <Accl> <Dist>) (f11 <KinEn1> <Mass>)) 
(<Time> (f8 <Spd2> <Spd1> <Accl>) (f9 <Spd1> <Accl> <Dist>)) 
(<Accl> (f8 <Spd2> <Spd1> <Time>) (f10 <Dist> <Spd1> <Time>) (f7 <Spd2> <Spd1> <Dist>) (f8 <FrPull> <FrFric> <Mass>)) (<Mass> (f8 <FrPull> <FrFric> <Accl>) (f13 <FrFric> <CfFric> <Cfg>) (f14 <KinEn2> <Spd2>) (f14 <KinEn1> <Spd1>)) 
(<FrFric> (f3 <FrPull> <Mass> <Accl>) (f15 <Cfg> <CfFric> <Mass>) (f12 <OpFric> <Dist>)) 
(<CfFric> (f13 <FrFric> <Mass> <Cfg>)) 
(<OpPull> (f18 <FrPull> <Dist>) (f17 <KinEn2> <KinEn1> <OpFric>)) 
(<OpFric> (f18 <FrFric> <Dist>) (f17 <OpPull> <KinEn2> <KinEn1>))
)
������� �������+ (x y z u)
����� (����� x y)
(����� '<FrFric> '((<Spd2> (f1 <Spd1> <Accl> <Time>) (f2 <Accl> <Dist> <Spd1>) (f11 <KinEn2> <Mass>)) 
(<Spd1> (f3 <Spd2> <Accl> <Time>) (f4 <Dist> <Time> <Accl>) (f5 <Spd2> <Accl> <Dist>) (f11 <KinEn1> <Mass>)) 
(<Time> (f8 <Spd2> <Spd1> <Accl>) (f9 <Spd1> <Accl> <Dist>)) 
(<Accl> (f8 <Spd2> <Spd1> <Time>) (f10 <Dist> <Spd1> <Time>) (f7 <Spd2> <Spd1> <Dist>) (f8 <FrPull> <FrFric> <Mass>)) (<Mass> (f8 <FrPull> <FrFric> <Accl>) (f13 <FrFric> <CfFric> <Cfg>) (f14 <KinEn2> <Spd2>) (f14 <KinEn1> <Spd1>)) 
(<FrFric> (f3 <FrPull> <Mass> <Accl>) (f15 <Cfg> <CfFric> <Mass>) (f12 <OpFric> <Dist>)) 
(<CfFric> (f13 <FrFric> <Mass> <Cfg>)) 
(<OpPull> (f18 <FrPull> <Dist>) (f17 <KinEn2> <KinEn1> <OpFric>)) 
(<OpFric> (f18 <FrFric> <Dist>) (f17 <OpPull> <KinEn2> <KinEn1>))
) )
�������::
(((<FrPull> <Mass> <Accl>) (<FrFric>) ((f3))) ((<Cfg> <CfFric> <Mass>) (<FrFric>) ((f15))) ((<OpFric> <Dist>) (<FrFric>) ((f12))))
������ ������:
(������ '(((<FrPull> <Mass> <Accl>) (<FrFric>) ((f3))) ((<Cfg> <CfFric> <Mass>) (<FrFric>) ((f15))) ((<OpFric> <Dist>) (<FrFric>) ((f12)))) )
�������::
(((<FrPull> <Mass> <Accl>) (<FrFric>) ((f3))) ((<Cfg> <CfFric> <Mass>) (<FrFric>) ((f15))) ((<OpFric> <Dist>) (<FrFric>) ((f12))))

����� ��������� �������+ ������ ��������::
(defun �������+ (x y z u)( LAMBDA (x y z u) (COND ((eql x NIL) (prin1 "������ �� ����� �������!")) (T (�������+2 x y z (����� x z) u)))) )
x � ������ ������ ������; y � ������ ���������; z � ������ �������������� ����������; u � ������ �������
(�������+ '(((<FrPull> <Mass> <Accl>) (<FrFric>) ((f3))) ((<Cfg> <CfFric> <Mass>) (<FrFric>) ((f15))) ((<OpFric> <Dist>) (<FrFric>) ((f12)))) '((<Spd2> (f1 <Spd1> <Accl> <Time>) (f2 <Accl> <Dist> <Spd1>) (f11 <KinEn2> <Mass>)) 
(<Spd1> (f3 <Spd2> <Accl> <Time>) (f4 <Dist> <Time> <Accl>) (f5 <Spd2> <Accl> <Dist>) (f11 <KinEn1> <Mass>)) 
(<Time> (f8 <Spd2> <Spd1> <Accl>) (f9 <Spd1> <Accl> <Dist>)) 
(<Accl> (f8 <Spd2> <Spd1> <Time>) (f10 <Dist> <Spd1> <Time>) (f7 <Spd2> <Spd1> <Dist>) (f8 <FrPull> <FrFric> <Mass>)) (<Mass> (f8 <FrPull> <FrFric> <Accl>) (f13 <FrFric> <CfFric> <Cfg>) (f14 <KinEn2> <Spd2>) (f14 <KinEn1> <Spd1>)) 
(<FrFric> (f3 <FrPull> <Mass> <Accl>) (f15 <Cfg> <CfFric> <Mass>) (f12 <OpFric> <Dist>)) 
(<CfFric> (f13 <FrFric> <Mass> <Cfg>)) 
(<OpPull> (f18 <FrPull> <Dist>) (f17 <KinEn2> <KinEn1> <OpFric>)) 
(<OpFric> (f18 <FrFric> <Dist>) (f17 <OpPull> <KinEn2> <KinEn1>))
) '(<Dist> <KinEn1> <KinEn2> <FrPull>) '((f1 3) (f2 3) (f3 3) (f4 3) (f5 3) (f6 3) (f7 3) (f8 3) (f9 3) (f10 3) (f11 2) (f12 2) (f13 3) (f14 2) (f15 3) (f16 2) (f17 3) (f18 2)))
�������::
==> (f12 (f17 (f18 <FrPull> <Dist>) <KinEn2> <KinEn1>) <Dist>)
--------
;������������ ����� �������, � ���� ����, ���������� ������ �� ������������ ��������; x � ������ ������; y � ������ ������������ ��������
(defun ����� (x y)( LAMBDA (x y) (COND ((eql x NIL) NIL) ((����� (CAR (CAR x)) y) (CAR x)) (T (����� (CDR x) y)))) )
--------
����� �����::
(����� '(((<FrPull> <Mass> <Accl>) (<FrFric>) ((f3))) ((<Cfg> <CfFric> <Mass>) (<FrFric>) ((f15))) ((<OpFric> <Dist>) (<FrFric>) ((f12)))) '(<Dist> <KinEn1> <KinEn2> <FrPull>))
�������::
NIL
---------
;������ �������� True ���� � ������ x ���������� ������� y
(defun ������ (x y)( LAMBDA (x y) (COND ((eql x NIL) NIL) ((eql y (CAR x)) T) (T (������ (CDR x) y)))) )
;������ �������� T ���� ��� �������� ������ x ���������� � ������ y
(defun ����� (x y)( LAMBDA (x y) (COND ((eql x NIL) T) ((������ y (CAR x)) (����� (CDR x) y)) (T NIL))) )
---------
-----
(defun �������+2 (x y z v u)( LAMBDA (x y z v u) (COND ((eql v NIL) (������� x y z u)) (T (������ (CAR v) (������� v 3) (����� (������� v 3) u))))) )
--------
������� �������+2::
(�������+2 '(((<FrPull> <Mass> <Accl>) (<FrFric>) ((f3))) ((<Cfg> <CfFric> <Mass>) (<FrFric>) ((f15))) ((<OpFric> <Dist>) (<FrFric>) ((f12)))) '((<Spd2> (f1 <Spd1> <Accl> <Time>) (f2 <Accl> <Dist> <Spd1>) (f11 <KinEn2> <Mass>)) 
(<Spd1> (f3 <Spd2> <Accl> <Time>) (f4 <Dist> <Time> <Accl>) (f5 <Spd2> <Accl> <Dist>) (f11 <KinEn1> <Mass>)) 
(<Time> (f8 <Spd2> <Spd1> <Accl>) (f9 <Spd1> <Accl> <Dist>)) 
(<Accl> (f8 <Spd2> <Spd1> <Time>) (f10 <Dist> <Spd1> <Time>) (f7 <Spd2> <Spd1> <Dist>) (f8 <FrPull> <FrFric> <Mass>)) (<Mass> (f8 <FrPull> <FrFric> <Accl>) (f13 <FrFric> <CfFric> <Cfg>) (f14 <KinEn2> <Spd2>) (f14 <KinEn1> <Spd1>)) 
(<FrFric> (f3 <FrPull> <Mass> <Accl>) (f15 <Cfg> <CfFric> <Mass>) (f12 <OpFric> <Dist>)) 
(<CfFric> (f13 <FrFric> <Mass> <Cfg>)) 
(<OpPull> (f18 <FrPull> <Dist>) (f17 <KinEn2> <KinEn1> <OpFric>)) 
(<OpFric> (f18 <FrFric> <Dist>) (f17 <OpPull> <KinEn2> <KinEn1>))
) '(<Dist> <KinEn1> <KinEn2> <FrPull>) NIL '((f1 3) (f2 3) (f3 3) (f4 3) (f5 3) (f6 3) (f7 3) (f8 3) (f9 3) (f10 3) (f11 2) (f12 2) (f13 3) (f14 2) (f15 3) (f16 2) (f17 3) (f18 2)))
�������::
==> (f12 (f17 (f18 <FrPull> <Dist>) <KinEn2> <KinEn1>) <Dist>)
------------
;������������ ���������� ���������� ������ ������ ��������; x � ������ ������ ������; y � ������ ���������; z � ������ �������������� ������������; u � ������ �������
(defun ������� (x y z u)( LAMBDA (x y z u) (�������+ (������ (������� x y)) y z u)) )
------------
����! ������ ������������ � �����! ������� ���� �� �������� ������� �� �� ���� ����������!
������� ������� (x y z u)::
(������� '(((<FrPull> <Mass> <Accl>) (<FrFric>) ((f3))) ((<Cfg> <CfFric> <Mass>) (<FrFric>) ((f15))) ((<OpFric> <Dist>) (<FrFric>) ((f12)))) '((<Spd2> (f1 <Spd1> <Accl> <Time>) (f2 <Accl> <Dist> <Spd1>) (f11 <KinEn2> <Mass>)) 
(<Spd1> (f3 <Spd2> <Accl> <Time>) (f4 <Dist> <Time> <Accl>) (f5 <Spd2> <Accl> <Dist>) (f11 <KinEn1> <Mass>)) 
(<Time> (f8 <Spd2> <Spd1> <Accl>) (f9 <Spd1> <Accl> <Dist>)) 
(<Accl> (f8 <Spd2> <Spd1> <Time>) (f10 <Dist> <Spd1> <Time>) (f7 <Spd2> <Spd1> <Dist>) (f8 <FrPull> <FrFric> <Mass>)) (<Mass> (f8 <FrPull> <FrFric> <Accl>) (f13 <FrFric> <CfFric> <Cfg>) (f14 <KinEn2> <Spd2>) (f14 <KinEn1> <Spd1>)) 
(<FrFric> (f3 <FrPull> <Mass> <Accl>) (f15 <Cfg> <CfFric> <Mass>) (f12 <OpFric> <Dist>)) 
(<CfFric> (f13 <FrFric> <Mass> <Cfg>)) 
(<OpPull> (f18 <FrPull> <Dist>) (f17 <KinEn2> <KinEn1> <OpFric>)) 
(<OpFric> (f18 <FrFric> <Dist>) (f17 <OpPull> <KinEn2> <KinEn1>))
))
�������::
(((<FrPull> <FrPull> <FrFric> <Accl> <Accl>) (<FrFric> <Mass>) ((f3 2) (f8))) 
((<FrPull> <FrFric> <CfFric> <Cfg> <Accl>) (<FrFric> <Mass>) ((f3 2) (f13))) 
((<FrPull> <KinEn2> <Spd2> <Accl>) (<FrFric> <Mass>) ((f3 2) (f14))) 
((<FrPull> <KinEn1> <Spd1> <Accl>) (<FrFric> <Mass>) ((f3 2) (f14))) 
((<Cfg> <FrFric> <Mass> <Cfg> <Mass>) (<FrFric> <CfFric>) ((f15 2) (f13))) 
((<FrFric> <Dist> <Dist>) (<FrFric> <OpFric>) ((f12 1) (f18))) 
((<OpPull> <KinEn2> <KinEn1> <Dist>) (<FrFric> <OpFric>) ((f12 1) (f17)))
)
������ ������::
(������ '(((<FrPull> <FrPull> <FrFric> <Accl> <Accl>) (<FrFric> <Mass>) ((f3 2) (f8))) ((<FrPull> <FrFric> <CfFric> <Cfg> <Accl>) (<FrFric> <Mass>) ((f3 2) (f13))) ((<FrPull> <KinEn2> <Spd2> <Accl>) (<FrFric> <Mass>) ((f3 2) (f14))) ((<FrPull> <KinEn1> <Spd1> <Accl>) (<FrFric> <Mass>) ((f3 2) (f14))) ((<Cfg> <FrFric> <Mass> <Cfg> <Mass>) (<FrFric> <CfFric>) ((f15 2) (f13))) ((<FrFric> <Dist> <Dist>) (<FrFric> <OpFric>) ((f12 1) (f18))) ((<OpPull> <KinEn2> <KinEn1> <Dist>) (<FrFric> <OpFric>) ((f12 1) (f17)))) )
�������: (���� ���������� ���������� ������ ������������ � ����� � � ����� �������� ������� ������� � ���������!)
(((<FrPull> <KinEn2> <Spd2> <Accl>) (<FrFric> <Mass>) ((f3 2) (f14))) 
((<FrPull> <KinEn1> <Spd1> <Accl>) (<FrFric> <Mass>) ((f3 2) (f14))) 
((<OpPull> <KinEn2> <KinEn1> <Dist>) (<FrFric> <OpFric>) ((f12 1) (f17)))
)

���� �������� ������� -> ����� NIL -> ������� -> 
(((<FrPull> <KinEn2> <Spd1> <Accl> <Time> <Accl>) (<FrFric> <Mass> <Spd2>) ((f3 2) (f14 3) (f1))) 
((<FrPull> <KinEn2> <Accl> <Dist> <Spd1> <Accl>) (<FrFric> <Mass> <Spd2>) ((f3 2) (f14 3) (f2))) 
((<FrPull> <KinEn2> <KinEn2> <Mass> <Accl>) (<FrFric> <Mass> <Spd2>) ((f3 2) (f14 3) (f11))) 
((<FrPull> <KinEn1> <Spd2> <Accl> <Time> <Accl>) (<FrFric> <Mass> <Spd1>) ((f3 2) (f14 3) (f3))) 
((<FrPull> <KinEn1> <Dist> <Time> <Accl> <Accl>) (<FrFric> <Mass> <Spd1>) ((f3 2) (f14 3) (f4))) 
((<FrPull> <KinEn1> <Spd2> <Accl> <Dist> <Accl>) (<FrFric> <Mass> <Spd1>) ((f3 2) (f14 3) (f5))) 
((<FrPull> <KinEn1> <KinEn1> <Mass> <Accl>) (<FrFric> <Mass> <Spd1>) ((f3 2) (f14 3) (f11))) 
((<FrPull> <Dist> <KinEn2> <KinEn1> <Dist>) (<FrFric> <OpFric> <OpPull>) ((f12 1) (f17 1) (f18))) 
((<KinEn2> <KinEn1> <OpFric> <KinEn2> <KinEn1> <Dist>) (<FrFric> <OpFric> <OpPull>) ((f12 1) (f17 1) (f17)))
)
-> ������::
(������ '(((<FrPull> <KinEn2> <Spd1> <Accl> <Time> <Accl>) (<FrFric> <Mass> <Spd2>) ((f3 2) (f14 3) (f1))) 
((<FrPull> <KinEn2> <Accl> <Dist> <Spd1> <Accl>) (<FrFric> <Mass> <Spd2>) ((f3 2) (f14 3) (f2))) 
((<FrPull> <KinEn2> <KinEn2> <Mass> <Accl>) (<FrFric> <Mass> <Spd2>) ((f3 2) (f14 3) (f11))) 
((<FrPull> <KinEn1> <Spd2> <Accl> <Time> <Accl>) (<FrFric> <Mass> <Spd1>) ((f3 2) (f14 3) (f3))) 
((<FrPull> <KinEn1> <Dist> <Time> <Accl> <Accl>) (<FrFric> <Mass> <Spd1>) ((f3 2) (f14 3) (f4))) 
((<FrPull> <KinEn1> <Spd2> <Accl> <Dist> <Accl>) (<FrFric> <Mass> <Spd1>) ((f3 2) (f14 3) (f5))) 
((<FrPull> <KinEn1> <KinEn1> <Mass> <Accl>) (<FrFric> <Mass> <Spd1>) ((f3 2) (f14 3) (f11))) 
((<FrPull> <Dist> <KinEn2> <KinEn1> <Dist>) (<FrFric> <OpFric> <OpPull>) ((f12 1) (f17 1) (f18))) 
((<KinEn2> <KinEn1> <OpFric> <KinEn2> <KinEn1> <Dist>) (<FrFric> <OpFric> <OpPull>) ((f12 1) (f17 1) (f17)))
) )
�������::
(((<FrPull> <KinEn2> <Spd1> <Accl> <Time> <Accl>) (<FrFric> <Mass> <Spd2>) ((f3 2) (f14 3) (f1))) 
((<FrPull> <KinEn2> <Accl> <Dist> <Spd1> <Accl>) (<FrFric> <Mass> <Spd2>) ((f3 2) (f14 3) (f2))) 
((<FrPull> <KinEn1> <Spd2> <Accl> <Time> <Accl>) (<FrFric> <Mass> <Spd1>) ((f3 2) (f14 3) (f3))) 
((<FrPull> <KinEn1> <Dist> <Time> <Accl> <Accl>) (<FrFric> <Mass> <Spd1>) ((f3 2) (f14 3) (f4))) 
((<FrPull> <KinEn1> <Spd2> <Accl> <Dist> <Accl>) (<FrFric> <Mass> <Spd1>) ((f3 2) (f14 3) (f5))) 
((<FrPull> <Dist> <KinEn2> <KinEn1> <Dist>) (<FrFric> <OpFric> <OpPull>) ((f12 1) (f17 1) (f18)))
)

���� �������� ������� -> ����� T
#����� �� �����! � ������ ������������!
--------------
;(������ (CAR v) (������� v 3) (����� (������� v 3) u))
--------------
������
�����











;(������� '(<FrFric> <Dist> <KinEn1> <KinEn2> <FrPull>) ��������)

1) (car x) - ���������� ������ ������� ������ ������
2) (cdr x) - ����� ��������� �������� ������ ����� (nth 0 '(a b c)) => a
3) (cons x) - ����� ����� ������ � ������ ���������� � ��� ������ (cons 'a '(bc)) => (a b c)
4) (list x) - ����� ����� ������ (list 'a 'b) => (a b) append ������� ��������� ������� � ����
5) (cond x y � ) - (cond ((> a 20) (format t "~% a is greater than 20")) (t (format t "~% value of a is ~d " a)))
6) (eql x y) - (cond ((eql a 20) (���������) )
7) (+ x y) -
8) (- x y) - 
1) T - true
2) nil - false

���� ������::
number -> {real -> {rational -> {ratio, integer -> {bignum, fixnum -> bit}}, float -> {long-float, double-float}}
single-float short-float
atom cons list null nill t
function compiled-function
array simple-array hash-table 
character standard-char symbol string simple-string [string-char]
signed-byte	unsigned-byte 
vector simple-vector bit-vector simple-bit-vector
complex keyword stream readtable package pathname random-state [common] sequence
(print (type-of x))
(typep 1 'fixnum)

(declare (type fixnum a b))
(declare (fixnum a b))
(declaim (optimize(speed 3)(compilation-speed 0)(safety 0)(debug 0)))

return-from ��� �������� ������ �������� �� �������

(print 'i=)
(printline i)
(write (atom 'abcd))
(write (list 1 2)) => (1 2)
(write (list 'a 'b)) => (a b)
(format t "x = ~2d y = ~2d ~%" x y)
(princ "Enter Number : ")
(setq n1 (read))
https://www.tutorialspoint.com/lisp/lisp_input_output.htm
common lisp SYMBOL equal
https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node74.html
������������ ���� - ��� ����� ���������� ������������ �������� ����������
�������� ������� ���������� � ������ � �������, � ���� ���� � ������� �������� ��� 1 ����.
���� ���� � ������� ���������� 1 ����
3GHz ��� 3.000.000.000 ������ � �������!
��������� � �������� ����� ������� ��� ����� ��� ������!
|#
#|
(����� '(<Spd2>) '((<Spd2> (f1 <Spd1> <Accl> <Time>) (f2 <Accl> <Dist> <Spd1>) (f11 <KinEn2> <Mass>)) (<Dist> (f6 <Spd1> <Time> <Accl>) (f7 <Spd2> <Spd1> <Accl>) (f12 <OpPull> <FrPull>) (f12 <OpFric> <FrFric>)) (<Mass> (f8 <FrPull> <FrFric> <Accl>) (f13 <FrFric> <CfFric> <Cfg>) (f14 <KinEn2> <Spd2>) (f14 <KinEn1> <Spd1>)) (<FrPull> (f1 <FrFric> <Mass> <Accl>) (f12 <OpPull> <Dist>)) (<FrFric> (f3 <FrPull> <Mass> <Accl>) (f15 <Cfg> <CfFric> <Mass>) (f12 <OpFric> <Dist>)) (<CfFric> (f13 <FrFric> <Mass> <Cfg>)) (<KinEn2> (f16 <Mass> <Spd2>) (f17 <OpPull> <OpFric> <KinEn1>)) (<KinEn1> (f16 <Mass> <Spd1>) (f17 <KinEn2> <OpPull> <OpFric>)) (<OpPull> (f18 <FrPull> <Dist>) (f17 <KinEn2> <KinEn1> <OpFric>)) (<OpFric> (f18 <FrFric> <Dist>) (f17 <OpPull> <KinEn2> <KinEn1>))) )
|#