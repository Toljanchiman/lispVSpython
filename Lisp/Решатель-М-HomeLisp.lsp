//
// "HomeLisp ���=1.13.4 Date=25.01.2012 Time=18:50:01 (������� �.�.)"
//
// �������
//
(SEXPR fact ( LAMBDA (x) (COND ((MINUSP x) (RAISEERROR "�������� FACT �����������")) ((EQ x 0) 1) (T (TIMES x (fact (DIFFERENCE x 1)))))) )
(SEXPR copy ( LAMBDA (x) (COND ((ATOM x) x) (T (CONS (copy (CAR x)) (copy (CDR x)))))) )
(SEXPR appendd ( LAMBDA (&OPTIONAL (x NIL) (y NIL)) (COND ((NULL x) y) (T (CONS (CAR x) (appendd (CDR x) y))))) )
(SEXPR equal ( LAMBDA (x y) (COND ((ATOM x) (EQ x y)) ((ATOM y) NIL) ((equal (CAR x) (CAR y)) (equal (CDR x) (CDR y))) (T NIL))) )
(SEXPR member ( LAMBDA (x y) (COND ((NULL y) NIL) ((equal x (CAR y)) T) (T (member x (CDR y))))) )
(SEXPR memb ( LAMBDA (x y) (PROG NIL L (COND ((NULL y) (RETURN NIL)) ((EQ x (CAR y)) (RETURN T))) (SETQ y (CDR y)) (GO L))) )
(SEXPR rev1 ( LAMBDA (u v) (COND ((NULL u) v) (T (rev1 (CDR u) (CONS (rev (CAR u)) v))))) )
(SEXPR rev ( LAMBDA (x) (COND ((ATOM x) x) (T (rev1 x NIL)))) )
(SEXPR remove ( LAMBDA (x L) (COND ((NULL L) NIL) ((equal x (CAR L)) (remove x (CDR L))) (T (CONS (CAR L) (remove x (CDR L)))))) )
(SEXPR removef ( LAMBDA (x y) (COND ((NULL y) NIL) ((equal x (CAR y)) (CDR y)) (T (CONS (CAR y) (removef x (CDR y)))))) )
(SEXPR last ( LAMBDA (x) (COND ((NULL x) NIL) ((NULL (CDR x)) (CAR x)) (T (last (CDR x))))) )
(SEXPR length ( LAMBDA (x) (COND ((ATOM x) 0) (T (PLUS 1 (length (CDR x)))))) )
(SEXPR addifnone ( LAMBDA (x L) (COND ((member x L) L) (T (CONS x L)))) )
(SEXPR collect ( LAMBDA (L) (COND ((NULL L) NIL) (T (CONS (CAR L) (collect (COND ((member (CAR L) (CDR L)) (CONS (CAR L) (removef (CAR L) (CDR L)))) (T (CDR L)))))))) )
(SEXPR reverse ( LAMBDA (x) (PROG (u) a (COND ((NULL x) (RETURN u))) (SETQ u (CONS (CAR x) u)) (SETQ x (CDR x)) (GO a))) )
(SEXPR flatten ( LAMBDA (x) (COND ((NULL x) NIL) ((ATOM x) (LIST x)) (T (appendd (flatten (CAR x)) (flatten (CDR x)))))) )
(SEXPR attach ( LAMBDA (x y) (RPLACA (RPLACD y (CONS (CAR y) (CDR y))) x)) )
(SEXPR dreverse ( LAMBDA (x) (PROG (u v) a (COND ((NULL x) (RETURN u))) (SETQ v x) (SETQ x (CDR x)) (SETQ u (RPLACD v u)) (GO a))) )
(SEXPR nconc ( LAMBDA (x y) (COND ((NULL x) y) (T (RPLACD x (nconc (CDR x) y))))) )
(SEXPR tconc ( LAMBDA (x q) (COND ((NULL q) (CONS (SETQ q (CONS x NIL)) q)) (T (RPLACD q (CDR (RPLACD (CDR q) (CONS x NIL))))))) )
(SEXPR efface ( LAMBDA (x y) (COND ((NULL y) NIL) ((equal x (CAR y)) (CDR y)) (T (RPLACD y (efface x (CDR y)))))) )
(SEXPR dremove ( LAMBDA (x y) (COND ((NULL y) NIL) ((equal x (CAR y)) (dremove x (CDR y))) (T (RPLACD y (dremove x (CDR y)))))) )
(SEXPR lcyclep ( LAMBDA (x) (AND (NOT (ATOM x)) (NOT (ATOM (CDR x))) (lcycle1 (CDR x) (CDDR x)))) )
(SEXPR lcycle1 ( LAMBDA (x y) (OR (EQ x y) (AND (NOT (ATOM y)) (NOT (ATOM (CDR y))) (lcycle1 (CDR x) (CDDR y))))) )
(SEXPR cyclep ( LAMBDA (x) (NOT (cycle1 x NIL (QUOTE (T))))) )
(SEXPR cycle1 ( LAMBDA (x u v) (COND ((ATOM x) v) ((memb x u) NIL) ((memb x v) v) ((NULL (SETQ v (cycle1 (CAR x) (SETQ u (CONS x u)) v))) NIL) ((NULL (SETQ v (cycle1 (CDR x) u v))) NIL) (T (CONS x v)))) )
(SEXPR forall ( LAMBDA (L p) (COND ((NULL L) T) ((FUNCALL p (CAR L)) (forall (CDR L) p)) (T NIL))) )
(SEXPR forsome ( LAMBDA (L p) (COND ((NULL L) NIL) ((FUNCALL p (CAR L)) T) (T (forsome (CDR L) p)))) )
(SEXPR forodd ( LAMBDA (L p) (COND ((NULL L) NIL) ((FUNCALL p (CAR L)) (NOT (forodd (CDR L) p))) (T (forodd (CDR L) p)))) )
(SEXPR atomlist ( LAMBDA (x) (COND ((NULL x) T) ((ATOM x) NIL) ((ATOM (CAR x)) (atomlist (CDR x))) (T NIL))) )
(SEXPR listp ( LAMBDA (x) (COND ((NULL x) T) ((ATOM x) NIL) ((OR (ATOM (CAR x)) (listp (CAR x))) (listp (CDR x))) (T NIL))) )
(SEXPR order ( LAMBDA (x y L) (COND ((NULL L) NIL) ((equal x (CAR L)) T) ((equal y (CAR L)) NIL) (T (order x y (CDR L))))) )
(SEXPR order1 ( LAMBDA (x y L) (COND ((NULL L) (QUOTE orderundef)) ((equal x (CAR L)) T) ((equal y (CAR L)) NIL) (T (order1 x y (CDR L))))) )
(SEXPR lexorder ( LAMBDA (x y L) (COND ((NULL x) T) ((NULL y) NIL) ((equal (CAR x) (CAR y)) (lexorder (CDR x) (CDR y) L)) (T (order1 (CAR x) (CAR y) L)))) )
(SEXPR lexorder1 ( LAMBDA (x y L) (COND ((NULL x) T) ((NULL y) NIL) ((NULL L) (QUOTE lexorderundef)) ((equal (CAR x) (CAR y)) (lexorder1 (CDR x) (CDR y) (CDR L))) (T (order1 (CAR x) (CAR y) (CAR L))))) )
(SEXPR first ( LAMBDA (x y) (COND ((NULL y) (CAR x)) ((member (CAR y) x) (CAR y)) (T (first x (CDR y))))) )
(SEXPR rank ( LAMBDA (x y) (COND ((NULL x) NIL) (T (CONS (first x y) (rank (removef (first x y) x) y))))) )
(SEXPR possessing ( LAMBDA (p L) (PROG (u) a (COND ((NULL L) (RETURN (reverse u))) ((FUNCALL p (CAR L)) (SETQ u (CONS (CAR L) u)))) (SETQ L (CDR L)) (GO a))) )
(SEXPR suchthat ( LAMBDA (p L) (COND ((NULL L) NIL) ((FUNCALL p (CAR L)) (CAR L)) (T (suchthat p (CDR L))))) )
(SEXPR suchthat1 ( LAMBDA (p L x y) (COND ((NULL L) x) ((FUNCALL p (CAR L)) y) (T (suchthat1 p (CDR L) x y)))) )
(SEXPR suchthat2 ( LAMBDA (p L f) (COND ((NULL L) NIL) ((FUNCALL p (CAR L)) (FUNCALL f L)) (T (suchthat2 p (CDR L) f)))) )
(SEXPR setof ( LAMBDA (x) (COND ((NULL x) NIL) ((member (CAR x) (CDR x)) (setof (CDR x))) (T (CONS (CAR x) (setof (CDR x)))))) )
(SEXPR makeset ( LAMBDA (x) (PROG (y) a (COND ((NULL x) (RETURN y)) ((NULL (member (CAR x) y)) (SETQ y (CONS (CAR x) y)))) (SETQ x (CDR x)) (GO a))) )
(SEXPR diflist ( LAMBDA (x y) (COND ((NULL y) x) (T (diflist (remove (CAR y) x) (CDR y))))) )
(SEXPR subset ( LAMBDA (x y) (NULL (diflist x y))) )
(SEXPR union0 ( LAMBDA (x y) (COND ((NULL x) y) ((member (CAR x) y) (union (CDR x) y)) (T (CONS (CAR x) (union (CDR x) y))))) )
(SEXPR union ( LAMBDA (x y) (PROG NIL (SETQ y (makeset y)) a (COND ((NULL x) (RETURN y)) ((NOT (member (CAR x) y)) (SETQ y (CONS (CAR x) y)))) (SETQ x (CDR x)) (GO a))) )
(SEXPR lunion ( LAMBDA (x) (COND ((NULL x) NIL) (T (union (CAR x) (lunion (CDR x)))))) )
(SEXPR intersection ( LAMBDA (x y) (COND ((NULL x) NIL) ((member (CAR x) y) (CONS (CAR x) (intersection (CDR x) y))) (T (intersection (CDR x) y)))) )
(SEXPR equalset ( LAMBDA (x y) (AND (subset x y) (subset y x))) )
(SEXPR cart ( LAMBDA (x y) (PROG (u v w) a (COND ((NULL x) (RETURN (reverse w)))) (SETQ u (CAR x)) (SETQ x (CDR x)) (SETQ v y) b (COND ((NULL v) (GO a))) (SETQ w (CONS (LIST u (CAR v)) w)) (SETQ v (CDR v)) (GO b))) )
(SEXPR pair ( LAMBDA (x y) (COND ((NULL x) NIL) (T (CONS (CONS (CAR x) (CAR y)) (pair (CDR x) (CDR y)))))) )
(SEXPR dpair ( LAMBDA (x y) (PROG (u) (SETQ u x) a (COND ((NULL u) (RETURN x))) (RPLACA u (CONS (CAR u) (CAR y))) (SETQ u (CDR u)) (SETQ y (CDR y)) (GO a))) )
(SEXPR assoc ( LAMBDA (x a) (COND ((NULL a) NIL) ((equal x (CAAR a)) (CAR a)) (T (assoc x (CDR a))))) )
(SEXPR pairlis ( LAMBDA (x y a) (COND ((NULL x) a) (T (CONS (CONS (CAR x) (CAR y)) (pairlis (CDR x) (CDR y) a))))) )
(SEXPR subst ( LAMBDA (x y z) (COND ((equal y z) x) ((ATOM z) z) (T (CONS (subst x y (CAR z)) (subst x y (CDR z)))))) )
(SEXPR sublis ( LAMBDA (a y) (COND ((NULL y) NIL) ((ATOM y) (sub2 a y)) (T (CONS (sublis a (CAR y)) (sublis a (CDR y)))))) )
(SEXPR sub2 ( LAMBDA (a y) (COND ((NULL a) y) ((EQ y (CAAR a)) (CDAR a)) (T (sub2 (CDR a) y)))) )
(SEXPR sassoc ( LAMBDA (x a f) (COND ((NULL a) (FUNCALL f)) ((equal x (CAAR a)) (CAR a)) (T (sassoc x (CDR a) f)))) )
(SEXPR maplist ( LAMBDA (x f) (COND ((NULL x) NIL) (T (CONS (FUNCALL f x) (maplist (CDR x) f))))) )
(SEXPR mapcar ( LAMBDA (x f) (maplist x (FUNCTION (LAMBDA (x) (FUNCALL f (CAR x)))))) )
(SEXPR map ( LAMBDA (x f) (PROG NIL a (COND ((ATOM x) (RETURN x))) (FUNCALL f x) (SETQ x (CDR x)) (GO a))) )
(SMACRO push ( LAMBDA (a p) (LIST (QUOTE SETQ) p (LIST (QUOTE CONS) a p))) )
(SMACRO pop ( LAMBDA (p) (LIST (QUOTE SETQ) p (LIST (QUOTE CDR) p))) )
(SMACRO popup ( LAMBDA (a p) (LIST (QUOTE PROG) NIL (LIST (QUOTE SETQ) a (LIST (QUOTE CAR) p)) (LIST (QUOTE SETQ) p (LIST (QUOTE CDR) p)))) )
(SMACRO define ( LAMBDA (x) (COND ((NULL x) NIL) (T (LIST (QUOTE CONS) (LIST (QUOTE SEXPR) (CAAR x) (CADAR x)) (LIST (QUOTE define) (CDR x)))))) )
(SMACRO deflist ( LAMBDA (x i) (COND ((NULL x) NIL) (T (LIST (QUOTE CONS) (LIST (QUOTE QUOTE) (putprop (CAAR x) i (CADAR x))) (LIST (QUOTE deflist) (CDR x) i))))) )
(SEXPR putprop ( LAMBDA (a i p) (PROG (u v) (SETQ u (CONS NIL (copy (PROPLIST a)))) (SETQ v u) a (COND ((NULL (CDR v)) (RPLACD v (LIST i p))) ((EQ (CAR (SETQ v (CDR v))) i) (COND ((NULL (CDR v)) (RPLACD v (LIST p))) (T (RPLACA (CDR v) p)))) (T (GO a))) (SPROPL a (CDR u)) (RETURN a))) )
(SEXPR select ( LAMBDA (e L e0) (COND ((NULL L) e0) ((EQ e (CAR L)) (CADR L)) (T (select e (CDDR L) e0)))) )
(SEXPR getprop ( LAMBDA (a i) (PROG (u) (SETQ u (PROPLIST a)) a (COND ((NULL u) (RETURN NIL)) ((EQ (CAR u) i) (GO b))) (SETQ u (CDR u)) (GO a) b (RETURN (COND ((EQ i EXPR) EXPR) ((EQ i FEXPR) FEXPR) ((EQ i APVAL) APVAL) ((EQ i FIXED) FIXED) ((EQ i BITS) BITS) ((EQ i STRING) STRING) ((EQ i FLOAT) FLOAT) ((EQ i SUBR) SUBR) ((EQ i FSUBR) FSUBR) ((NULL (CDR u)) NIL) (T (CADR u)))))) )
(SEXPR prop ( LAMBDA (a i f) (PROG (u) (SETQ u (PROPLIST a)) a (COND ((NULL u) (RETURN (f))) ((EQ (CAR u) i) (RETURN (CDR u)))) (SETQ u (CDR u)) (GO a))) )
(SEXPR remprop ( LAMBDA (a i) (PROG (u v) (COND ((EQ i EXPR) (GO c)) ((EQ i FEXPR) (GO c)) ((EQ i APVAL) (GO c)) ((EQ i FIXED) (GO c)) ((EQ i BITS) (GO c)) ((EQ i STRING) (GO c)) ((EQ i FLOAT) (GO c))) ((EQ i WINDOW) (GO c))) ((EQ i DIALOG) (GO c))) )
(SEXPR putflag ( LAMBDA (a i) (SPROPL a (CONS i (PROPLIST a)))) )
(SEXPR flagp ( LAMBDA (a i) (member i (PROPLIST a))) )
(SEXPR flag ( LAMBDA (L i) (PROG (u) a (COND ((NULL L) (RETURN NIL))) (SETQ u (PROPLIST (CAR L))) (COND ((NOT (member i u)) (SPROPL (CAR L) (CONS i u)))) (SETQ L (CDR L)) (GO a))) )
(SEXPR remflag ( LAMBDA (L i) (PROG (u v) a (COND ((NULL L) (RETURN NIL))) (SETQ u (CONS NIL (PROPLIST (CAR L)))) (SETQ v u) b (COND ((NULL (CDR v)) (GO d)) ((EQ (CADR v) i) (GO c))) (SETQ v (CDR v)) (GO b) c (RPLACD v (CDDR v)) (GO b) d (SPROPL (CAR L) (CDR u)) (SETQ L (CDR L)) (GO a))) )
(SEXPR alert ( LAMBDA NIL (PROG NIL (TERPRI) (PRINT "��������� �� ������!") (TERPRI))) )
(SMACRO for ( LAMBDA (i iBeg iEnd body) (BACKQUOTE (PROG (, i) (SETQ , i , iBeg) $loop ,@ body (SETQ , i (ADD1 , i)) (COND ((<= , i , iEnd) (GO $loop))) (RETURN , iEnd)))) )
(SEXPR prlists ( LAMBDA (x) (PROG (tail) (SETQ tail x) @l (COND ((NULL tail) (RETURN T))) (PRINTS (CAR tail)) (TERPRI) (SETQ tail (CDR tail)) (GO @l))) )
(SEXPR FLOOR ( LAMBDA (x) (FIX x)) )
(SEXPR CEIL ( LAMBDA (x) (PLUS 1 (FIX x))) )
(SEXPR fdir ( LAMBDA (aDir) (PROG (tail ff fn) (SETQ tail (SYSDIR (STRCAT aDir "\*.*") &H1F)) @l (COND ((NULL tail) (RETURN T))) (SETQ ff (CAR tail)) (SETQ tail (CDR tail)) (SETQ fn (STRCAT (STRCAT aDir "\") ff)) (PRINTS (CONS fn (SYSGETATTR fn))) (TERPRI) (GO @l))) )
(SEXPR setmarg ( LAMBDA (m) (PROG NIL (COND ((GREATERP m 1) (PRINTS " ")) (T (RETURN NIL))) (setmarg (DIFFERENCE m 1)))) )
(SEXPR pprint ( LAMBDA (L marg) (COND ((ATOM L) (PROG NIL (setmarg marg) (PRINT L))) ((LESSP (length L) 3) (PROG NIL (setmarg marg) (PRINT L))) (T (PROG NIL (TERPRI) (setmarg marg) (PRINTS "(") (PRINTS (CAR L)) (print-tail (CDR L) marg))))) )
(SEXPR print-tail ( LAMBDA (L marg) (COND ((NULL L) (PRINTS ")")) (T (PROG NIL (TERPRI) (setmarg marg) (pprint (CAR L) (PLUS marg 3)) (print-tail (CDR L) marg))))) )
(SEXPR mklist1 ( LAMBDA (x) (COND ((EQ x 1) (QUOTE (1))) (T (CONS x (mklist1 (DIFFERENCE x 1)))))) )
(SEXPR mklist2 ( LAMBDA (n1 n2) (COND ((EQ n1 n2) (LIST n1)) (T (appendd (LIST n1) (mklist2 (PLUS n1 1) n2))))) )
(SEXPR lastItem ( LAMBDA (x) (COND ((NULL (CDR x)) (CAR x)) (T (lastItem (CDR x))))) )
(SEXPR get_even ( LAMBDA (x) (COND ((NULL x) NIL) (T (appendd (LIST (CADR x)) (get_even (CDDR x)))))) )
(SEXPR get_odd ( LAMBDA (x) (COND ((NULL x) NIL) (T (appendd (LIST (CAR x)) (get_odd (CDDR x)))))) )
(SEXPR add_uniq ( LAMBDA (item lst) (COND ((NULL lst) (LIST item)) ((EQ item (CAR lst)) lst) (T (appendd (LIST (CAR lst)) (add_uniq item (CDR lst)))))) )
(SEXPR amax ( LAMBDA (x) (COND ((NULL x) NIL) ((NULL (CDR x)) (CAR x)) (T (COND ((GREQP (CAR x) (amax (CDR x))) (CAR x)) (T (amax (CDR x))))))) )
(SEXPR amin ( LAMBDA (x) (COND ((NULL x) NIL) ((NULL (CDR x)) (CAR x)) (T (COND ((LEEQP (CAR x) (amin (CDR x))) (CAR x)) (T (amin (CDR x))))))) )
(SEXPR filter ( LAMBDA (lst n fun) (COND ((NULL lst) NIL) (T (COND ((FUNCALL fun (CAR lst) n) (CONS (CAR lst) (filter (CDR lst) n fun))) (T (filter (CDR lst) n fun)))))) )
(SEXPR prlist ( LAMBDA (x) (COND ((NULL x) NIL) (T (PROG NIL (PRINTSLINE (CAR x)) (prlist (CDR x)))))) )
(SEXPR chk1p ( LAMBDA (x) (PROG (hd tl c) (SETQ c x) LOOP (SETQ hd (CAR c)) (SETQ tl (CDR c)) (COND ((NOT (ATOM hd)) (RETURN NIL))) (COND ((NULL tl) (RETURN T))) (SETQ c tl) (GO LOOP))) )
(SEXPR weight ( LAMBDA (x) (COND ((EQ x (QUOTE +)) 1) ((EQ x (QUOTE -)) 1) ((EQ x (QUOTE *)) 2) ((EQ x (QUOTE \)) 2) ((EQ x (QUOTE /)) 2) ((EQ x (QUOTE ^)) 3) (T 5))) )
(SEXPR opcode ( LAMBDA (op) (COND ((EQ op (QUOTE +)) (QUOTE PLUS)) ((EQ op (QUOTE -)) (QUOTE DIFFERENCE)) ((EQ op (QUOTE *)) (QUOTE TIMES)) ((EQ op (QUOTE \)) (QUOTE QUOTIENT)) ((EQ op (QUOTE /)) (QUOTE DIVIDE)) ((EQ op (QUOTE ^)) (QUOTE EXPT)) (T (RAISEERROR (STRCAT "������� ��� �������� " (OUTPUT op)))))) )
(SEXPR inf-aux ( LAMBDA (ae operators operands) (inf-iter (CDR ae) operators (CONS (CAR ae) operands))) )
(SEXPR inf-iter ( LAMBDA (ae operators operands) (PROG NIL (COND ((AND (NULL ae) (NULL operators)) (RETURN (CAR operands)))) (COND ((AND (NOT (NULL ae)) (OR (NULL operators) (GREATERP (weight (CAR ae)) (weight (CAR operators))))) (RETURN (inf-aux (CDR ae) (CONS (CAR ae) operators) operands)))) (RETURN (inf-iter ae (CDR operators) (CONS (LIST (opcode (CAR operators)) (CADR operands) (CAR operands)) (CDDR operands)))))) )
(SEXPR inf2pref ( LAMBDA (x) (PROG (hd tl cc xx rr) (COND ((chk1p x) (RETURN (inf-aux x NIL NIL)))) (SETQ rr NIL) (SETQ xx x) @loop (SETQ hd (CAR xx)) (SETQ tl (CDR xx)) (COND ((memb hd (QUOTE (SIN COS LOG EXP ATN ASN ACS SH CH SQR SIGN))) (PROGN (SETQ rr (appendd rr (LIST (LIST hd (inf2pref (CAR tl)))))) (SETQ tl (CDR tl)))) ((ATOM hd) (SETQ rr (appendd rr (LIST hd)))) (T (SETQ rr (appendd rr (LIST (inf2pref hd)))))) (COND ((NULL tl) (RETURN (inf-aux rr NIL NIL)))) (SETQ xx tl) (GO @loop))) )
(SEXPR ��������� ( LAMBDA (x) (COND ((= x NIL) (PRINTS "������� �� ���������.")) (T (���������+ x NIL)))) )
(SEXPR ���������+ ( LAMBDA (x w) (COND ((= x NIL) (PRINTS "��� ������� ���������.")) (T (���������+ (CDR x) (EVAL (CAR x)))))) )
(SEXPR ������ ( LAMBDA (x) (������+ x NIL)) )
(SEXPR ������+ ( LAMBDA (x w) (COND ((= (CDR x) NIL) (SET (CAR (CAR x)) (������� (CAR x) 2))) (T (������+ (CDR x) (SET (CAR (CAR x)) (������� (CAR x) 2)))))) )
(SEXPR ������� ( LAMBDA (x n) (COND ((= n 1) (CAR x)) (T (������� (CDR x) (- n 1))))) )
(SEXPR ���� ( LAMBDA (x y) (COND ((= x NIL) y) (T (CONS (CAR x) (���� (CDR x) y))))) )
(SEXPR ����� ( LAMBDA (x y) (���� x (LIST y))) )
(SEXPR �����1 ( LAMBDA (x y) (����� (����� x) (����� (������ x) y))) )
(SEXPR ����� ( LAMBDA (x) (COND ((= (CDR x) NIL) NIL) (T (�����+ (CDR x) (LIST (CAR x)))))) )
(SEXPR ������ ( LAMBDA (x) (COND ((= (CDR x) NIL) (CAR x)) (T (������ (CDR x))))) )
(SEXPR �����+ ( LAMBDA (x y) (COND ((= (CDR x) NIL) y) (T (�����+ (CDR x) (����� y (CAR x)))))) )
(SEXPR ���� ( LAMBDA (x) (����� x (������ x))) )
(SEXPR ������ ( LAMBDA (x n) (COND ((= n 2) (LIST (CAR x))) ((= n 1) NIL) (T (���� (LIST (CAR x)) (������ (CDR x) (- n 1)))))) )
(SEXPR ����� ( LAMBDA (x n) (COND ((= n 1) x) (T (����� (CDR x) (- n 1))))) )
(SEXPR ������ ( LAMBDA (x y n) (���� (������ x n) (���� y (����� x (+ n 1))))) )
(SEXPR ������� ( LAMBDA (x y n) (���� (������ x n) (CONS y (����� x n)))) )
(SEXPR �������1 ( LAMBDA (x y n) (���� (������ x n) (CONS (����� (������� x n) y) (����� x (+ n 1))))) )
(SEXPR ������ ( LAMBDA (x y) (COND ((= x NIL) NIL) ((= y (CAR x)) T) (T (������ (CDR x) y)))) )
(SEXPR ������� ( LAMBDA (x y) (COND ((= y NIL) NIL) ((������ x (CAR y)) T) (T (������� x (CDR y))))) )
(SEXPR ����� ( LAMBDA (x y) (COND ((= x NIL) T) ((������ y (CAR x)) (����� (CDR x) y)) (T NIL))) )
(SEXPR ��������1 ( LAMBDA (x y) (��������1+ x y 1)) )
(SEXPR ��������1+ ( LAMBDA (x y n) (COND ((= x NIL) 0) ((= y (CAR (CAR x))) n) (T (��������1+ (CDR x) y (+ n 1))))) )
(SEXPR ����� ( LAMBDA (x) (COND ((= x NIL) 0) (T (+ 1 (����� (CDR x)))))) )
(SEXPR ������+2 ( LAMBDA (x w) (PRINTS x)) )
(SEXPR ������+ ( LAMBDA (x w) (COND ((= x NIL) NIL) (T (������+ (CDR x) (������+2 (CAR x) (PRINTS " ")))))) )
(SEXPR ���� ( LAMBDA (x) (COND ((= x NIL) NIL) (T (CONS (CAR (CAR x)) (���� (CDR x)))))) )
(SEXPR ������� ( LAMBDA (x) (�������+ x NIL 1 0 NIL)) )
(SEXPR �������+ ( LAMBDA (x v m n w) (COND ((= x NIL) (LIST v w)) (T (COND ((= n 0) (�������+ (CDR x) (����� v (CONS (CAR (CAR x)) (LIST (����� (������� (CAR x) 2))))) (+ m 1) (��������1 v (CAR (CAR (CDR x)))) (CONS (CONS (QUOTE DEFUN) (CAR x)) w))) (T (LIST (LIST 0 (CAR (CAR x)) n m))))))) )
(SEXPR ������� ( LAMBDA (x y n) (COND ((= n 0) (����� y (LIST (CAR x) (CDR x)))) (T (�������1 y (CDR x) n)))) )
(SEXPR ��������� ( LAMBDA (x) (���������+ x NIL)) )
(SEXPR ���������+ ( LAMBDA (x y) (COND ((= x NIL) y) (T (���������+ (CDR x) (������� (CAR x) y (��������1 y (CAR (CAR x)))))))) )
(SEXPR ��������1 ( LAMBDA (x y) (��������1+ (CDR x) y (��������1 y (CAR (CAR (CDR x)))))) )
(SEXPR ��������1+ ( LAMBDA (x y n) (COND ((= x NIL) NIL) ((= n 0) (LIST 1 (CAR (CAR x)))) (T (COND ((= (����� (CDR (CAR x))) (CAR (CDR (������� y n)))) (��������1+ (CDR x) y (��������1 y (CAR (CAR (CDR x)))))) (T (LIST 2 (CAR (CAR x)))))))) )
(SEXPR �������� ( LAMBDA (x y) (��������+ x y x (��������1 (CAR x) y) 1)) )
(SEXPR ��������+ ( LAMBDA (x y u v m) (COND ((= x NIL) u) ((= v NIL) (��������+ (CDR x) y u (��������1 (CAR (CDR x)) y) (+ m 1))) (T (����� v m)))) )
(SEXPR ������ ( LAMBDA (x) (������+ (CAR x) (������� x 2) (������� x 3) (������� x 4) (PRINTS "���� ������ �� �������!"))) )
(SEXPR ������+ ( LAMBDA (k f m n w) (COND ((= k 0) (������ (LIST (QUOTE �������) f (QUOTE ����������) (QUOTE ��) m (QUOTE �) n (QUOTE ��������.)))) ((= k 1) (������ (LIST (QUOTE �������) f (QUOTE ���������) m (QUOTE ��) (QUOTE ������.)))) ((= k 2) (������ (LIST (QUOTE �������) f (QUOTE �) (QUOTE ���������) m (QUOTE ����������) (QUOTE ������������) (QUOTE ����������) (QUOTE ����������.)))))) )
(SEXPR ���� ( LAMBDA (x y z) (����+ x (CAR (������� y)) (�������� (��������� z) (CAR (������� y))) (������� (������� y) 2))) )
(SEXPR ����+ ( LAMBDA (x y z v) (COND ((= v "��� ������� ���������.") (SET x (LIST y z))) ((= (CAR y) 0) (������ y)) ((= (CAR z) 1) (������ z)) ((= (CAR z) 2) (������ z)) (T (����+ x y z (��������� v))))) )
(SEXPR ������ ( LAMBDA (x y) (COND ((= x NIL) NIL) ((������ y (CAR (CAR x))) (������ (CDR x) y)) (T (CONS (CAR x) (������ (CDR x) y))))) )
(SEXPR ������ ( LAMBDA (x y k) (COND ((= k 1) (����� x y)) (T (����� (����� x) (������ (������ x) y (- k 1)))))) )
(SEXPR ������ ( LAMBDA (x) (COND ((= x NIL) NIL) ((= (CAR x) 0) (������ (CDR x))) (T (CONS (- (CAR x) 1) (CDR x))))) )
(SEXPR ������ ( LAMBDA (x y z) (������+ x (CDR y) (CDR z) (LIST (CAR z)) (LIST (CAR (CAR y))) (CAR (CDR (CAR y))) 1 1)) )
(SEXPR ������+ ( LAMBDA (x y z v f n k i) (COND ((= x NIL) f) ((= i n) (������+ x (CDR y) (CDR z) (CONS (CAR z) v) (������ f (LIST (CAR (CAR y))) k) (CAR (CDR (CAR y))) (+ k 1) i)) ((= (CAR v) 1) (������+ (CDR x) y z (������ (������ v)) (������ f (CAR x) k) n (- k 1) (+ i 1))) (T (������+ (CDR x) y z (������ v) (������ f (CAR x) k) n k (+ i 1))))) )
(SEXPR ����� ( LAMBDA (x y) (�����+ x y 1 (��������1 y (CAR x)))) )
(SEXPR �����+ ( LAMBDA (x y n m) (COND ((= x NIL) (QUOTE (0 0))) ((= m 0) (�����+ (CDR x) y (+ n 1) (��������1 y (CAR (CDR x))))) (T (����� (LIST n) m)))) )
(SEXPR ������ ( LAMBDA (x y z) (COND ((= (CDR x) NIL) (LIST (CONS (CAR x) (CONS (CAR y) (LIST (CAR z)))))) (T (CONS (CONS (CAR x) (CONS (CAR y) (LIST (CAR z)))) (������ (CDR x) (CDR y) (CDR z)))))) )
(SEXPR ������� ( LAMBDA (x y) (�������+ x y (����� (CAR x) y))) )
(SEXPR �������+ ( LAMBDA (x y z) (�������+2 x y (CAR z) (CAR (CDR z)))) )
(SEXPR �������+2 ( LAMBDA (x y n m) (COND ((= n 0) x) (T (�������+3 (CAR x) (������� x 2) (������� x 3) (������� y m) n)))) )
(SEXPR �������+3 ( LAMBDA (x y z u n) (�������+4 (LIST (������ x (CDR (CAR (CDR u))) n)) (LIST (����� y (CAR u))) (COND ((= z NIL) (LIST (LIST (LIST (CAR (CAR (CDR u))))))) (T (LIST (����� (�����1 z n) (LIST (CAR (CAR (CDR u)))))))) (CDR (CDR u)) x n)) )
(SEXPR �������+4 ( LAMBDA (x y z u v n) (COND ((= u NIL) (������ x y z)) (T (�������+4 (����� x (������ v (CDR (CAR u)) n)) (���� y) (����� z (����� (����� (CAR z)) (LIST (CAR (CAR u))))) (CDR u) v n)))) )
(SEXPR ������� ( LAMBDA (x y) (COND ((= (CDR x) NIL) (������� (CAR x) y)) (T (���� (������� (CAR x) y) (������� (CDR x) y))))) )
(SEXPR ������ ( LAMBDA (x) (COND ((= x NIL) NIL) ((������� (CAR (CAR x)) (������� (CAR x) 2)) (������ (CDR x))) (T (CONS (CAR x) (������ (CDR x)))))) )
(SEXPR ����� ( LAMBDA (x y) (������� (CONS (LIST x) (CONS NIL (LIST NIL))) y)) )
(SEXPR ����� ( LAMBDA (x y) (COND ((= x NIL) NIL) ((����� (CAR (CAR x)) y) (CAR x)) (T (����� (CDR x) y)))) )
(SEXPR ����� ( LAMBDA (x y) (COND ((= x NIL) NIL) (T (CONS (������� (������� y (��������1 y (CAR (CAR x)))) 2) (����� (CDR x) y))))) )
(SEXPR ������� ( LAMBDA (x y z u) (�������+ (������ (������� x y)) y z u)) )
(SEXPR �������+ ( LAMBDA (x y z u) (COND ((= x NIL) (PRINTS "������ �� ����� �������!")) (T (�������+2 x y z (����� x z) u)))) )
(SEXPR �������+2 ( LAMBDA (x y z v u) (COND ((= v NIL) (������� x y z u)) (T (������ (CAR v) (������� v 3) (����� (������� v 3) u))))) )
(SEXPR ������� ( LAMBDA (x y) (�������+ (CAR x) (������ (������� y 2) (CDR x)) (CDR x) (CAR y))) )
(SEXPR �������+ ( LAMBDA (x y z u) (�������+ (������ (����� x y)) y z u)) )
(SEXPR �������1 ( LAMBDA (x y) (EVAL (�������1+ (CONS (CAR x) (���� (CDR x))) y (������ (CDR x))))) )
(SEXPR �������1+ ( LAMBDA (x y z) (������� x y)) )
(SEXPR f1 ( LAMBDA (x y z) (+ x (* y z))) )
(SEXPR f2 ( LAMBDA (x y z) (SQR (+ (* 2 x y) (* z z)))) )
(SEXPR f3 ( LAMBDA (x y z) (- x (* y z))) )
(SEXPR f4 ( LAMBDA (x y z) (- (/ x y) (* 0.5 y z))) )
(SEXPR f5 ( LAMBDA (x y z) (SQR (- (* x x) (* 2 y z)))) )
(SEXPR f6 ( LAMBDA (x y z) (+ (* x y) (* 0.5 y y z))) )
(SEXPR f7 ( LAMBDA (x y z) (/ (- (* x x) (* y y)) (* 2 z))) )
(SEXPR f8 ( LAMBDA (x y z) (/ (- x y) z)) )
(SEXPR f9 ( LAMBDA (x y z) (/ (- (SQR (+ (* x x) (* 2 y z))) x) y)) )
(SEXPR f10 ( LAMBDA (x y z) (/ (* 2 (- x (* y z))) (* z z))) )
(SEXPR f11 ( LAMBDA (x y) (SQR (/ (* 2 x) y))) )
(SEXPR f12 ( LAMBDA (x y) (/ x y)) )
(SEXPR f13 ( LAMBDA (x y z) (/ (/ x y) z)) )
(SEXPR f14 ( LAMBDA (x y) (/ (* 2 x) (* y y))) )
(SEXPR f15 ( LAMBDA (x y z) (* x y z)) )
(SEXPR f16 ( LAMBDA (x y) (* 0.5 x y y)) )
(SEXPR f17 ( LAMBDA (x y z) (+ (- x y) z)) )
(SEXPR f18 ( LAMBDA (x y) (* x y)) )
//
// ���������
//
(CSETQ _Pi 3.14159265358979323846)
(CSETQ _E 2.7182818284590452354)
(CSETQ _LPAR "(")
(CSETQ _RPAR ")")
(CSETQ _INPUT 0)
(CSETQ _OUTPUT 1)
(CSETQ _APPEND 2)
(CSETQ _BINARY_READ 3)
(CSETQ _BINARY_WRITE 4)
(CSETQ _BINARY_READ_WRITE 5)
(CSETQ _TEXT_ARRAY -1)
(CSETQ _WHITE &HFFFFFF)
(CSETQ _BLACK &H000000)
(CSETQ _RED &HFF0000)
(CSETQ _LIME &H00FF00)
(CSETQ _BLUE &H0000FF)
(CSETQ _YELLOW &HFFFF00)
(CSETQ _AQUA &H00FFFF)
(CSETQ _FUCHSIA &HFF00FF)
(CSETQ _GREEN &H008000)
(CSETQ _SILVER &HC0C0C0)
(CSETQ _GRAY &H808080)
(CSETQ _MAROON &H800000)
(CSETQ _OLIVE &H808000)
(CSETQ _NAVY &H000080)
(CSETQ _PURPLE &H800080)
(CSETQ _TEAL &H008080)
(CSETQ _LABEL 1)
(CSETQ _TEXT 2)
(CSETQ _LIST 3)
(CSETQ _COMBO 4)
(CSETQ _BUTTON 5)
(CSETQ _CHECK 6)
(CSETQ _OPTION 7)
(CSETQ _CENTER 2)
(CSETQ _LEFT 0)
(CSETQ _RIGHT 1)
(CSETQ HKEY_CLASSES_ROOT &H80000000)
(CSETQ HKEY_CURRENT_CONFIG &H80000005)
(CSETQ HKEY_CURRENT_USER &H80000001)
(CSETQ HKEY_DYN_DATA &H80000006)
(CSETQ HKEY_LOCAL_MACHINE &H80000002)
(CSETQ HKEY_PERFORMANCE_DATA &H80000004)
(CSETQ HKEY_USERS &H80000003)
(CSETQ KEY_ALL_ACCESS &HF003F)
(CSETQ KEY_CREATE_LINK &H20)
(CSETQ KEY_CREATE_SUB_KEY &H4)
(CSETQ KEY_ENUMERATE_SUB_KEYS &H8)
(CSETQ KEY_EXECUTE &H20019)
(CSETQ KEY_NOTIFY &H10)
(CSETQ KEY_QUERY_VALUE &H1)
(CSETQ KEY_READ &H20019)
(CSETQ KEY_SET_VALUE &H2)
(CSETQ KEY_WRITE &H20006)
(CSETQ REG_BINARY 3)
(CSETQ REG_DWORD 4)
(CSETQ REG_DWORD_BIG_ENDIAN 5)
(CSETQ REG_DWORD_LITTLE_ENDIAN 4)
(CSETQ REG_EXPAND_SZ 2)
(CSETQ REG_LINK 6)
(CSETQ REG_MULTI_SZ 7)
(CSETQ REG_NONE 0)
(CSETQ REG_RESOURCE_LIST 8)
(CSETQ REG_SZ 1)
//
// ���������� ����������
//
(SETQ �������� '(((f1 3) (f2 3) (f3 3) (f4 3) (f5 3) (f6 3) (f7 3) (f8 3) (f9 3) (f10 3) (f11 2) (f12 2) (f13 3) (f14 2) (f15 3) (f16 2) (f17 3) (f18 2)) ((<Spd2> (f1 <Spd1> <Accl> <Time>) (f2 <Accl> <Dist> <Spd1>) (f11 <KinEn2> <Mass>)) (<Spd1> (f3 <Spd2> <Accl> <Time>) (f4 <Dist> <Time> <Accl>) (f5 <Spd2> <Accl> <Dist>) (f11 <KinEn1> <Mass>)) (<Dist> (f6 <Spd1> <Time> <Accl>) (f7 <Spd2> <Spd1> <Accl>) (f12 <OpPull> <FrPull>) (f12 <OpFric> <FrFric>)) (<Time> (f8 <Spd2> <Spd1> <Accl>) (f9 <Spd1> <Accl> <Dist>)) (<Accl> (f8 <Spd2> <Spd1> <Time>) (f10 <Dist> <Spd1> <Time>) (f7 <Spd2> <Spd1> <Dist>) (f8 <FrPull> <FrFric> <Mass>)) (<Mass> (f8 <FrPull> <FrFric> <Accl>) (f13 <FrFric> <CfFric> <Cfg>) (f14 <KinEn2> <Spd2>) (f14 <KinEn1> <Spd1>)) (<FrPull> (f1 <FrFric> <Mass> <Accl>) (f12 <OpPull> <Dist>)) (<FrFric> (f3 <FrPull> <Mass> <Accl>) (f15 <Cfg> <CfFric> <Mass>) (f12 <OpFric> <Dist>)) (<CfFric> (f13 <FrFric> <Mass> <Cfg>)) (<KinEn2> (f16 <Mass> <Spd2>) (f17 <OpPull> <OpFric> <KinEn1>)) (<KinEn1> (f16 <Mass> <Spd1>) (f17 <KinEn2> <OpPull> <OpFric>)) (<OpPull> (f18 <FrPull> <Dist>) (f17 <KinEn2> <KinEn1> <OpFric>)) (<OpFric> (f18 <FrFric> <Dist>) (f17 <OpPull> <KinEn2> <KinEn1>)))))
