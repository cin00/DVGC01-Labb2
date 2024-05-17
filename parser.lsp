;;=====================================================================
;; LISP READER & LEXER - new version 161202
;;=====================================================================

;;=====================================================================
;; Help functions
;;=====================================================================
;; ctos         convert a character to a string
;; str_con      concatenate 2 strings str, c
;; whitespace   is c whitespace?
;;=====================================================================

(defun ctos (c)        (make-string 1 :initial-element c))
(defun str-con (str c) (concatenate 'string str (ctos c)))
(defun whitespace (c)  (member c '(#\Space #\Tab #\Newline)))

;;=====================================================================
;; get-wspace   remove whitespace
;;=====================================================================

(defun get-wspace (ip)
   (setf c (read-char ip nil 'EOF))
   (cond   
           ((whitespace c)  (get-wspace ip))
           (t                             c)
   )
)

;;=====================================================================
;; Read an Identifier         Compare this with C's do-while construct
;;=====================================================================

(defun get-name (ip lexeme c)
   (setf lexeme (str-con lexeme c))
   (setf c      (read-char ip nil 'EOF))
   (cond        
                ((alphanumericp c)  (get-name ip lexeme c))
                (t                  (list        c lexeme))
   )
)

;;=====================================================================
;; Read a Number              Compare this with C's do-while construct
;;=====================================================================

(defun get-number (ip lexeme c)
   (setf lexeme (str-con lexeme c))
   (setf c      (read-char ip nil 'EOF))
   (cond
         ((not (null (digit-char-p c)))  (get-number ip lexeme c))
         (t                              (list          c lexeme))
   )
  )

;;=====================================================================
;; Read a single character or ":="
;;=====================================================================

(defun get-symbol (ip lexeme c)
   (setf lexeme (str-con lexeme c))
   (setf c1 c)
   (setf c (read-char ip nil 'EOF))
   (cond
         ((and (char= c1 #\:) (char= c #\=))  (get-symbol ip lexeme c))
         (t                                   (list          c lexeme))
   )
)

;;=====================================================================
;; Read a Lexeme                       lexeme is an "accumulator"
;;                                     Compare this with the C version
;;=====================================================================

(defun get-lex (state)
   (setf lexeme "")
   (setf ip (pstate-stream   state))
   (setf c  (pstate-nextchar state))
   (if (whitespace c) (setf c (get-wspace ip)))
   (cond
         ((eq c 'EOF)                     (list 'EOF ""))
         ((alpha-char-p c)                (get-name   ip lexeme c))
         ((not (null (digit-char-p c)))   (get-number ip lexeme c))
         (t                               (get-symbol ip lexeme c))
   )
)

;;=====================================================================
; map-lexeme(lexeme) returns a list: (token, lexeme)
;;=====================================================================

(defun map-lexeme (lexeme)
(format t "Symbol: ~S ~%" lexeme)
   (list (cond
        ((string=   lexeme "program")  'PROGRAM )
        ((string=   lexeme "input"  )  'INPUT   )
        ((string=   lexeme "output" )  'OUTPUT  )
        ((string=   lexeme "var"    )  'VAR     )
        ((string=   lexeme "begin"  )  'BEGIN   )
        ((string=   lexeme "end"    )  'END     )
        ((string=   lexeme "boolean")  'BOOL    )
        ((string=   lexeme "integer")  'INTEGER )
        ((string=   lexeme "real"   )  'REAL    )    
        ((string=   lexeme "("      )  'LP      )
        ((string=   lexeme ")"      )  'RP      )
        ((string=   lexeme ":="     )  'ASSIGN  )
        ((string=   lexeme ":"      )  'COLON   )
        ((string=   lexeme ","      )  'COMMA   )
        ((string=   lexeme "+"      )  'PLUS    )
        ((string=   lexeme "*"      )  'MULT    )        
        ((string=   lexeme ";"      )  'SCOLON  )
        ((string=   lexeme "."      )  'ENDDOT  )
        ((string=   lexeme ""       )  'EOF     )
        ((is-id     lexeme          )  'ID      )
        ((is-number lexeme          )  'NUM     )
        (t                             'UNKNOWN )
         )
    lexeme)
)

;;=====================================================================
; ID is [A-Z,a-z][A-Z,a-z,0-9]*          number is [0-9][0-9]*
;;=====================================================================

;; If first symbol is letter and rest alfanumerical -> true
(defun is-id (str)
    (and (alpha-char-p (char str 0)) 
         (every (function alphanumericp) str)
    )
)

(defun is-number (str)
    (every (function digit-char-p) str)
)

;;=====================================================================
; THIS IS THE PARSER PART
;;=====================================================================

;;=====================================================================
; Create a stucture - parse state descriptor
;;=====================================================================
; lookahead is the list (token, lexeme)
; stream    is the input filestream
; nextchar  is the char following the last lexeme read
; status    is the parse status (OK, NOTOK)
; symtab    is the symbol table
;;=====================================================================

(defstruct pstate
    (lookahead)
    (stream)
    (nextchar)
    (status)
    (symtab)
)

;;=====================================================================
; Constructor for the structure pstate - initialise
; stream to the input file stream (ip)
;;=====================================================================

(defun create-parser-state (ip)
   (make-pstate
      :stream        ip
      :lookahead     ()
      :nextchar      #\Space
      :status        'OK
      :symtab        ()
    )
)

;;=====================================================================
; SYMBOL TABLE MANIPULATION
;;=====================================================================

;;=====================================================================
; token  - returns the token  from (token lexeme)(reader)
; lexeme - returns the lexeme from (token lexeme)(reader)
;;=====================================================================

(defun token  (state)
    (first (pstate-lookahead state))
)
(defun lexeme (state)
    (second (pstate-lookahead state))
)

;;=====================================================================
; symbol table manipulation: add + lookup + display
;;=====================================================================

(defun symtab-add (state id)
    ;;If id not in symtab -> if empty initialize, else -> cons id
    ;;Cons because it's faster(?), this will show the symbol table as "reversed" in the output compared 
    ;;to the given output. Change to append if that's unnecessarily necessary!
    (unless (symtab-member state id)
        (if (null (pstate-symtab state))
            (setf (pstate-symtab state) (list id))
            (setf (pstate-symtab state) (cons id (pstate-symtab state)))
        )
    )
)

(defun symtab-member (state id)
    ;;Check if id is in symtab
    (member id (pstate-symtab state) :test (function string-equal))
)

(defun symtab-display (state)
   (format t "------------------------------------------------------~%")
   (format t "Symbol Table is: ~S ~%" (pstate-symtab state))
   (format t "------------------------------------------------------~%")
)

;;=====================================================================
; Error functions: Syntax & Semantic
;;=====================================================================

(defun synerr1 (state symbol)
    (format t "*** Syntax error:   Expected ~8S found ~8S ~%"
           symbol (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

(defun synerr2 (state)
    (format t "*** Syntax error:   Expected TYPE     found ~S ~%"
           (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

(defun synerr3 (state)
    (format t "*** Syntax error:   Expected OPERAND  found ~S ~%"
           (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

(defun semerr1 (state)
    (format t "*** Semantic error: ~S already declared.~%"
                (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

(defun semerr2 (state)
    (format t "*** Semantic error: ~S not declared.~%"
          (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

(defun semerr3 (state)
    (format t "*** Semantic error: found ~8S expected EOF.~%"
          (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

;;=====================================================================
; The return value from get-token is always a list. (token lexeme)
;;=====================================================================

(defun get-token (state)
  (let    ((result (get-lex state)))
    (setf (pstate-nextchar  state) (first result))
    (setf (pstate-lookahead state) (map-lexeme (second result)))
  )
 )

;;=====================================================================
; match compares lookahead with symbol (the expected token)
; if symbol == lookahead token ==> get next token else Syntax error
;;=====================================================================

(defun match (state symbol)
   (if (eq symbol (token state))
       (get-token  state)
       (synerr1    state symbol)
       )
)

;;=====================================================================
; THE GRAMMAR RULES
;;=====================================================================

;;=====================================================================
; <stat-part>     --> begin <stat-list> end .
; <stat-list>     --> <stat> | <stat> ; <stat-list>
; <stat>          --> <assign-stat>
; <assign-stat>   --> id := <expr>
; <expr>          --> <term>     | <term> + <expr>
; <term>          --> <factor>   | <factor> * <term>
; <factor>        --> ( <expr> ) | <operand>
; <operand>       --> id | number
;;=====================================================================

(defun operand (state)
    (let ((tok (token state)))
        (cond
        ((eq tok 'ID)
            (when (not (symtab-member state (lexeme state)))
                (semerr2 state)
            )
            (match state 'ID)
        )
        ((eq tok 'NUM) 
            (match state 'NUM)
        )
        (t (synerr3 state))
        )
    )
)

(defun factor (state)
    (if (eq (token state) 'LP)
    (progn
        (match state 'LP)
        (expr state)
        (match state 'RP)
    )
    (operand state)
    )
)

(defun term (state)
    (factor state)
    (when (eq (token state) 'MULT)
        (match state 'MULT)
        (term state)
    )
)

(defun expr (state)
    (term state)
    (when (eq (token state) 'PLUS)
        (match state 'PLUS)
        (expr state)
    )
)

(defun assign-stat (state)
    (when (and
        (not (symtab-member state (lexeme state)))
        (eq (token state) 'ID))
    (semerr2 state)
    )
    (match state 'ID)
    (match state 'ASSIGN)
    (expr state)
)

(defun stat (state)
    (assign-stat state)
)

(defun stat-list (state)
    (stat state)
    (when (eq (token state) 'SCOLON)
        (match state 'SCOLON)
        (stat-list state)
    )
)

(defun stat-part (state)
    (match state 'BEGIN)
    (stat-list state)
    (match state 'END)
    (match state 'ENDDOT)
)

;;=====================================================================
; <var-part>     --> var <var-dec-list>
; <var-dec-list> --> <var-dec> | <var-dec><var-dec-list>
; <var-dec>      --> <id-list> : <type> ;
; <id-list>      --> id | id , <id-list>
; <type>         --> integer | real | boolean
;;=====================================================================

(defun type (state)
    (let ((tok (token state)))
        (cond
            ((eq tok 'INTEGER) (match state 'INTEGER))
            ((eq tok 'REAL) (match state 'REAL))
            ((eq tok 'BOOLEAN) (match state 'BOOLEAN))
            (t (synerr2 state))
        )
    )
)

(defun id-list (state)
    (if (eq (token state) 'ID)
        (progn
            (if (not (symtab-member state (lexeme state)))
                (symtab-add state (lexeme state))
                (semerr1 state)
            )
            (match state 'ID)
        )
        (synerr1 state 'ID)
    )
    (if (eq (token state) 'COMMA)
        (progn
            (match state 'COMMA)
            (id-list state)
        )
    )
)

(defun var-dec (state)
    (id-list state)
    (match state 'COLON)
    (type state)
    (match state 'SCOLON)
)

(defun var-dec-list (state)
    (var-dec state)
    (if (eq (token state) 'ID)
        (var-dec-list state)
    )
)

(defun var-part (state)
    (match state 'VAR)
    (var-dec-list state)
)

;;=====================================================================
; <program-header>
;;=====================================================================

(defun program-header (state)
    (match state 'PROGRAM)
    (match state 'ID)
    (match state 'LP)
    (match state 'INPUT)
    (match state 'COMMA)
    (match state 'OUTPUT)
    (match state 'RP)
    (match state 'SCOLON)
)

;;=====================================================================
; <program> --> <program-header><var-part><stat-part>
;;=====================================================================
(defun program (state)
   (program-header state)
   (var-part       state)
   (stat-part      state)
)

;;=====================================================================
; THE PARSER - parse a file
;;=====================================================================

(defun check-end (state)
    (when (not (eq (token state) 'EOF))
        (semerr3 state)
        (get-token state)
        (check-end state)
    )
)

;;=====================================================================
; Test parser for file name input
;;=====================================================================

(defun parse (filename)
   (format t "~%------------------------------------------------------")
   (format t "~%--- Parsing program: ~S " filename)
   (format t "~%------------------------------------------------------~%")
   (with-open-file (ip (open filename) :direction :input)
      (setf state (create-parser-state ip))
      (setf (pstate-nextchar state) (read-char ip nil 'EOF))
      (get-token      state)
      (program        state)
      (check-end      state)
      (symtab-display state)
      )
   (if (eq (pstate-status state) 'OK)
      (format t "Parse Successful. ~%")
      (format t "Parse Fail. ~%")
      )
   (format t "------------------------------------------------------~%")
)

;;=====================================================================
; THE PARSER - parse all the test files
;;=====================================================================

;; "Dynamic solution" to parse all the files. Unfortunately I had to hardcode the filenames to compare with the given correct output
; ===================================================================================================================================
;(defun parse-all ()
;    (let ((test-files (directory "testfiles/*.pas")))
;        (mapcar (lambda (file)
;            (parse (namestring file)))
;                test-files))
;)

(defvar testingfiles 
    '(
    "testa" "testb" "testc" "testd" "teste" "testf" "testg" "testh" "testi" "testj"
    "testk" "testl" "testm" "testn" "testo" "testp" "testq" "testr" "tests" "testt"
    "testu" "testv" "testw" "testx" "testy" "testz" "testok1" "testok2" "testok3"
    "testok4" "testok5" "testok6" "testok7" "fun1" "fun2" "fun3" "fun4" "fun5"
    "sem1" "sem2" "sem3" "sem4" "sem5"
    )
)

;;Iterates over the testfiles and concatenates .pas to them
(defun parse-all ()
    (mapcar (function parse)
        (mapcar (lambda (file) 
            (concatenate 'string "testfiles/" file ".pas")) testingfiles
        )
    )


)




;;=====================================================================
; THE PARSER - test all files
;;=====================================================================

(parse-all)

;;=====================================================================
; THE PARSER - test a single file
;;=====================================================================

;;(parse "testfiles/testok1.pas")

;;=====================================================================
; THE PARSER - end of code
;;=====================================================================


