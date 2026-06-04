
;;; ============================================================
;;; G6 (patched)
;;; Features:
;;;  - Draw chained LINE segments at fixed angles with typed lengths
;;;  - Optional break markers as two LINE bars (||) or two copies of a selected object, size adjustable
;;;    with live preview and applies to selected lines (persisted).
;;;  - Continue/branch-from point: press C while in length input to pick a connected start point without ending.
;;;  - Dimensions with satisfaction loop (re-pick offset distance).
;;;  - Layer assignment popup + per-layer suffix saved; moves lines + dims + breaks.
;;;  - Connected LINE networks stay attached when joints move.
;;; ============================================================

(defun g6:deg->rad (d) (* pi (/ d 180.0)))
(defun g6:rad->deg (r) (* 180.0 (/ r pi)))

(defun g6:fmtLen (v / prec s)
  (setq prec (getvar "LUPREC"))
  (if (equal v (fix v) 1e-8)
    (rtos v 2 0)
    (progn
      (setq s (rtos v 2 prec))
      (while (and (> (strlen s) 0) (= (substr s (strlen s) 1) "0"))
        (setq s (substr s 1 (1- (strlen s))))
      )
      (if (and (> (strlen s) 0) (= (substr s (strlen s) 1) "."))
        (setq s (substr s 1 (1- (strlen s))))
      )
      s
    )
  )
)

(defun g6:list-set (lst idx val / i res)
  (setq i 0 res '())
  (while lst
    (setq res (cons (if (= i idx) val (car lst)) res))
    (setq lst (cdr lst) i (1+ i))
  )
  (reverse res)
)

(defun g6:index-of (item lst / i)
  (setq i 0)
  (while (and lst (not (eq item (car lst))))
    (setq lst (cdr lst) i (1+ i))
  )
  (if lst i nil)
)
(defun g6:resolveLayerSourceLine (eSel entList breakMap / src pair tailEnt)
  (if (not (null (g6:index-of eSel entList)))
    eSel
    (progn
      (setq src nil)
      (while (and breakMap (null src))
        (setq pair (car breakMap)
              tailEnt (nth 2 (cdr pair)))
        ;; Tail lines are visible continuations of their source; marker helpers are ignored.
        (if (eq eSel tailEnt) (setq src (car pair)))
        (setq breakMap (cdr breakMap))
      )
      src
    )
  )
)


;;; ------------------------------------------------------------
;;; basic point/vector helpers
;;; ------------------------------------------------------------
(defun g6:pt3 (p) (list (car p) (cadr p) (if (caddr p) (caddr p) 0.0)))
(defun g6:pt+ (p v) (mapcar '+ (g6:pt3 p) (g6:pt3 v)))
(defun g6:pt- (p q) (mapcar '- (g6:pt3 p) (g6:pt3 q)))
(defun g6:ptNear (p q tol) (< (distance (g6:pt3 p) (g6:pt3 q)) tol))
(defun g6:vecNear (v1 v2 tol) (< (distance (g6:pt3 v1) (g6:pt3 v2)) tol))

(defun g6:translateEntity (e vec / ss base to)
  ;; Native MOVE supports any normal AutoCAD entity that can be selected.
  (if (and e (entget e))
    (progn
      (setq ss (ssadd e)
            base '(0.0 0.0 0.0)
            to (g6:pt3 vec))
      (if ss (command "_.MOVE" ss "" base to))
    )
  )
)

(defun g6:translateLine (e vec / ed p1 p2)
  (if (and e (entget e))
    (progn
      (setq ed (entget e)
            p1 (cdr (assoc 10 ed))
            p2 (cdr (assoc 11 ed)))
      (if (and p1 p2)
        (progn
          (setq ed (subst (cons 10 (g6:pt+ p1 vec)) (assoc 10 ed) ed))
          (setq ed (subst (cons 11 (g6:pt+ p2 vec)) (assoc 11 ed) ed))
          (entmod ed)
          (entupd e)
        )
      )
    )
  )
)

;;; ------------------------------------------------------------
;;; ENV helpers (reals)
;;; ------------------------------------------------------------
(defun g6:getEnvReal (key def / s v)
  (setq s (getenv key))
  (if (and s (/= s ""))
    (progn
      (setq v (atof s))
      (if (> v 0.0) v def)
    )
    def
  )
)

(defun g6:setEnvReal (key val)
  (setenv key (rtos val 2 6))
)

;;; ------------------------------------------------------------
;;; Break parameters and live preview
;;; params = (gapW barH mode templateEnt templateBasePt)
;;; ------------------------------------------------------------
(defun g6:getBreakMode ( / mode)
  (setq mode (getenv "G6_BRK_MODE"))
  (if mode (setq mode (strcase mode)))
  (if (member mode '("BARS" "SELECT")) mode "BARS")
)

(defun g6:setBreakMode (mode)
  (if (member mode '("BARS" "SELECT")) (setenv "G6_BRK_MODE" mode))
)

(defun g6:breakParamsDefault ( / gapW barH mode)
  (setq gapW (g6:getEnvReal "G6_BRK_GAPW" 0.12)
        barH (g6:getEnvReal "G6_BRK_BARH" 0.10)
        mode (g6:getBreakMode))
  ;; Selected entity names are drawing/session-specific, so only the mode is persisted.
  (list gapW barH mode nil nil)
)
(defun g6:dimensionEligibleP (userLen)
  (and userLen (>= userLen 150.0))
)

(defun g6:drawPreviewSegs (segs col)
  (foreach s segs (grdraw (car s) (cadr s) col))
)

(defun g6:getDimSpanEnds (lineEnt breakMap / ed p1 p2 brkPair tailEnt tailEd)
  (if (and lineEnt (entget lineEnt))
    (progn
      (setq ed (entget lineEnt)
            p1 (cdr (assoc 10 ed))
            p2 (cdr (assoc 11 ed))
            brkPair (assoc lineEnt breakMap))
      (if brkPair
        (progn
          (setq tailEnt (nth 2 (cdr brkPair)))
          (if (and tailEnt (entget tailEnt))
            (progn
              (setq tailEd (entget tailEnt))
              (if (assoc 11 tailEd)
                (setq p2 (cdr (assoc 11 tailEd)))
              )
            )
          )
        )
      )
      (if (and p1 p2) (list p1 p2) nil)
    )
  )
)

(defun g6:buildDimPreviewSegs (eligible breakMap off / segs span p1 p2 mid ang nAng p1off p2off)
  (setq segs '())
  (if (and off (> off 0.0))
    (foreach e eligible
      (setq span (g6:getDimSpanEnds e breakMap))
      (if span
        (progn
          (setq p1 (car span)
                p2 (cadr span)
                mid (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2)
                ang (angle p1 p2)
                nAng (+ ang (/ pi 2.0))
                p1off (polar p1 nAng off)
                p2off (polar p2 nAng off))
          (setq segs (append segs (list (list p1 p1off) (list p2 p2off) (list p1off p2off))))
        )
      )
    )
  )
  segs
)

(defun g6:firstDimRef (eligible breakMap / span p1 p2 mid ang nAng)
  (while (and eligible (null span))
    (setq span (g6:getDimSpanEnds (car eligible) breakMap))
    (if (null span) (setq eligible (cdr eligible)))
  )
  (if span
    (progn
      (setq p1 (car span)
            p2 (cadr span)
            mid (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2)
            ang (angle p1 p2)
            nAng (+ ang (/ pi 2.0)))
      (list mid (list (cos nAng) (sin nAng) 0.0))
    )
  )
)

(defun g6:pickDimOffsetPreview (eligible breakMap / done gr typ dat typed ref refMid refPerp vec off cand oldSegs)
  ;; Finish any ordinary command before grread consumes dimension-offset input.
  (while (= 1 (logand 1 (getvar "CMDACTIVE"))) (command))
  (setq done nil off nil typed "" oldSegs '())
  (setq ref (g6:firstDimRef eligible breakMap)
        refMid (if ref (car ref) nil)
        refPerp (if ref (cadr ref) nil))
  (prompt "\nDimension offset (move mouse / type / click, ESC = none): ")
  (while (not done)
    ;; Include the all-keys bit so unsupported keys are consumed here, not by AutoCAD.
    (setq gr (grread T 15 0)
          typ (car gr)
          dat (cadr gr))
    (cond
      ((= typ 5)
        (if (and refMid refPerp (= (strlen typed) 0) dat (listp dat))
          (progn
            (setq vec (g6:pt- dat refMid)
                  off (abs (+ (* (car vec) (car refPerp)) (* (cadr vec) (cadr refPerp)))))
            (g6:drawPreviewSegs oldSegs 0)
            (setq oldSegs (g6:buildDimPreviewSegs eligible breakMap off))
            (g6:drawPreviewSegs oldSegs 1)
          )
        )
      )
      ((= typ 3)
        (if (> (strlen typed) 0)
          (setq off (abs (atof typed)))
          (if (and refMid refPerp dat (listp dat))
            (progn
              (setq vec (g6:pt- dat refMid)
                    off (abs (+ (* (car vec) (car refPerp)) (* (cadr vec) (cadr refPerp)))))
            )
          )
        )
        (setq done T)
      )
      ((= typ 2)
        (cond
          ((= dat 27)
            (setq off nil done T)
          )
          ((= dat 13)
            (if (> (strlen typed) 0)
              (setq off (abs (atof typed)))
            )
            (setq done T)
          )
          ((= dat 8)
            (if (> (strlen typed) 0)
              (setq typed (substr typed 1 (1- (strlen typed))))
            )
            (if (> (strlen typed) 0)
              (setq off (abs (atof typed)))
              (setq off nil)
            )
            (g6:drawPreviewSegs oldSegs 0)
            (setq oldSegs (g6:buildDimPreviewSegs eligible breakMap off))
            (g6:drawPreviewSegs oldSegs 1)
          )
          ((or (and (>= dat 48) (<= dat 57)) (= dat 46))
            (setq typed (strcat typed (chr dat))
                  off (abs (atof typed)))
            (g6:drawPreviewSegs oldSegs 0)
            (setq oldSegs (g6:buildDimPreviewSegs eligible breakMap off))
            (g6:drawPreviewSegs oldSegs 1)
          )
        )
      )
    )
  )
  (g6:drawPreviewSegs oldSegs 0)
  (if (and off (> off 0.0))
    (max off 1e-6)
    nil
  )
)

(defun g6:previewBreakSegs (mid ang gapW barH / ux uy vx vy b1 b2 t1 t2 segs)
  (setq ux (cos ang) uy (sin ang))
  (setq vx (- (sin ang)) vy (cos ang))
  (setq b1 (list (+ (car mid) (* ux (- (/ gapW 2.0)))) (+ (cadr mid) (* uy (- (/ gapW 2.0)))) 0.0))
  (setq b2 (list (+ (car mid) (* ux (/ gapW 2.0))) (+ (cadr mid) (* uy (/ gapW 2.0))) 0.0))
  (setq t1 (list (+ (car b1) (* vx (- (/ barH 2.0)))) (+ (cadr b1) (* vy (- (/ barH 2.0)))) 0.0))
  (setq t2 (list (+ (car b1) (* vx (/ barH 2.0))) (+ (cadr b1) (* vy (/ barH 2.0))) 0.0))
  (setq segs (list (list t1 t2)))
  (setq t1 (list (+ (car b2) (* vx (- (/ barH 2.0)))) (+ (cadr b2) (* vy (- (/ barH 2.0)))) 0.0))
  (setq t2 (list (+ (car b2) (* vx (/ barH 2.0))) (+ (cadr b2) (* vy (/ barH 2.0))) 0.0))
  (setq segs (append segs (list (list t1 t2))))
  segs
)

(defun g6:pickBreakOneValue (msg mid ang gapW barH mode / oldSegs done val typed gr typ dat vec dotv)
  (setq val (if (= mode "GAP") gapW barH))
  (setq typed "")
  (setq oldSegs (g6:previewBreakSegs mid ang gapW barH))
  (g6:drawPreviewSegs oldSegs 1)
  (prompt msg)
  (setq done nil)
  (while (not done)
    (setq gr (grread T 13 0) typ (car gr) dat (cadr gr))
    (cond
      ((= typ 5)
        (if (and dat (listp dat))
          (progn
            (setq vec (g6:pt- dat mid))
            (if (= mode "GAP")
              (setq dotv (+ (* (car vec) (cos ang)) (* (cadr vec) (sin ang))))
              (setq dotv (+ (* (car vec) (- (sin ang))) (* (cadr vec) (cos ang))))
            )
            (setq val (max 0.001 (* 2.0 (abs dotv))))
            (if (= mode "GAP") (setq gapW val) (setq barH val))
            (g6:drawPreviewSegs oldSegs 0)
            (setq oldSegs (g6:previewBreakSegs mid ang gapW barH))
            (g6:drawPreviewSegs oldSegs 1)
          )
        )
      )
      ((= typ 3) (setq done T))
      ((= typ 2)
        (cond
          ((= dat 13) (setq done T))
          ((= dat 27) (setq val nil done T))
          ((= dat 8)
            (if (> (strlen typed) 0)
              (setq typed (substr typed 1 (1- (strlen typed))))
            )
            (if (> (strlen typed) 0)
              (progn
                (setq val (max 0.001 (atof typed)))
                (if (= mode "GAP") (setq gapW val) (setq barH val))
                (g6:drawPreviewSegs oldSegs 0)
                (setq oldSegs (g6:previewBreakSegs mid ang gapW barH))
                (g6:drawPreviewSegs oldSegs 1)
              )
            )
          )
          ((or (and (>= dat 48) (<= dat 57)) (= dat 46))
            (setq typed (strcat typed (chr dat)))
            (setq val (max 0.001 (atof typed)))
            (if (= mode "GAP") (setq gapW val) (setq barH val))
            (g6:drawPreviewSegs oldSegs 0)
            (setq oldSegs (g6:previewBreakSegs mid ang gapW barH))
            (g6:drawPreviewSegs oldSegs 1)
          )
        )
      )
    )
  )
  (g6:drawPreviewSegs oldSegs 0)
  val
)

(defun g6:selectBreakerTemplate ( / sel ent basePt)
  (setq sel (entsel "\nSelect breaker marker object: "))
  (if sel
    (progn
      (setq ent (car sel)
            basePt (cadr sel))
      (if (and ent basePt (entget ent))
        (list ent (g6:pt3 basePt))
      )
    )
  )
)

(defun g6:pickBreakParams (sampleEnt params / ed p1 p2 mid ang gapW barH mode templateEnt templateBase modeAns templateParams v ans ok)
  (setq ed (entget sampleEnt)
        p1 (cdr (assoc 10 ed))
        p2 (cdr (assoc 11 ed))
        gapW (nth 0 params)
        barH (nth 1 params)
        mode (nth 2 params)
        templateEnt (nth 3 params)
        templateBase (nth 4 params)
        ok nil)
  (if (not (member mode '("BARS" "SELECT"))) (setq mode "BARS"))
  (if (and (= mode "SELECT") (or (null templateEnt) (null (entget templateEnt)) (null templateBase)))
    (setq templateEnt nil templateBase nil))
  (if (and p1 p2)
    (progn
      (initget "Bars Select")
      (setq modeAns (getkword (strcat "\nBreaker marker [Bars/Select] <" (if (= mode "SELECT") "Select" "Bars") ">: ")))
      (if modeAns (setq mode (strcase modeAns)))
      (if (= mode "SELECT")
        (progn
          (setq templateParams (g6:selectBreakerTemplate))
          (if templateParams
            (setq templateEnt (nth 0 templateParams)
                  templateBase (nth 1 templateParams))
            (progn
              (prompt "\nNo marker object selected; using Bars mode.")
              (setq mode "BARS")
            )
          )
        )
      )
      (setq mid (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2)
            ang (angle p1 p2))
      (while (not ok)
        (setq v (g6:pickBreakOneValue "\nPick breaker width (gapW): move/click or type value, Enter=accept, Esc=cancel. " mid ang gapW barH "GAP"))
        (if (null v) (setq ok 'CANCEL) (setq gapW v))
        (if (not (eq ok 'CANCEL))
          (progn
            (setq v (g6:pickBreakOneValue "\nPick breaker height (barH): move/click or type value, Enter=accept, Esc=cancel. " mid ang gapW barH "BAR"))
            (if (null v) (setq ok 'CANCEL) (setq barH v))
          )
        )
        (if (eq ok 'CANCEL)
          (setq ok T params nil)
          (progn
            (initget "Yes No")
            (setq ans (getkword "\nBreaker size OK? [Yes/No] <Yes>: "))
            (if (or (null ans) (= ans "Yes"))
              (setq ok T params (list gapW barH mode templateEnt templateBase))
            )
          )
        )
      )
    )
  )
  (if params
    (progn
      (g6:setEnvReal "G6_BRK_GAPW" (nth 0 params))
      (g6:setEnvReal "G6_BRK_BARH" (nth 1 params))
      (g6:setBreakMode (nth 2 params))
    )
  )
  params
)

(defun g6:createBreakerBars (lineEnt gapW barH / ed p1 p2 mid ang ux uy vx vy c1 c2 s1 e1 s2 e2 lay b1 b2)
  (setq ed (entget lineEnt)
        p1 (cdr (assoc 10 ed))
        p2 (cdr (assoc 11 ed))
        lay (cdr (assoc 8 ed)))
  (if (null lay) (setq lay "0"))
  (if (and p1 p2)
    (progn
      (setq mid (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2)
            ang (angle p1 p2)
            ux (cos ang)
            uy (sin ang)
            vx (- (sin ang))
            vy (cos ang))
      (setq c1 (list (+ (car mid) (* ux (- (/ gapW 2.0)))) (+ (cadr mid) (* uy (- (/ gapW 2.0)))) 0.0)
            c2 (list (+ (car mid) (* ux (/ gapW 2.0))) (+ (cadr mid) (* uy (/ gapW 2.0))) 0.0))
      (setq s1 (list (+ (car c1) (* vx (- (/ barH 2.0)))) (+ (cadr c1) (* vy (- (/ barH 2.0)))) 0.0)
            e1 (list (+ (car c1) (* vx (/ barH 2.0))) (+ (cadr c1) (* vy (/ barH 2.0))) 0.0)
            s2 (list (+ (car c2) (* vx (- (/ barH 2.0)))) (+ (cadr c2) (* vy (- (/ barH 2.0)))) 0.0)
            e2 (list (+ (car c2) (* vx (/ barH 2.0))) (+ (cadr c2) (* vy (/ barH 2.0))) 0.0))
      (setq b1 (entmakex (list (cons 0 "LINE") (cons 8 lay) (cons 10 s1) (cons 11 e1))))
      (setq b2 (entmakex (list (cons 0 "LINE") (cons 8 lay) (cons 10 s2) (cons 11 e2))))
      (list b1 b2)
    )
  )
)

(defun g6:copyEntityToPoint (templateEnt templateBase targetPt / before copied)
  (if (and templateEnt (entget templateEnt) templateBase targetPt)
    (progn
      (setq before (entlast))
      (command "_.COPY" templateEnt "" templateBase targetPt)
      (setq copied (entlast))
      (if (eq copied before) nil copied)
    )
  )
)

(defun g6:createBreakerSelected (lineEnt gapW templateEnt templateBase / ed p1 p2 lay mid ang c1 c2 marker1 marker2)
  (setq ed (entget lineEnt)
        p1 (cdr (assoc 10 ed))
        p2 (cdr (assoc 11 ed))
        lay (cdr (assoc 8 ed)))
  (if (null lay) (setq lay "0"))
  (if (and p1 p2 templateEnt (entget templateEnt) templateBase)
    (progn
      (setq mid (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2)
            ang (angle p1 p2)
            c1 (polar mid (+ ang pi) (/ gapW 2.0))
            c2 (polar mid ang (/ gapW 2.0))
            marker1 (g6:copyEntityToPoint templateEnt templateBase c1)
            marker2 (g6:copyEntityToPoint templateEnt templateBase c2))
      (g6:setLayerEnt marker1 lay)
      (g6:setLayerEnt marker2 lay)
      (list marker1 marker2)
    )
  )
)

(defun g6:createBreakerMarker (lineEnt gapW barH mode templateEnt templateBase)
  (if (= mode "SELECT")
    (g6:createBreakerSelected lineEnt gapW templateEnt templateBase)
    (g6:createBreakerBars lineEnt gapW barH)
  )
)

(defun g6:applyBreakerToLine (eSel breakMap gapW barH mode templateEnt templateBase / brkPair oldTail oldTailEd oldTailEnd oldLineEd ed p1 p2 lay lineLen gapUse mid ang g1 g2 markers tailEnt be)
  (setq brkPair (assoc eSel breakMap))
  (if brkPair
    (progn
      (setq oldTail (nth 2 (cdr brkPair)))
      (if (and oldTail (entget oldTail))
        (progn
          (setq oldTailEd (entget oldTail)
                oldTailEnd (cdr (assoc 11 oldTailEd))
                oldLineEd (entget eSel))
          (if (and oldTailEnd oldLineEd (assoc 11 oldLineEd))
            (progn
              (setq oldLineEd (subst (cons 11 oldTailEnd) (assoc 11 oldLineEd) oldLineEd))
              (entmod oldLineEd)
              (entupd eSel)
            )
          )
        )
      )
      (foreach be (cdr brkPair) (if (and be (entget be)) (entdel be)))
    )
  )

  (setq ed (entget eSel)
        p1 (cdr (assoc 10 ed))
        p2 (cdr (assoc 11 ed))
        lay (cdr (assoc 8 ed)))
  (if (null lay) (setq lay "0"))

  (if (and p1 p2)
    (progn
      (setq lineLen (distance p1 p2)
            gapUse gapW)
      (if (>= gapUse lineLen)
        (setq gapUse (max 0.001 (* 0.5 lineLen)))
      )
      (setq mid (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2)
            ang (angle p1 p2)
            g1 (polar mid (+ ang pi) (/ gapUse 2.0))
            g2 (polar mid ang (/ gapUse 2.0)))

      (setq markers (g6:createBreakerMarker eSel gapUse barH mode templateEnt templateBase))

      (setq ed (subst (cons 11 g1) (assoc 11 ed) ed))
      (entmod ed)
      (entupd eSel)

      (setq tailEnt (entmakex (list (cons 0 "LINE") (cons 8 lay) (cons 10 g2) (cons 11 p2))))

      (if (null markers) (setq markers (list nil nil)))
      (setq markers (append markers (list tailEnt)))
      (if brkPair
        (setq breakMap (subst (cons eSel markers) brkPair breakMap))
        (setq breakMap (cons (cons eSel markers) breakMap))
      )
    )
  )
  breakMap
)

;;; ------------------------------------------------------------
;;; DIM helpers
;;; ------------------------------------------------------------
(defun g6:lastDim ( / e n et)
  (setq e (entlast) n 0)
  (while (and e (< n 20))
    (setq et (entget e))
    (if (and et (= (cdr (assoc 0 et)) "DIMENSION"))
      (setq n 999)
      (progn (setq e (entprev e)) (setq n (1+ n)))
    )
  )
  (if (and e et (= (cdr (assoc 0 et)) "DIMENSION")) e nil)
)

(defun g6:setLayerEnt (ent lay / ed)
  (if (and ent (entget ent))
    (progn
      (setq ed (entget ent))
      (if (assoc 8 ed)
        (setq ed (subst (cons 8 lay) (assoc 8 ed) ed))
        (setq ed (append ed (list (cons 8 lay))))
      )
      (entmod ed)
      (entupd ent)
    )
  )
)

(defun g6:setDimText (dimEnt txt / ed)
  (if (and dimEnt (entget dimEnt))
    (progn
      (setq ed (entget dimEnt))
      (if (assoc 1 ed)
        (setq ed (subst (cons 1 txt) (assoc 1 ed) ed))
        (setq ed (append ed (list (cons 1 txt))))
      )
      (entmod ed)
      (entupd dimEnt)
    )
  )
)

(defun g6:deleteEntList (lst)
  (foreach e lst (if (and e (entget e)) (entdel e)))
)

(defun g6:buildSegStack (entList userList / out i e ed p1 p2 userLen)
  (setq out '()
        i 0)
  (while (< i (length entList))
    (setq e (nth i entList)
          userLen (nth i userList))
    (if (and e (entget e))
      (progn
        (setq ed (entget e)
              p1 (cdr (assoc 10 ed))
              p2 (cdr (assoc 11 ed)))
        (if (and p1 p2)
          (setq out (cons (list e p1 p2 userLen (g6:fmtLen userLen)) out))
        )
      )
    )
    (setq i (1+ i))
  )
  (reverse out)
)

;;; ------------------------------------------------------------
;;; Continue-from helper (temporarily restore osnap for pick)
;;; ------------------------------------------------------------
(defun g6:pickContinuePoint (osOn osBack / p)
  (setvar "OSMODE" osOn)
  (setq p (getpoint "\nContinue/branch from point: "))
  (setvar "OSMODE" osBack)
  p
)

(defun g6:snapToRunEndpoint (picked entStack tol / best bestDist e ed p1 p2 endpoint d)
  ;; Normalize a branch pick to the nearest exact endpoint created in this G6 run.
  (setq best nil
        bestDist nil)
  (foreach e entStack
    (if (and e (entget e))
      (progn
        (setq ed (entget e)
              p1 (cdr (assoc 10 ed))
              p2 (cdr (assoc 11 ed)))
        (foreach endpoint (list p1 p2)
          (if endpoint
            (progn
              (setq d (distance (g6:pt3 picked) (g6:pt3 endpoint)))
              (if (and (< d tol) (or (null bestDist) (< d bestDist)))
                (setq best endpoint
                      bestDist d)
              )
            )
          )
        )
      )
    )
  )
  (if best best picked)
)

;;; ------------------------------------------------------------
;;; Layer picker (simple DCL list)
;;; ------------------------------------------------------------
(defun g6:getLayerNames ( / rec lst)
  (setq lst '())
  (setq rec (tblnext "LAYER" T))
  (while rec
    (setq lst (cons (cdr (assoc 2 rec)) lst))
    (setq rec (tblnext "LAYER"))
  )
  (reverse lst)
)

(defun g6:writeLayerDCL (dclpath / fp)
  (setq fp (open dclpath "w"))
  (if fp
    (progn
      (write-line "g6_layerdlg : dialog {" fp)
      (write-line "  label = \"Select Layer\";" fp)
      (write-line "  : list_box { key = \"lst\"; width = 40; height = 20; }" fp)
      (write-line "  ok_cancel;" fp)
      (write-line "}" fp)
      (close fp)
    )
  )
  dclpath
)

(defun g6:pickLayer ( / layers dclpath dclid result curLayer curIdx i)
  (setq layers (g6:getLayerNames))
  (if (not layers)
    nil
    (progn
      (setq dclpath (strcat (getvar "TEMPPREFIX") "g6_layers.dcl"))
      (g6:writeLayerDCL dclpath)
      (setq dclid (load_dialog dclpath))
      (if (< dclid 0)
        nil
        (progn
          (if (not (new_dialog "g6_layerdlg" dclid))
            (progn (unload_dialog dclid) nil)
            (progn
              (start_list "lst")
              (foreach L layers (add_list L))
              (end_list)

              (setq curLayer (getvar "CLAYER") curIdx 0 i 0)
              (foreach L layers
                (if (= (strcase L) (strcase curLayer)) (setq curIdx i))
                (setq i (1+ i))
              )
              (set_tile "lst" (itoa curIdx))
              (setq g6_layer_index curIdx)

              (action_tile "lst" "(setq g6_layer_index (atoi $value))")
              (action_tile "accept" "(done_dialog 1)")
              (action_tile "cancel" "(done_dialog 0)")

              (if (= (start_dialog) 1)
                (setq result (nth g6_layer_index layers))
                (setq result nil)
              )
              (unload_dialog dclid)
              result
            )
          )
        )
      )
    )
  )
)

;;; ------------------------------------------------------------
;;; Per-layer suffix settings (persisted)
;;; ------------------------------------------------------------
(defun g6:sanitizeKey (s / out i ch)
  ;; replace non [A-Z0-9_] with "_", so setenv/getenv keys are stable
  (setq out "" i 1)
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (if (wcmatch ch "[A-Z0-9_]")
      (setq out (strcat out ch))
      (setq out (strcat out "_"))
    )
    (setq i (1+ i))
  )
  out
)

(defun g6:suffixKey (layer) (strcat "G6_SUFFIX_" (g6:sanitizeKey (strcase layer))))

(defun g6:getSuffix (layer / v oldKey)
  (setq v (getenv (g6:suffixKey layer)))
  ;; backward-compat: try the old unsanitized key too
  (if (or (null v) (= v ""))
    (progn
      (setq oldKey (strcat "G6_SUFFIX_" (strcase layer)))
      (setq v (getenv oldKey))
    )
  )
  (if v v "")
)

(defun g6:promptSuffix (layer / cur inp key oldKey)
  (setq key (g6:suffixKey layer))
  (setq oldKey (strcat "G6_SUFFIX_" (strcase layer)))
  (setq cur (g6:getSuffix layer))
  (setq inp
    (getstring T
      (strcat "\nSuffix for layer \"" layer "\" (Enter=keep, ! = clear) <" cur ">: ")
    )
  )
  (cond
    ;; Enter keeps current (saved)
    ((= inp "") cur)

    ;; "!" clears
    ((= inp "!")
      (progn
        (setenv key "")
        (setenv oldKey "")
        ""
      )
    )

    ;; otherwise save new
    (T
      (progn
        (setenv key inp)
        (setenv oldKey inp)
        inp
      )
    )
  )
)

;;; ------------------------------------------------------------
;;; Connected-line collection and external movement
;;; ------------------------------------------------------------
(defun g6:linesAtPoint (pt tol / p ss res i e ed p1 p2)
  (setq p (g6:pt3 pt))
  (setq ss (ssget "C"
                  (list (- (car p) tol) (- (cadr p) tol) (- (caddr p) tol))
                  (list (+ (car p) tol) (+ (cadr p) tol) (+ (caddr p) tol))
                  '((0 . "LINE"))))
  (setq res '())
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq e (ssname ss i))
        (setq ed (entget e))
        (setq p1 (cdr (assoc 10 ed)))
        (setq p2 (cdr (assoc 11 ed)))
        (if (or (g6:ptNear p p1 tol) (g6:ptNear p p2 tol))
          (setq res (cons e res))
        )
        (setq i (1+ i))
      )
    )
  )
  res
)

(defun g6:collectConnectedLines (seedPt tol exclude / queue visited res p lines e ed p1 p2)
  ;; BFS across LINE endpoints; returns LINE entities in the connected component that touches seedPt.
  (setq queue (list (g6:pt3 seedPt)))
  (setq visited '())
  (setq res '())
  (while queue
    (setq p (car queue))
    (setq queue (cdr queue))
    (setq lines (g6:linesAtPoint p tol))
    (foreach e lines
      (if (and (not (member e visited)) (not (member e exclude)))
        (progn
          (setq visited (cons e visited))
          (setq res (cons e res))
          (setq ed (entget e))
          (setq p1 (cdr (assoc 10 ed)))
          (setq p2 (cdr (assoc 11 ed)))
          (cond
            ((and p1 p2 (g6:ptNear p p1 tol)) (setq queue (cons (g6:pt3 p2) queue)))
            ((and p1 p2 (g6:ptNear p p2 tol)) (setq queue (cons (g6:pt3 p1) queue)))
          )
        )
      )
    )
  )
  res
)

(defun g6:firstMember (lst set / r)
  (setq r nil)
  (while (and lst (null r))
    (if (member (car lst) set) (setq r (car lst)))
    (setq lst (cdr lst))
  )
  r
)

(defun g6:compIndexByEnt (comps ent / idx found entry)
  (setq idx 0 found nil)
  (while (and comps (null found))
    (setq entry (car comps))
    (if (member ent (car entry)) (setq found idx))
    (setq comps (cdr comps))
    (setq idx (1+ idx))
  )
  found
)

(defun g6:updateEndpointsForEnts (ents oldPt newPt tol / e ed p1 p2)
  (foreach e ents
    (if (and e (entget e))
      (progn
        (setq ed (entget e))
        (setq p1 (cdr (assoc 10 ed)))
        (setq p2 (cdr (assoc 11 ed)))
        (cond
          ((and p1 (g6:ptNear p1 oldPt tol))
           (setq ed (subst (cons 10 (g6:pt3 newPt)) (assoc 10 ed) ed))
           (entmod ed) (entupd e))
          ((and p2 (g6:ptNear p2 oldPt tol))
           (setq ed (subst (cons 11 (g6:pt3 newPt)) (assoc 11 ed) ed))
           (entmod ed) (entupd e))
        )
      )
    )
  )
)

(defun g6:pointOnSegment (pt a b tol / ap ab cross dotv len2)
  (setq pt (g6:pt3 pt)
        a (g6:pt3 a)
        b (g6:pt3 b)
        ap (g6:pt- pt a)
        ab (g6:pt- b a)
        cross (- (* (car ap) (cadr ab)) (* (cadr ap) (car ab)))
        dotv (+ (* (car ap) (car ab)) (* (cadr ap) (cadr ab)))
        len2 (+ (* (car ab) (car ab)) (* (cadr ab) (cadr ab))))
  (and (<= (abs cross) tol)
       (>= dotv (- tol))
       (<= dotv (+ len2 tol)))
)

(defun g6:connectedToPoint (lineRec pt tol / p1 p2)
  (setq p1 (cadr lineRec)
        p2 (caddr lineRec))
  (or (g6:ptNear pt p1 tol)
      (g6:ptNear pt p2 tol)
      (g6:pointOnSegment pt p1 p2 tol))
)

(defun g6:buildLineRecords (entList / lineRecs endpoints e ed p1 p2)
  (setq lineRecs '()
        endpoints '())
  (foreach e entList
    (if (and e (entget e))
      (progn
        (setq ed (entget e)
              p1 (cdr (assoc 10 ed))
              p2 (cdr (assoc 11 ed)))
        (if (and p1 p2)
          (progn
            (setq p1 (g6:pt3 p1)
                  p2 (g6:pt3 p2)
                  lineRecs (cons (list e p1 p2) lineRecs)
                  endpoints (cons p1 (cons p2 endpoints)))
          )
        )
      )
    )
  )
  (list (reverse lineRecs) (reverse endpoints))
)

(defun g6:updateLineRecord (recs ent newP1 newP2 / out rec)
  (setq out '())
  (foreach rec recs
    (if (eq (car rec) ent)
      (setq out (cons (list ent newP1 newP2) out))
      (setq out (cons rec out))
    )
  )
  (reverse out)
)


(defun g6:endpointsFromLineRecs (lineRecs / out rec)
  ;; returns list of pt3 endpoints from current recs
  (setq out '())
  (foreach rec lineRecs
    (setq out (cons (g6:pt3 (cadr rec)) out)
          out (cons (g6:pt3 (caddr rec)) out))
  )
  (reverse out)
)

(defun g6:queueHasNearPoint (pts pt tol / found)
  (setq found nil)
  (while (and pts (null found))
    (if (g6:ptNear (car pts) pt tol) (setq found T))
    (setq pts (cdr pts))
  )
  found
)

(defun g6:collectDownstreamLines (lineRecs endpoints srcEnt startPt anchorPt tol / queue seenPts moved rec ent p1 p2 pt ep)
  (setq queue (list (g6:pt3 startPt))
        seenPts '()
        moved '())
  (while queue
    (setq pt (car queue)
          queue (cdr queue))
    (if (not (g6:ptNear pt anchorPt tol))
      (if (not (g6:queueHasNearPoint seenPts pt tol))
        (progn
          (setq seenPts (cons pt seenPts))
          (foreach rec lineRecs
            (setq ent (car rec)
                  p1 (cadr rec)
                  p2 (caddr rec))
            (if (and (not (eq ent srcEnt))
                     (not (member ent moved))
                     (g6:connectedToPoint rec pt tol))
              (progn
                (setq moved (cons ent moved))
                (if (and (not (g6:ptNear p1 anchorPt tol)) (not (g6:queueHasNearPoint seenPts p1 tol))) (setq queue (cons p1 queue)))
                (if (and (not (g6:ptNear p2 anchorPt tol)) (not (g6:queueHasNearPoint seenPts p2 tol))) (setq queue (cons p2 queue)))
                (foreach ep endpoints
                  (if (and (not (g6:ptNear ep anchorPt tol))
                           (not (g6:queueHasNearPoint seenPts ep tol))
                           (g6:pointOnSegment ep p1 p2 tol))
                    (setq queue (cons ep queue))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  (reverse moved)
)

(defun g6:moveDimEntity (dimEnt delta / ss base to)
  (if (and dimEnt (entget dimEnt))
    (progn
      (setq ss (ssadd dimEnt)
            base '(0.0 0.0 0.0)
            to (g6:pt3 delta))
      (if ss (command "_.MOVE" ss "" base to))
    )
  )
)

(defun g6:translateLinkedEntities (lineEnt delta breakMap dimMap / brkPair be dimE)
  (setq brkPair (assoc lineEnt breakMap))
  (if brkPair
    (foreach be (cdr brkPair)
      (g6:translateEntity be delta)
    )
  )
  (setq dimE (cdr (assoc lineEnt dimMap)))
  (if dimE (g6:moveDimEntity dimE delta))
)

(defun g6:moveExternalComponents (movedPairs tol excludeChain / comps processed pair oldPt newPt delta comp seed idx entry ents deltas base ok d2)
  ;; Build components connected to each moved joint and either translate them (rigid)
  ;; or (if conflicting deltas) update endpoints at the moved joints.
  (setq comps '())
  (setq processed '())

  (foreach pair movedPairs
    (setq oldPt (car pair))
    (setq newPt (cdr pair))
    (setq delta (g6:pt- newPt oldPt))

    (setq comp (g6:collectConnectedLines oldPt tol excludeChain))
    (if comp
      (progn
        (setq seed (g6:firstMember comp processed))
        (if seed
          (progn
            (setq idx (g6:compIndexByEnt comps seed))
            (if (not (null idx))
              (progn
                (setq entry  (nth idx comps))
                (setq ents   (car entry))
                (setq deltas (cadr entry))
                (setq deltas (cons delta deltas))
                (setq comps (g6:list-set comps idx (list ents deltas)))
              )
            )
          )
          (progn
            (setq comps (cons (list comp (list delta)) comps))
            (setq processed (append processed comp))
          )
        )
      )
    )
  )

  (foreach entry comps
    (setq ents (car entry))
    (setq deltas (cadr entry))
    (if deltas
      (progn
        (setq base (car deltas))
        (setq ok T)
        (foreach d2 (cdr deltas)
          (if (not (g6:vecNear base d2 tol)) (setq ok nil))
        )
        (if ok
          (foreach e ents (g6:translateLine e base))
          (foreach pair movedPairs
            (g6:updateEndpointsForEnts ents (car pair) (cdr pair) tol)
          )
        )
      )
    )
  )
)

;;; ------------------------------------------------------------
;;; Break map helpers
;;; breakMap: alist (lineEnt . (marker1Ent marker2Ent tailEnt))
;;; ------------------------------------------------------------
(defun g6:deleteBreaks (breakMap / pair ents)
  (while breakMap
    (setq pair (car breakMap))
    (setq ents (cdr pair))
    (foreach e ents (if (and e (entget e)) (entdel e)))
    (setq breakMap (cdr breakMap))
  )
)

;;; ------------------------------------------------------------
;;; Internal recompute helper (currently not used by c:G6)
;;; ------------------------------------------------------------
(defun g6:recomputeChains (entList userList chainList shortenFlags scaleFactor breakMap breakParams
                          / chains cid idxs i angs e ed p1 p2 startPt curPt uval drawLen
                            oldEnds newEnds tol movedPairs chainEnts oldEnd newEnd)
  ;; remove old breaks from this run
  (g6:deleteBreaks breakMap)

  (setq tol 0.001)
  (setq chains '() i 0)
  (while (< i (length chainList))
    (setq cid (nth i chainList))
    (if (not (member cid chains)) (setq chains (append chains (list cid))))
    (setq i (1+ i))
  )

  ;; recompute each chain
  (foreach cid chains
    (setq idxs '() i 0)
    (while (< i (length chainList))
      (if (= (nth i chainList) cid) (setq idxs (append idxs (list i))))
      (setq i (1+ i))
    )

    (if idxs
      (progn
        (setq chainEnts '())
        (foreach k idxs (setq chainEnts (cons (nth k entList) chainEnts)))

        ;; start point = current startpoint of first segment in chain
        (setq e (nth (car idxs) entList))
        (setq startPt (cdr (assoc 10 (entget e))))

        ;; capture angles and old endpoints
        (setq angs '() oldEnds '())
        (foreach k idxs
          (setq e (nth k entList))
          (setq ed (entget e))
          (setq p1 (cdr (assoc 10 ed)))
          (setq p2 (cdr (assoc 11 ed)))
          (setq angs (append angs (list (angle p1 p2))))
          (setq oldEnds (append oldEnds (list p2)))
        )

        ;; rewrite segments
        (setq curPt startPt)
        (setq newEnds '())
        (setq i 0)
        (foreach k idxs
          (setq e (nth k entList))
          (setq uval (nth k userList))
          (setq drawLen
            (* scaleFactor
               (if (and (nth k shortenFlags) (> uval 150.0)) 150.0 uval)
            )
          )
          (setq p1 curPt)
          (setq p2 (polar p1 (nth i angs) drawLen))
          (setq ed (entget e))
          (setq ed (subst (cons 10 p1) (assoc 10 ed) ed))
          (setq ed (subst (cons 11 p2) (assoc 11 ed) ed))
          (entmod ed)
          (entupd e)
          (setq newEnds (append newEnds (list p2)))
          (setq curPt p2)
          (setq i (1+ i))
        )

        ;; move connected external components for all moved joints in this chain
        (setq movedPairs '())
        (setq i 0)
        (while (< i (length oldEnds))
          (setq oldEnd (nth i oldEnds))
          (setq newEnd (nth i newEnds))
          (if (> (distance (g6:pt3 oldEnd) (g6:pt3 newEnd)) tol)
            (setq movedPairs (cons (cons oldEnd newEnd) movedPairs))
          )
          (setq i (1+ i))
        )
        (if movedPairs
          (g6:moveExternalComponents movedPairs tol chainEnts)
        )
      )
    )
  )

  (command "_.REGEN")
  '()
)


;;; ============================================================
;;; Main command
;;; ============================================================
(defun c:G6 ( / *error* oldOsmode oldCmdecho
               pt prevPt2 lenStr finishedInput done lastLen gr key
               angList idx scaleFactor connectionTol
               ptsStack entStack lenStack userLenStack chainStack chainId
               entList userList chainList breakMap
               i e userLen len nextpt
               dimOff dimOk dimEnts dimMap dimEligible dimAns ed p1 p2 mid ang nAng dimPt dimEnt userV tailEnt
               layAns lay suf againAns selSS selectedSources j eSel idxSel brkPair dimE txtOvr be
               brkAns brkSS eligibleSS entCandidate brkParams gapW barH brkMode brkTemplate brkTemplateBase
               shortAns shortSS shortEligible shortOrder shortThreshold shortCap shortTargets shortRecs shortEndpoints
               shortEnt shortIdx anchor oldEnd targetLen newEnd delta tol shortMoved moveEnt moveEd moveP1 moveP2 userCapLen requiredCapLen capUsedLen shortPt dist
               segStack shortLoop shortOk shortRollback shortMarkPlaced reDimAns reDimLayMap reDimPair oldDimEnt oldDimLay
               )

  (defun *error* (msg)
    (if oldOsmode  (setvar "OSMODE"  oldOsmode))
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq oldOsmode  (getvar "OSMODE")
        oldCmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; angles: 30, 90, 150, -150, -90, -30
  (setq angList (mapcar 'g6:deg->rad '(30 90 150 -150 -90 -30)))
  (setq idx 0)
  (setq scaleFactor 0.01)
  (setq connectionTol 0.001)

  (setq ptsStack '()
        entStack '()
        lenStack '()
        userLenStack '()
        chainStack '()
        chainId 0
        breakMap '()
  )

  (defun updatePreview ( / curAng curLen curEnd tmp)
    (if prevPt2 (grdraw pt prevPt2 0))
    (setq curAng (nth idx angList))
    (if (= lenStr "")
      (setq curLen (if lastLen lastLen 1.0))
      (progn
        (setq tmp (abs (atof lenStr)))
        (setq curLen (* scaleFactor tmp))
      )
    )
    (setq curEnd (polar pt curAng curLen))
    (grdraw pt curEnd 1)
    (setq prevPt2 curEnd)
  )

  (setq pt (getpoint "\nStart point: "))
  (if (not pt)
    (progn
      (setvar "OSMODE" oldOsmode)
      (setvar "CMDECHO" oldCmdecho)
      (princ)
    )
    (progn
      ;; disable OSNAP during construction
      (setvar "OSMODE" 0)
      (setq done nil lastLen 1.0 prevPt2 nil)

      (while (not done)
        (setq lenStr "" finishedInput nil)
        (prompt
          (strcat
            "\nCurrent angle = "
            (rtos (g6:rad->deg (nth idx angList)) 2 0)
            "  | Enter length (scale 0.01; TAB=angle, U=undo, C=continue/branch from point, ENTER=finish): "
          )
        )
        (updatePreview)

        (while (not finishedInput)
          (setq gr (grread T 1 0))
          (cond
            ((= (car gr) 2)
             (setq key (cadr gr))
             (cond
               ((= key 9) ;; TAB
                (setq idx (1+ idx))
                (if (>= idx (length angList)) (setq idx 0))
                (prompt (strcat "\nAngle: " (rtos (g6:rad->deg (nth idx angList)) 2 0)))
                (updatePreview)
               )

               ((= key 13) ;; ENTER
                (if (= lenStr "")
                  (progn
                    (if prevPt2 (grdraw pt prevPt2 0))
                    (setq prevPt2 nil done T finishedInput T)
                  )
                  (progn
                    (setq userLen (abs (atof lenStr)))
                    (setq len (* scaleFactor userLen))

                    (if prevPt2 (grdraw pt prevPt2 0))
                    (setq prevPt2 nil)

                    (setq ptsStack (cons pt ptsStack))
                    (setq lenStack (cons len lenStack))
                    (setq userLenStack (cons userLen userLenStack))

                    (setq nextpt (polar pt (nth idx angList) len))
                    (command "_.LINE" pt nextpt "")

                    (setq entStack (cons (entlast) entStack))
                    (setq chainStack (cons chainId chainStack))

                    (setq pt nextpt lastLen len finishedInput T)
                    (command "_.REGEN")
                  )
                )
               )

               ((or (= key 67) (= key 99)) ;; C / c
                (if prevPt2 (grdraw pt prevPt2 0))
                (setq prevPt2 nil)
                (setq nextpt (g6:pickContinuePoint oldOsmode 0))
                (if nextpt
                  (progn
                    (setq nextpt (g6:snapToRunEndpoint nextpt entStack connectionTol))
                    (setq chainId (1+ chainId))
                    (setq pt nextpt)
                    (prompt "\nContinue/branch point set."))
                  (prompt "\nContinue cancelled; keeping current point."))
                (command "_.REGEN")
                (updatePreview)
               )

               ((or (= key 85) (= key 117)) ;; U/u undo
                (if entStack
                  (progn
                    (if prevPt2 (grdraw pt prevPt2 0))
                    (setq prevPt2 nil)

                    (entdel (car entStack))
                    (setq entStack (cdr entStack))
                    (setq chainStack (cdr chainStack))

                    (setq pt (car ptsStack) ptsStack (cdr ptsStack))
                    (setq lastLen (car lenStack) lenStack (cdr lenStack))
                    (setq userLenStack (cdr userLenStack))

                    (prompt "\nLast segment undone.")
                    (updatePreview)
                    (command "_.REGEN")
                  )
                  (prompt "\nNo segment to undo.")
                )
               )

               ((= key 8) ;; backspace
                (if (> (strlen lenStr) 0)
                  (progn
                    (setq lenStr (substr lenStr 1 (1- (strlen lenStr))))
                    (prompt (strcat "\rLength (input): " lenStr " "))
                    (updatePreview)
                  )
                )
               )

               ;; digits/dot/minus (minus allowed in input but abs() will be used)
               ((or (and (>= key 48) (<= key 57)) (= key 46) (= key 45))
                (setq lenStr (strcat lenStr (chr key)))
                (prompt (strcat "\rLength (input): " lenStr " "))
                (updatePreview)
               )
             )
            )
            ((= (car gr) 5) (updatePreview)) ;; mouse move
          )
        )
      )

      ;; restore OSNAP for post steps
      (setvar "OSMODE" oldOsmode)

      ;; lists in draw order
      (if entStack
        (progn
          (setq entList   (reverse entStack))
          (setq userList  (reverse userLenStack))
          (setq chainList (reverse chainStack))
          (setq segStack (g6:buildSegStack entList userList))
        )
      )
      ;; shorten selected long lines from this run, propagate downstream, then auto-break shortened lines
      (if entStack
        (progn
          (setq shortLoop T
                shortMarkPlaced nil)
          (while shortLoop
            (setq shortLoop nil
                  shortTargets '()
                  tol connectionTol)
            (initget "Yes No")
            (setq shortAns (getkword "\nShorten long lines? [Yes/No] <No>: "))
            (if (= shortAns "Yes")
              (progn
                (command "_.UNDO" "_Mark")
                (setq shortMarkPlaced T)

                (setq shortThreshold (getreal "\nThreshold <150>: "))
                (if (or (null shortThreshold) (<= shortThreshold 0.0)) (setq shortThreshold 150.0))
                (setq shortCap (getreal "\nCap length <150>: "))
                (if (or (null shortCap) (<= shortCap 0.0)) (setq shortCap 150.0))

                (setq shortSS (ssget "_:L" '((0 . "LINE"))))
                (setq shortEligible (ssadd))
                (if shortSS
                  (progn
                    (setq j 0)
                    (while (< j (sslength shortSS))
                      (setq entCandidate (ssname shortSS j))
                      (setq shortIdx (g6:index-of entCandidate entList))
                      (if (and (not (null shortIdx)) (> (nth shortIdx userList) shortThreshold))
                        (setq shortEligible (ssadd entCandidate shortEligible))
                      )
                      (setq j (1+ j))
                    )
                  )
                )

                (setq shortOrder '()
                      j 0)
                (while (< j (length entList))
                  (setq e (nth j entList))
                  (if (ssmemb e shortEligible)
                    (setq shortOrder (append shortOrder (list e)))
                  )
                  (setq j (1+ j))
                )

                (if (null shortOrder)
                  (prompt "\nNo eligible long lines selected from this G6 run.")
                  (progn
                    (setq shortRecs (car (g6:buildLineRecords entList))
                          shortEndpoints (g6:endpointsFromLineRecs shortRecs))
                    (foreach shortEnt shortOrder
                      (if (and shortEnt (entget shortEnt))
                        (progn
                          (setq ed (entget shortEnt)
                                anchor (cdr (assoc 10 ed))
                                oldEnd (cdr (assoc 11 ed)))
                          (if (and anchor oldEnd)
                            (progn
                              (setq shortEndpoints (g6:endpointsFromLineRecs shortRecs)
                                    userCapLen (* shortCap scaleFactor)
                                    requiredCapLen 0.0)
                              (foreach shortPt shortEndpoints
                                (if (and (not (g6:ptNear shortPt anchor tol))
                                         (not (g6:ptNear shortPt oldEnd tol))
                                         (g6:pointOnSegment shortPt anchor oldEnd tol))
                                  (progn
                                    (setq dist (distance (g6:pt3 anchor) (g6:pt3 shortPt)))
                                    (if (> dist requiredCapLen) (setq requiredCapLen dist))
                                  )
                                )
                              )
                              (setq capUsedLen (max userCapLen requiredCapLen))
                              (if (> capUsedLen userCapLen)
                                (prompt "\nCap increased to preserve mid connections."))
                              (setq targetLen capUsedLen
                                    newEnd (polar anchor (angle anchor oldEnd) targetLen)
                                    delta (g6:pt- newEnd oldEnd))
                              (if (> (distance (g6:pt3 newEnd) (g6:pt3 oldEnd)) tol)
                                (progn
                                  (setq ed (subst (cons 11 newEnd) (assoc 11 ed) ed))
                                  (entmod ed)
                                  (entupd shortEnt)

                                  (setq shortRecs (g6:updateLineRecord shortRecs shortEnt anchor newEnd))
                                  (setq shortEndpoints (g6:endpointsFromLineRecs shortRecs))
                                  (setq shortMoved (g6:collectDownstreamLines shortRecs shortEndpoints shortEnt oldEnd anchor tol))
                                  (foreach moveEnt shortMoved
                                    (if (and moveEnt (entget moveEnt))
                                      (progn
                                        (setq moveEd (entget moveEnt)
                                              moveP1 (cdr (assoc 10 moveEd))
                                              moveP2 (cdr (assoc 11 moveEd)))
                                        (if (and moveP1 moveP2)
                                          (progn
                                            (setq moveP1 (g6:pt+ moveP1 delta)
                                                  moveP2 (g6:pt+ moveP2 delta)
                                                  moveEd (subst (cons 10 moveP1) (assoc 10 moveEd) moveEd)
                                                  moveEd (subst (cons 11 moveP2) (assoc 11 moveEd) moveEd))
                                            (entmod moveEd)
                                            (entupd moveEnt)
                                            (setq shortRecs (g6:updateLineRecord shortRecs moveEnt moveP1 moveP2))
                                            (g6:translateLinkedEntities moveEnt delta breakMap dimMap)
                                          )
                                        )
                                      )
                                    )
                                  )
                                  (setq shortTargets (append shortTargets (list shortEnt)))
                                )
                              )
                            )
                          )
                        )
                      )
                    )

                    (if shortTargets
                      (progn
                        (setq brkParams (g6:pickBreakParams (car shortTargets) (g6:breakParamsDefault)))
                        (if brkParams
                          (progn
                            (setq gapW (nth 0 brkParams)
                                  barH (nth 1 brkParams)
                                  brkMode (nth 2 brkParams)
                                  brkTemplate (nth 3 brkParams)
                                  brkTemplateBase (nth 4 brkParams))
                            (foreach shortEnt shortTargets
                              (setq breakMap (g6:applyBreakerToLine shortEnt breakMap gapW barH brkMode brkTemplate brkTemplateBase))
                            )
                          )
                        )
                      )
                    )
                  )
                )

                (initget "Yes No")
                (setq shortOk (getkword "\nShorten + breakers OK? [Yes/No] <Yes>: "))
                (if (= shortOk "No")
                  (progn
                    (command "_.UNDO" "_Back")
                    (setq breakMap '()
                          dimMap '()
                          shortTargets '()
                          segStack (g6:buildSegStack entList userList)
                          shortLoop T
                          shortMarkPlaced nil)
                  )
                  (setq shortMarkPlaced nil)
                )
              )
            )
          )
        )
      )

      ;; add break markers to selected lines from this run
      (if entStack
        (progn
          (initget "Yes No")
          (setq brkAns (getkword "\nAdd break markers to selected lines? [Yes/No] <No>: "))
          (if (= brkAns "Yes")
            (progn
              (setq brkSS (ssget "_:L" '((0 . "LINE"))))
              (setq eligibleSS (ssadd))
              (if brkSS
                (progn
                  (setq j 0)
                  (while (< j (sslength brkSS))
                    (setq entCandidate (ssname brkSS j))
                    (if (not (null (g6:index-of entCandidate entList)))
                      (setq eligibleSS (ssadd entCandidate eligibleSS))
                    )
                    (setq j (1+ j))
                  )
                )
              )
              (if (= (sslength eligibleSS) 0)
                (prompt "\nNo eligible lines selected from this G6 run.")
                (progn
                  (setq brkParams (g6:pickBreakParams (ssname eligibleSS 0) (g6:breakParamsDefault)))
                  (if brkParams
                    (progn
                      (setq gapW (nth 0 brkParams)
                            barH (nth 1 brkParams)
                            brkMode (nth 2 brkParams)
                            brkTemplate (nth 3 brkParams)
                            brkTemplateBase (nth 4 brkParams)
                            j 0)
                      (while (< j (sslength eligibleSS))
                        (setq eSel (ssname eligibleSS j))
                        (setq breakMap (g6:applyBreakerToLine eSel breakMap gapW barH brkMode brkTemplate brkTemplateBase))
                        (setq j (1+ j))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

      ;; 1) Dimensions with satisfaction loop
      (setq dimMap '())
      (if entStack
        (progn
          (setq dimEligible '())
          (setq i 0)
          (while (< i (length entList))
            (setq userV (nth i userList))
            (if (g6:dimensionEligibleP userV)
              (setq dimEligible (cons (nth i entList) dimEligible))
            )
            (setq i (1+ i))
          )
          (setq dimEligible (reverse dimEligible))
          (if dimEligible
            (setq dimOff (g6:pickDimOffsetPreview dimEligible breakMap))
            (progn
              (prompt "\nNo segments eligible for dimensions (typed length >= 150).")
              (setq dimOff nil)
            )
          )
          (if dimOff
            (progn
              (setq dimOk nil)
              (setq dimEnts '())
              (while (not dimOk)
                (g6:deleteEntList dimEnts)
                (setq dimEnts '())
                (setq dimMap '())

                (setq i 0)
                (while (< i (length entList))
                  (setq e (nth i entList))
                  (setq userV (nth i userList))
                  (if (g6:dimensionEligibleP userV)
                    (progn
                      (setq ed (entget e)
                            p1 (cdr (assoc 10 ed))
                            p2 (cdr (assoc 11 ed))
                            brkPair (assoc e breakMap))
                      (if brkPair
                        (progn
                          (setq tailEnt (nth 2 (cdr brkPair)))
                          (if (and tailEnt (entget tailEnt))
                            (setq p2 (cdr (assoc 11 (entget tailEnt))))
                          )
                        )
                      )
                      (if (and p1 p2)
                        (progn
                          (setq mid (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2))
                          (setq ang (angle p1 p2))
                          (setq nAng (+ ang (/ pi 2.0)))
                          (setq dimPt (polar mid nAng dimOff))
                          (command "_.DIMALIGNED" p1 p2 dimPt "")
                          (setq dimEnt (g6:lastDim))
                          (if dimEnt
                            (progn
                              (g6:setDimText dimEnt (g6:fmtLen userV))
                              (setq dimEnts (cons dimEnt dimEnts))
                              (setq dimMap (cons (cons e dimEnt) dimMap))
                            )
                            (setq dimMap (cons (cons e nil) dimMap))
                          )
                        )
                      )
                    )
                  )
                  (setq i (1+ i))
                )

                (initget "Yes No")
                (setq dimAns (getkword "\nDimension placement OK? [Yes/No] <Yes>: "))
                (if (or (null dimAns) (= dimAns "Yes"))
                  (setq dimOk T)
                  (progn
                    (setq dimOff (g6:pickDimOffsetPreview dimEligible breakMap))
                    (if (null dimOff)
                      (progn
                        (g6:deleteEntList dimEnts)
                        (setq dimEnts '())
                        (setq dimMap '())
                        (setq dimOk T)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

      ;; 2) Layer assignment + suffix
      (if entStack
        (progn
          (initget "Yes No")
          (setq layAns (getkword "\nAssign layers (popup list) and apply suffix to dimensions? [Yes/No] <No>: "))
          (if (= layAns "Yes")
            (progn
              (setq againAns "Yes")
              (while (= againAns "Yes")
                (setq lay (g6:pickLayer))
                (if (null lay)
                  (setq againAns "No")
                  (progn
                    (setq suf (g6:promptSuffix lay))
                    (prompt (strcat "\nSelect source or tail G6 segments to move to layer \"" lay "\". Breaker helper lines are ignored."))
                    (setq selSS (ssget '((0 . "LINE"))))
                    (if selSS
                      (progn
                        ;; Resolve source and tail selections once; ignore breaker marker helpers.
                        (setq selectedSources '()
                              j 0)
                        (while (< j (sslength selSS))
                          (setq eSel (g6:resolveLayerSourceLine (ssname selSS j) entList breakMap))
                          (if (and eSel (not (member eSel selectedSources)))
                            (setq selectedSources (cons eSel selectedSources))
                          )
                          (setq j (1+ j))
                        )

                        (if selectedSources
                          (foreach eSel (reverse selectedSources)
                            (setq idxSel (g6:index-of eSel entList))
                            (g6:setLayerEnt eSel lay)

                            ;; breaks and tail for that source line (if any)
                            (setq brkPair (assoc eSel breakMap))
                            (if brkPair
                              (foreach be (cdr brkPair) (g6:setLayerEnt be lay))
                            )

                            ;; dimension + suffix (if any)
                            (setq dimE (cdr (assoc eSel dimMap)))
                            (if dimE
                              (progn
                                (g6:setLayerEnt dimE lay)
                                (setq userV (nth idxSel userList))
                                (setq txtOvr (if (< userV 120.0) (g6:fmtLen userV) (strcat (g6:fmtLen userV) suf)))
                                (g6:setDimText dimE txtOvr)
                              )
                            )
                          )
                          (prompt "\nNo source or tail G6 segments selected; breaker marker helpers were ignored.")
                        )
                      )
                      (prompt "\nNo selection made.")
                    )

                    (initget "Yes No")
                    (setq againAns (getkword "\nAssign another layer? [Yes/No] <No>: "))
                    (if (null againAns) (setq againAns "No"))
                  )
                )
              )
            )
          )
        )
      )

      (initget "Yes No")
      (setq reDimAns (getkword "\nReposition dimensions? [Yes/No] <No>: "))
      (if (= reDimAns "Yes")
        (if (null dimMap)
          (prompt "\nNo dimensions from this run to reposition.")
          (progn
            (setq dimOff (g6:pickDimOffsetPreview dimEligible breakMap))
            (if dimOff
              (progn
                (setq reDimLayMap '())
                (foreach reDimPair dimMap
                  (setq oldDimEnt (cdr reDimPair)
                        oldDimLay nil)
                  (if (and oldDimEnt (entget oldDimEnt))
                    (setq oldDimLay (cdr (assoc 8 (entget oldDimEnt)))))
                  (if (null oldDimLay) (setq oldDimLay "0"))
                  (setq reDimLayMap (cons (cons (car reDimPair) oldDimLay) reDimLayMap)))

                (g6:deleteEntList (mapcar 'cdr dimMap))
                (setq dimMap '()
                      i 0)
                (while (< i (length entList))
                  (setq e (nth i entList)
                        userV (nth i userList))
                  (if (g6:dimensionEligibleP userV)
                    (progn
                      (setq ed (entget e)
                            p1 (cdr (assoc 10 ed))
                            p2 (cdr (assoc 11 ed))
                            brkPair (assoc e breakMap))
                      (if brkPair
                        (progn
                          (setq tailEnt (nth 2 (cdr brkPair)))
                          (if (and tailEnt (entget tailEnt))
                            (setq p2 (cdr (assoc 11 (entget tailEnt)))))))
                      (if (and p1 p2)
                        (progn
                          (setq mid (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2)
                                ang (angle p1 p2)
                                nAng (+ ang (/ pi 2.0))
                                dimPt (polar mid nAng dimOff))
                          (command "_.DIMALIGNED" p1 p2 dimPt "")
                          (setq dimEnt (g6:lastDim))
                          (if dimEnt
                            (progn
                              (setq oldDimLay (cdr (assoc e reDimLayMap)))
                              (if oldDimLay (g6:setLayerEnt dimEnt oldDimLay))
                              (setq txtOvr (g6:fmtLen userV))
                              (if (< userV 120.0)
                                (setq txtOvr (g6:fmtLen userV))
                                (progn
                                  (setq suf "")
                                  (setq lay (cdr (assoc 8 (entget dimEnt))))
                                  (if (null lay) (setq lay "0"))
                                  (setq suf (g6:getSuffix lay))
                                  (setq txtOvr (strcat (g6:fmtLen userV) suf))))
                              (g6:setDimText dimEnt txtOvr)
                              (setq dimMap (cons (cons e dimEnt) dimMap)))
                            (setq dimMap (cons (cons e nil) dimMap)))))))
                  (setq i (1+ i)))
              )
            )
          )
        )
      )

      ;; restore sys vars
      (setvar "OSMODE"  oldOsmode)
      (setvar "CMDECHO" oldCmdecho)
    )
  )
  (princ)
)

(princ "\nG6 patched loaded. Run command: G6")
(princ)
