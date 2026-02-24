
;;; ============================================================
;;; G6 (patched)
;;; Features:
;;;  - Draw chained LINE segments at fixed angles with typed lengths
;;;  - Optional shortening: selected segments with typed length >150 are drawn as 150 (display),
;;;    but dimensions show the typed value.
;;;  - Break marker with empty gap (WIPEOUT) + bars; size adjustable once with live preview
;;;    and applies to all breaks (persisted).
;;;  - Continue-from point: press C while in length input to pick a new start point without ending.
;;;  - Dimensions with satisfaction loop (re-pick offset distance).
;;;  - Layer assignment popup + per-layer suffix saved; moves lines + dims + breaks.
;;;  - Connected LINE networks stay attached when joints move (handles multiple shorten selections).
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

;;; ------------------------------------------------------------
;;; basic point/vector helpers
;;; ------------------------------------------------------------
(defun g6:pt3 (p) (list (car p) (cadr p) (if (caddr p) (caddr p) 0.0)))
(defun g6:pt+ (p v) (mapcar '+ (g6:pt3 p) (g6:pt3 v)))
(defun g6:pt- (p q) (mapcar '- (g6:pt3 p) (g6:pt3 q)))
(defun g6:ptNear (p q tol) (< (distance (g6:pt3 p) (g6:pt3 q)) tol))
(defun g6:vecNear (v1 v2 tol) (< (distance (g6:pt3 v1) (g6:pt3 v2)) tol))

(defun g6:translateLine (e vec / ed p1 p2)
  (if (and e (entget e))
    (progn
      (setq ed (entget e)
            p1 (cdr (assoc 10 ed))
            p2 (cdr (assoc 11 ed))
      )
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
;;; params = (d h gap wipeH)
;;; ------------------------------------------------------------
(defun g6:breakParamsDefault ( / h d gap wipeH)
  ;; h is the master size; defaults to 0.06 (about 2x the old small break)
  (setq h (g6:getEnvReal "G6_BREAK_H" 0.06))
  (setq d (* h 0.70))
  (setq gap (* h 2.00))
  (setq wipeH (* h 3.20))
  (list d h gap wipeH)
)

(defun g6:drawPreviewSegs (segs col)
  (foreach s segs (grdraw (car s) (cadr s) col))
)

(defun g6:previewBreakSegs (mid ang d h gap wipeH / ux uy vx vy b1 b2 t1 t2 r1 r2 r3 r4 segs)
  (setq ux (cos ang) uy (sin ang))
  (setq vx (- (sin ang)) vy (cos ang))

  (setq b1 (list (+ (car mid) (* ux (- (/ d 2.0))))
                 (+ (cadr mid) (* uy (- (/ d 2.0))))
                 0.0))
  (setq b2 (list (+ (car mid) (* ux (/ d 2.0)))
                 (+ (cadr mid) (* uy (/ d 2.0)))
                 0.0))

  (setq t1 (list (+ (car b1) (* vx (- (/ h 2.0))))
                 (+ (cadr b1) (* vy (- (/ h 2.0))))
                 0.0))
  (setq t2 (list (+ (car b1) (* vx (/ h 2.0)))
                 (+ (cadr b1) (* vy (/ h 2.0)))
                 0.0))
  (setq segs (list (list t1 t2)))

  (setq t1 (list (+ (car b2) (* vx (- (/ h 2.0))))
                 (+ (cadr b2) (* vy (- (/ h 2.0))))
                 0.0))
  (setq t2 (list (+ (car b2) (* vx (/ h 2.0)))
                 (+ (cadr b2) (* vy (/ h 2.0)))
                 0.0))
  (setq segs (append segs (list (list t1 t2))))

  ;; wipeout preview rectangle
  (setq r1 (list (+ (car mid) (+ (* ux (/ gap 2.0)) (* vx (/ wipeH 2.0))))
                 (+ (cadr mid) (+ (* uy (/ gap 2.0)) (* vy (/ wipeH 2.0))))
                 0.0))
  (setq r2 (list (+ (car mid) (+ (* ux (/ gap 2.0)) (* vx (- (/ wipeH 2.0)))))
                 (+ (cadr mid) (+ (* uy (/ gap 2.0)) (* vy (- (/ wipeH 2.0)))))
                 0.0))
  (setq r3 (list (+ (car mid) (+ (* ux (- (/ gap 2.0))) (* vx (- (/ wipeH 2.0)))))
                 (+ (cadr mid) (+ (* uy (- (/ gap 2.0))) (* vy (- (/ wipeH 2.0)))))
                 0.0))
  (setq r4 (list (+ (car mid) (+ (* ux (- (/ gap 2.0))) (* vx (/ wipeH 2.0))))
                 (+ (cadr mid) (+ (* uy (- (/ gap 2.0))) (* vy (/ wipeH 2.0))))
                 0.0))
  (setq segs (append segs (list (list r1 r2) (list r2 r3) (list r3 r4) (list r4 r1))))
  segs
)

(defun g6:pickBreakParams (sampleEnt params / ed p1 p2 mid ang perp gr typ dat vec dist h d gap wipeH done oldSegs out)
  (setq out params)
  (if (null sampleEnt) out
    (progn
      (setq ed (entget sampleEnt))
      (setq p1 (cdr (assoc 10 ed)) p2 (cdr (assoc 11 ed)))
      (if (or (null p1) (null p2)) out
        (progn
          (setq mid (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2))
          (setq ang (angle p1 p2))
          (setq perp (list (- (sin ang)) (cos ang) 0.0))

          (prompt "\nBreak size: move cursor (live preview) and click to set. Enter = keep saved.")
          (setq oldSegs (g6:previewBreakSegs mid ang (nth 0 out) (nth 1 out) (nth 2 out) (nth 3 out)))
          (g6:drawPreviewSegs oldSegs 1)

          (setq done nil h nil)
          (while (not done)
            (setq gr (grread T 13 0))
            (setq typ (car gr) dat (cadr gr))
            (cond
              ((= typ 5) ;; mouse move
               (if (and dat (listp dat))
                 (progn
                   (setq vec (g6:pt- dat mid))
                   (setq dist (abs (+ (* (car vec) (car perp)) (* (cadr vec) (cadr perp)))))
                   (setq h (max 0.01 (* 2.0 dist)))
                   (setq d (* h 0.70))
                   (setq gap (* h 2.00))
                   (setq wipeH (* h 3.20))
                   (g6:drawPreviewSegs oldSegs 0)
                   (setq oldSegs (g6:previewBreakSegs mid ang d h gap wipeH))
                   (g6:drawPreviewSegs oldSegs 1)
                 )
               )
              )
              ((= typ 3) ;; click
               (g6:drawPreviewSegs oldSegs 0)
               (if (null h) (setq h (nth 1 out)))
               (g6:setEnvReal "G6_BREAK_H" h)
               (setq d (* h 0.70))
               (setq gap (* h 2.00))
               (setq wipeH (* h 3.20))
               (setq out (list d h gap wipeH))
               (setq done T)
              )
              ((= typ 2) ;; key
               (cond
                 ((= dat 13) (progn (g6:drawPreviewSegs oldSegs 0) (setq done T))) ;; Enter
                 ((= dat 27) (progn (g6:drawPreviewSegs oldSegs 0) (setq done T))) ;; Esc
               )
              )
            )
          )
          out
        )
      )
    )
  )
)

(defun g6:breakBlockName (d h / a b)
  (setq a (itoa (fix (* d 1000000.0))))
  (setq b (itoa (fix (* h 1000000.0))))
  (strcat "G6_BRK_" a "_" b)
)

(defun g6:ensureBreakBlock (d h / blkName)
  (setq blkName (g6:breakBlockName d h))
  (if (not (tblsearch "BLOCK" blkName))
    (progn
      (entmake (list (cons 0 "BLOCK") (cons 2 blkName) (cons 70 0)
                     (cons 10 (list 0.0 0.0 0.0)) (cons 3 blkName) (cons 1 "")))
      ;; left bar
      (entmake (list (cons 0 "LINE") (cons 8 "0")
                     (cons 10 (list (- (/ d 2.0)) (- (/ h 2.0)) 0.0))
                     (cons 11 (list (- (/ d 2.0)) (/ h 2.0) 0.0))))
      ;; right bar
      (entmake (list (cons 0 "LINE") (cons 8 "0")
                     (cons 10 (list (/ d 2.0) (- (/ h 2.0)) 0.0))
                     (cons 11 (list (/ d 2.0) (/ h 2.0) 0.0))))
      (entmake (list (cons 0 "ENDBLK")))
    )
  )
  blkName
)

(defun g6:tryWipeoutGap (mid ang gap wipeH / ux uy vx vy p1 p2 p3 p4 plEnt wEnt)
  (setq ux (cos ang) uy (sin ang))
  (setq vx (- (sin ang)) vy (cos ang))

  (setq p1 (list (+ (car mid) (+ (* ux (/ gap 2.0)) (* vx (/ wipeH 2.0))))
                 (+ (cadr mid) (+ (* uy (/ gap 2.0)) (* vy (/ wipeH 2.0))))
                 0.0))
  (setq p2 (list (+ (car mid) (+ (* ux (/ gap 2.0)) (* vx (- (/ wipeH 2.0)))))
                 (+ (cadr mid) (+ (* uy (/ gap 2.0)) (* vy (- (/ wipeH 2.0)))))
                 0.0))
  (setq p3 (list (+ (car mid) (+ (* ux (- (/ gap 2.0))) (* vx (- (/ wipeH 2.0)))))
                 (+ (cadr mid) (+ (* uy (- (/ gap 2.0))) (* vy (- (/ wipeH 2.0)))))
                 0.0))
  (setq p4 (list (+ (car mid) (+ (* ux (- (/ gap 2.0))) (* vx (/ wipeH 2.0))))
                 (+ (cadr mid) (+ (* uy (- (/ gap 2.0))) (* vy (/ wipeH 2.0))))
                 0.0))

  (setq plEnt
    (entmakex
      (list
        (cons 0 "LWPOLYLINE")
        (cons 100 "AcDbEntity")
        (cons 8 "0")
        (cons 100 "AcDbPolyline")
        (cons 90 4)
        (cons 70 1)
        (cons 10 (list (car p1) (cadr p1)))
        (cons 10 (list (car p2) (cadr p2)))
        (cons 10 (list (car p3) (cadr p3)))
        (cons 10 (list (car p4) (cadr p4)))
      )
    )
  )

  (setq wEnt nil)
  (if plEnt
    (progn
      (command "_.WIPEOUT" "_P" plEnt "_Y")
      (setq wEnt (entlast))
    )
  )
  wEnt
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

;;; ------------------------------------------------------------
;;; Continue-from helper (temporarily restore osnap for pick)
;;; ------------------------------------------------------------
(defun g6:pickContinuePoint (osOn osBack / p)
  (setvar "OSMODE" osOn)
  (setq p (getpoint "\nContinue from point: "))
  (setvar "OSMODE" osBack)
  p
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
;;; breakMap: alist (lineEnt . (wipeoutEnt blockEnt))
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
;;; Recompute chains after shortening (and move connected lines)
;;; ------------------------------------------------------------
(defun g6:recomputeChains (entList userList chainList shortenFlags scaleFactor breakMap breakParams
                          / chains cid idxs i angs e ed p1 p2 startPt curPt uval drawLen
                            oldEnds newEnds tol movedPairs chainEnts
                            d h gap wipeH blkName mid wEnt blkEnt lay newBreakMap k oldEnd newEnd ss)
  ;; remove old breaks from this run
  (g6:deleteBreaks breakMap)

  (setq tol 0.001)
  (setq chains '() i 0)
  (while (< i (length chainList))
    (setq cid (nth i chainList))
    (if (not (member cid chains)) (setq chains (append chains (list cid))))
    (setq i (1+ i))
  )

  ;; unpack break params
  (setq d (nth 0 breakParams)
        h (nth 1 breakParams)
        gap (nth 2 breakParams)
        wipeH (nth 3 breakParams))
  (setq blkName (g6:ensureBreakBlock d h))

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

  ;; insert breaks for shortened segments (after recompute)
  (setq newBreakMap '())
  (setq i 0)
  (while (< i (length entList))
    (setq e (nth i entList))
    (setq uval (nth i userList))
    (if (and (nth i shortenFlags) (> uval 150.0))
      (progn
        (setq ed (entget e))
        (setq p1 (cdr (assoc 10 ed)))
        (setq p2 (cdr (assoc 11 ed)))
        (setq ang (angle p1 p2))
        (setq mid (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2))

        (setq wEnt (g6:tryWipeoutGap mid ang gap wipeH))
        (command "_.-INSERT" blkName mid 1.0 1.0 (g6:rad->deg ang))
        (setq blkEnt (entlast))

        ;; same layer as line
        (setq lay (cdr (assoc 8 ed)))
        (if lay
          (progn
            (if wEnt   (g6:setLayerEnt wEnt lay))
            (if blkEnt (g6:setLayerEnt blkEnt lay))
          )
        )

        ;; bring to front (avoid bleed-through)
        (setq ss (ssadd))
        (if wEnt   (ssadd wEnt ss))
        (if blkEnt (ssadd blkEnt ss))
        (command "_.DRAWORDER" ss "" "_Front" "")

        (setq newBreakMap (cons (cons e (list wEnt blkEnt)) newBreakMap))
      )
    )
    (setq i (1+ i))
  )
  newBreakMap
)

;;; ============================================================
;;; Main command
;;; ============================================================
(defun c:G6 ( / *error* oldOsmode oldCmdecho
               pt prevPt2 lenStr finishedInput done lastLen gr key
               angList idx scaleFactor
               ptsStack entStack lenStack userLenStack chainStack chainId
               entList userList chainList shortenFlags breakMap breakParams
               shortAns ss selEnts i e userLen len nextpt
               dimOff dimOk dimEnts dimMap dimAns ed p1 p2 mid ang nAng dimPt dimEnt userV
               layAns lay suf againAns selSS j eSel idxSel brkPair dimE txtOvr
               anyShort sampleEnt)

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

  (setq ptsStack '()
        entStack '()
        lenStack '()
        userLenStack '()
        chainStack '()
        chainId 0
        breakMap '()
  )

  (setq breakParams (g6:breakParamsDefault))

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
            "  | Enter length (scale 0.01; TAB=angle, U=undo, C=continue, ENTER=finish): "
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
                    (setq chainId (1+ chainId))
                    (setq pt nextpt)
                    (prompt "\nContinue point set.")
                  )
                  (prompt "\nContinue cancelled; keeping current point.")
                )
                (command "_.REGEN")
                (setq finishedInput T)
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
        )
      )

      ;; initialize shorten flags
      (setq shortenFlags '())
      (if entStack
        (progn
          (setq i 0)
          (while (< i (length entList))
            (setq shortenFlags (append shortenFlags (list nil)))
            (setq i (1+ i))
          )
        )
      )

      ;; 1) Shorten feature removed (per user request)

      ;; 2) Dimensions with satisfaction loop
      (setq dimMap '())
      (if entStack
        (progn
          (setq dimOff (getdist "\nDimension offset distance <Enter for no dimensions>: "))
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
                  (if (> userV 25.0)
                    (progn
                      (setq ed (entget e)
                            p1 (cdr (assoc 10 ed))
                            p2 (cdr (assoc 11 ed)))
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
                    (setq dimOff (getdist "\nNew dimension offset distance <Enter to cancel dimensions>: "))
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

      ;; 3) Layer assignment + suffix
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
                    (prompt (strcat "\nSelect segments to move to layer \"" lay "\". Press Enter when done."))
                    (setq selSS (ssget '((0 . "LINE"))))
                    (if selSS
                      (progn
                        (setq j 0)
                        (while (< j (sslength selSS))
                          (setq eSel (ssname selSS j))
                          (setq idxSel (g6:index-of eSel entList))
                          (if (not (null idxSel))
                            (progn
                              (g6:setLayerEnt eSel lay)

                              ;; breaks for that line (if any)
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
                          )
                          (setq j (1+ j))
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

      ;; restore sys vars
      (setvar "OSMODE"  oldOsmode)
      (setvar "CMDECHO" oldCmdecho)
    )
  )
  (princ)
)

(princ "\nG6 patched loaded. Run command: G6")
(princ)
