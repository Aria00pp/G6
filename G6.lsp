(defun c:G6 ( / pt angList idx done lenStr len gr key nextpt
                prevPt2 lastLen finishedInput segStack
                scaleFactor oldOsmode oldCmdecho *error* dimOff
                userLen userVal seg segList e ed p1 p2 mid ang nAng dimPt
                shortenAns sel ssN i selEnt drawOrder newStack cumDelta
                lineEnt breakEnts oldEnd targetDrawLen newEnd deltaShort
                dimEnt dimEd dimTxt assignAns chosenLayer keepAssign
                selCount selIdx pickEnt segMatch lyrNames lyrIdx lyrRec
                dclPath dclFd dclId dclResult dclSel segMap errnoVal
                editAns editSel editCnt editIdx editEnt editStr curStr curNum
                continuePt continueOsmode)

  ;;------------------------------------------------------------
  ;; Error handler: restore system variables
  ;;------------------------------------------------------------
  (defun *error* (msg)
    (if oldOsmode  (setvar "OSMODE"  oldOsmode))
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
    (if (and msg
             (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
        )
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  ;; Save current system variables (OSNAP ON at this point)
  (setq oldOsmode  (getvar "OSMODE")
        oldCmdecho (getvar "CMDECHO")
  )
  (setvar "CMDECHO" 0)

  ;;------------------------------------------------------------
  ;; degree <-> radian helpers
  ;;------------------------------------------------------------
  (defun deg->rad (d) (* pi (/ d 180.0)))
  (defun rad->deg (r) (* 180.0 (/ r pi)))

  ;; remove nil values from list (pure AutoLISP)
  (defun rm-nil (lst / out)
    (setq out '())
    (while lst
      (if (car lst) (setq out (cons (car lst) out)))
      (setq lst (cdr lst))
    )
    (reverse out)
  )

  (defun getx (p) (if p (car p) 0.0))
  (defun gety (p) (if p (cadr p) 0.0))
  (defun getz (p) (if (and p (cddr p)) (caddr p) 0.0))

  ;; true if vector delta is effectively nonzero
  (defun nonzero-delta-p (delta / tol)
    (setq tol 1e-9)
    (and delta
         (or (> (abs (getx delta)) tol)
             (> (abs (gety delta)) tol)
             (> (abs (getz delta)) tol)
         )
    )
  )

  (defun add-delta (p d)
    (list (+ (getx p) (getx d)) (+ (gety p) (gety d)) (+ (getz p) (getz d)))
  )

  (defun sub-pts (a b)
    (list (- (getx a) (getx b)) (- (gety a) (gety b)) (- (getz a) (getz b)))
  )

  ;; move a LINE entity by delta, guarded and no-op for tiny deltas
  (defun move-line-by-delta (ent delta / ed sp ep)
    (if (and ent (nonzero-delta-p delta))
      (progn
        (setq ed (entget ent)
              sp (and ed (assoc 10 ed))
              ep (and ed (assoc 11 ed))
        )
        (if (and ed sp ep)
          (progn
            (setq ed (subst (cons 10 (add-delta (cdr sp) delta)) sp ed))
            (setq ed (subst (cons 11 (add-delta (cdr ep) delta)) ep ed))
            (entmod ed)
            (entupd ent)
          )
        )
      )
    )
  )

  (defun move-breaks-by-delta (breakEnts delta / be)
    (if (nonzero-delta-p delta)
      (foreach be (rm-nil breakEnts)
        (move-line-by-delta be delta)
      )
    )
  )

  (defun move-entity-by-delta (ent delta / ed ss)
    (if (and ent (nonzero-delta-p delta))
      (progn
        (setq ed (entget ent))
        (if ed
          (if (= (cdr (assoc 0 ed)) "LINE")
            (move-line-by-delta ent delta)
            (progn
              (setq ss (ssadd ent))
              (if ss
                (command "_.MOVE" ss "" '(0 0 0) delta)
              )
            )
          )
        )
      )
    )
  )

  (defun make-break-markers (sp ep / mid ang nAng m1a m1b m2a m2b mkLen gap b1 b2)
    (setq mid  (mapcar (function (lambda (a b) (/ (+ a b) 2.0))) sp ep)
          ang  (angle sp ep)
          nAng (+ ang (/ pi 2.0))
          mkLen 0.08
          gap   0.06
          m1a (polar (polar mid ang (- gap)) nAng (- (/ mkLen 2.0)))
          m1b (polar (polar mid ang (- gap)) nAng (/ mkLen 2.0))
          m2a (polar (polar mid ang gap)      nAng (- (/ mkLen 2.0)))
          m2b (polar (polar mid ang gap)      nAng (/ mkLen 2.0))
          b1 (entmakex (list (cons 0 "LINE") (cons 10 m1a) (cons 11 m1b)))
          b2 (entmakex (list (cons 0 "LINE") (cons 10 m2a) (cons 11 m2b)))
    )
    (rm-nil (list b1 b2))
  )

  (defun set-entity-layer (ent lay / entData layPair)
    (if (and ent lay)
      (progn
        (setq entData (entget ent))
        (if entData
          (progn
            (setq layPair (assoc 8 entData))
            (if layPair
              (setq entData (subst (cons 8 lay) layPair entData))
              (setq entData (append entData (list (cons 8 lay))))
            )
            (entmod entData)
            (entupd ent)
          )
        )
      )
    )
  )

  (defun set-dim-override-text (dimEnt txt / dimEd)
    (if (and dimEnt txt)
      (progn
        (setq dimEd (entget dimEnt))
        (if dimEd
          (progn
            (if (assoc 1 dimEd)
              (setq dimEd (subst (cons 1 txt) (assoc 1 dimEd) dimEd))
              (setq dimEd (append dimEd (list (cons 1 txt))))
            )
            (entmod dimEd)
            (entupd dimEnt)
          )
        )
      )
    )
  )

  (defun g6-sanitize (txt / i ch code out up)
    (setq up (strcase (if txt txt ""))
          out ""
          i 1
    )
    (while (<= i (strlen up))
      (setq ch (substr up i 1)
            code (ascii ch)
      )
      (if code
        (if (or (and (>= code 48) (<= code 57))
                (and (>= code 65) (<= code 90))
            )
          (setq out (strcat out ch))
          (setq out (strcat out "_"))
        )
        (setq out (strcat out "_"))
      )
      (setq i (1+ i))
    )
    out
  )

  (defun g6-sfx-key (layerName)
    (strcat "G6_SFX_" (g6-sanitize layerName))
  )

  (defun g6-get-sfx (layerName / val)
    (setq val (getenv (g6-sfx-key layerName)))
    (if val val "")
  )

  (defun g6-set-sfx (layerName suffix)
    (setenv (g6-sfx-key layerName) (if suffix suffix ""))
  )

  (defun g6-find-seg-by-line (stk ent / rec out)
    (setq out nil)
    (while (and stk (not out))
      (setq rec (car stk))
      (if (= (cdr (assoc 'lineEnt rec)) ent)
        (setq out rec)
      )
      (setq stk (cdr stk))
    )
    out
  )

  (defun g6-replace-seg-by-line (stk ent newSeg / out rec)
    (setq out '())
    (while stk
      (setq rec (car stk))
      (if (= (cdr (assoc 'lineEnt rec)) ent)
        (setq out (cons newSeg out))
        (setq out (cons rec out))
      )
      (setq stk (cdr stk))
    )
    (reverse out)
  )

  (defun collect-layer-names ( / out rec)
    (setq out '()
          rec (tblnext "LAYER" T)
    )
    (while rec
      (setq out (cons (cdr (assoc 2 rec)) out))
      (setq rec (tblnext "LAYER"))
    )
    (reverse out)
  )

  (defun choose-layer-dialog (layers / result)
    (setq result nil
          dclPath (strcat (getvar "TEMPPREFIX") "g6_layer_pick.dcl")
          dclFd (open dclPath "w")
    )
    (if dclFd
      (progn
        (write-line "g6layerpick : dialog {" dclFd)
        (write-line "  label = \"Select Layer\";" dclFd)
        (write-line "  : list_box { key = \"layer_list\"; width = 40; height = 14; }" dclFd)
        (write-line "  ok_cancel;" dclFd)
        (write-line "}" dclFd)
        (close dclFd)
        (setq dclId (load_dialog dclPath))
        (if (and dclId (>= dclId 0))
          (progn
            (if (new_dialog "g6layerpick" dclId)
              (progn
                (start_list "layer_list")
                (foreach lyrRec layers
                  (add_list lyrRec)
                )
                (end_list)
                (setq dclSel "0")
                (action_tile "layer_list" "(setq dclSel $value)")
                (action_tile "accept" "(done_dialog 1)")
                (action_tile "cancel" "(done_dialog 0)")
                (setq dclResult (start_dialog))
                (if (= dclResult 1)
                  (progn
                    (setq lyrIdx (atoi dclSel))
                    (if (and (>= lyrIdx 0) (< lyrIdx (length layers)))
                      (setq result (nth lyrIdx layers))
                    )
                  )
                )
              )
            )
            (unload_dialog dclId)
          )
        )
      )
    )
    result
  )

  ;; allowed angles: 30, 90, 150, -150, -90, -30
  (setq angList (mapcar 'deg->rad '(30 90 150 -150 -90 -30)))
  (setq idx 0)   ;; start at 30 deg

  ;; scale factor for drawing length
  ;; user 150 -> drawn length = 1.5
  (setq scaleFactor 0.01)

  ;; stack for undo/history (segment records)
  (setq segStack '())

  ;;------------------------------------------------------------
  ;; preview function
  ;;------------------------------------------------------------
  (defun updatePreview ( / curAng curLen curEnd userLen)
    ;; erase previous preview
    (if prevPt2 (grdraw pt prevPt2 0))
    (setq curAng (nth idx angList))
    (if (= lenStr "")
      ;; if no current input, use last scaled length or 1.0
      (setq curLen (if lastLen lastLen 1.0))
      (progn
        (setq userLen (abs (atof lenStr)))         ;; user length, absolute
        (setq curLen (* scaleFactor userLen))      ;; scaled length
      )
    )
    (setq curEnd (polar pt curAng curLen))
    (grdraw pt curEnd 1)         ;; draw preview
    (setq prevPt2 curEnd)
  )

  ;;------------------------------------------------------------
  ;; main interaction
  ;;------------------------------------------------------------

  ;; OSNAP is still in user's original mode here
  (setq pt (getpoint "\nStart point: "))
  (if pt
    (progn
      ;; After start point picked, disable OSNAP for the segment construction
      (setvar "OSMODE" 0)

      (setq done      nil
            lastLen   1.0
            prevPt2   nil
      )
      (while (not done)
        (setq lenStr ""
              finishedInput nil
        )

        (prompt
          (strcat
            "\nCurrent angle = "
            (rtos (rad->deg (nth idx angList)) 2 0)
            "  | Enter length (scale 0.01; TAB = angle, C = continue from point, U = undo last, ENTER = finish): "
          )
        )

        (updatePreview)

        (while (not finishedInput)
          (setq gr (grread T 1 0))
          (cond
            ;; keyboard input
            ((= (car gr) 2)
             (setq key (cadr gr))
             (cond
               ;; TAB -> change angle
               ((= key 9)
                (setq idx (1+ idx))
                (if (>= idx (length angList)) (setq idx 0))
                (prompt
                  (strcat
                    "\nAngle: "
                    (rtos (rad->deg (nth idx angList)) 2 0)
                  )
                )
                (updatePreview)
               )

               ;; ENTER
               ((= key 13)
                (if (= lenStr "")
                  (progn
                    ;; finish command
                    (if prevPt2 (grdraw pt prevPt2 0))
                    (setq prevPt2 nil
                          done    T
                          finishedInput T
                    )
                  )
                  (progn
                    ;; compute user length and scaled length
                    (setq userLen (abs (atof lenStr)))                 ;; raw user input
                    (setq len     (* scaleFactor userLen))             ;; scaled & positive

                    (if prevPt2 (grdraw pt prevPt2 0))
                    (setq prevPt2 nil)

                    ;; draw line (OSNAP is OFF, so no interference)
                    (setq nextpt (polar pt (nth idx angList) len))
                    (command "_.LINE" pt nextpt "")

                    ;; store segment record for undo and dimensions
                    (setq segStack
                      (cons
                        (list
                          (cons 'p1 pt)
                          (cons 'p2 nextpt)
                          (cons 'lineEnt (entlast))
                          (cons 'drawLen len)
                          (cons 'userLen userLen)
                          (cons 'userLenStr lenStr)
                          (cons 'shortened nil)
                          (cons 'breakEnts nil)
                          (cons 'dimEnt nil)
                        )
                        segStack
                      )
                    )

                    ;; update current point
                    (setq pt       nextpt
                          lastLen  len
                          finishedInput T
                    )

                    ;; redraw graphics, so the line appears normally
                    (command "_.REGEN")
                  )
                )
               )

               ;; U or u => undo last segment
               ((or (= key 85) (= key 117))
                (if segStack
                  (progn
                    ;; remove preview
                    (if prevPt2 (grdraw pt prevPt2 0))
                    (setq prevPt2 nil)

                    ;; top segment
                    (setq seg (car segStack))

                    ;; delete last entity
                    (if (cdr (assoc 'lineEnt seg))
                      (entdel (cdr (assoc 'lineEnt seg)))
                    )

                    ;; pop stack
                    (setq segStack (cdr segStack))

                    ;; restore previous point and length
                    (setq pt      (cdr (assoc 'p1 seg))
                          lastLen (cdr (assoc 'drawLen seg))
                    )

                    (prompt "\nLast segment undone.")
                    (updatePreview)
                    (command "_.REGEN")
                  )
                  (prompt "\nNo segment to undo.")
                )
               )

               ;; C or c => continue from another point
               ((or (= key 67) (= key 99))
                (if prevPt2 (grdraw pt prevPt2 0))
                (setq prevPt2 nil)
                (setq continueOsmode (getvar "OSMODE"))
                (setvar "OSMODE" oldOsmode)
                (setq continuePt (getpoint "\nContinue from point: "))
                (setvar "OSMODE" continueOsmode)
                (if continuePt
                  (setq pt continuePt)
                )
                (updatePreview)
               )

               ;; Backspace => delete last character of length
               ((= key 8)
                (if (> (strlen lenStr) 0)
                  (progn
                    (setq lenStr (substr lenStr 1 (1- (strlen lenStr))))
                    (prompt
                      (strcat
                        "\rLength (input): "
                        lenStr
                        " "
                      )
                    )
                    (updatePreview)
                  )
                )
               )

               ;; digits 0–9
               ((and (>= key 48) (<= key 57))
                (setq lenStr (strcat lenStr (chr key)))
                (prompt
                  (strcat
                    "\rLength (input): "
                    lenStr
                  )
                )
                (updatePreview)
               )

               ;; decimal point '.'
               ((= key 46)
                (setq lenStr (strcat lenStr "."))
                (prompt
                  (strcat
                    "\rLength (input): "
                    lenStr
                  )
                )
                (updatePreview)
               )

               ;; minus sign '-'  -> ignored to avoid direction flip
               ((= key 45)
                (prompt "\nNegative length is not allowed; value kept positive.")
               )
             )
            )
          )
        )
      )

      ;;--------------------------------------------------------
      ;; After drawing: restore OSNAP first, then manage dimensions
      ;;--------------------------------------------------------
      (setvar "OSMODE" oldOsmode)

      (initget "Yes No")
      (setq editAns (getkword "\nEdit typed measurements? [Yes/No] <No>: "))
      (if (= editAns "Yes")
        (progn
          (setq editSel (ssget '((0 . "LINE"))))
          (if editSel
            (progn
              (setq editCnt (sslength editSel)
                    editIdx 0
              )
              (while (< editIdx editCnt)
                (setq editEnt (ssname editSel editIdx)
                      seg (g6-find-seg-by-line segStack editEnt)
                )
                (if seg
                  (progn
                    (setq curStr (cdr (assoc 'userLenStr seg))
                          curNum (cdr (assoc 'userLen seg))
                    )
                    (if (or (not curStr) (= curStr ""))
                      (setq curStr (rtos curNum 2 8))
                    )
                    (setq editStr (getstring T (strcat "\nNew typed length for selected segment <" curStr ">: ")))
                    (if (and editStr (> (strlen editStr) 0))
                      (progn
                        (setq seg (subst (cons 'userLenStr editStr) (assoc 'userLenStr seg) seg))
                        (setq seg (subst (cons 'userLen (abs (atof editStr))) (assoc 'userLen seg) seg))
                        (setq segStack (g6-replace-seg-by-line segStack editEnt seg))

                        (setq dimEnt (cdr (assoc 'dimEnt seg)))
                        (if dimEnt
                          (progn
                            (setq userVal (cdr (assoc 'userLen seg))
                                  userLen (cdr (assoc 'userLenStr seg))
                            )
                            (if (or (not userLen) (= userLen ""))
                              (setq userLen (rtos userVal 2 8))
                            )
                            (setq dimEd (entget dimEnt)
                                  chosenLayer (if dimEd (cdr (assoc 8 dimEd)) nil)
                                  dimTxt (if chosenLayer (g6-get-sfx chosenLayer) "")
                            )
                            (set-dim-override-text dimEnt (strcat userLen dimTxt))
                          )
                        )
                      )
                    )
                  )
                )
                (setq editIdx (1+ editIdx))
              )
            )
          )
        )
      )

      (initget "Yes No")
      (setq editAns (getkword "\nApply edited measurements to geometry? [Yes/No] <Yes>: "))
      (if (or (not editAns) (= editAns "Yes"))
        (progn
          (setq drawOrder (reverse segStack)
                cumDelta '(0.0 0.0 0.0)
                newStack '()
          )
          (while drawOrder
            (setq seg      (car drawOrder)
                  drawOrder (cdr drawOrder)
                  lineEnt  (cdr (assoc 'lineEnt seg))
                  breakEnts (cdr (assoc 'breakEnts seg))
                  dimEnt   (cdr (assoc 'dimEnt seg))
            )

            ;; move current entities first so chain continuity is preserved
            (move-entity-by-delta lineEnt cumDelta)
            (move-breaks-by-delta breakEnts cumDelta)
            (move-entity-by-delta dimEnt cumDelta)

            ;; refresh live endpoints after movement
            (setq ed (and lineEnt (entget lineEnt))
                  p1 (and ed (cdr (assoc 10 ed)))
                  p2 (and ed (cdr (assoc 11 ed)))
            )
            (if p1 (setq seg (subst (cons 'p1 p1) (assoc 'p1 seg) seg)))
            (if p2 (setq seg (subst (cons 'p2 p2) (assoc 'p2 seg) seg)))

            (if (and p1 p2 lineEnt)
              (progn
                (setq userVal (cdr (assoc 'userLen seg)))
                (setq len (distance p1 p2))
                (setq targetDrawLen (* userVal scaleFactor))

                (if (and (not (cdr (assoc 'shortened seg)))
                         (> targetDrawLen 0.0)
                         (> (abs (- targetDrawLen len)) 1e-6)
                    )
                  (progn
                    (setq oldEnd p2
                          ang (angle p1 p2)
                          newEnd (polar p1 ang targetDrawLen)
                          deltaShort (sub-pts newEnd oldEnd)
                    )
                    (if (assoc 11 ed)
                      (progn
                        (setq ed (subst (cons 11 newEnd) (assoc 11 ed) ed))
                        (entmod ed)
                        (entupd lineEnt)
                        (setq seg (subst (cons 'p2 newEnd) (assoc 'p2 seg) seg))
                        (setq seg (subst (cons 'drawLen targetDrawLen) (assoc 'drawLen seg) seg))
                        (if (and dimEnt (nonzero-delta-p deltaShort))
                          (move-entity-by-delta dimEnt deltaShort)
                        )
                        (setq cumDelta (add-delta cumDelta deltaShort))
                      )
                    )
                  )
                )
              )
            )
            (setq newStack (cons seg newStack))
          )
          (setq segStack newStack)
        )
      )

      (initget "Yes No")
      (setq shortenAns (getkword "\nShorten selected long lines to 150? [Yes/No] <No>: "))
      (if (= shortenAns "Yes")
        (progn
          (setq sel (ssget '((0 . "LINE"))))
          (if (and sel segStack)
            (progn
              (setq targetDrawLen (* 150.0 scaleFactor)
                    drawOrder (reverse segStack)
                    newStack '()
                    cumDelta '(0.0 0.0 0.0)
              )
              (while drawOrder
                (setq seg      (car drawOrder)
                      drawOrder (cdr drawOrder)
                      lineEnt  (cdr (assoc 'lineEnt seg))
                      breakEnts (cdr (assoc 'breakEnts seg))
                )

                ;; move current segment and existing markers by cumulative delta
                (move-line-by-delta lineEnt cumDelta)
                (move-breaks-by-delta breakEnts cumDelta)

                ;; refresh from live entity
                (setq ed (and lineEnt (entget lineEnt))
                      p1 (and ed (cdr (assoc 10 ed)))
                      p2 (and ed (cdr (assoc 11 ed)))
                )
                (if p1 (setq seg (subst (cons 'p1 p1) (assoc 'p1 seg) seg)))
                (if p2 (setq seg (subst (cons 'p2 p2) (assoc 'p2 seg) seg)))

                ;; selected + this run + long user length
                (setq ssN (if sel (sslength sel) 0)
                      i 0
                      selEnt nil
                )
                (while (and (< i ssN) (not selEnt))
                  (if (= (ssname sel i) lineEnt)
                    (setq selEnt lineEnt)
                  )
                  (setq i (1+ i))
                )

                (if (and selEnt p1 p2 (> (cdr (assoc 'userLen seg)) 150.0))
                  (progn
                    (setq oldEnd p2
                          ang (angle p1 p2)
                          newEnd (polar p1 ang targetDrawLen)
                          deltaShort (sub-pts newEnd oldEnd)
                    )
                    (if (assoc 11 ed)
                      (progn
                        (setq ed (subst (cons 11 newEnd) (assoc 11 ed) ed))
                        (entmod ed)
                        (entupd lineEnt)
                        (setq seg (subst (cons 'p2 newEnd) (assoc 'p2 seg) seg))
                        (setq seg (subst (cons 'drawLen targetDrawLen) (assoc 'drawLen seg) seg))
                        (setq seg (subst (cons 'shortened T) (assoc 'shortened seg) seg))
                        (setq breakEnts (make-break-markers p1 newEnd))
                        (setq seg (subst (cons 'breakEnts breakEnts) (assoc 'breakEnts seg) seg))
                        (setq cumDelta (add-delta cumDelta deltaShort))
                      )
                    )
                  )
                )
                (setq newStack (cons seg newStack))
              )
              (setq segStack newStack)
            )
          )
        )
      )

      ;; Ask for dimension offset and create aligned dimensions
      (if segStack
        (progn
          (setq dimOff (getdist "\nDimension offset distance <Enter for no dimensions>: "))
          (if dimOff
            (progn
              ;; walk segments in draw order
              (setq segList (reverse segStack)
                    newStack '()
              )
              (while segList
                (setq seg     (car segList)
                      segList (cdr segList)
                      userVal (cdr (assoc 'userLen seg))
                      dimEnt nil
                )
                ;; only dimension if user-entered value > 25
                (if (> userVal 25.0)
                  (progn
                    ;; critical parity: use live LINE endpoints from entget
                    (setq e  (cdr (assoc 'lineEnt seg))
                          ed (and e (entget e))
                          p1 (and ed (cdr (assoc 10 ed)))
                          p2 (and ed (cdr (assoc 11 ed)))
                    )
                    (if (and p1 p2)
                      (progn
                        ;; midpoint of segment
                        (setq mid (mapcar
                                    (function (lambda (a b) (/ (+ a b) 2.0)))
                                    p1 p2
                                  )
                        )
                        ;; angle and perpendicular direction
                        (setq ang   (angle p1 p2)
                              nAng  (+ ang (/ pi 2.0))   ;; +90 degrees
                              dimPt (polar mid nAng dimOff)
                        )
                        ;; create aligned dimension
                        (command "_.DIMALIGNED" p1 p2 dimPt "")
                        (setq dimEnt (entlast))
                        (if dimEnt
                          (progn
                            (setq dimTxt (cdr (assoc 'userLenStr seg)))
                            (if (or (not dimTxt) (= dimTxt ""))
                              (setq dimTxt (rtos userVal 2 8))
                            )
                            (set-dim-override-text dimEnt dimTxt)
                            (if (assoc 'dimEnt seg)
                              (setq seg (subst (cons 'dimEnt dimEnt) (assoc 'dimEnt seg) seg))
                              (setq seg (append seg (list (cons 'dimEnt dimEnt))))
                            )
                          )
                        )
                      )
                    )
                  )
                )
                (setq newStack (cons seg newStack))
              )
              (setq segStack newStack)
            )
          )
        )
      )

      (if segStack
        (progn
          (initget "Yes No")
          (setq assignAns (getkword "\nAssign layers to lines and dimensions? [Yes/No] <No>: "))
          (if (= assignAns "Yes")
            (progn
              (setq lyrNames (collect-layer-names)
                    keepAssign T
                    segMap '()
              )
              (foreach seg segStack
                (setq segMap (cons (cons (cdr (assoc 'lineEnt seg)) seg) segMap))
              )
              (while (and keepAssign lyrNames)
                (setq chosenLayer (choose-layer-dialog lyrNames))
                (if (not chosenLayer)
                  (setq keepAssign nil)
                  (progn
                    (initget "Keep Set Clear")
                    (setq dimTxt (getkword (strcat "\nSuffix for layer \"" chosenLayer "\" [Keep/Set/Clear] <Keep>: ")))
                    (if (= dimTxt "Set")
                      (progn
                        (setq userLen (getstring T "\nEnter suffix to append (exactly as desired): "))
                        (if userLen
                          (g6-set-sfx chosenLayer userLen)
                        )
                      )
                    )
                    (if (= dimTxt "Clear")
                      (g6-set-sfx chosenLayer "")
                    )
                    (setq dimTxt (g6-get-sfx chosenLayer))
                    (setq sel (ssget '((0 . "LINE"))))
                    (if (not sel)
                      (progn
                        (setq errnoVal (getvar "ERRNO"))
                        (if (= errnoVal 52)
                          (setq keepAssign nil)
                        )
                      )
                      (progn
                        (setq selCount (sslength sel)
                              selIdx 0
                        )
                        (while (< selIdx selCount)
                          (setq pickEnt (ssname sel selIdx)
                                segMatch (assoc pickEnt segMap)
                          )
                          (if segMatch
                            (progn
                              (set-entity-layer (cdr (assoc 'lineEnt (cdr segMatch))) chosenLayer)
                              (foreach be (rm-nil (cdr (assoc 'breakEnts (cdr segMatch))))
                                (set-entity-layer be chosenLayer)
                              )
                              (set-entity-layer (cdr (assoc 'dimEnt (cdr segMatch))) chosenLayer)
                              (setq dimEnt (cdr (assoc 'dimEnt (cdr segMatch))))
                              (if dimEnt
                                (progn
                                  (setq userVal (cdr (assoc 'userLen (cdr segMatch)))
                                        userLen (cdr (assoc 'userLenStr (cdr segMatch)))
                                  )
                                  (if (or (not userLen) (= userLen ""))
                                    (setq userLen (rtos userVal 2 8))
                                  )
                                  (set-dim-override-text dimEnt (strcat userLen dimTxt))
                                )
                              )
                            )
                          )
                          (setq selIdx (1+ selIdx))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  ;; restore system variables on normal end
  (setvar "OSMODE"  oldOsmode)
  (setvar "CMDECHO" oldCmdecho)

  (princ)
)
