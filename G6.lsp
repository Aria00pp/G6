(defun c:G6 ( / pt angList idx done lenStr len gr key nextpt
                prevPt2 lastLen finishedInput segStack
                scaleFactor oldOsmode oldCmdecho *error* dimOff
                userLen userVal seg segList e ed p1 p2 mid ang nAng dimPt
                ans ss i selEnts targetDrawLen cumDelta newSegs delta segRec
                breakGap breakHalf marker1 marker2 along1 along2 perp pA pB
                shortenedP2 deltaShort dimStr dimEnt)

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
  ;; helper functions
  ;;------------------------------------------------------------
  (defun deg->rad (d) (* pi (/ d 180.0)))
  (defun rad->deg (r) (* 180.0 (/ r pi)))

  (defun v+ (a b) (mapcar '+ a b))
  (defun v- (a b) (mapcar '- a b))
  (defun v* (s v) (mapcar '(lambda (x) (* s x)) v))

  (defun move-line-by-delta (ent d / ed p10 p11)
    (if (and ent d)
      (progn
        (setq ed (entget ent)
              p10 (cdr (assoc 10 ed))
              p11 (cdr (assoc 11 ed))
        )
        (if (and p10 p11)
          (progn
            (setq ed (subst (cons 10 (v+ p10 d)) (assoc 10 ed) ed))
            (setq ed (subst (cons 11 (v+ p11 d)) (assoc 11 ed) ed))
            (entmod ed)
            (entupd ent)
          )
        )
      )
    )
  )

  (defun seg-selected-p (ent picked / found)
    (setq found nil)
    (while (and picked (not found))
      (if (= ent (car picked))
        (setq found T)
      )
      (setq picked (cdr picked))
    )
    found
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
            "  | Enter length (scale 0.01; TAB = angle, U = undo last, ENTER = finish): "
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
      ;; After drawing: restore OSNAP first, then optional shorten + dimensions
      ;;--------------------------------------------------------
      (setvar "OSMODE" oldOsmode)

      ;; Ask whether to shorten long lines
      (if segStack
        (progn
          (initget "Yes No")
          (setq ans (getkword "\nShorten selected long lines to 150? [Yes/No] <No>: "))
          (if (= ans "Yes")
            (progn
              (setq ss (ssget '((0 . "LINE"))))
              (if ss
                (progn
                  (setq selEnts '()
                        i 0
                  )
                  (while (< i (sslength ss))
                    (setq selEnts (cons (ssname ss i) selEnts)
                          i (1+ i)
                    )
                  )

                  (setq targetDrawLen (* scaleFactor 150.0)
                        cumDelta      '(0.0 0.0 0.0)
                        segList       (reverse segStack)
                        newSegs       '()
                        breakGap      0.12
                        breakHalf     0.08
                  )

                  (while segList
                    (setq seg     (car segList)
                          segList (cdr segList)
                    )

                    ;; move this segment (and markers) by accumulated delta
                    (if cumDelta
                      (progn
                        (move-line-by-delta (cdr (assoc 'lineEnt seg)) cumDelta)
                        (foreach e (cdr (assoc 'breakEnts seg))
                          (move-line-by-delta e cumDelta)
                        )
                      )
                    )

                    ;; refresh endpoints after translation
                    (setq e  (cdr (assoc 'lineEnt seg))
                          ed (and e (entget e))
                          p1 (and ed (cdr (assoc 10 ed)))
                          p2 (and ed (cdr (assoc 11 ed)))
                    )

                    ;; shorten selected long segments that belong to this command
                    (if (and p1 p2
                             (seg-selected-p e selEnts)
                             (> (cdr (assoc 'userLen seg)) 150.0)
                        )
                      (progn
                        (setq ang        (angle p1 p2)
                              shortenedP2 (polar p1 ang targetDrawLen)
                              deltaShort  (v- shortenedP2 p2)
                        )

                        ;; set new end point (keep start fixed)
                        (setq ed (subst (cons 11 shortenedP2) (assoc 11 ed) ed))
                        (entmod ed)
                        (entupd e)

                        ;; create simple || marker near midpoint
                        (setq mid   (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 shortenedP2)
                              along1 (polar mid ang (* -0.5 breakGap))
                              along2 (polar mid ang (*  0.5 breakGap))
                              perp   (+ ang (/ pi 2.0))
                              pA     (polar along1 perp breakHalf)
                              pB     (polar along1 perp (- breakHalf))
                              marker1
                                (entmakex
                                  (list
                                    '(0 . "LINE")
                                    (cons 10 pA)
                                    (cons 11 pB)
                                  )
                                )
                              pA     (polar along2 perp breakHalf)
                              pB     (polar along2 perp (- breakHalf))
                              marker2
                                (entmakex
                                  (list
                                    '(0 . "LINE")
                                    (cons 10 pA)
                                    (cons 11 pB)
                                  )
                                )
                        )

                        (setq seg (subst (cons 'p1 p1) (assoc 'p1 seg) seg))
                        (setq seg (subst (cons 'p2 shortenedP2) (assoc 'p2 seg) seg))
                        (setq seg (subst (cons 'drawLen targetDrawLen) (assoc 'drawLen seg) seg))
                        (setq seg (subst (cons 'shortened T) (assoc 'shortened seg) seg))
                        (setq seg (subst (cons 'breakEnts (vl-remove nil (list marker1 marker2))) (assoc 'breakEnts seg) seg))

                        ;; subsequent connected segments must follow
                        (setq cumDelta (v+ cumDelta deltaShort))
                      )
                      (progn
                        ;; keep segment endpoints synchronized if only translated
                        (if (and p1 p2)
                          (progn
                            (setq seg (subst (cons 'p1 p1) (assoc 'p1 seg) seg))
                            (setq seg (subst (cons 'p2 p2) (assoc 'p2 seg) seg))
                          )
                        )
                      )
                    )

                    (setq newSegs (cons seg newSegs))
                  )

                  ;; restore stack newest-first
                  (setq segStack newSegs)
                  (command "_.REGEN")
                )
              )
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
                    newSegs '()
              )
              (while segList
                (setq seg     (car segList)
                      segList (cdr segList)
                      userVal (cdr (assoc 'userLen seg))
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
                        (if (and (cdr (assoc 'shortened seg)) dimEnt)
                          (progn
                            (setq dimStr (cdr (assoc 'userLenStr seg)))
                            (if (or (not dimStr) (= dimStr ""))
                              (setq dimStr (rtos userVal 2 8))
                            )
                            (setq ed (entget dimEnt))
                            (if (assoc 1 ed)
                              (setq ed (subst (cons 1 dimStr) (assoc 1 ed) ed))
                              (setq ed (append ed (list (cons 1 dimStr))))
                            )
                            (entmod ed)
                            (entupd dimEnt)
                          )
                        )
                        (setq seg (subst (cons 'dimEnt dimEnt) (assoc 'dimEnt seg) seg))
                      )
                    )
                  )
                )
                (setq newSegs (cons seg newSegs))
              )
              (setq segStack newSegs)
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
