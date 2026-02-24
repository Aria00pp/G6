(defun c:G6 ( / pt angList idx done lenStr len gr key nextpt
                prevPt2 lastLen finishedInput segStack
                scaleFactor oldOsmode oldCmdecho *error* dimOff
                userLen userVal seg segList e ed p1 p2 mid ang nAng dimPt
                shortAns ss i se cumDelta newSegs oldEnd newEnd delta
                targetLen drawTarget midS bMid1 bMid2 b1s b1e b2s b2e
                breakEnts beList be userLenStr dimStr)

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

  ;;------------------------------------------------------------
  ;; helpers
  ;;------------------------------------------------------------
  (defun rm-nil (lst / out)
    (setq out '())
    (while lst
      (if (car lst)
        (setq out (cons (car lst) out))
      )
      (setq lst (cdr lst))
    )
    (reverse out)
  )

  (defun nonzero-delta-p (d / tol)
    (setq tol 1e-9)
    (if (and d
             (or (> (abs (car d)) tol)
                 (> (abs (cadr d)) tol)
                 (> (abs (caddr d)) tol)
             )
        )
      T
      nil
    )
  )

  (defun v+ (a b)
    (list (+ (car a) (car b)) (+ (cadr a) (cadr b)) (+ (caddr a) (caddr b)))
  )

  (defun v- (a b)
    (list (- (car a) (car b)) (- (cadr a) (cadr b)) (- (caddr a) (caddr b)))
  )

  (defun move-line-by-delta (ent d / ed p10 p11)
    (if (and ent (nonzero-delta-p d))
      (progn
        (setq ed (entget ent)
              p10 (and ed (assoc 10 ed))
              p11 (and ed (assoc 11 ed))
        )
        (if (and ed p10 p11)
          (progn
            (setq ed (subst (cons 10 (v+ (cdr p10) d)) p10 ed))
            (setq ed (subst (cons 11 (v+ (cdr p11) d)) p11 ed))
            (entmod ed)
            (entupd ent)
          )
        )
      )
    )
  )

  (defun ent-in-ss-p (ent ss / j found)
    (setq j 0
          found nil
    )
    (if (and ent ss)
      (while (and (< j (sslength ss)) (not found))
        (if (= ent (ssname ss j))
          (setq found T)
        )
        (setq j (1+ j))
      )
    )
    found
  )

  (defun seg-set (s k v / out pr done)
    (setq out '()
          done nil
    )
    (while s
      (setq pr (car s))
      (if (= (car pr) k)
        (progn
          (setq out (cons (cons k v) out))
          (setq done T)
        )
        (setq out (cons pr out))
      )
      (setq s (cdr s))
    )
    (if (not done)
      (setq out (cons (cons k v) out))
    )
    (reverse out)
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
                    (setq userLenStr lenStr)

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
                          (cons 'userLenStr userLenStr)
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

      (setq shortAns (getkword "\nShorten selected long lines to 150? [Yes/No] <No>: "))
      (if (= shortAns "Yes")
        (progn
          (setq ss (ssget '((0 . "LINE"))))
          (if (and ss segStack)
            (progn
              (setq segList (reverse segStack)
                    cumDelta '(0.0 0.0 0.0)
                    newSegs '()
                    targetLen 150.0
                    drawTarget (* targetLen scaleFactor)
              )
              (while segList
                (setq seg     (car segList)
                      segList (cdr segList)
                )

                ;; move this segment (and markers) by accumulated delta
                (if (nonzero-delta-p cumDelta)
                  (progn
                    (move-line-by-delta (cdr (assoc 'lineEnt seg)) cumDelta)
                    (setq beList (cdr (assoc 'breakEnts seg)))
                    (while beList
                      (setq be (car beList))
                      (move-line-by-delta be cumDelta)
                      (setq beList (cdr beList))
                    )
                    (setq seg (seg-set seg 'p1 (v+ (cdr (assoc 'p1 seg)) cumDelta)))
                    (setq seg (seg-set seg 'p2 (v+ (cdr (assoc 'p2 seg)) cumDelta)))
                  )
                )

                ;; shorten only selected lines from this command with userLen > 150
                (if (and (ent-in-ss-p (cdr (assoc 'lineEnt seg)) ss)
                         (> (cdr (assoc 'userLen seg)) 150.0)
                    )
                  (progn
                    (setq e  (cdr (assoc 'lineEnt seg))
                          ed (and e (entget e))
                          p1 (and ed (cdr (assoc 10 ed)))
                          p2 (and ed (cdr (assoc 11 ed)))
                    )
                    (if (and p1 p2)
                      (progn
                        (setq oldEnd p2
                              ang    (angle p1 p2)
                              newEnd (polar p1 ang drawTarget)
                        )
                        (setq ed (subst (cons 11 newEnd) (assoc 11 ed) ed))
                        (entmod ed)
                        (entupd e)

                        ;; break symbol "||" near midpoint using entmakex
                        (setq midS  (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 newEnd)
                              bMid1 (polar midS ang 0.03)
                              bMid2 (polar midS ang -0.03)
                              nAng  (+ ang (/ pi 2.0))
                              b1s   (polar bMid1 nAng 0.05)
                              b1e   (polar bMid1 (+ nAng pi) 0.05)
                              b2s   (polar bMid2 nAng 0.05)
                              b2e   (polar bMid2 (+ nAng pi) 0.05)
                        )
                        (setq breakEnts
                          (rm-nil
                            (list
                              (entmakex (list (cons 0 "LINE") (cons 10 b1s) (cons 11 b1e)))
                              (entmakex (list (cons 0 "LINE") (cons 10 b2s) (cons 11 b2e)))
                            )
                          )
                        )

                        (setq seg (seg-set seg 'p2 newEnd))
                        (setq seg (seg-set seg 'shortened T))
                        (setq seg (seg-set seg 'breakEnts breakEnts))

                        ;; accumulate delta for all subsequent segments
                        (setq delta (v- newEnd oldEnd))
                        (if (nonzero-delta-p delta)
                          (setq cumDelta (v+ cumDelta delta))
                        )
                      )
                    )
                  )
                )

                (setq newSegs (cons seg newSegs))
              )
              (setq segStack (reverse newSegs))
              (command "_.REGEN")
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
              (setq segList (reverse segStack))
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
                        (setq seg (seg-set seg 'dimEnt (entlast)))
                        (if (and (cdr (assoc 'shortened seg))
                                 (cdr (assoc 'dimEnt seg))
                            )
                          (progn
                            (setq dimStr (cdr (assoc 'userLenStr seg)))
                            (if (or (not dimStr) (= dimStr ""))
                              (setq dimStr (rtos userVal 2 8))
                            )
                            (setq ed (entget (cdr (assoc 'dimEnt seg))))
                            (if (assoc 1 ed)
                              (setq ed (subst (cons 1 dimStr) (assoc 1 ed) ed))
                              (setq ed (append ed (list (cons 1 dimStr))))
                            )
                            (entmod ed)
                            (entupd (cdr (assoc 'dimEnt seg)))
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
  )

  ;; restore system variables on normal end
  (setvar "OSMODE"  oldOsmode)
  (setvar "CMDECHO" oldCmdecho)

  (princ)
)
