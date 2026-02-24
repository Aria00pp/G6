(defun c:G6 ( / pt angList idx done lenStr len gr key nextpt
                prevPt2 lastLen finishedInput segStack segRec
                scaleFactor oldOsmode oldCmdecho *error* dimOff
                userLen segList userVal e
                doShorten threshold capLen eligibleEnts tmpSegList tmpRec
                sel chosenEnts i entSel chosenEntsOrdered orderedSegs
                curRec seekSeg oldStart oldEnd capDraw liveP1 liveP2 liveAng newEnd delta
                adjMap moveSegs moveRec moveEnt moveEd moveP1 moveP2 newP1 newP2
                updates updPair stackOut stackRec movedAny maxTyped
                ed p1 p2 mid ang nAng dimPt)

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
  (defun pt+ (a b) (mapcar '+ a b))
  (defun pt- (a b) (mapcar '- a b))
  (defun rec-set (rec key val)
    (if (assoc key rec)
      (subst (cons key val) (assoc key rec) rec)
      (cons (cons key val) rec)
    )
  )
  (defun update-seg-in-stack (stack ent newRec / out cur)
    (setq out '())
    (while stack
      (setq cur   (car stack)
            stack (cdr stack)
      )
      (if (= (cdr (assoc 'lineEnt cur)) ent)
        (setq out (cons newRec out))
        (setq out (cons cur out))
      )
    )
    (reverse out)
  )
  (defun refresh-seg-endpoints (stack / out cur ent ed p1 p2)
    (setq out '())
    (while stack
      (setq cur   (car stack)
            stack (cdr stack)
            ent   (cdr (assoc 'lineEnt cur))
            ed    (and ent (entget ent))
            p1    (and ed (cdr (assoc 10 ed)))
            p2    (and ed (cdr (assoc 11 ed)))
      )
      (if (and p1 p2)
        (setq cur (rec-set (rec-set cur 'p1 p1) 'p2 p2))
      )
      (setq out (cons cur out))
    )
    (reverse out)
  )
  (defun add-adj (adj key rec / pair)
    (setq pair (assoc key adj))
    (if pair
      (subst (cons key (cons rec (cdr pair))) pair adj)
      (cons (cons key (list rec)) adj)
    )
  )
  (defun build-adj-map (stack / adj cur p1 p2)
    (setq adj '())
    (while stack
      (setq cur (car stack)
            p1  (cdr (assoc 'p1 cur))
            p2  (cdr (assoc 'p2 cur))
      )
      (if p1 (setq adj (add-adj adj p1 cur)))
      (if p2 (setq adj (add-adj adj p2 cur)))
      (setq stack (cdr stack))
    )
    adj
  )
  (defun downstream-segs (adj startPt skipEnt / queue seenPts seenEnts out node pair rec recs ent rp1 rp2)
    (setq queue   (list startPt)
          seenPts '()
          seenEnts '()
          out     '()
    )
    (while queue
      (setq node  (car queue)
            queue (cdr queue)
      )
      (if (not (member node seenPts))
        (progn
          (setq seenPts (cons node seenPts)
                pair    (assoc node adj)
                recs    (if pair (cdr pair) nil)
          )
          (while recs
            (setq rec  (car recs)
                  recs (cdr recs)
                  ent  (cdr (assoc 'lineEnt rec))
            )
            (if (and (/= ent skipEnt)
                     (not (member ent seenEnts))
                )
              (progn
                (setq seenEnts (cons ent seenEnts)
                      out      (cons rec out)
                      rp1      (cdr (assoc 'p1 rec))
                      rp2      (cdr (assoc 'p2 rec))
                )
                (if rp1 (setq queue (append queue (list rp1))))
                (if rp2 (setq queue (append queue (list rp2))))
              )
            )
          )
        )
      )
    )
    out
  )

  ;; allowed angles: 30, 90, 150, -150, -90, -30
  (setq angList (mapcar 'deg->rad '(30 90 150 -150 -90 -30)))
  (setq idx 0)   ;; start at 30 deg

  ;; scale factor for drawing length
  ;; user 150 -> drawn length = 1.5
  (setq scaleFactor 0.01)

  ;; segment stack for undo and post-processing
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

                    ;; store segment record (newest first) and update current point
                    (setq segRec
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
                    )
                    (setq segStack (cons segRec segStack))
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

                    ;; delete last entity
                    (setq segRec (car segStack))
                    (if (cdr (assoc 'lineEnt segRec))
                      (entdel (cdr (assoc 'lineEnt segRec)))
                    )
                    (setq segStack (cdr segStack))

                    ;; restore previous point and length
                    (setq pt      (cdr (assoc 'p1 segRec))
                          lastLen (cdr (assoc 'drawLen segRec))
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
      ;; After drawing: restore OSNAP first, then manage dimensions
      ;;--------------------------------------------------------
      (setvar "OSMODE" oldOsmode)

      ;; Optional shortening of long lines (this run only)
      (if segStack
        (progn
          (initget "Yes No")
          (setq doShorten (getkword "\nShorten long lines? [Yes/No] <No>: "))
          (if (= doShorten "Yes")
            (progn
              (setq threshold (getreal "\nThreshold <150>: "))
              (if (null threshold) (setq threshold 150.0))
              (setq capLen (getreal "\nCap length <150>: "))
              (if (null capLen) (setq capLen 150.0))

              (setq segStack (refresh-seg-endpoints segStack))
              (setq eligibleEnts '()
                    tmpSegList   segStack
                    maxTyped     0.0
              )
              (while tmpSegList
                (setq tmpRec    (car tmpSegList)
                      tmpSegList (cdr tmpSegList)
                )
                (if (> (cdr (assoc 'userLen tmpRec)) maxTyped)
                  (setq maxTyped (cdr (assoc 'userLen tmpRec)))
                )
                (if (> (cdr (assoc 'userLen tmpRec)) threshold)
                  (setq eligibleEnts (cons (cdr (assoc 'lineEnt tmpRec)) eligibleEnts))
                )
              )

              (prompt
                (strcat
                  "\nG6 shorten: eligible="
                  (itoa (length eligibleEnts))
                  ", maxTyped="
                  (rtos maxTyped 2 2)
                )
              )

              (if eligibleEnts
                (progn
                  (prompt "\nSelect eligible LINE objects to shorten.")
                  (setq sel (ssget "_:L" '((0 . "LINE"))))
                  (setq chosenEnts '())
                  (if sel
                    (progn
                      (setq i 0)
                      (while (< i (sslength sel))
                        (setq entSel (ssname sel i)
                              i      (1+ i)
                        )
                        (if (and (member entSel eligibleEnts)
                                 (not (member entSel chosenEnts))
                            )
                          (setq chosenEnts (cons entSel chosenEnts))
                        )
                      )
                    )
                    (prompt "\nNo lines selected.")
                  )

                  (if chosenEnts
                    (progn
                      (setq chosenEntsOrdered '()
                            orderedSegs      (reverse segStack)
                      )
                      (while orderedSegs
                        (setq tmpRec      (car orderedSegs)
                              orderedSegs (cdr orderedSegs)
                              entSel      (cdr (assoc 'lineEnt tmpRec))
                        )
                        (if (member entSel chosenEnts)
                          (setq chosenEntsOrdered (cons entSel chosenEntsOrdered))
                        )
                      )
                      (setq chosenEntsOrdered (reverse chosenEntsOrdered))

                      (while chosenEntsOrdered
                        (setq entSel (car chosenEntsOrdered)
                              chosenEntsOrdered (cdr chosenEntsOrdered)
                        )

                        (setq segStack (refresh-seg-endpoints segStack))
                        (setq curRec  nil
                              seekSeg segStack
                        )
                        (while seekSeg
                          (if (= (cdr (assoc 'lineEnt (car seekSeg))) entSel)
                            (setq curRec (car seekSeg)
                                  seekSeg nil
                            )
                            (setq seekSeg (cdr seekSeg))
                          )
                        )

                        (if curRec
                          (progn
                            (setq ed (entget entSel))
                            (if ed
                              (progn
                                (setq liveP1    (cdr (assoc 10 ed))
                                      liveP2    (cdr (assoc 11 ed))
                                )
                                (if (and liveP1 liveP2)
                                  (progn
                                    (setq oldStart  liveP1
                                          oldEnd    liveP2
                                          capDraw   (* capLen scaleFactor)
                                          liveAng   (angle liveP1 liveP2)
                                          newEnd    (polar liveP1 liveAng capDraw)
                                          delta     (pt- newEnd oldEnd)
                                    )

                                    (setq ed (subst (cons 11 newEnd) (assoc 11 ed) ed))
                                    (entmod ed)
                                    (entupd entSel)

                                    (setq curRec (rec-set curRec 'p2 newEnd))
                                    (setq curRec (rec-set curRec 'drawLen capDraw))
                                    (setq curRec (rec-set curRec 'shortened T))
                                    (setq segStack (update-seg-in-stack segStack entSel curRec))

                                    (if (not (equal newEnd oldEnd 1e-12))
                                      (progn
                                        (setq segStack (refresh-seg-endpoints segStack))
                                        (setq adjMap   (build-adj-map segStack))
                                        (setq moveSegs (downstream-segs adjMap oldEnd entSel))
                                        (setq updates  '())
                                        (setq movedAny nil)

                                        (while moveSegs
                                          (setq moveRec  (car moveSegs)
                                                moveSegs (cdr moveSegs)
                                                moveEnt  (cdr (assoc 'lineEnt moveRec))
                                                moveEd   (and moveEnt (entget moveEnt))
                                          )
                                          (if moveEd
                                            (progn
                                              (setq moveP1 (cdr (assoc 10 moveEd))
                                                    moveP2 (cdr (assoc 11 moveEd))
                                              )
                                              (if (and moveP1 moveP2)
                                                (progn
                                                  (setq newP1 (pt+ moveP1 delta)
                                                        newP2 (pt+ moveP2 delta)
                                                  )
                                                  (setq moveEd (subst (cons 10 newP1) (assoc 10 moveEd) moveEd))
                                                  (setq moveEd (subst (cons 11 newP2) (assoc 11 moveEd) moveEd))
                                                  (entmod moveEd)
                                                  (entupd moveEnt)
                                                  (setq updates  (cons (cons moveEnt (list newP1 newP2)) updates))
                                                  (setq movedAny T)
                                                )
                                              )
                                            )
                                          )
                                        )

                                        (if movedAny
                                          (progn
                                            (setq stackOut '()
                                                  tmpSegList segStack
                                            )
                                            (while tmpSegList
                                              (setq stackRec   (car tmpSegList)
                                                    tmpSegList (cdr tmpSegList)
                                                    updPair    (assoc (cdr (assoc 'lineEnt stackRec)) updates)
                                              )
                                              (if updPair
                                                (progn
                                                  (setq stackRec (rec-set stackRec 'p1 (car (cdr updPair))))
                                                  (setq stackRec (rec-set stackRec 'p2 (cadr (cdr updPair))))
                                                )
                                              )
                                              (setq stackOut (cons stackRec stackOut))
                                            )
                                            (setq segStack (reverse stackOut))
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
                    (prompt "\nNo eligible lines selected.")
                  )
                )
                (prompt "\nNo lines in this run exceed the threshold.")
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
              (setq segList (reverse segStack))
              (while segList
                (setq segRec   (car segList)
                      segList  (cdr segList)
                      e        (cdr (assoc 'lineEnt segRec))
                      userVal  (cdr (assoc 'userLen segRec))
                )
                ;; only dimension if user-entered value > 25
                (if (and e (> userVal 25.0))
                  (progn
                    (setq ed   (entget e)
                          p1   (cdr (assoc 10 ed))
                          p2   (cdr (assoc 11 ed))
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
